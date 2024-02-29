{-# LANGUAGE DataKinds #-}

module Fdep.Plugin (plugin) where

import Bag (bagToList)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Reference (biplateRef, (^?))
import Data.Generics.Uniplate.Data ()
import Data.List (nub)
import Debug.Trace (traceShowId)
import GHC
  ( GRHS (..),
    GRHSs (..),
    GenLocated (L),
    HsValBinds (..),
    GhcTc,
    HsBindLR (..),
    HsConDetails (..),
    HsConPatDetails,
    HsExpr (..),
    HsRecField' (..),
    HsRecFields (..),
    LGRHS,
    LHsExpr,
    LHsRecField,
    LMatch,
    LPat,
    Match (m_grhss),
    MatchGroup (..),
    Name,
    Pat (..),
    PatSynBind (..),
    noLoc, Module (moduleName), moduleNameString
  )
import GHC.Hs.Binds
import GhcPlugins (Var (varName), getOccString, unLoc, Plugin (pluginRecompile), PluginRecompile (..),showSDocUnsafe,ppr)
import HscTypes (ModSummary (..))
import Name (nameStableString)
import Plugins (CommandLineOption, Plugin (typeCheckResultAction), defaultPlugin)
import TcRnTypes (TcGblEnv (..), TcM)
import Prelude hiding (id,writeFile)
import Data.Aeson
import Data.ByteString.Lazy (writeFile)
import System.Directory (createDirectoryIfMissing,getHomeDirectory)
import Data.Maybe (fromMaybe)
import Control.Exception (try,SomeException)
import SrcLoc
import Annotations
import Outputable ()
import GhcPlugins ()
import DynFlags ()
import Control.Monad (foldM)
import Data.List
import Data.List.Extra (replace,splitOn)
import Data.Maybe (fromJust,isJust)
import Fdep.Types
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Concurrent
import System.Directory
plugin :: Plugin
plugin = defaultPlugin {
    typeCheckResultAction = fDep
    , pluginRecompile = purePlugin
    }

purePlugin :: [CommandLineOption] -> IO PluginRecompile
purePlugin _ = return NoForceRecompile

fDep :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
fDep _ modSummary tcEnv = do
  liftIO $ forkIO $ do
      let moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
      let modulePath = "/tmp/fdep/" <> ms_hspp_file modSummary
      depsMapList <- mapM loopOverLHsBindLR $ bagToList $ tcg_binds tcEnv
      let path = (intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
      print ("generated dependancy for module: " <> moduleName' <> " at path: " <> path)
      createDirectoryIfMissing True path
      writeFile ((modulePath) <> ".json") (encodePretty $ concat depsMapList)
  return tcEnv

transformFromNameStableString :: Maybe String -> Maybe FunctionInfo
transformFromNameStableString (Just str) =
  let parts = filter (\x -> x /= "") $ splitOn ("$") str
  in Just $ if length parts == 2 then  FunctionInfo "" (parts !! 0) (parts !! 1) else FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2)

loopOverLHsBindLR :: LHsBindLR GhcTc GhcTc -> IO [Function]
loopOverLHsBindLR (L _ (FunBind _ id matches _ _)) = do
  let funName = getOccString $ unLoc id
      matchList = mg_alts matches
  (list,funcs) <- foldM (\(x,y) xx -> do
                                  (l,f) <- processMatch xx
                                  pure $ (x <> l,y <> f)
                        ) ([],[]) (unLoc matchList)
  let listTransformed = map transformFromNameStableString $ nub $ list
  pure [(Function funName listTransformed (nub funcs))]
loopOverLHsBindLR (L _ (PatBind _ _ pat_rhs _)) = do
  let l = map transformFromNameStableString $ concatMap processGRHS $ grhssGRHSs pat_rhs
  pure [(Function "" l [])]
loopOverLHsBindLR (L _ VarBind {var_rhs = rhs}) = do
  pure [(Function "" (map transformFromNameStableString $ processExpr rhs) [])]
loopOverLHsBindLR (L _ AbsBinds {abs_binds = binds}) = do
  list <- mapM loopOverLHsBindLR $ bagToList binds
  pure (concat list)
loopOverLHsBindLR (L _ (PatSynBind _ PSB {psb_def = def})) = do
  let list = map transformFromNameStableString $ map (Just . traceShowId . nameStableString) $ processPat def
  pure [(Function "" list [])]
loopOverLHsBindLR (L _ (PatSynBind _ (XPatSynBind _))) = do
  pure []
loopOverLHsBindLR (L _ (XHsBindsLR _)) = do
  pure []

processMatch :: LMatch GhcTc (LHsExpr GhcTc) -> IO ([Maybe String],[Function])
processMatch (L _ match) = do
  let grhss = m_grhss match
  whereClause <- processHsLocalBinds $ unLoc $ grhssLocalBinds grhss
  pure $ (concatMap processGRHS $ grhssGRHSs grhss,whereClause)

processGRHS :: LGRHS GhcTc (LHsExpr GhcTc) -> [Maybe String]
processGRHS (L _ (GRHS _ _ body)) = processExpr body
processGRHS _ = []

processHsLocalBinds :: HsLocalBindsLR GhcTc GhcTc -> IO [Function]
processHsLocalBinds (HsValBinds _ (ValBinds _ x y)) = do
  res <- mapM loopOverLHsBindLR $ bagToList $ x
  pure $ concat res
processHsLocalBinds (HsValBinds _ (XValBindsLR (NValBinds x y))) = do
  res <- foldM (\acc (recFlag,binds) -> do 
                  funcs <- mapM loopOverLHsBindLR $ bagToList binds
                  pure (acc <> funcs)
                ) [] x
  pure $ concat res
processHsLocalBinds x =
  pure []

processExpr :: LHsExpr GhcTc -> [Maybe String]
processExpr (L _ (HsVar _ (L _ var))) =
  let name = nameStableString $ varName var
   in [Just name]
processExpr (L _ (HsUnboundVar _ _)) = []
processExpr (L _ (HsApp _ funl funr)) =
  processExpr funl <> processExpr funr
processExpr (L _ (OpApp _ funl funm funr)) =
  processExpr funl <> processExpr funm <> processExpr funr
processExpr (L _ (NegApp _ funl _)) =
  processExpr funl
processExpr (L _ (HsTick _ _ fun)) =
  processExpr fun
processExpr (L _ (HsStatic _ fun)) =
  processExpr fun
processExpr (L _ (HsWrap _ _ fun)) =
  processExpr (noLoc fun)
processExpr (L _ (HsBinTick _ _ _ fun)) =
  processExpr fun
processExpr (L _ (ExplicitList _ _ funList)) =
  concatMap processExpr funList
processExpr (L _ (HsTickPragma _ _ _ _ fun)) =
  processExpr fun
processExpr (L _ (HsSCC _ _ _ fun)) =
  processExpr fun
processExpr (L _ (HsCoreAnn _ _ _ fun)) =
  processExpr fun
processExpr (L _ (ExprWithTySig _ fun _)) =
  processExpr fun
processExpr (L _ (HsDo _ _ exprLStmt)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in nub $ concatMap processExpr stmts
processExpr (L _ (HsLet _ exprLStmt func)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in processExpr func <> nub (concatMap processExpr stmts)
processExpr (L _ (HsMultiIf _ exprLStmt)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsIf _ exprLStmt funl funm funr)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr $ [funl, funm, funr] <> stmts)
processExpr (L _ (HsCase _ funl exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr $ [funl] <> stmts)
processExpr (L _ (ExplicitSum _ _ _ fun)) = processExpr fun
processExpr (L _ (SectionR _ funl funr)) = processExpr funl <> processExpr funr
processExpr (L _ (ExplicitTuple _ exprLStmt _)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (RecordUpd _ rupd_expr rupd_flds)) = processExpr rupd_expr <> concatMap extractLHsRecUpdField rupd_flds
processExpr (L _ (HsPar _ fun)) = processExpr fun
processExpr (L _ (HsAppType _ fun _)) = processExpr fun
processExpr (L _ (HsLamCase _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsLam _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsLit _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsOverLit _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsRecFld _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmts)
processExpr (L _ (HsSpliceE exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
processExpr (L _ (ArithSeq _ (Just exprLStmtL) exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
processExpr (L _ (ArithSeq _ Nothing exprLStmtR)) =
  let stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr stmtsR)
processExpr (L _ (HsRnBracketOut _ exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
processExpr (L _ (HsTcBracketOut _ exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap processExpr (stmtsL <> stmtsR))
-- HsIPVar (XIPVar p) HsIPName
-- HsOverLabel (XOverLabel p) (Maybe (IdP p)) FastString
-- HsConLikeOut (XConLikeOut p) ConLike
processExpr _ = []

extractLHsRecUpdField :: GenLocated l (HsRecField' id (LHsExpr GhcTc)) -> [Maybe String]
extractLHsRecUpdField (L _ (HsRecField _ fun _)) = processExpr fun

processPat :: LPat GhcTc -> [Name]
processPat (L _ pat) = case pat of
  ConPatIn _ details -> processDetails details
  VarPat _ (L _ var) -> [varName var]
  ParPat _ pat' -> processPat pat'
  _ -> []

processDetails :: HsConPatDetails GhcTc -> [Name]
processDetails (PrefixCon args) = concatMap processPat args
processDetails (InfixCon arg1 arg2) = processPat arg1 <> processPat arg2
processDetails (RecCon rec) = concatMap processPatField (rec_flds rec)

processPatField :: LHsRecField GhcTc (LPat GhcTc) -> [Name]
processPatField (L _ HsRecField {hsRecFieldArg = arg}) = processPat arg