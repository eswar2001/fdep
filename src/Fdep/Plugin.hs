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
    GRHSs (grhssGRHSs),
    GenLocated (L),
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
  ( LHsBindLR,
  )
import GhcPlugins (Var (varName), getOccString, unLoc, Plugin (pluginRecompile), PluginRecompile (..))
import HscTypes (ModSummary (..))
import Name (nameStableString)
import Plugins (CommandLineOption, Plugin (typeCheckResultAction), defaultPlugin)
import TcRnTypes (TcGblEnv (..), TcM)
import Prelude hiding (id,writeFile)
import Data.Aeson
import Data.ByteString.Lazy (writeFile)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

plugin :: Plugin
plugin = defaultPlugin {
    typeCheckResultAction = fDep
    , pluginRecompile = purePlugin
    }

purePlugin :: [CommandLineOption] -> IO PluginRecompile
purePlugin _ = return NoForceRecompile

basePath :: IO (Maybe String)
basePath = lookupEnv "FDEP_DIR"

fDep :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
fDep _ modSummary tcEnv = do
  let modulePath = moduleNameString $ moduleName $ ms_mod modSummary
  basePath' <- liftIO basePath
  depsMapList <- liftIO $ mapM loopOverLHsBindLR $ bagToList $ tcg_binds tcEnv
  liftIO $ do
      createDirectoryIfMissing True (fromMaybe "./fdep/" basePath')
      print ("generated dependancy for module: " <> modulePath)
      writeFile ((fromMaybe "./fdep/" basePath') <> modulePath <> ".json") (encode depsMapList)
  return tcEnv

loopOverLHsBindLR :: LHsBindLR GhcTc GhcTc -> IO [(String, [Maybe String])]
loopOverLHsBindLR (L _ (FunBind _ id matches _ _)) = do
  -- print ("FunBind" :: String)
  let funName = getOccString $ unLoc id
      matchList = mg_alts matches
      list = map processMatch (unLoc matchList)
  pure [(funName, concat list)]
loopOverLHsBindLR (L _ (PatBind _ _ pat_rhs _)) = do
  -- print ("patBind" :: String)
  let l = concatMap processGRHS $ grhssGRHSs pat_rhs
  pure [("", l)]
loopOverLHsBindLR (L _ VarBind {var_rhs = rhs}) = do
  -- print ("varBind" :: String)
  pure [("", processExpr rhs)]
loopOverLHsBindLR (L _ AbsBinds {abs_binds = binds}) = do
  -- print ("absBind" :: String)
  list <- mapM loopOverLHsBindLR $ bagToList binds
  pure (concat list)
loopOverLHsBindLR (L _ (PatSynBind _ PSB {psb_def = def})) = do
  -- print ("patSynBind PSB" :: String)
  let list = map (Just . traceShowId . nameStableString) $ processPat def
  pure [("", list)]
loopOverLHsBindLR (L _ (PatSynBind _ (XPatSynBind _))) = do
  -- print ("patSynBind XPatSynBind" :: String)
  pure []
loopOverLHsBindLR (L _ (XHsBindsLR _)) = do
  -- print ("XHsBindsLR" :: String)
  pure []

processMatch :: LMatch GhcTc (LHsExpr GhcTc) -> [Maybe String]
processMatch (L _ match) =
  let grhss = m_grhss match
   in concatMap processGRHS $ grhssGRHSs grhss

processGRHS :: LGRHS GhcTc (LHsExpr GhcTc) -> [Maybe String]
processGRHS (L _ (GRHS _ _ body)) = processExpr body
processGRHS _ = []

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