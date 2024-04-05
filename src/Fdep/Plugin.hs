{-# LANGUAGE DataKinds,ViewPatterns,FlexibleContexts,TypeFamilies #-}

module Fdep.Plugin (plugin) where

import Bag (bagToList,listToBag)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Reference (biplateRef, (^?))
import Data.Generics.Uniplate.Data ()
import Data.List (nub)
import GHC
  ( GRHS (..),
    GRHSs (..),
    OutputableBndrId,
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
    noLoc, Module (moduleName), moduleNameString,Id(..),getName,nameSrcSpan,IdP(..),GhcPass
  )
import GHC.Hs.Binds
import GhcPlugins (pprPrefixOcc,idName,Var (varName), getOccString, unLoc, Plugin (pluginRecompile), PluginRecompile (..),showSDocUnsafe,ppr,elemNameSet,pprPrefixName,idType,tidyOpenType)
import HscTypes (ModSummary (..))
import Name (nameStableString)
import Plugins (CommandLineOption, Plugin (typeCheckResultAction), defaultPlugin)
import TcRnTypes (TcGblEnv (..), TcM)
import Prelude hiding (id,writeFile)
import Data.Aeson
import Data.ByteString.Lazy (writeFile)
import System.Directory (createDirectoryIfMissing,getHomeDirectory)
import Data.Maybe (fromMaybe,catMaybes)
import Control.Exception (try,SomeException)
import SrcLoc
import Annotations
import Outputable ()
import GhcPlugins ()
import DynFlags ()
import Control.Monad (foldM,when)
import Data.List
import Data.List.Extra (replace,splitOn)
import Data.Maybe (fromJust,isJust,mapMaybe)
import Fdep.Types
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Concurrent
import System.Directory
import PatSyn
import Avail
import TcEnv
import GHC.Hs.Utils as GHCHs
import TyCoPpr ( pprUserForAll, pprTypeApp, pprSigmaType )
import Data.Bool (bool)
import qualified Data.Map as Map
import Data.Data (toConstr)
import GHC.Hs.Expr (unboundVarOcc)

plugin :: Plugin
plugin = defaultPlugin {
    typeCheckResultAction = fDep
    , pluginRecompile = purePlugin
    }

purePlugin :: [CommandLineOption] -> IO PluginRecompile
purePlugin _ = return NoForceRecompile

fDep :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
fDep opts modSummary tcEnv = do
  liftIO $ forkIO $ do
      let prefixPath = case opts of
                            []    -> "/tmp/fdep/"
                            local : _ -> local
          moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
          modulePath = prefixPath <> ms_hspp_file modSummary
      depsMapList <- mapM loopOverLHsBindLR $ bagToList $ tcg_binds tcEnv
      let path = (intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
      print ("generated dependancy for module: " <> moduleName' <> " at path: " <> path)
      createDirectoryIfMissing True path
      writeFile ((modulePath) <> ".json") (encodePretty $ concat depsMapList)
      writeFile ((modulePath) <> ".missing.signatures.json") $
        encodePretty
          $ Map.fromList
            $ map (\element -> (\(x,y) -> (x,typeSignature y)) $ filterForMaxLenTypSig element)
              $ groupBy (\a b -> (srcSpan a) == (srcSpan b))
                $ dumpMissingTypeSignatures tcEnv
  return tcEnv
    where
      filterForMaxLenTypSig :: [MissingTopLevelBindsSignature] -> (String, MissingTopLevelBindsSignature)
      filterForMaxLenTypSig x =
          case x of
            [el] -> (srcSpan $ el,el)
            [el1,el2] -> (srcSpan el1,bool (el2) (el1) ((length $ typeSignature $ el1) > (length $ typeSignature $ el2)))
            (xx:xs) -> (\(y,yy) -> (srcSpan xx,bool (yy) (xx) ((length $ typeSignature $ xx) > (length $ typeSignature $ yy)))) $ filterForMaxLenTypSig xs

dumpMissingTypeSignatures :: TcGblEnv -> [MissingTopLevelBindsSignature]
dumpMissingTypeSignatures gbl_env =
  let binds    =  (collectHsBindsBinders $ tcg_binds $ gbl_env)
      whereBinds  = concatMap (\x -> ((concatMap collectHsBindsBinders $ processHsLocalBindsForWhereFunctions $ unLoc $ processMatchForWhereFunctions x)))  ((bagToList $ tcg_binds $ gbl_env) ^? biplateRef :: [LMatch GhcTc (LHsExpr GhcTc)])
  in nub $ mapMaybe add_bind_warn (binds <> whereBinds)
  where
    add_bind_warn :: Id -> Maybe MissingTopLevelBindsSignature
    add_bind_warn id
      = let name    = idName id
            ty = (idType id)
            ty_msg  = pprSigmaType ty
        in add_warn (showSDocUnsafe $ ppr $ nameSrcSpan $ getName name) (showSDocUnsafe $ pprPrefixName name) (showSDocUnsafe $ ppr $ ty_msg)

    add_warn "<no location info>" msg ty_msg = Nothing
    add_warn "<wired into compiler>" msg ty_msg = Nothing
    add_warn _ msg "*" = Nothing
    add_warn _ msg "* -> *" = Nothing
    add_warn _ msg ('_':xs) = Nothing
    add_warn name msg ty_msg
      = if "$" `isPrefixOf` msg
          then Nothing
          else Just $ MissingTopLevelBindsSignature { srcSpan = (name), typeSignature = (msg <> " :: " <> ty_msg)}

    processMatchForWhereFunctions :: LMatch GhcTc (LHsExpr GhcTc) -> LHsLocalBinds GhcTc
    processMatchForWhereFunctions (L _ match) = (grhssLocalBinds (m_grhss match))

    processHsLocalBindsForWhereFunctions :: HsLocalBindsLR GhcTc GhcTc -> [LHsBindsLR GhcTc GhcTc]
    processHsLocalBindsForWhereFunctions (HsValBinds _ (ValBinds _ x _)) = [x]
    processHsLocalBindsForWhereFunctions (HsValBinds _ (XValBindsLR (NValBinds x _))) = map (\(_,binds) -> binds) $ x
    processHsLocalBindsForWhereFunctions x = []

transformFromNameStableString :: (Maybe String,Maybe String,[String]) -> Maybe FunctionInfo
transformFromNameStableString (Just str,Just loc,args) =
  let parts = filter (\x -> x /= "") $ splitOn ("$") str
  in Just $ if length parts == 2 then  FunctionInfo "" (parts !! 0) (parts !! 1) loc args else FunctionInfo (parts !! 0) (parts !! 1) (parts !! 2) loc args

filterFunctionInfos :: [Maybe FunctionInfo] -> [FunctionInfo]
filterFunctionInfos infos =
    let grouped = groupBy (\info1 info2 -> src_Loc info1 == src_Loc info2 && name info1 == name info2) $ catMaybes infos
    in concat $ map (\group -> concat $ map (\x -> if (null $ arguments x) then [] else [x]) group) grouped

loopOverLHsBindLR :: LHsBindLR GhcTc GhcTc -> IO [Function]
loopOverLHsBindLR (L _ (FunBind _ id matches _ _)) = do
  let funName = getOccString $ unLoc id
      matchList = mg_alts matches
  (list,funcs) <- foldM (\(x,y) xx -> do
                                  (l,f) <- processMatch xx
                                  pure $ (x <> l,y <> f)
                        ) ([],[]) (unLoc matchList)
  let listTransformed = map (\x -> Just x) $ filterFunctionInfos $ map transformFromNameStableString list
  pure [(Function funName listTransformed (nub funcs) (showSDocUnsafe $ ppr $ getLoc id))]
loopOverLHsBindLR (L _ (PatBind _ _ pat_rhs _)) = do
  let l = map transformFromNameStableString $ concatMap processGRHS $ grhssGRHSs pat_rhs
  pure [(Function "" l [] "")]
loopOverLHsBindLR (L _ VarBind {var_rhs = rhs}) = do
  pure [(Function "" (map transformFromNameStableString $ processExpr [] rhs) [] "")]
loopOverLHsBindLR (L _ AbsBinds {abs_binds = binds}) = do
  list <- mapM loopOverLHsBindLR $ bagToList binds
  pure (concat list)
loopOverLHsBindLR (L _ (PatSynBind _ PSB {psb_def = def})) = do
  let list = map transformFromNameStableString $ map (\(n,srcLoc) -> (Just $ nameStableString n, srcLoc,[])) $ processPat def
  pure [(Function "" list [] "")]
loopOverLHsBindLR (L _ (PatSynBind _ (XPatSynBind _))) = do
  pure []
loopOverLHsBindLR (L _ (XHsBindsLR _)) = do
  pure []

processMatch :: LMatch GhcTc (LHsExpr GhcTc) -> IO ([(Maybe String,Maybe String,[String])],[Function])
processMatch (L _ match) = do
  whereClause <- processHsLocalBinds $ unLoc $ grhssLocalBinds (m_grhss match)
  pure $ (concatMap processGRHS $ grhssGRHSs (m_grhss match),whereClause)

processGRHS :: LGRHS GhcTc (LHsExpr GhcTc) -> [(Maybe String,Maybe String,[String])]
processGRHS (L _ (GRHS _ _ body)) = 
    let r = processExpr [] body
    in r
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

processArgs (funr) = case funr of
                (HsUnboundVar _ uv) -> [showSDocUnsafe $ pprPrefixOcc (unboundVarOcc uv)]
                (HsConLikeOut _ c)  -> [showSDocUnsafe $ pprPrefixOcc c]
                (HsIPVar _ v)       -> [showSDocUnsafe $ ppr v]
                (HsOverLabel _ _ l) -> [showSDocUnsafe $ ppr l]
                (HsLit _ lit)       -> [showSDocUnsafe $ ppr lit]
                (HsOverLit _ lit)   -> [showSDocUnsafe $ ppr lit]
                (HsPar _ e)         -> [showSDocUnsafe $ ppr e]
                (HsApp _ funl funr) -> processArgs (unLoc funr) <> processArgs (unLoc funl)
                _ -> []

processExpr :: [String] -> LHsExpr GhcTc -> [(Maybe String,Maybe String,[String])]
processExpr arguments x@(L _ (HsVar _ (L _ var))) =
  let name = nameStableString $ varName var
  in [(Just name,Just $ showSDocUnsafe $ ppr $ getLoc $ x,arguments)]
processExpr arguments (L _ (HsUnboundVar _ _)) = []
processExpr arguments (L _ (HsApp _ funl funr)) =
  if (show $ toConstr $ unLoc funr) `elem` ["HsOverLabel","HsIPVar","HsPar","HsLit","HsOverLit","HsUnboundVar","HsConLikeOut"]
    then
      let processedArgs = nub $ processArgs (unLoc funr) <> arguments
          l = processExpr processedArgs funl
          r = processExpr arguments funr
      in l
    else
      let l = processExpr [] funl
          r = processExpr arguments funr
      in l <> r
processExpr arguments (L _ (OpApp _ funl funm funr)) =
  let l = processExpr arguments funl
      m = processExpr arguments funm
      r = processExpr arguments funr
  in nub $ l <> m <> r
processExpr arguments (L _ (NegApp _ funl _)) =
  processExpr arguments funl
processExpr arguments (L _ (HsTick _ _ fun)) =
  processExpr arguments fun
processExpr arguments (L _ (HsStatic _ fun)) =
  processExpr arguments fun
processExpr arguments (L _ x@(HsWrap _ _ fun)) =
  let r = processArgs x
  in processExpr (arguments <> r) (noLoc fun)
processExpr arguments (L _ (HsBinTick _ _ _ fun)) =
  processExpr arguments fun
processExpr arguments (L _ (ExplicitList _ _ funList)) =
  concatMap (processExpr arguments) funList
processExpr arguments (L _ (HsTickPragma _ _ _ _ fun)) =
  processExpr arguments fun
processExpr arguments (L _ (HsSCC _ _ _ fun)) =
  processExpr arguments fun
processExpr arguments (L _ (HsCoreAnn _ _ _ fun)) =
  processExpr arguments fun
processExpr arguments (L _ (ExprWithTySig _ fun _)) =
  processExpr arguments fun
processExpr arguments (L _ (HsDo _ _ exprLStmt)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in nub $ concatMap (\x -> 
                            let processedArgs = processArgs (unLoc x)
                            in processExpr (processedArgs) x
                      ) stmts
processExpr arguments (L _ (HsLet _ exprLStmt func)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in processExpr arguments func <> nub (concatMap (processExpr arguments) stmts)
processExpr arguments (L _ (HsMultiIf _ exprLStmt)) =
  let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
   in nub (concatMap (processExpr arguments) stmts)
processExpr arguments (L _ (HsIf _ exprLStmt funl funm funr)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap (processExpr arguments) $ [funl, funm, funr] <> stmts)
processExpr arguments (L _ (HsCase _ funl exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap (processExpr arguments) $ [funl] <> stmts)
processExpr arguments (L _ (ExplicitSum _ _ _ fun)) = processExpr arguments fun
processExpr arguments (L _ (SectionR _ funl funr)) = processExpr arguments funl <> processExpr arguments funr
processExpr arguments (L _ (ExplicitTuple _ exprLStmt _)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap (processExpr arguments) stmts)
processExpr arguments (L _ (RecordUpd _ rupd_expr rupd_flds)) = (processExpr arguments rupd_expr) <> concatMap extractLHsRecUpdField rupd_flds
processExpr arguments (L _ (HsPar _ fun)) =
  let processedArgs = processArgs (unLoc fun)
  in processExpr (processedArgs) fun
processExpr arguments (L _ (HsAppType _ fun _)) = processExpr arguments fun
processExpr arguments (L _ x@(HsLamCase _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
      processedArgs = processArgs (x)
      res = nub (concatMap (\x ->
                            let processedArgs = processArgs (unLoc x)
                            in processExpr (processedArgs) x
                            ) 
                            stmts
                )
  in case res of
      [(x,y,[])] -> [(x,y,processedArgs)]
      _ -> res
processExpr arguments (L _ x@(HsLam _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
      processedArgs = processArgs (x)
      res = nub (concatMap (\x ->
                            let processedArgs = processArgs (unLoc x)
                            in processExpr (processedArgs <> arguments) x
                            ) 
                            stmts
                )
  in case res of
      [(x,y,[])] -> [(x,y,processedArgs)]
      _ -> res
processExpr arguments (L _ x@(HsLit _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
      processedArgs = processArgs (x)
      res = nub (concatMap (\x ->
                            let processedArgs = processArgs (unLoc x)
                            in processExpr (processedArgs) x
                            ) 
                            stmts
                )
  in case res of
      [(x,y,[])] -> [(x,y,processedArgs)]
      _ -> res
processExpr arguments (L _ x@(HsOverLit _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
      processedArgs = processArgs (x)
      res = nub (concatMap (\x -> 
                          let processedArgs = processArgs (unLoc x)
                          in processExpr (processedArgs <> arguments) x
                      ) stmts)
  in case res of
      [(x,y,[])] -> [(x,y,processedArgs)]
      _ -> res
processExpr arguments (L _ (HsRecFld _ exprLStmt)) =
  let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap (processExpr arguments) stmts)
processExpr arguments (L _ (HsSpliceE exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap (processExpr arguments) (stmtsL <> stmtsR))
processExpr arguments (L _ (ArithSeq _ (Just exprLStmtL) exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap (processExpr arguments) (stmtsL <> stmtsR))
processExpr arguments (L _ (ArithSeq _ Nothing exprLStmtR)) =
  let stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap (processExpr arguments) stmtsR)
processExpr arguments (L _ (HsRnBracketOut _ exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap (processExpr arguments) (stmtsL <> stmtsR))
processExpr arguments (L _ (HsTcBracketOut _ exprLStmtL exprLStmtR)) =
  let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
      stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
   in nub (concatMap (processExpr arguments) (stmtsL <> stmtsR))
-- HsIPVar (XIPVar p) HsIPName
-- HsOverLabel (XOverLabel p) (Maybe (IdP p)) FastString
-- HsConLikeOut (XConLikeOut p) ConLike
processExpr arguments _ = []

extractLHsRecUpdField :: GenLocated l (HsRecField' id (LHsExpr GhcTc)) -> [(Maybe String,Maybe String,[String])]
extractLHsRecUpdField (L _ (HsRecField _ fun _)) = 
    let r = processExpr [] fun
    in r

processPat :: LPat GhcTc -> [(Name,Maybe String)]
processPat (L _ pat) = case pat of
  ConPatIn _ details -> processDetails details
  VarPat _ x@(L _ var) -> [(varName var,Just $ showSDocUnsafe $ ppr $ getLoc $ x)]
  ParPat _ pat' -> processPat pat'
  _ -> []

processDetails :: HsConPatDetails GhcTc -> [(Name,Maybe String)]
processDetails (PrefixCon args) = concatMap processPat args
processDetails (InfixCon arg1 arg2) = processPat arg1 <> processPat arg2
processDetails (RecCon rec) = concatMap processPatField (rec_flds rec)

processPatField :: LHsRecField GhcTc (LPat GhcTc) -> [(Name,Maybe String)]
processPatField (L _ HsRecField {hsRecFieldArg = arg}) = processPat arg