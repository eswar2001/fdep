module Fdep.Types where
import Data.Aeson

data FunctionInfo = FunctionInfo
  { package_name :: String
  , module_name :: String
  , name    :: String
  } deriving (Show, Eq, Ord)

data Function = Function
  {  function_name    :: String
  , functions_called :: [Maybe FunctionInfo]
  , where_functions :: [Function]
  } deriving (Show, Eq, Ord)

instance ToJSON FunctionInfo where
  toJSON (FunctionInfo pkg modName funcName) =
    object [ "package_name" .= pkg
           , "module_name"  .= modName
           , "name"         .= funcName
           ]

instance ToJSON Function where
  toJSON (Function funcName funcsCalled whereFuncs) =
    object [ "function_name"    .= funcName
           , "functions_called" .= funcsCalled
           , "where_functions"  .= whereFuncs
           ]

instance FromJSON FunctionInfo where
  parseJSON = withObject "FunctionInfo" $ \v ->
    FunctionInfo <$> v .: "package_name"
                 <*> v .: "module_name"
                 <*> v .: "name"

instance FromJSON Function where
  parseJSON = withObject "Function" $ \v ->
    Function <$> v .: "function_name"
             <*> v .: "functions_called"
             <*> v .: "where_functions"