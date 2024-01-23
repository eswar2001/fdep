{-# LANGUAGE RankNTypes #-}
module Main (main) where

import qualified Fdep.Plugin ()

main :: IO ()
main = do
    putStrLn "Test suite not yet implemented."
    print ("HI there" :: String)