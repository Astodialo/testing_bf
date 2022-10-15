{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
  where

import Blockfrost.Client
import Control.Lens
import Blockfrost.Lens
import Data.Text
import Control.Monad
import Data.Aeson.Types
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

main = do
  putStrLn "Give AssetID:" 
  ass <- getLine
  let addr =  mkAddress "addr_test1qq9nxds4hzf7t0w8u438rpkecsndgr4vhja557epdvvkcwcrslr6z7k4cu83jrg63ap36a7p9wld2hkt8ef3ev37dwmsv4hxlt"
  prj <- projectFromEnv
  res <- runBlockfrost prj $ getAssetAddresses (mkAssetId $ pack ass)
  print res
 
