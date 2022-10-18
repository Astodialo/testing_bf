{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Main
  where

import Blockfrost.Client
import Control.Lens
import Blockfrost.Lens
import qualified Data.Text as T
import Control.Monad
import Data.Aeson.Types
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Either
import Blockfrost.Pretty.Ada
import Servant.Docs (toSample)
import Money (someDiscreteCurrency, someDiscreteAmount, UnitScale)

main = do
  putStrLn "1. Address Assets"
  putStrLn "2. Asset Adresses"
  putStrLn "CHOOSE!"
  choice <- getChar
  case choice of 
    '1' -> addrTok
    '2' -> assAddr
    otherwise -> print "BOOOOO!!!!!"


assAddr :: IO ()
assAddr = do
  putStrLn "Give AssetID:" 
  ass <- getLine
  let addr =  mkAddress "addr_test1qq9nxds4hzf7t0w8u438rpkecsndgr4vhja557epdvvkcwcrslr6z7k4cu83jrg63ap36a7p9wld2hkt8ef3ev37dwmsv4hxlt"
  prj <- projectFromEnv
  res <- runBlockfrost prj $ getAssetAddresses (mkAssetId $ T.pack ass)
  case res of
    Left err  -> putStrLn "AssetID doesnt exist"
    Right res -> print (cleanAssAddr res)


cleanAssAddr :: [AssetAddress] -> [(T.Text, Integer)]
cleanAssAddr res = zip (fmap (unAddress . _assetAddressAddress) res) (fmap (unQuantity . _assetAddressQuantity) res)


addrTok :: IO ()
addrTok = do
  putStrLn "Give Address:" 
  addr <- getLine
  prj <- projectFromEnv
  res <- runBlockfrost prj $ getAddressInfo (mkAddress $ T.pack addr)
  case res of
    Left err  -> print "Address doesnt exist"
    Right res -> do
      let tkns = res ^. amount
  ---    let ppTkns =  fmap ppTok tkns
      print (fmap ppTok tkns)
-- getAddrTok :: [AddressInfo] -> a

ppTok :: Amount -> T.Text
ppTok tkn = case tkn of
    AdaAmount  lvl ->  T.pack (show lvl)
    AssetAmount tok -> T.append (T.append (someDiscreteCurrency tok) " : ") (T.pack (show (someDiscreteAmount tok)))     

