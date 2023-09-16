module Main (main) where

import Lib

main :: IO ()
main = do
  parsedArgs <- getParsedArgs
  case parsedArgs of
    Add description itemDate itemValue -> do
      inventory <- loadInventory
      date <- parseDateOrCurrent itemDate
      let newInventory = addItem description itemValue date inventory
      saveInventory newInventory
    Remove itemId -> do
      inventory <- loadInventory
      let newInventory = removeItem itemId inventory
      saveInventory newInventory
    Value -> do
      inventory <- loadInventory
      putStrLn $ show $ totalValue inventory
