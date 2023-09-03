module Main (main) where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("add" : "-d" : description : "-v" : valueStr : []) -> do
      let value = read valueStr :: Float
      inventory <- loadInventory
      let newInventory = addItem description value inventory
      saveInventory newInventory

    ("remove" : itemIDStr : []) -> do
      let itemID = read itemIDStr :: Int
      inventory <- loadInventory
      let newInventory = removeItem itemID inventory
      saveInventory newInventory

    _ -> putStrLn "Invalid arguments"
