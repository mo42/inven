module Main (main) where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("add" : "-t" : description : "-v" : valueStr : "-d" : dateStr : []) -> do
      let value = read valueStr :: Float
      inventory <- loadInventory
      date <- parseDateOrCurrent dateStr
      let newInventory = addItem description value date inventory
      saveInventory newInventory

    ("remove" : itemIDStr : []) -> do
      let itemID = read itemIDStr :: Int
      inventory <- loadInventory
      let newInventory = removeItem itemID inventory
      saveInventory newInventory

    _ -> putStrLn "Invalid arguments"
