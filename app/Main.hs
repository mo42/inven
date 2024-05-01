module Main (main) where

import Data.List (intercalate)
import Data.Maybe
import Lib
import System.Environment
import System.Exit
import System.Process
import Text.Printf

main :: IO ()
main = do
  inventory <- loadInventory
  parsedArgs <- getParsedArgs
  case parsedArgs of
    Add description itemDate quantity itemValue itemPrice itemCategory itemContainer itemLocation itemExpiry -> do
      date <- parseDateOrCurrent itemDate
      let newInventory = addItem description itemValue itemPrice date quantity itemCategory itemContainer itemLocation (parseMaybeDate itemExpiry) inventory
      saveInventory newInventory
    Remove itemId -> do
      let newInventory = removeItem itemId inventory
      saveInventory newInventory
    Value -> do
      print (totalValue inventory)
    Count -> do
      print (length inventory)
    Edit -> do
      path <- appendToPath "inventory.yml"
      maybeEditor <- lookupEnv "EDITOR"
      let command = fromMaybe "vi" maybeEditor ++ " " ++ path
      (_, _, _, processHandle) <- createProcess (shell command)
      exitCode <- waitForProcess processHandle
      case exitCode of
        ExitSuccess -> do
          exitSuccess
        ExitFailure _ -> do
          putStrLn "Could not open inventory in editor"
          exitFailure
    Consume itemId -> do
      let newInventory = consume inventory itemId
      saveInventory newInventory
    Prune -> do
      let newInventory = prune inventory
      saveInventory newInventory
      printf "Pruned %d items with zero quantity.\n" (length inventory - length newInventory)
    Show itemId -> do
      let maybeItem = findItemById inventory itemId
      case maybeItem of
        Just item -> putStr $ formatItem item
        Nothing -> putStrLn "Not found"
    Find regex -> do
      let matchedItems = findItemsByRegex regex inventory
      putStrLn headerLine
      putStr $ intercalate "" $ map formatItemShort matchedItems
