module Main (main) where

import System.Process
import System.Exit
import Lib

main :: IO ()
main = do
  parsedArgs <- getParsedArgs
  case parsedArgs of
    Add description itemDate quantity itemValue itemPrice itemCategory -> do
      inventory <- loadInventory
      date <- parseDateOrCurrent itemDate
      let newInventory = addItem description itemValue itemPrice date quantity itemCategory inventory
      saveInventory newInventory
    Remove itemId -> do
      inventory <- loadInventory
      let newInventory = removeItem itemId inventory
      saveInventory newInventory
    Value -> do
      inventory <- loadInventory
      putStrLn $ show $ totalValue inventory
    Count -> do
      inventory <- loadInventory
      putStrLn $ show $ count inventory
    Edit itemId -> do
      path <- appendToPath "inventory.yml"
      let command = "nvim +/\"id: " ++ show itemId ++ "\" " ++ path
      (_, _, _, processHandle) <- createProcess (shell command)
      exitCode <- waitForProcess processHandle
      case exitCode of
        ExitSuccess -> putStrLn $ "Opened file at id: " ++ show itemId
        ExitFailure _ -> putStrLn "Could not open inventory in NeoVim"
    Consume itemId -> do
      inventory <- loadInventory
      let newInventory = consume inventory itemId
      saveInventory newInventory
