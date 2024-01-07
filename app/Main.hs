module Main (main) where

import Lib
import System.Environment
import System.Exit
import System.Process

getEditor :: Maybe String -> String
getEditor (Just editor) = editor
getEditor Nothing = "vi"

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
      maybeEditor <- lookupEnv "EDITOR"
      let command = getEditor maybeEditor ++ " " ++ path
      (_, _, _, processHandle) <- createProcess (shell command)
      exitCode <- waitForProcess processHandle
      case exitCode of
        ExitSuccess -> do
          exitSuccess
        ExitFailure _ -> do
          putStrLn "Could not open inventory in editor"
          exitFailure
    Consume itemId -> do
      inventory <- loadInventory
      let newInventory = consume inventory itemId
      saveInventory newInventory
