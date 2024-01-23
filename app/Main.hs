module Main (main) where

import Data.List
import Lib
import System.Environment
import System.Exit
import System.Process
import Text.Printf

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
            print (totalValue inventory)
        Count -> do
            inventory <- loadInventory
            print (length inventory)
        Edit -> do
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
        Prune -> do
            inventory <- loadInventory
            let newInventory = prune inventory
            saveInventory newInventory
            printf "Pruned %d items with zero quantity.\n" (length inventory - length newInventory)
        Show itemId -> do
            inventory <- loadInventory
            let maybeItem = findItemById inventory itemId
            case maybeItem of
                Just item -> putStr $ formatItem item
                Nothing -> putStrLn "Not found"
        Find regpex -> do
            inventory <- loadInventory
            let matchedItems = findItemByRegex inventory regpex
            putStr $ intercalate "" $ map formatItemShort matchedItems
