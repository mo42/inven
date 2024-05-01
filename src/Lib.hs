{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( loadInventory
  , saveInventory
  , addItem
  , removeItem
  , parseDateOrCurrent
  , getParsedArgs
  , Command (Add, Remove, Value, Count, Edit, Consume, Prune, Show, Find)
  , totalValue
  , consume
  , prune
  , findItemById
  , findItemsByRegex
  , appendToPath
  , formatItem
  , headerLine
  , formatItemShort
  )
where

import Control.Exception (SomeException, catch, try)
import qualified Data.ByteString.Char8 as DBS
import Data.Char (toLower)
import Data.Function (on)
import Data.List (elemIndex)
import Data.Maybe
import Data.Text (pack)
import Data.Time
import Data.Yaml
import Data.Yaml.Pretty
import GHC.Generics
import Options.Applicative hiding (value)
import qualified Options.Applicative as OA
import System.Environment.XDG.BaseDir
import System.FilePath ((</>))
import Text.Printf
import Text.Regex.Posix

data Command
  = Add String String Int (Maybe Float) (Maybe Float) (Maybe String) (Maybe String) (Maybe String) (Maybe String)
  | Remove Int
  | Value
  | Count
  | Consume Int
  | Prune
  | Edit
  | Show Int
  | Find String

addParser :: OA.Parser Command
addParser =
  Add
    <$> strOption
      ( long "text"
          <> metavar "description"
          <> help "Textual description of the item to add"
      )
    <*> strOption
      ( long "date"
          <> metavar "date"
          <> OA.value ""
          <> help "Date of the item to add"
      )
    <*> option
      auto
      ( long "quantity"
          <> metavar "quantity"
          <> OA.value 1
          <> help "Number of items of this kind"
      )
    <*> optional
      ( option
          auto
          ( long "value"
              <> metavar "value"
              <> help "Value of the item to add"
          )
      )
    <*> optional
      ( option
          auto
          ( long "price"
              <> metavar "price"
              <> help "Optional price of the item"
          )
      )
    <*> optional
      ( strOption
          ( long "category"
              <> metavar "category"
              <> help "Optional category of the item"
          )
      )
    <*> optional
      ( strOption
          ( long "container"
              <> metavar "container"
              <> help "Optional container in which this item is located"
          )
      )
    <*> optional
      ( strOption
          ( long "location"
              <> metavar "location"
              <> help "Optional location where this item is located"
          )
      )
    <*> optional
      ( strOption
          ( long "expiry"
              <> metavar "expiry"
              <> help "Expiration Date of the item"
          )
      )

removeParser :: OA.Parser Command
removeParser = Remove <$> argument auto (metavar "Item ID")

consumeParser :: OA.Parser Command
consumeParser = Consume <$> argument auto (metavar "Item ID")

showParser :: OA.Parser Command
showParser = Show <$> argument auto (metavar "Item ID")

findParser :: OA.Parser Command
findParser =
  Find
    <$> strOption
      ( long "regexp"
          <> metavar "search regexp"
          <> help "Regular expression for searching in description"
      )

mainParser :: OA.Parser Command
mainParser =
  subparser $
    command "add" (info addParser (progDesc "Add an item"))
      <> command "find" (info findParser (progDesc "Find item by reg. exp."))
      <> command "remove" (info removeParser (progDesc "Remove an item"))
      <> command "value" (info (pure Value) (progDesc "Sum of all values"))
      <> command "count" (info (pure Count) (progDesc "Number of items"))
      <> command "edit" (info (pure Edit) (progDesc "Edit item in editor manually"))
      <> command "consume" (info consumeParser (progDesc "Consume item (ie, decrement quantity)"))
      <> command "prune" (info (pure Prune) (progDesc "Clear items from database where quantity is zero"))
      <> command "show" (info showParser (progDesc "Show item"))

getParsedArgs :: IO Command
getParsedArgs = execParser $ info (mainParser <**> helper) fullDesc

data Item = Item
  { itemId :: Int
  , description :: String
  , value :: Maybe Float
  , price :: Maybe Float
  , date :: Day
  , quantity :: Int
  , category :: Maybe String
  , container :: Maybe String
  , location :: Maybe String
  , expiry :: Maybe String
  }
  deriving (Generic, Show)

instance ToJSON Item

instance FromJSON Item

getCurrentDay :: IO Day
getCurrentDay = utctDay <$> getCurrentTime

parseDateOrCurrent :: String -> IO Day
parseDateOrCurrent input = do
  let parsedDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" input :: Maybe Day
  maybe getCurrentDay return parsedDate

appendToPath :: String -> IO FilePath
appendToPath filename = do
  filePath <- getUserDataDir "inven"
  return (filePath </> filename)

loadInventory :: IO [Item]
loadInventory = do
  filePath <- appendToPath "inventory.yml"
  result <- tryDecode filePath `catch` handleYamlError
  case result of
    Left _ -> return []
    Right items -> return items
 where
  tryDecode :: FilePath -> IO (Either SomeException [Item])
  tryDecode = try . decodeFileThrow

  handleYamlError :: SomeException -> IO (Either SomeException [Item])
  handleYamlError err = do
    putStrLn $ "Error while reading YAML file: " ++ show err
    return $ Left err

saveInventory :: [Item] -> IO ()
saveInventory items = do
  path <- appendToPath "inventory.yml"
  DBS.writeFile path (encodePretty opts items)
 where
  opts = setConfCompare (compare `on` fieldIndex) defConfig
  fieldIndex s = fromMaybe (length fields) $ s `elemIndex` fields
  fields =
    map
      pack
      [ "itemId"
      , "category"
      , "description"
      , "container"
      , "location"
      , "date"
      , "value"
      , "price"
      , "quantity"
      ]

maxIdPlusOne :: [Item] -> Int
maxIdPlusOne [] = 0
maxIdPlusOne inventory = maximum (map itemId inventory) + 1

addItem :: String -> Maybe Float -> Maybe Float -> Day -> Int -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> [Item] -> [Item]
addItem desc val price date qty cat cont loc exp inventory =
  inventory ++ [Item (maxIdPlusOne inventory) desc val price date qty cat cont loc exp]

removeItem :: Int -> ([Item] -> [Item])
removeItem removeItemId = filter (\item -> itemId item /= removeItemId)

optItemValue :: Item -> Float
optItemValue Item{value = optVal, price = optPrice} = case optVal of
  Just val -> val
  Nothing -> fromMaybe 0.0 optPrice

totalItemValue :: Item -> Float
totalItemValue item = (\Item{quantity = q} -> fromIntegral q) item * optItemValue item

totalValue :: [Item] -> Float
totalValue inventory = sum $ map totalItemValue inventory

decrementValue :: Item -> Item
decrementValue item = item{quantity = quantity item - 1}

consume :: [Item] -> Int -> [Item]
consume inventory consumeId = map applyConsumeItem inventory
 where
  applyConsumeItem item
    | itemId item == consumeId = decrementValue item
    | otherwise = item

prune :: ([Item] -> [Item])
prune = filter (\item -> quantity item /= 0)

findItemById :: [Item] -> Int -> Maybe Item
findItemById [] _ = Nothing
findItemById (item : items) targetId
  | itemId item == targetId = Just item
  | otherwise = findItemById items targetId

instance PrintfArg Day where
  formatArg day fmt
    | fmtChar fmt == 'D' = formatString (formatTime defaultTimeLocale "%Y-%m-%d" day) (fmt{fmtChar = 's'})
    | otherwise = error "Unsupported format specifier for type Day"

instance PrintfArg (Maybe String) where
  formatArg mayStr = formatString (fromMaybe "none" mayStr)

instance PrintfArg (Maybe Float) where
  formatArg mayFloat fmt
    | fmtChar fmt == 'F' = formatString (maybe "none" show mayFloat) (fmt{fmtChar = 's'})
    | otherwise = error "Unsupported format specifier for type Maybe Float"

formatItem :: Item -> String
formatItem item =
  printf
    ( "%s\n"
        ++ "- category: %s\n"
        ++ "- purchased: %D\n"
        ++ "- value: %F\n"
        ++ "- price: %F\n"
        ++ "- quantity: %d\n"
    )
    (description item)
    (category item)
    (date item)
    (value item)
    (price item)
    (quantity item)

headerLine :: String
headerLine = "\ESC[4m" ++ "   ID Category             Description" ++ "\ESC[0m"

padWithSpaces :: Int -> Int -> String
padWithSpaces width n = replicate (width - length (show n)) ' ' ++ show n

truncateDots :: Int -> String -> String
truncateDots width string
  | length string <= width = string ++ replicate (width - length string) ' '
  | otherwise = take (width - 3) string ++ "..."

transformMaybe :: (Int -> String -> String) -> Int -> Maybe String -> Maybe String
transformMaybe _ _ Nothing = Nothing
transformMaybe f x (Just s) = Just (f x s)

formatItemShort :: Item -> String
formatItemShort item = printf "%s %s %s\n" (padWithSpaces 5 (itemId item)) (transformMaybe truncateDots 20 (category item)) (truncateDots 30 (description item))

matchCaseInsensitive :: String -> String -> Bool
matchCaseInsensitive s regex = map toLower s =~ map toLower regex

matchCaseInsensitiveMaybeString :: Maybe String -> String -> Bool
matchCaseInsensitiveMaybeString (Just s) regex = matchCaseInsensitive s regex
matchCaseInsensitiveMaybeString Nothing _ = False

matchExpression :: Item -> String -> Bool
matchExpression item regex = matchCaseInsensitive (description item) regex || matchCaseInsensitiveMaybeString (category item) regex

findItemsByRegex :: String -> ([Item] -> [Item])
findItemsByRegex regex = filter (`matchExpression` regex)
