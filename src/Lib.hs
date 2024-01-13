{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Lib
    ( loadInventory
    , saveInventory
    , addItem
    , removeItem
    , parseDateOrCurrent
    , getParsedArgs
    , Command (Add, Remove, Value, Count, Edit, Consume)
    , totalValue
    , count
    , consume
    , appendToPath
    ) where

import Data.Yaml
import Data.Time.Format
import Data.Time
import System.FilePath ((</>))
import System.Environment.XDG.BaseDir
import Options.Applicative
import GHC.Generics

data Command
  = Add String String Int (Maybe Float) (Maybe Float) (Maybe String)
  | Remove Int
  | Value
  | Count
  | Consume Int
  | Edit

addParser :: Options.Applicative.Parser Command
addParser = Add
    <$> strOption
      (long "text"
      <> metavar "description"
      <> help "Textual description of the item to add"
      )
    <*> strOption
      (long "date"
      <> metavar "date"
      <> Options.Applicative.value ""
      <> help "Date of the item to add"
      )
    <*> option auto
      (long "quantity"
      <> metavar "quantity"
      <> Options.Applicative.value 1
      <> help "Number of items of this kind"
      )
    <*> (optional $ option auto
          (long "value"
          <> metavar "value"
          <> help "Value of the item to add"
          )
        )
    <*> (optional $ option auto
          (long "price"
          <> metavar "price"
          <> help "Optional price of the item"
          )
        )
    <*> (optional $ strOption
          (long "category"
          <> metavar "category"
          <> help "Optional category of the item"
          )
        )

removeParser :: Options.Applicative.Parser Command
removeParser = Remove <$> argument auto (metavar "ID")

consumeParser :: Options.Applicative.Parser Command
consumeParser = Consume <$> argument auto (metavar "ID")

mainParser :: Options.Applicative.Parser Command
mainParser = subparser $
  command "add" (info addParser (progDesc "Add an item"))
  <> command "remove" (info removeParser (progDesc "Remove an item"))
  <> command "value" (info (pure Value) (progDesc "Sum of all values"))
  <> command "count" (info (pure Count) (progDesc "Number of items"))
  <> command "edit" (info (pure Edit) (progDesc "Edit item in editor manually"))
  <> command "consume" (info consumeParser (progDesc "Consume item (ie, decrement quantity)"))

getParsedArgs :: IO Command
getParsedArgs = execParser $ info mainParser fullDesc

data Item = Item
  { itemId :: Int
  , description :: String
  , value :: Maybe Float
  , price :: Maybe Float
  , date :: Day
  , quantity :: Int
  , category :: Maybe String
  } deriving (Generic, Show)

instance ToJSON Item

instance FromJSON Item

getCurrentDay :: IO Day
getCurrentDay = utctDay <$> getCurrentTime

parseDateOrCurrent :: String -> IO Day
parseDateOrCurrent input = do
  let parsedDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" input :: Maybe Day
  case parsedDate of
    Just date -> return date
    Nothing   -> getCurrentDay

appendToPath :: String -> IO FilePath
appendToPath filename = do
  filePath <- getUserDataDir "inven"
  return (filePath </> filename)

loadInventory :: IO [Item]
loadInventory = do
  path <- appendToPath "inventory.yml"
  contents <- decodeFileThrow path
  return contents

saveInventory :: [Item] -> IO ()
saveInventory items = do
  path <- appendToPath "inventory.yml"
  encodeFile path items

maxIdPlusOne :: [Item] -> Int
maxIdPlusOne [] = 0
maxIdPlusOne inventory = maximum (map itemId inventory) + 1

addItem :: String -> Maybe Float -> Maybe Float -> Day -> Int -> Maybe String -> [Item] -> [Item]
addItem desc val price date qty cat inventory =
  inventory ++ [Item (maxIdPlusOne inventory) desc val price date qty cat]

removeItem :: Int -> [Item] -> [Item]
removeItem removeItemId inventory = filter (\item -> itemId item /= removeItemId) inventory

optItemValue :: Item -> Float
optItemValue Item { value = optVal, price = optPrice } = case  optVal of
    Just val -> val
    Nothing ->
      case optPrice of
        Just pr -> pr
        Nothing -> 0.0

totalItemValue :: Item -> Float
totalItemValue item = (\Item { quantity = q } -> fromIntegral q) item * optItemValue item

totalValue :: [Item] -> Float
totalValue inventory = sum $ map totalItemValue inventory

count :: [Item] -> Int
count inventory = length inventory

decrementValue :: Item -> Item
decrementValue item = item { quantity = quantity item - 1 }

consume :: [Item] -> Int -> [Item]
consume inventory consumeId = map applyConsumeItem inventory
  where
    applyConsumeItem item
      | itemId item == consumeId = decrementValue item
      | otherwise = item
