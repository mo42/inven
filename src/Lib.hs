{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( loadInventory
    , saveInventory
    , addItem
    , removeItem
    , parseDateOrCurrent
    , getParsedArgs
    , Command (Add, Remove)
    ) where

import Data.Yaml
import Data.Time.Format
import Data.Time
import System.FilePath ((</>))
import System.Environment.XDG.BaseDir
import Options.Applicative

data Command
  = Add String String Float
  | Remove Int

addParser :: Options.Applicative.Parser Command
addParser = Add
    <$> strOption (long "text" <> metavar "description" <> help "Textual description of the item to add")
    <*> strOption (long "date" <> metavar "date" <> value "" <> help "Date of the item to add")
    <*> option auto (long "value" <> metavar "value" <> value 0.0 <> help "Value of the item to add (default: 0.0)")

removeParser :: Options.Applicative.Parser Command
removeParser = Remove <$> argument auto (metavar "ID")

mainParser :: Options.Applicative.Parser Command
mainParser = subparser $
  command "add" (info addParser (progDesc "add an item"))
  <> command "remove" (info removeParser (progDesc "Remove an item"))

getParsedArgs :: IO Command
getParsedArgs = execParser (info mainParser fullDesc)

data Item = Item
  { itemId :: Int
  , itemDescription :: String
  , itemValue :: Float
  , itemDate :: Day
  } deriving (Show, Eq)

instance ToJSON Item where
  toJSON (Item id desc val date) = object ["id" .= id, "description" .= desc, "value" .= val, "date" .= date]

instance FromJSON Item where
  parseJSON = withObject "Item" $ \v -> Item
    <$> v .: "id"
    <*> v .: "description"
    <*> v .: "value"
    <*> v .: "date"

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

addItem :: String -> Float -> Day -> [Item] -> [Item]
addItem description value date inventory = inventory ++ [Item (maxIdPlusOne inventory) description value date]

removeItem :: Int -> [Item] -> [Item]
removeItem itemID inventory = filter (\item -> itemId item /= itemID) inventory
