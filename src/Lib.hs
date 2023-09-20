{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( loadInventory
    , saveInventory
    , addItem
    , removeItem
    , parseDateOrCurrent
    , getParsedArgs
    , Command (Add, Remove, Value)
    , totalValue
    ) where

import Data.Yaml
import Data.Time.Format
import Data.Time
import System.FilePath ((</>))
import System.Environment.XDG.BaseDir
import Options.Applicative

data Command
  = Add String String Int (Maybe Float) (Maybe Float)
  | Remove Int
  | Value

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
      <> value ""
      <> help "Date of the item to add"
      )
    <*> option auto
      (long "quantity"
      <> metavar "quantity"
      <> value 1
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

removeParser :: Options.Applicative.Parser Command
removeParser = Remove <$> argument auto (metavar "ID")

mainParser :: Options.Applicative.Parser Command
mainParser = subparser $
  command "add" (info addParser (progDesc "add an item"))
  <> command "remove" (info removeParser (progDesc "Remove an item"))
  <> command "value" (info (pure Value) (progDesc "Sum of all values"))

getParsedArgs :: IO Command
getParsedArgs = execParser (info mainParser fullDesc)

data Item = Item
  { itemId :: Int
  , itemDescription :: String
  , itemValue :: Maybe Float
  , itemPrice :: Maybe Float
  , itemDate :: Day
  , itemQuantity :: Int
  } deriving (Show, Eq)

instance ToJSON Item where
  toJSON (Item id desc (Just val) (Just price) date quantity) =
    object ["id" .= id
           , "description" .= desc
           , "value" .= val
           , "price" .= price
           , "date" .= date
           , "quantity" .= quantity
           ]
  toJSON (Item id desc Nothing (Just price) date quantity) =
    object ["id" .= id
           , "description" .= desc
           , "price" .= price
           , "date" .= date
           , "quantity" .= quantity
           ]
  toJSON (Item id desc (Just val) Nothing date quantity) =
    object ["id" .= id
           , "description" .= desc
           , "value" .= val
           , "date" .= date
           , "quantity" .= quantity
           ]
  toJSON (Item id desc Nothing Nothing date quantity) =
    object ["id" .= id
           , "description" .= desc
           , "date" .= date
           , "quantity" .= quantity
           ]

instance FromJSON Item where
  parseJSON = withObject "Item" $ \v -> Item
    <$> v .: "id"
    <*> v .: "description"
    <*> v .:? "value"
    <*> v .:? "price"
    <*> v .: "date"
    <*> v .: "quantity"

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

addItem :: String -> Maybe Float -> Maybe Float -> Day -> Int -> [Item] -> [Item]
addItem description value price date quantity inventory =
  inventory ++ [Item (maxIdPlusOne inventory) description value price date quantity]

removeItem :: Int -> [Item] -> [Item]
removeItem itemID inventory = filter (\item -> itemId item /= itemID) inventory

optItemValue :: Item -> Float
optItemValue (Item _ _ optVal _ _ _) = case optVal of
  Just val -> val
  Nothing -> 0.0

totalValue :: [Item] -> Float
totalValue inventory = sum $ map optItemValue inventory
