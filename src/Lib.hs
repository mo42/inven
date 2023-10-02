{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( loadInventory
    , saveInventory
    , addItem
    , removeItem
    , parseDateOrCurrent
    , getParsedArgs
    , Command (Add, Remove, Value, Edit)
    , totalValue
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
  = Add String String Int (Maybe Float) (Maybe Float)
  | Remove Int
  | Value
  | Edit Int

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

removeParser :: Options.Applicative.Parser Command
removeParser = Remove <$> argument auto (metavar "ID")

editParser :: Options.Applicative.Parser Command
editParser = Edit <$> argument auto (metavar "ID")

mainParser :: Options.Applicative.Parser Command
mainParser = subparser $
  command "add" (info addParser (progDesc "add an item"))
  <> command "remove" (info removeParser (progDesc "Remove an item"))
  <> command "value" (info (pure Value) (progDesc "Sum of all values"))
  <> command "edit" (info editParser (progDesc "Edit item in Vim manually"))

getParsedArgs :: IO Command
getParsedArgs = execParser (info mainParser fullDesc)

data Item = Item
  { itemId :: Int
  , description :: String
  , value :: Maybe Float
  , price :: Maybe Float
  , date :: Day
  , quantity :: Int
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

addItem :: String -> Maybe Float -> Maybe Float -> Day -> Int -> [Item] -> [Item]
addItem desc val price date qty inventory =
  inventory ++ [Item (maxIdPlusOne inventory) desc val price date qty]

removeItem :: Int -> [Item] -> [Item]
removeItem removeItemId inventory = filter (\item -> itemId item /= removeItemId) inventory

optItemValue :: Item -> Float
optItemValue (Item _ _ optVal _ _ _) = case optVal of
  Just val -> val
  Nothing -> 0.0

totalValue :: [Item] -> Float
totalValue inventory = sum $ map optItemValue inventory
