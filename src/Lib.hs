{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( loadInventory
    , saveInventory
    , addItem
    , removeItem
    , parseDateOrCurrent
    ) where

import Data.Yaml
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time
import Data.Time.Calendar

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

loadInventory :: IO [Item]
loadInventory = do
  contents <- decodeFileThrow ".inventory.yml"
  return contents

saveInventory :: [Item] -> IO ()
saveInventory items = encodeFile ".inventory.yml" items

maxIdPlusOne :: [Item] -> Int
maxIdPlusOne [] = 0
maxIdPlusOne inventory = maximum (map itemId inventory) + 1

addItem :: String -> Float -> Day -> [Item] -> [Item]
addItem description value date inventory = inventory ++ [Item (maxIdPlusOne inventory) description value date]

removeItem :: Int -> [Item] -> [Item]
removeItem itemID inventory = filter (\item -> itemId item /= itemID) inventory
