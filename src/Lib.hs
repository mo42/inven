{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( loadInventory
    , saveInventory
    , addItem
    , removeItem
    ) where

import Data.Yaml

data Item = Item
  { itemId :: Int
  , itemDescription :: String
  , itemValue :: Float
  } deriving (Show, Eq)

instance ToJSON Item where
  toJSON (Item id desc val) = object ["id" .= id, "description" .= desc, "value" .= val]

instance FromJSON Item where
  parseJSON = withObject "Item" $ \v -> Item
    <$> v .: "id"
    <*> v .: "description"
    <*> v .: "value"

loadInventory :: IO [Item]
loadInventory = do
  contents <- decodeFileThrow ".inventory.yml"
  return contents

saveInventory :: [Item] -> IO ()
saveInventory items = encodeFile ".inventory.yml" items

maxIdPlusOne :: [Item] -> Int
maxIdPlusOne [] = 0
maxIdPlusOne inventory = maximum (map itemId inventory) + 1

addItem :: String -> Float -> [Item] -> [Item]
addItem description value inventory = inventory ++ [Item (maxIdPlusOne inventory) description value]

removeItem :: Int -> [Item] -> [Item]
removeItem itemID inventory = filter (\item -> itemId item /= itemID) inventory

