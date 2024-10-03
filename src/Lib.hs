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
  , parseMaybeDate
  , getParsedArgs
  , Command (Add, Remove, Value, Count, Edit, Consume, Prune, Show, Find, Expired, List, Serve)
  , totalValue
  , consume
  , prune
  , findItemById
  , findItemsByRegex
  , findExpiredItems
  , appendToPath
  , formatTable
  , moveFile
  , renderInventory
  , serveInventory
  )
where

import Control.Exception (SomeException, catch, try)
import qualified Data.ByteString.Char8 as DBS
import Data.Char (toLower)
import Data.Function (on)
import Data.List (elemIndex)
import Data.Maybe
import Data.Text (pack)
import qualified Data.Text as T
import Data.Time
import Data.Yaml
import Data.Yaml.Pretty
import GHC.Generics
import GHC.Generics (Generic)
import Lucid
import Options.Applicative hiding (value)
import qualified Options.Applicative as OA
import System.Directory (doesFileExist, renameFile)
import System.Environment.XDG.BaseDir
import System.FilePath (takeExtension, (</>))
import Text.Layout.Table
import Text.Printf
import Text.Regex.Posix
import Web.Scotty
import Network.Wai.Middleware.Static
import Control.Monad.IO.Class (liftIO)

type ItemId = Int

data Command
  = Add String String ItemId (Maybe Float) (Maybe Float) (Maybe String) (Maybe String) (Maybe String) (Maybe String) (Maybe FilePath)
  | Remove ItemId
  | Value
  | Count
  | Consume ItemId
  | Prune
  | Edit
  | Show ItemId
  | Find String
  | Expired
  | List
  | Serve

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
    <*> optional
      ( strOption
          ( long "photo"
              <> metavar "photo"
              <> help "Photograph of the item"
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
      ( long "regex"
          <> metavar "search regex"
          <> help "Regular expression for searching in description and category"
      )

mainParser :: OA.Parser Command
mainParser =
  subparser $
    command "add" (info addParser (progDesc "Add item"))
      <> command "find" (info findParser (progDesc "Find item by reg. exp."))
      <> command "remove" (info removeParser (progDesc "Remove item"))
      <> command "value" (info (pure Value) (progDesc "Sum of all values"))
      <> command "count" (info (pure Count) (progDesc "Number of items"))
      <> command "edit" (info (pure Edit) (progDesc "Edit items in editor manually"))
      <> command "consume" (info consumeParser (progDesc "Consume item (i.e., decrement quantity)"))
      <> command "prune" (info (pure Prune) (progDesc "Clear items where quantity is zero"))
      <> command "show" (info showParser (progDesc "Show item"))
      <> command "expired" (info (pure Expired) (progDesc "List expired items"))
      <> command "list" (info (pure List) (progDesc "List all items"))
      <> command "serve" (info (pure Serve) (progDesc "Start web server for browsing inventory"))

getParsedArgs :: IO Command
getParsedArgs = execParser $ info (mainParser <**> helper) fullDesc

data Item = Item
  { itemId :: ItemId
  , description :: String
  , value :: Maybe Float
  , price :: Maybe Float
  , date :: Day
  , quantity :: Int
  , category :: Maybe String
  , container :: Maybe String
  , location :: Maybe String
  , expiry :: Maybe Day
  }
  deriving (Generic, Show)

instance ToJSON Item

instance FromJSON Item

getCurrentDay :: IO Day
getCurrentDay = utctDay <$> getCurrentTime

parseDateOrCurrent :: String -> IO Day
parseDateOrCurrent str = do
  let parsedDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" str :: Maybe Day
  maybe getCurrentDay return parsedDate

parseMaybeDate :: Maybe String -> Maybe Day
parseMaybeDate Nothing = Nothing
parseMaybeDate (Just str) = parseTimeM True defaultTimeLocale "%Y-%m-%d" str :: Maybe Day

appendToPath :: String -> IO FilePath
appendToPath filename = do
  filePath <- getUserDataDir "inven"
  return (filePath </> filename)

moveFile :: Maybe FilePath -> String -> IO ()
moveFile Nothing _ = return ()
moveFile (Just srcPath) newFileName = do
  fileExists <- doesFileExist srcPath
  invenDir <- getUserDataDir "inven"
  if fileExists
    then do
      let extension = takeExtension srcPath
      let destPath = invenDir </> newFileName ++ extension
      renameFile srcPath destPath
    else putStrLn "Warning: photo does not exist"

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
      , "expiry"
      ]

maxIdPlusOne :: [Item] -> ItemId
maxIdPlusOne [] = 0
maxIdPlusOne inventory = maximum (map itemId inventory) + 1

addItem :: String -> Maybe Float -> Maybe Float -> Day -> Int -> Maybe String -> Maybe String -> Maybe String -> Maybe Day -> [Item] -> ([Item], ItemId)
addItem desc val price date qty cat cont loc exp inventory =
  (inventory ++ [Item itemId desc val price date qty cat cont loc exp], itemId)
 where
  itemId = maxIdPlusOne inventory

removeItem :: ItemId -> ([Item] -> [Item])
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

consume :: [Item] -> ItemId -> [Item]
consume inventory consumeId = map applyConsumeItem inventory
 where
  applyConsumeItem item
    | itemId item == consumeId = decrementValue item
    | otherwise = item

prune :: ([Item] -> [Item])
prune = filter (\item -> quantity item /= 0)

findItemById :: [Item] -> ItemId -> Maybe Item
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

headerLine :: [String]
headerLine = ["ID", "Category", "Description", "Purchased", "Value", "Price", "Quantity"]

itemLine :: Item -> [String]
itemLine item =
  [ printf "%d" $ itemId item
  , formatCategory item
  , description item
  , formatDate item
  , formatValue item
  , printf "%F" $ price item
  , printf "%d" $ quantity item
  ]

formatCategory :: Item -> String
formatCategory item = printf "%s" $ category item

formatDate :: Item -> String
formatDate item = printf "%D" $ date item

formatValue :: Item -> String
formatValue item = printf "%F" $ value item

formatTable :: [Item] -> String
formatTable items =
  gridString
    [ column expand right def def
    , column expand left def def
    , column (expandBetween 0 30) right def def
    , column expand right def def
    , column expand right def def
    , column expand right def def
    , column expand right def def
    ]
    $ headerLine : map itemLine items

matchCaseInsensitive :: String -> String -> Bool
matchCaseInsensitive s regex = map toLower s =~ map toLower regex

matchCaseInsensitiveMaybeString :: Maybe String -> String -> Bool
matchCaseInsensitiveMaybeString (Just s) regex = matchCaseInsensitive s regex
matchCaseInsensitiveMaybeString Nothing _ = False

matchExpression :: Item -> String -> Bool
matchExpression item regex = matchCaseInsensitive (description item) regex || matchCaseInsensitiveMaybeString (category item) regex

findItemsByRegex :: String -> ([Item] -> [Item])
findItemsByRegex regex = filter (`matchExpression` regex)

findExpiredItems :: Day -> ([Item] -> [Item])
findExpiredItems today = filter $ isExpiredItem today
 where
  isExpiredItem today item = case expiry item of
    Just day -> day <= today
    Nothing -> False

-- Serving inventory in web browser

invenGridStyle = "\
\  body {\
\    font-family: Arial, sans-serif;\
\    margin: 0;\
\    padding: 0;\
\    background-color: #f5f5f5;\
\  }\
\  h1 {\
\    text-align: center;\
\    padding: 20px;\
\  }\
\  .grid-container {\
\    display: grid;\
\    grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));\
\    gap: 20px;\
\    padding: 20px;\
\  }\
\  .grid-item {\
\    background-color: #fff;\
\    border: 1px solid #ddd;\
\    border-radius: 8px;\
\    padding: 15px;\
\    box-shadow: 0 4px 8px rgba(0,0,0,0.1);\
\    transition: transform 0.2s;\
\  }\
\  .grid-item:hover {\
\    transform: scale(1.05);\
\  }\
\  .item-image {\
\    width: 100%;\
\    height: auto;\
\    border-bottom: 1px solid #ddd;\
\    margin-bottom: 10px;\
\  }\
\  .item-info {\
\    text-align: center;\
\  }"

renderInventory :: [Item] -> Html ()
renderInventory items = html_ $ do
  head_ $ do
    title_ "Inventory Overview"
    style_ invenGridStyle
  body_ $ do
    h1_ "Inventory"
    div_ [class_ "grid-container"] $ do
      mapM_ renderItem items

renderItem :: Item -> Html ()
renderItem item = div_ [class_ "grid-item"] $ do
  h3_ $ toHtml $ description item  -- Item name or title
  img_ [src_ $ T.pack $ printf "%d.jpg" $ itemId item, alt_ "Item image", class_ "item-image"]

serveInventory :: [Item] -> String -> IO ()
serveInventory inventory staticDir = scotty 4200 $ do
  middleware $ staticPolicy (addBase staticDir)
  get "/" $ do
    let pageContent = renderText (renderInventory inventory)
    html pageContent
