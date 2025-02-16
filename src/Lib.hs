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
  , copyFileTo
  , renderInventory
  , serveInventory
  )
where

import Control.Exception (SomeException, catch, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as DBS
import Data.Char (toLower)
import Data.Function (on)
import Data.IORef
import Data.List (elemIndex)
import Data.Maybe
import Data.Text (pack)
import qualified Data.Text as T
import Data.Time
import Data.Yaml
import Data.Yaml.Pretty
import GHC.Generics
import Lucid
import Network.HTTP.Types (badRequest400)
import Network.Wai.Middleware.Static
import Options.Applicative hiding (value)
import qualified Options.Applicative as OA
import System.Directory (copyFile, doesFileExist, renameFile)
import System.Environment.XDG.BaseDir
import System.FilePath (takeExtension, (</>))
import Text.Layout.Table
import Text.Printf
import Text.Read (readMaybe)
import Text.Regex.Posix
import Web.Scotty

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

copyFileTo :: FilePath -> IO ()
copyFileTo fileName = do
  fileExists <- doesFileExist fileName
  invenDir <- getUserDataDir "inven"
  if fileExists
    then do
      let destPath = invenDir </> fileName
      copyFile fileName destPath
    else putStrLn "Warning: file does not exist"

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
matchCaseInsensitive s pattern = map toLower s =~ map toLower pattern

matchCaseInsensitiveMaybeString :: Maybe String -> String -> Bool
matchCaseInsensitiveMaybeString (Just s) pattern = matchCaseInsensitive s pattern
matchCaseInsensitiveMaybeString Nothing _ = False

matchExpression :: Item -> String -> Bool
matchExpression item pattern = matchCaseInsensitive (description item) pattern || matchCaseInsensitiveMaybeString (category item) pattern

findItemsByRegex :: String -> ([Item] -> [Item])
findItemsByRegex pattern = filter (`matchExpression` pattern)

findExpiredItems :: Day -> [Item] -> [Item]
findExpiredItems today = filter (maybe False (<= today) . expiry)

-- Serving inventory in web browser

renderInventory :: [Item] -> Html ()
renderInventory items = html_ $ do
  head_ $ do
    title_ "Inventory Overview"
    link_ [rel_ "stylesheet", href_ "style.css"]
    script_ [src_ "script.js"] ("" :: T.Text)
  body_ $ do
    h1_ "Inventory"
    div_ [class_ "search-bar-container"] $ do
      input_
        [ type_ "text"
        , id_ "search"
        , placeholder_ "Search inventory..."
        , oninput_ "handleSearch()"
        ]
      button_ [id_ "search-btn", onclick_ "handleSearch()"] "Search"
      a_ [href_ "/add", id_ "add-btn"] "➕ Add"
    div_ [class_ "grid-container"] $ do
      mapM_ renderItem items

renderItem :: Item -> Html ()
renderItem item = div_ [class_ "grid-item"] $ do
  div_ [class_ "header-container"] $ do
    h3_ [class_ "item-description"] $ toHtml $ description item
    button_ [class_ "delete-btn", id_ (pack $ printf "delete-%d" (itemId item)), onclick_ (pack $ printf "deleteItem(%d)" (itemId item))] $ do
      toHtml $ pack "🗑️"
  img_ [src_ $ pack $ printf "%d.jpg" $ itemId item, alt_ "Item image", class_ "item-image"]
  p_ $ toHtml $ pack $ printf "#%d item from %s category at location %s" (quantity item) (category item) (location item)

renderAddItemForm :: Html ()
renderAddItemForm = html_ $ do
  head_ $ do
    title_ "Add Item"
    link_ [rel_ "stylesheet", href_ "style.css"]
    script_ [src_ "script.js"] ("" :: T.Text)
  body_ $ do
    h1_ "Add New Item"
    form_ [action_ "/add", method_ "post", onsubmit_ "addItem()"] $ do
      input_ [type_ "text", name_ "description", id_ "item-description", required_ "true", placeholder_ "Description"]
      input_ [type_ "number", name_ "value", id_ "item-value", required_ "false", placeholder_ "Value", step_ "0.01"]
      input_ [type_ "number", name_ "price", id_ "item-price", required_ "false", placeholder_ "Price", step_ "0.01"]
      input_ [type_ "number", name_ "quantity", id_ "item-quantity", required_ "false", placeholder_ "Quantity", step_ "1"]
      input_ [type_ "text", name_ "category", id_ "item-category", required_ "false", placeholder_ "Category"]
      input_ [type_ "text", name_ "container", id_ "item-container", required_ "false", placeholder_ "Container"]
      input_ [type_ "text", name_ "location", id_ "item-location", required_ "false", placeholder_ "Location"]
      button_ [type_ "submit"] "Add Item"
    a_ [href_ "/"] "Back to Inventory"

serveInventory :: IORef [Item] -> String -> IO ()
serveInventory inventoryRef staticDir = scotty 4200 $ do
  middleware $ staticPolicy (addBase staticDir)
  get "/" $ do
    inventory <- liftIO $ readIORef inventoryRef
    let pageContent = renderText (renderInventory inventory)
    html pageContent
  get "/add" $ do
    let addItemForm = renderText renderAddItemForm
    html addItemForm
  post "/add" $ do
    desc <- param "description"
    valStr <- param "value"
    let val = readMaybe valStr :: Maybe Float
    priceStr <- param "price"
    let price = readMaybe priceStr :: Maybe Float
    qtyStr <- param "quantity"
    let qty = fromMaybe 1 (readMaybe qtyStr :: Maybe Int)
    cat <- (Just <$> param "category") `rescue` (\_ -> return Nothing)
    cont <- (Just <$> param "container") `rescue` (\_ -> return Nothing)
    loc <- (Just <$> param "location") `rescue` (\_ -> return Nothing)
    liftIO $ do
      curDate <- parseDateOrCurrent ""
      inventory <- readIORef inventoryRef
      let (updatedInventory, _) = addItem desc val price curDate qty cat cont loc Nothing inventory
      writeIORef inventoryRef updatedInventory
      saveInventory inventory
    redirect "/"
  get "/inventory" $ do
    inventory <- liftIO $ readIORef inventoryRef
    json inventory
  delete "/inventory/:id" $ do
    itemIdStr <- param "id"
    let maybeItemId = readMaybe itemIdStr :: Maybe Int
    case maybeItemId of
      Nothing -> do
        status badRequest400
        json $ object ["error" .= ("Invalid ID format" :: String)]
      Just delItemId -> do
        liftIO $ putStrLn $ "Deleting item with ID " ++ show delItemId
        liftIO $ do
          inventory <- readIORef inventoryRef
          let updatedInventory = removeItem delItemId inventory
          writeIORef inventoryRef updatedInventory
          saveInventory inventory
