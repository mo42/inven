{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (
    loadInventory,
    saveInventory,
    addItem,
    removeItem,
    parseDateOrCurrent,
    getParsedArgs,
    Command (Add, Remove, Value, Count, Edit, Consume, Prune, Show, Find),
    totalValue,
    consume,
    prune,
    findItemById,
    findItemByRegex,
    appendToPath,
    formatItem,
    formatItemShort,
) where

import Data.Maybe
import Data.Time
import Data.Time.Format
import Data.Yaml
import GHC.Generics
import Options.Applicative
import System.Environment.XDG.BaseDir
import System.FilePath ((</>))
import Text.Printf
import Text.Regex.Posix

data Command
    = Add String String Int (Maybe Float) (Maybe Float) (Maybe String)
    | Remove Int
    | Value
    | Count
    | Consume Int
    | Prune
    | Edit
    | Show Int
    | Find String

addParser :: Options.Applicative.Parser Command
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
                <> Options.Applicative.value ""
                <> help "Date of the item to add"
            )
        <*> option
            auto
            ( long "quantity"
                <> metavar "quantity"
                <> Options.Applicative.value 1
                <> help "Number of items of this kind"
            )
        <*> optional
                (option
                    auto
                    ( long "value"
                        <> metavar "value"
                        <> help "Value of the item to add"
                    )
                    )
        <*>  optional (
                option
                    auto
                    ( long "price"
                        <> metavar "price"
                        <> help "Optional price of the item"
                    )
            )
        <*>  optional (
                strOption
                    ( long "category"
                        <> metavar "category"
                        <> help "Optional category of the item"
                    )
            )

removeParser :: Options.Applicative.Parser Command
removeParser = Remove <$> argument auto (metavar "ID")

consumeParser :: Options.Applicative.Parser Command
consumeParser = Consume <$> argument auto (metavar "ID")

showParser :: Options.Applicative.Parser Command
showParser = Show <$> argument auto (metavar "ID")

findParser :: Options.Applicative.Parser Command
findParser =
    Find
        <$> strOption
            ( long "regexp"
                <> metavar "search regexp"
                <> help "Regular expression for searching in description"
            )

mainParser :: Options.Applicative.Parser Command
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
getParsedArgs = execParser $ info mainParser fullDesc

data Item = Item
    { itemId :: Int
    , description :: String
    , value :: Maybe Float
    , price :: Maybe Float
    , date :: Day
    , quantity :: Int
    , category :: Maybe String
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
    path <- appendToPath "inventory.yml"
    decodeFileThrow path

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

test :: Maybe Float -> String
test (Just f) = show f
test Nothing = "none"

instance PrintfArg (Maybe String) where
    formatArg mayStr = formatString (fromMaybe "none" mayStr)

instance PrintfArg (Maybe Float) where
    formatArg mayFloat fmt
        | fmtChar fmt == 'F' = formatString (maybe "none" show mayFloat) (fmt{fmtChar = 'f'})
        | otherwise = error "Unsupported format specifier for type Maybe Float"

formatItem :: Item -> String
formatItem (Item _ desc val price date qty cat) =
    printf
        ( "%s\n"
            ++ "- category: %s\n"
            ++ "- purchased: %D\n"
            ++ "- value: %F\n"
            ++ "- price: %F\n"
            ++ "- quantity: %d\n"
        )
        desc
        cat
        date
        val
        price
        qty

formatItemShort :: Item -> String
formatItemShort (Item _ desc _ _ _ qty cat) = printf "%03d %s %s\n" qty cat desc

matchMaybeString :: Maybe String -> String -> Bool
matchMaybeString (Just str) regex = str =~ regex
matchMaybeString Nothing _ = False

matchExpression :: Item -> String -> Bool
matchExpression item regex = description item =~ regex || matchMaybeString (category item) regex

findItemByRegex :: [Item] -> String -> [Item]
findItemByRegex [] _ = []
findItemByRegex items regex = filter (`matchExpression` regex) items
