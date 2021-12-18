{-|
Module      : Database
Description : Database
Copyright   : (c) Yixun Zhou, 2021
                  Lyujun Kang, 2021
                  Qingyuan Zheng, 2021
License     : GPL-3
Maintainer  : yixun.zhou@se16.qmul.ac.uk
Stability   : experimental
Portability : POSIX

This module creates DB tables, saves or retrieves data from or to a database using again the appropriate Haskell data types.
-}

{-# LANGUAGE OverloadedStrings #-}

-- or, on GHCI:
-- > :set -XOverloadedStrings

module Database (
    initialiseDB,
    getOrCreateCountry,
    createRecord,
    saveRecords,
    queryCountryAllEntries,
    queryYearAllEntries,
    queryCountryTotalPopulation,
    orderYearAllEntries,
    insertData,
) where

import Types
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import System.Directory

-- See more Database.SQLite.Simple examples at
-- https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple.html
instance FromRow Record where
    fromRow = Record <$> field <*> field <*> field <*> field 

instance FromRow Entry where
    fromRow = Entry <$> field <*> field <*> field 

instance ToRow Entry where
    toRow (Entry id_ country_code_ country_name_)
        = toRow (id_, country_code_, country_name_)

instance FromRow Population where
    fromRow = Population <$> field <*> field <*> field 

instance ToRow Population where
    toRow (Population  population_ year_ fk_country)
        = toRow (population_, year_, fk_country)


-- |Initiate database
initialiseDB :: IO Connection  
initialiseDB = do
        conn <- open "data/population.sqlite"
        execute_ conn "CREATE TABLE IF NOT EXISTS entries (\
            \id INTEGER PRIMARY KEY AUTOINCREMENT,\
            \country_name VARCHAR(80) NOT NULL, \
            \country_code VARCHAR(50) NOT NULL\
            \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS population(\
            \value INTEGER NOT NULL, \
            \year INT NOT NULL, \
            \fk_country INTEGER\
            \)"
        return conn


getOrCreateCountry :: Connection -> String -> String -> IO Entry
getOrCreateCountry conn country_name country_code = do
    results <- queryNamed conn "SELECT * FROM entries WHERE country_name=:country_name AND country_code=:country_code" [":country_name" := country_name, ":country_code" := country_code]    
    if length results > 0 then
        return . head $ results
    else do
        execute conn "INSERT INTO entries (country_name, country_code) VALUES (?, ?)" (country_name, country_code)
        getOrCreateCountry conn country_name country_code


-- |Create record   
createRecord :: Connection -> Record -> IO ()
createRecord conn record = do
    entry <- getOrCreateCountry conn (country_name record) (country_code record) 
    let population = Population {
        value_ = value record,
        year_ = year record,
        fk_country = id_ entry
    }
    execute conn "INSERT INTO population VALUES (?,?,?)" population


-- |Save records into database
saveRecords :: Connection -> [Record] -> IO ()
saveRecords conn = mapM_ (createRecord conn)


-- |Query by country name
queryCountryAllEntries :: Connection -> IO[Record] 
queryCountryAllEntries conn = do
    putStr "Enter country name > "
    countryName <- getLine
    let cacheDirectory = "temp/"
    flag <- doesDirectoryExist cacheDirectory
    createDirectoryIfMissing flag cacheDirectory
    let cachePath = cacheDirectory ++ "/searchByCountry.txt"
    writeFile cachePath countryName
    putStrLn $ "Looking for " ++ countryName ++ " entries..."
    let sql =  "SELECT country_code, country_name, value, year FROM population inner join entries on population.fk_country == entries.id WHERE country_name=?"
    query conn sql [countryName]


-- |Query by year
queryYearAllEntries :: Connection -> IO[Record] 
queryYearAllEntries conn = do
    putStr "Enter the year > "
    queryYear <- getLine
    let cacheDirectory = "temp/"
    flag <- doesDirectoryExist cacheDirectory
    createDirectoryIfMissing flag cacheDirectory
    let cachePath = cacheDirectory ++ "/searchByYear.txt"
    writeFile cachePath queryYear
    putStrLn $ "Looking for " ++ queryYear ++ " year entries..."
    let sql = "SELECT country_code, country_name, value, year FROM population inner join entries on population.fk_country == entries.id WHERE year=?"
    query conn sql [queryYear]


-- |Query by world pupolation by given year
queryCountryTotalPopulation :: Connection -> IO ()
queryCountryTotalPopulation conn = do
    putStr "Enter the year > "
    queryYear <- getLine
    let sql = "SELECT country_code, country_name, value, year FROM population inner join entries on population.fk_country == entries.id WHERE country_name='World' AND year=?"
    countryEntries <- query conn sql [queryYear]
    let total = sum (map value countryEntries)
    putStrLn $ "Total population in " ++ queryYear ++ " is: " ++ show(total)


-- |Query by year in order
orderYearAllEntries :: Connection -> IO[Record] 
orderYearAllEntries conn = do
    putStr "Enter the year to order > "
    queryYear <- getLine
    let cacheDirectory = "temp/"
    flag <- doesDirectoryExist cacheDirectory
    createDirectoryIfMissing flag cacheDirectory
    let cachePath = cacheDirectory ++ "/searchByYear.txt"
    let cachePath_ = cacheDirectory ++ "/order.txt"
    writeFile cachePath queryYear
    putStrLn "  Choose population order options:  "
    putStrLn "  (1) Ascending             "
    putStrLn "  (2) descending sort    "
    option <- readLn :: IO Int
    case option of
        1 -> do
            writeFile cachePath_ "ascending"
            putStrLn $ "Population ordered by Ascending for " ++ queryYear ++ " year entries..."
            let sql = "SELECT country_code, country_name, value, year FROM population inner join entries on population.fk_country == entries.id WHERE year=? ORDER BY value ASC"
            query conn sql [queryYear]
        2 -> do
            writeFile cachePath_ "descending"
            putStrLn $ "Population ordered by descending for " ++ queryYear ++ " year entries..."
            let sql = "SELECT country_code, country_name, value, year FROM population inner join entries on population.fk_country == entries.id WHERE year=? ORDER BY value DESC;"
            query conn sql [queryYear]


-- |Insert data into database
insertData :: Connection -> IO()
insertData conn = do
    putStr "Enter your administrator code > "
    adminCode <- getLine
    putStrLn "Enter country name > "
    countryName <- getLine
    putStrLn "Enter country code >"
    countryCode <- getLine
    putStrLn "Enter population > "
    valuePopulation <- getLine
    putStrLn "Enter year >"
    valueYear <- getLine
    let record = Record {country_code=countryCode, country_name=countryName,value=(read valuePopulation :: Integer),year=(read valueYear :: Int)} : []
    saveRecords conn record
    putStrLn "Done"
    