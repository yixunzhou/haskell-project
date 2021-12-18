{-|
Module      : Main
Description : Main
Copyright   : (c) Yixun Zhou, 2021
                  Lyujun Kang, 2021
                  Fanyuqian Yang, 2021
                  Qingyuan Zheng, 2021
License     : GPL-3
Maintainer  : yixun.zhou@se16.qmul.ac.uk
Stability   : experimental
Portability : POSIX

A user should be able to (interactively) create and initialise an sqlite database, download data and save to database, run queries on the database. 
The user could be able to download data from Internet and save it into a .json file, create database using sqlite and save data into a .sqlite file. The path is 'data/'.
The user could also search data by country name or year and sort the data by population value and find the total population all over the world in some given year.
All the data of querying resaults will be stored in directory 'out/'.
What's more, the user is able to insert data into database with a correct admin code.
-}

module Main where

import System.IO
import Fetch 
import Parse
import Types
import Database
import Database.SQLite.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as B
import qualified Parse
import System.Directory


main :: IO ()
main = do
    putStrLn "---------------------------------------------------------"
    putStrLn "  Welcome to the Population data app                     "
    putStrLn "  (1) Download data                                      "
    putStrLn "  (2) Search by country name                             "
    putStrLn "  (3) Search by year (1960-2018)                         "
    putStrLn "  (4) Search by year and order data (1960-2018)          "
    putStrLn "  (5) Search world total population by year              "
    putStrLn "  (6) Insert new data to database(Using admin code: ec21)"
    putStrLn "  (0) Quit                                               "
    putStrLn "---------------------------------------------------------"
    hSetBuffering stdout NoBuffering
    putStr "Choose an option> "
    option <- readLn :: IO Int
    let dataDirectory = "data"
    let tempDirectory = "temp"
    let outputDirectory = "out"
    flag_ <- doesDirectoryExist dataDirectory
    createDirectoryIfMissing flag_ dataDirectory
    let populationDBFilePath = dataDirectory ++ "/population.sqlite"
    conn <- open populationDBFilePath
    initialiseDB
    case option of
        1 -> do
            let url = "https://pkgstore.datahub.io/core/population/population_json/data/315178266aa86b71057e993f98faf886/population_json.json"
            putStrLn "Downloading..."
            json <- download url
            putStrLn "Downloaded!"
            putStrLn "Saving into files..."
            flag <- doesDirectoryExist dataDirectory
            createDirectoryIfMissing flag dataDirectory
            let jsonPath = dataDirectory ++ "/population.json"
            writeFile jsonPath $ L8.unpack json
            putStrLn $ "Data saved at " ++ jsonPath
            putStrLn "Parsing..."
            mydata <- parseData json
            putStrLn "Parsed!"
            putStrLn "Saving on DB..."
            saveRecords conn mydata
            putStrLn $ "Data saved at " ++ populationDBFilePath
            main
        2 -> do
            hSetBuffering stdout NoBuffering
            entries <- queryCountryAllEntries conn
            let title = "country_code;country_name;value;year\n"
            let csv= title ++ toCsvFormat entries
            let cachePath = tempDirectory ++ "/searchByCountry.txt"
            countryName <- readFile cachePath
            removeFile cachePath
            flag <- doesDirectoryExist outputDirectory
            createDirectoryIfMissing flag outputDirectory
            let outPath = outputDirectory ++ "/" ++ countryName ++ ".csv"
            writeFile outPath csv
            putStrLn $ "Data saved at " ++ outPath
            mapM_ print entries
            main
        3 -> do
            hSetBuffering stdout NoBuffering
            entries <- queryYearAllEntries conn
            let title = "country_code;country_name;value;year\n"
            let csv = title ++ toCsvFormat entries
            let cachePath = tempDirectory ++ "/searchByYear.txt"
            queryYear <- readFile cachePath
            removeFile cachePath
            flag <- doesDirectoryExist outputDirectory
            createDirectoryIfMissing flag outputDirectory
            let outPath = outputDirectory ++ "/" ++ queryYear ++ ".csv"
            writeFile outPath csv
            putStrLn $ "Data saved at " ++ outPath
            mapM_ print entries
            main
        4 -> do
            hSetBuffering stdout NoBuffering
            entries <- orderYearAllEntries conn
            let title = "country_code;country_name;value;year\n"
            let csv = title ++ toCsvFormat entries
            let cachePath = tempDirectory ++ "/searchByYear.txt"
            let cachePath_ = tempDirectory ++ "/order.txt"
            queryYear <- readFile cachePath
            order <- readFile cachePath_
            removeFile cachePath
            removeFile cachePath_
            flag <- doesDirectoryExist outputDirectory
            createDirectoryIfMissing flag outputDirectory
            let outPath = outputDirectory ++ "/" ++ queryYear ++ "_" ++ order ++ ".csv"
            writeFile outPath csv
            putStrLn $ "Data saved at " ++ outPath
            mapM_ print entries
            main
        5 -> do
            hSetBuffering stdout NoBuffering
            queryCountryTotalPopulation conn
            main
        6 -> do
            hSetBuffering stdout NoBuffering
            insertData conn
            main
        0 -> do
            flag <- doesDirectoryExist outputDirectory
            if flag then do
                removeDirectory tempDirectory
                putStrLn "Hope you've enjoyed using the app!"
            else do
                putStrLn "Hope you've enjoyed using the app!"
        otherwise -> do
            putStrLn "Invalid option"

