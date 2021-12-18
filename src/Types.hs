{-|
Module      : Types
Description : Types
Copyright   : (c) Fanyuqian Yang, 2021
                  Yixun Zhou, 2021
                  Qingyuan Zheng, 2021
License     : GPL-3
Maintainer  : jp2016213454@qmul.ac.uk
Stability   : experimental
Portability : POSIX

In this module, we define the new data types that we will use in our project.
-}
{-# LANGUAGE DeriveGeneric #-}

module Types (
    Entry(..),
    Population(..),
    Record(..),
    Records(..)
)where

import GHC.Generics

-- |New data type Entry
data Entry = Entry {
    id_ :: Int,
    country_name_ :: String,
    country_code_ :: String
 } deriving (Show)

-- |New data type Population
data Population = Population {
    value_ :: Integer,
    year_ :: Int,
    fk_country :: Int
} deriving (Show)

-- |New data type Record
data Record = Record {
    country_code :: String,
    country_name :: String,
    value :: Integer,
    year :: Int
} deriving (Show, Generic)

-- |New data type Records
data Records = Records {
    records :: [Record]
} deriving (Show, Generic)




