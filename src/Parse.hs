{-|
Module      : Parse
Description : Parse
Copyright   : (c) Qingyuan Zheng, 2021
                  Yixun Zhou, 2021
                  Lyujun Kang, 2021
License     : GPL-3
Maintainer  : jp2016213096@qmul.ac.uk
Stability   : experimental
Portability : POSIX

In this module, we download the Json file from the URl. Then I load the file, which is in the format of IO ByteString, into mydata. This step convert
   the Json file into ByteString. Thanks to Data.Bytestring.Lazy, we can do comprehension for ByteString data. I split the who Bytestring
   into a list, whose each element is an entry of data. Then I define a recursive function that do decode for every entry in the list. 
   This Step transfer Bytestring entry into Maybe Record. Lastly, I define a function convert Maybe Record to Record, and map the function
   to every entry. Finally, we get a lsit of Record, which precisely is [Record].
-}

{-# LANGUAGE OverloadedStrings #-}

module Parse (
    parseData,
    toCsvFormat
) where


import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Functor
import Data.ByteString.Lazy
import Control.Applicative
import Control.Monad
import Types


instance FromJSON Record where
   parseJSON (Object v) = Record <$>
                          v .: "Country Code" <*>
                          v .: "Country Name" <*>
                          v .: "Value" <*>
                          v .: "Year" 
   parseJSON _ = mzero

instance ToJSON Record where
   toJSON (Record country_code country_name value year) = object ["Country Code" .= country_code, "Country Name" .= country_name, "Value" .= value, "Year" .= year]


-- |Decode json files into record.
decodeJSON :: [ByteString] -> [Maybe Record]
decodeJSON [] = []
decodeJSON (x:xs) = (decode x :: Maybe Record) : (decodeJSON xs)

-- |Tool function that helps decoding.
suffix_ :: ByteString
suffix_ = "}"

-- |Transfer Maybe Record to Record
justTrans :: Maybe a -> a
justTrans (Just a) = a

-- |Do comprehension on ByteString data and generate a list of records.
parseData :: ByteString -> IO [Record]
parseData json = do
            let myJSONtem = B.tail (B.init json)
            let myJSONtem2 = B.split 125 myJSONtem
            let myJSONtem3 = Prelude.tail $ Prelude.init myJSONtem2
            let myJSONtem4 = Prelude.map (B.tail) myJSONtem3
            let myJSONtem5 = Prelude.map (<> suffix_) myJSONtem4
            let mydata1 = decodeJSON myJSONtem5
            let mydata = Prelude.map justTrans mydata1
            return mydata

-- |Transfer the list of Record, so that we can write output into files.
toCsvFormat :: [Record] -> String
toCsvFormat [] = []
toCsvFormat (x:xs) = (country_code x) ++ ";" ++ (country_name x) ++ ";" ++ (show $ value x) ++ ";" ++ (show $ year x) ++ "\n" ++ toCsvFormat xs
