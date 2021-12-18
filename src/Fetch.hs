{-|
Module      : Fetch
Description : Fetch
Copyright   : (c) Yixun Zhou, 2021
License     : GPL-3
Maintainer  : yixun.zhou@se16.qmul.ac.uk
Stability   : experimental
Portability : POSIX

This module defines a function for downloading the json file from the web
-}


module Fetch (
    download
) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

type URL = String

-- |Downloading data from Internet by given url
download :: URL -> IO L8.ByteString
download url = do
    request <- parseRequest url
    response <- httpLBS request
    return $ getResponseBody response


