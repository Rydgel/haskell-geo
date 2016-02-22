{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib
import           Web.Spock.Safe
import qualified Data.Text      as T


main :: IO ()
main =
    runSpock 3000 $ spockT id $
    do get ("echo" <//> var) $ \something ->
        text $ T.concat ["Echo: ", something]
