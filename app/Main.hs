{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Coordinates
import qualified Data.Text              as T
import           Orphans.Lib_PathPieces ()
import           Web.Spock.Safe


distancePath :: Path '[Double, Double, Double, Double]
distancePath = "distance" <//> var <//> var <//> var <//> var


main :: IO ()
main =
    runSpock 3000 $ spockT id $
    do  get distancePath $ \lat1 lng1 lat2 lng2 ->
            text $ T.pack $ show $
                distance (Coordinates lat1 lng1) (Coordinates lat2 lng2)
        get ("test" <//> var) $ \hello ->
            text hello
