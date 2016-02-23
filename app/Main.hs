{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Main where

import           Coordinates
import qualified Data.Text      as T
import           Web.PathPieces
import           Web.Spock.Safe

-- | Here we want to parse Double's from the URL.
-- since parsing double is subjective to our need
-- we have to define it.
instance PathPiece Double where
    fromPathPiece = Just . read . T.unpack
    toPathPiece = T.pack . show


distancePath :: Path '[Double, Double, Double, Double]
distancePath = "distance" <//> var <//> var <//> var <//> var


main :: IO ()
main =
    runSpock 3000 $ spockT id $
    do  get distancePath $ \lat1 lng1 lat2 lng2 ->
            text $ T.pack $ show $
                distance (coord lat1 lng1) (coord lat2 lng2)
        get ("test" <//> var) $ \hello ->
            text hello
