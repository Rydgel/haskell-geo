{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}


module Main where

import           Coordinates
import qualified Data.Text              as T
import           Orphans.Lib_PathPieces ()
import           Web.Spock.Safe
import           Models.Country
import           Control.Monad.IO.Class


distancePath :: Path '[Double, Double, Double, Double]
distancePath = "distance" <//> var <//> var <//> var <//> var


main :: IO ()
main = do
    -- let countryFile = countryShpFile
    runSpock 3000 $ spockT id $ do
        -- | /distance/:lat1/:lng1/:lat2/:lng2
        -- Get the distance between 2 points.
        get distancePath $ \lat1 lng1 lat2 lng2 ->
            text $ T.pack $ show $
              distance (Coordinates lat1 lng1) (Coordinates lat2 lng2)
        get "test" $ do
            c <- liftIO $ getCountry $ Coordinates 1 1
            text $ T.pack $ show c
