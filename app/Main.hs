{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           Control.Monad.IO.Class
import           Coordinates
import qualified Data.Text              as T
import           GDAL
import           Models.Country
import           Orphans.Lib_Aeson      ()
import           Orphans.Lib_PathPieces ()
import           Web.Spock.Safe


distancePath :: Path '[Double, Double, Double, Double]
distancePath = "distance" <//> var <//> var <//> var <//> var

countryPath :: Path '[Double, Double]
countryPath = "country" <//> var <//> var


main :: IO ()
main =
    withGDAL $ runSpock 3000 $ spockT id $ do
        -- | /distance/:lat1/:lng1/:lat2/:lng2
        -- Get the distance between 2 points.
        get distancePath $ \lat1 lng1 lat2 lng2 ->
            text $ T.pack $ show $
              distance (Coordinates lat1 lng1) (Coordinates lat2 lng2)
        get countryPath $ \lat lng -> do
            -- 48.8534100 2.3488000
            c <- liftIO $ getCountry (Coordinates lat lng)
            json c
