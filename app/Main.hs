{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           Control.Monad.IO.Class
import           Coordinates
import           Data.Aeson                (ToJSON)
import qualified Data.Text                 as T
import           Errors
import           GDAL
import           Models.Country
import           Network.HTTP.Types.Status (Status, status404)
import           Orphans.Lib_Aeson         ()
import           Orphans.Lib_PathPieces    ()
import           Web.Spock.Safe


distancePath :: Path '[Double, Double, Double, Double]
distancePath = "distance" <//> var <//> var <//> var <//> var

countryPath :: Path '[Double, Double]
countryPath = "country" <//> var <//> var

-- | Throws a json error with the appropriate statusCode and
-- error message.
throwError :: (MonadIO m) => Status -> Errors -> ActionCtxT ctw m b
throwError st err = setStatus st >> json err

-- | Throws a 404 with the appropriate Errors message
-- when no results are found.
maybe404 :: (MonadIO m, ToJSON s) => Maybe s -> String -> ActionCtxT ctx m b
maybe404 Nothing err = throwError status404 (Errors err 404)
maybe404 (Just a) _  = json a

main :: IO ()
main =
  withGDAL $ runSpock 3000 $ spockT id $ do
    -- | /distance/:lat1/:lng1/:lat2/:lng2
    -- Get the distance between 2 points.
    get distancePath $ \lat1 lng1 lat2 lng2 ->
      text $ T.pack $ show $
        distance (Coordinates lat1 lng1) (Coordinates lat2 lng2)
    -- | /country/:lat/lng
    -- Get the country who belongs those coordinates.
    get countryPath $ \lat lng -> do
      c <- liftIO $ getCountry (Coordinates lat lng)
      maybe404 c "No country found"
