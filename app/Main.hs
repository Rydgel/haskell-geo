{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}


module Main where

import           Coordinates
import qualified Data.Text              as T
import           Orphans.Lib_PathPieces ()
import           Web.Spock.Safe
import           GDAL
import           OGR
import qualified Data.Conduit.List as CL
import           Paths_haskellGeo


distancePath :: Path '[Double, Double, Double, Double]
distancePath = "distance" <//> var <//> var <//> var <//> var

getShapePath :: GDAL s FilePath
getShapePath = liftIO $ getDataFileName "ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"


main :: IO ()
main = do
    withGDAL $ runGDAL_ $ do
      ds <- getShapePath >>= OGR.openReadOnly
      let sql = "SELECT name, name_long, formal_en, iso_a2, iso_a3, continent,\
                \       region_un, subregion, region_wb \
                \ FROM  ne_10m_admin_0_countries \
                \ WHERE ST_Intersects(GeomFromText('POINT(2.3488000 48.8534100)'), ne_10m_admin_0_countries.geometry)"
      let src = sourceLayer_ $ executeSQL SqliteDialect sql Nothing ds
      (fs :: [Feature]) <- runOGR (src $$ CL.consume)
      liftIO (print fs)

    runSpock 3000 $ spockT id $ do
        -- | /distance/:lat1/:lng1/:lat2/:lng2
        -- Get the distance between 2 points.
        get distancePath $ \lat1 lng1 lat2 lng2 ->
            text $ T.pack $ show $
              distance (Coordinates lat1 lng1) (Coordinates lat2 lng2)
        get ("test" <//> var) $ \hello ->
            text hello
