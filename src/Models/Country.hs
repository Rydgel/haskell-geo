{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Models.Country
  ( countryShpFile
  , getCountry
  , Country(..)
  ) where

import           Control.DeepSeq
import           Coordinates
import qualified Data.Conduit.List as CL
import qualified Data.Text         as T
import           GDAL
import           GHC.Generics      (Generic)
import           OGR
import           Paths_haskellGeo
import           Text.Printf


data Country = Country
    { countryName      :: !String
    , countryNameLong  :: !String
    , countryFormalEn  :: !String
    , countryIsoA2     :: !String
    , countryIsoA3     :: !String
    , countryContinent :: !String
    , countryRegionUn  :: !String
    , countrySubregion :: !String
    , countryRegionWb  :: !String
    } deriving (Show, Generic, NFData)

file :: String
file = "ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"

getShapePath :: forall s. GDAL s FilePath
getShapePath = liftIO $ getDataFileName file

countryShpFile :: forall s. GDAL s (RODataSource s)
countryShpFile = getShapePath >>= OGR.openReadOnly


-- | Get the country with Coordinates
getCountry :: Coordinates -> IO (Either GDALException Country)
getCountry coord =
    runGDAL $ do
        ds <- countryShpFile
        let sql = printf "SELECT name, name_long, formal_en, iso_a2, iso_a3, continent,\
                       \         region_un, subregion, region_wb \
                       \  FROM   ne_10m_admin_0_countries \
                       \  WHERE  ST_Intersects(GeomFromText('%s'), ne_10m_admin_0_countries.geometry)" (toGeometryPoint coord)
        let src = sourceLayer_ $ executeSQL SqliteDialect (T.pack sql) Nothing ds
        (fs :: [Feature]) <- runOGR (src $$ CL.consume)
        liftIO $ print fs
        return $ Country "" "" "" "" "" "" "" "" ""
