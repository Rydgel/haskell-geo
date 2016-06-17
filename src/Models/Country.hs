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
import           GDAL
import           GHC.Generics      (Generic)
import           OGR
import           Paths_haskellGeo


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

getShapePath :: GDAL s FilePath
getShapePath = liftIO $ getDataFileName file

countryShpFile :: GDAL s (RODataSource s)
countryShpFile = getShapePath >>= OGR.openReadOnly

-- | Get the country with Coordinates
getCountry :: Coordinates -> IO (Either GDALException Country)
getCountry (Coordinates lat long) =
    withGDAL $ runGDAL $ do
        ds <- countryShpFile
        let sql = "SELECT name, name_long, formal_en, iso_a2, iso_a3, continent,\
                \       region_un, subregion, region_wb \
                \  FROM  ne_10m_admin_0_countries \
                \  WHERE ST_Intersects(GeomFromText('POINT(2.3488000 48.8534100)'), ne_10m_admin_0_countries.geometry)"
        let src = sourceLayer_ $ executeSQL SqliteDialect sql Nothing ds
        (fs :: [Feature]) <- runOGR (src $$ CL.consume)
        liftIO $ print fs
        return $ Country "" "" "" "" "" "" "" "" ""
