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
import           Control.Monad           (join)
import           Coordinates
import qualified Data.Conduit.List       as CL
import           Data.Either.Combinators
import qualified Data.HashMap.Strict     as HM
import qualified Data.Text               as T
import           GDAL
import           GHC.Generics            (Generic)
import           OGR
import           Paths_haskellGeo
import           Text.Printf


data Country = Country
    { _countryName      :: !T.Text
    , _countryNameLong  :: !T.Text
    , _countryFormalEn  :: !T.Text
    , _countryIsoA2     :: !T.Text
    , _countryIsoA3     :: !T.Text
    , _countryContinent :: !T.Text
    , _countryRegionUn  :: !T.Text
    , _countrySubregion :: !T.Text
    , _countryRegionWb  :: !T.Text
    } deriving (Show, Generic, NFData)

-- | Corresponding shapefile for querying countries data.
file :: String
file = "ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"

-- | Get full path of the shapefile.
getShapePath :: forall s. GDAL s FilePath
getShapePath = liftIO $ getDataFileName file

-- | Open the shapefile.
countryShpFile :: forall s. GDAL s (RODataSource s)
countryShpFile = getShapePath >>= OGR.openReadOnly

-- | We parse the fields from the shapefile into our datatype.
parseResponseOne :: [Feature] -> Maybe Country
parseResponseOne [] = Nothing
parseResponseOne fs = Just country
    where
        fields = fFields (head fs)
        fieldToText (OGRString x) = x
        fieldToText x             = T.pack (show x)
        country = Country { _countryName = fieldToText $ fields HM.! "NAME"
                          , _countryNameLong = fieldToText $ fields HM.! "NAME_LONG"
                          , _countryFormalEn = fieldToText $ fields HM.! "FORMAL_EN"
                          , _countryIsoA2 = fieldToText $ fields HM.! "WB_A2"
                          , _countryIsoA3 = fieldToText $ fields HM.! "WB_A3"
                          , _countryContinent = fieldToText $ fields HM.! "CONTINENT"
                          , _countryRegionUn = fieldToText $ fields HM.! "REGION_UN"
                          , _countrySubregion = fieldToText $ fields HM.! "SUBREGION"
                          , _countryRegionWb = fieldToText $ fields HM.! "REGION_WB"
                          }

-- | Query the intersection of a point and the countries
-- and returns the needed SQL query string.
getCountrySQL :: Coordinates -> T.Text
getCountrySQL coord = T.pack $ printf s (toGeometryPoint coord)
    where
        s = unlines [ "SELECT name, name_long, formal_en, wb_a2, wb_a3, continent,"
                    , "region_un, subregion, region_wb "
                    , "FROM ne_10m_admin_0_countries "
                    , "WHERE ST_Intersects(GeomFromText('%s'), geometry)"
                    ]

-- | Get the country with Coordinates.
getCountry :: Coordinates -> IO (Maybe Country)
getCountry coord = do
    gdalResponse <- runGDAL $ do
        ds <- countryShpFile
        let src = sourceLayer_ $ executeSQL SqliteDialect (getCountrySQL coord) Nothing ds
        (fs :: [Feature]) <- runOGR (src $$ CL.consume)
        return $ parseResponseOne fs
    return $ join $ rightToMaybe gdalResponse
