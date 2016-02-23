module Coordinates
    ( coord
    , distance
    ) where


type Latitude = Double
type Longitude = Double

-- | Representating Coordinates using Latitude and
-- Longitude.
data Coordinates =
    Coordinates Latitude Longitude
    deriving (Show)

-- | Distances are expressed in meters.
type Distance = Double


coord :: Double -> Double -> Coordinates
coord = Coordinates

-- | Earth radius expressed in kilometers.
radius :: Double
radius = 6372.8

-- | Calculate the distance between two coordinates
-- This is an implementation of the Haversine function.
distance :: Coordinates -> Coordinates -> Distance
distance (Coordinates lat1 lng1) (Coordinates lat2 lng2) =
    let deg2rad deg = pi * deg / 180.0
        [rLat1,rLng1,rLat2,rLng2] = deg2rad <$> [lat1,lng1,lat2,lng2]
        dLong = rLng2 - rLng1
        dLat = rLat2 - rLat1
        a = sin (dLat/2) ** 2 + cos rLat1 * cos rLat2 * sin (dLong/2) ** 2
        c = 2 * asin (sqrt a)
    in  radius * c * 1000