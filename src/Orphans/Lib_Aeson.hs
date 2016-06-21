{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Orphans.Lib_Aeson () where

import           Data.Aeson
import           Data.Monoid    ((<>))
import           Errors
import           Models.Country


-- | JSON serialization
instance ToJSON Errors where
  toEncoding e =
    pairs (  "code"    .= _errorCode e
          <> "message" .= _errorMessage e
          )
  {-# INLINE toEncoding #-}


instance ToJSON Country where
  toEncoding c =
    pairs (  "name"      .= _countryName c
          <> "name_long" .= _countryNameLong c
          <> "formal_en" .= _countryFormalEn c
          <> "iso_a2"    .= _countryIsoA2 c
          <> "iso_a3"    .= _countryIsoA3 c
          <> "continent" .= _countryContinent c
          <> "region_un" .= _countryRegionUn c
          <> "subregion" .= _countrySubregion c
          <> "region_wb" .= _countryRegionWb c
          )
  {-# INLINE toEncoding #-}
