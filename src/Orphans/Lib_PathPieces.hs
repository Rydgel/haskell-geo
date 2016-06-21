{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans.Lib_PathPieces () where

import qualified Data.Text      as T
import           Web.PathPieces

-- | Here we want to parse Double's from the URL.
-- since parsing double is subjective to our need
-- we have to define it.
instance PathPiece Double where
  fromPathPiece = Just . read . T.unpack
  toPathPiece = T.pack . show
