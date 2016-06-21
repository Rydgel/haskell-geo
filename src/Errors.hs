{-# LANGUAGE DeriveGeneric #-}

module Errors
  ( Errors(..)
  ) where

import           GHC.Generics (Generic)

data Errors = Errors
  { _errorMessage :: String
  , _errorCode    :: Int
  } deriving (Show, Generic)
