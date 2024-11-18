{-# LANGUAGE DeriveDataTypeable #-}
module Error.Error (RaincatError(..), showError) where

import Data.Typeable
import Control.Exception as E

data RaincatError
    = BadLevelData String
    | BadVerticesData
    | BadRectData
    deriving (Typeable, Show)

instance Exception RaincatError where

showError :: RaincatError -> String
showError err = case err of
    BadLevelData obj -> "Invalid level data: " ++ show obj
    BadVerticesData      -> "Unmatched vertice count"
    BadRectData          -> "Unmatched coord count. 8 coords expected."
