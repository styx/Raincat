{-# LANGUAGE DeriveDataTypeable #-}
module Error.Error (RaincatError(..), throwEx, catchEx, showError) where

import Data.Typeable
import Control.Exception.Extensible as EE
import Control.Monad.Error

data RaincatError
    = BadLevelData String
    | BadVerticesData
    | BadRectData
    deriving (Typeable, Show)

instance Error RaincatError where
instance Exception RaincatError where

throwEx :: RaincatError -> a
throwEx = EE.throw

catchEx :: IO a -> (RaincatError -> IO a) -> IO a
catchEx = EE.catch

showError :: RaincatError -> String
showError err = case err of
    BadLevelData obj -> "Invalid level data: " ++ show obj
    BadVerticesData      -> "Unmatched vertice count"
    BadRectData          -> "Unmatched coord count. 8 coords expected."
