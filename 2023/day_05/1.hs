{-# LANGUAGE TupleSections #-}
import Data.Functor ((<&>))
import Data.Char (isDigit)
import qualified Data.List as L
import Data.Bifunctor (second, first)
import Data.Maybe (mapMaybe)

both :: (t -> b) -> (t, t) -> (b, b)
both f (a,b) = (f a, f b)