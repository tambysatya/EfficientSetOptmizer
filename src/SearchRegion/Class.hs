{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module SearchRegion.Class where

import qualified Data.Array as A
import Data.Function

type Bound = A.Array Int Double
newtype Ideal = Ideal Bound
newtype AntiIdeal = AntiIdeal Bound
type GlobalBounds = (AntiIdeal, Ideal)


data Point = Point {_ptPerf :: A.Array Int Double,
                    _ptSol :: A.Array Int Double}

newtype ProjDir = ProjDir {fromProjDir :: Int}
	deriving (Eq, Num, Ord, A.Ix)
instance Show ProjDir where
    show (ProjDir p) = "proj=" ++ show p



class Boundary a where
    toBound :: a -> Bound

instance Boundary Point where
    toBound = _ptPerf

instance Boundary (A.Array Int Double) where
    toBound = id
instance Boundary AntiIdeal where
    toBound (AntiIdeal yU) = yU

dimension :: Boundary a => a -> Int
dimension z = snd $ A.bounds $ toBound z

proj :: (Boundary a) => ProjDir -> a -> Bound -- TODO linear type ?
proj (ProjDir k) z = A.listArray (1, dimension z - 1) [toBound z A.! i | i <- [1..dimension z], i /= k]

domS :: (Boundary a, Boundary b) => a -> b -> Bool
x `domS` y = and $ zipWith (<) (A.elems $ toBound x) (A.elems $ toBound y)

domL :: (Boundary a, Boundary b) => a -> b -> Bool
x `domL` y = and $ zipWith (<=) (A.elems $ toBound x) (A.elems $ toBound y)

instance Show Point where
    show = show . A.elems . _ptPerf
instance Eq Point where
    x == y = A.elems (_ptPerf x) == A.elems (_ptPerf y) -- TODO dangereux
