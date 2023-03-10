{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, BangPatterns #-}
module SearchRegion.Class where

import qualified Data.Array.Unboxed as A
import Data.Function
import Control.DeepSeq

type Bound = A.UArray Int Double
newtype Ideal = Ideal Bound
newtype AntiIdeal = AntiIdeal Bound
type GlobalBounds = (AntiIdeal, Ideal)


data Point = Point {_ptPerf :: !(A.UArray Int Double),
                    _ptSol :: !(A.UArray Int Double)}

newtype ProjDir = ProjDir {fromProjDir :: Int}
	deriving (Eq, Num, Ord, A.Ix)
instance Show ProjDir where
    show (ProjDir p) = "proj=" ++ show p

instance NFData ProjDir where rnf (ProjDir !v) = v `seq` ()
instance NFData Point where rnf (Point !x !y) = x `seq` y `seq` ()

class Boundary a where
    toBound :: a -> Bound

instance Boundary Point where
    toBound = _ptPerf

instance Boundary (A.UArray Int Double) where
    toBound = id
instance Boundary AntiIdeal where
    toBound (AntiIdeal yU) = yU

dimension :: Boundary a => a -> Int
dimension z = snd $ A.bounds $ toBound z
{-# INLINE dimension #-}

proj :: (Boundary a) => ProjDir -> a -> [Double]-- TODO linear type ?
proj (ProjDir k) z = [toBound z A.! i | i <- [1..dimension z], i /= k]
{-# INLINE proj #-}

domS :: (Boundary a, Boundary b) => a -> b -> Bool
x `domS` y = and $ zipWith (<) (A.elems $ toBound x) (A.elems $ toBound y)
{-# INLINE domS #-}

domL :: (Boundary a, Boundary b) => a -> b -> Bool
x `domL` y = and $ zipWith (<=) (A.elems $ toBound x) (A.elems $ toBound y)
{-# INLINE domL #-}

lDomL :: [Double] -> [Double] -> Bool
x `lDomL` y = and $ zipWith (<=) x y
{-# INLINE lDomL #-}
lDomS :: [Double] -> [Double] -> Bool
x `lDomS` y = and $ zipWith (<) x y
{-# INLINE lDomS #-}


instance Show Point where
    show = show . A.elems . _ptPerf
instance Eq Point where
    x == y = A.elems (_ptPerf x) == A.elems (_ptPerf y) -- TODO dangereux
instance Ord Point where
    x `compare` y = _ptPerf x `compare` _ptPerf y
