{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module SearchRegion.UB where

import SearchRegion.Class
import Control.Lens
import qualified Data.Array as A
import qualified Data.List as L
import Data.Maybe
import Data.Function

maxbound :: Double
maxbound = 100000000

{-| Wrapping some integers to avoid spending brain ticks on stupid mistakes -}
newtype ChildDir = ChildDir {fromChildDir :: Int}
    deriving (Show, Eq, Ord, Num, A.Ix)




newtype HyperOpt = HyperOpt {fromHyperOpt :: Double}
    deriving (Eq, Ord, Num)
newtype SubOpt = SubOpt {fromSubOpt :: Double}
    deriving (Eq, Ord, Num)
instance Show SubOpt where show (SubOpt s) = "subopt="++ show s
instance Show HyperOpt where show (HyperOpt s) = "hyperopt="++ show s


data UB = UB {_szU :: !Bound,
              _szDefiningPoint :: (A.Array Int [(Point,SubOpt)]),
              _szMaxProj :: ((Int,Double), ProjDir),
              _szLB :: HyperOpt
              }

makeLenses ''UB


newtype ExploredUB = ExploredUB {fromExplored :: UB }
instance Show ExploredUB where
    show (ExploredUB ub) = "zexp=" ++ show ub

instance Boundary UB where
    toBound = _szU
instance Eq UB where
    z == z' = fst (_szMaxProj z) == fst (_szMaxProj z')
instance Ord UB where
    compare = compare `on` (fst . _szMaxProj)

instance Show UB where
    show ub = show (A.elems $ _szU ub)

instance Boundary ExploredUB where
    toBound (ExploredUB ub) = toBound ub



mkZone :: GlobalBounds -> UB
mkZone (yA,yI) = UB (fmap (+1) $ toBound yA) (A.listArray (1,p) $ take p $ repeat []) ((-p,0),ProjDir 2) $ HyperOpt (-maxbound)
    where (1,p) = A.bounds $ toBound yA




{-| Generates the child *childir* from ub and pt after exploring zexp. Checks if the reduction rule applies -}
child :: GlobalBounds 
       -> Point -- The point that have been found (minimizing y_l over u_{-l})
       -> SubOpt 
       -> UB  -- The search zone to be subdivided
       -> ChildDir -- The direction of the subdivision (index of the child) 
       -> Maybe UB -- The result if it stil has a componnent to explore 
child (yA, ideal@(Ideal yI)) pt subopt ub (ChildDir cdir) 
      | _ptPerf pt A.! cdir == yI A.! cdir = Nothing -- Ideal contributing point found
      | and [not $ null $  _szDefiningPoint child A.! i |  -- All unbounded components have at least one defining point
                      i <- [1..p], 
                      childub A.! i /= (toBound yA A.! i + 1)] 
                = Just child
      | otherwise = Nothing
    where p = snd $ A.bounds childub
          childub = _szU ub A.// [(cdir, _ptPerf pt A.! cdir)]
          --childmaxproj = (projVal yA yI ub $ ProjDir cdir, ProjDir cdir)  --projdir = child dir
          childmaxproj = computeMaxProj (yA,ideal) childub  --hypervolume
          childdefpts = A.array (1,p) $ (cdir,[(pt,subopt)]):[(i, validPts) | i <- [1..p],
                                                                              i /= cdir, 
                                                                              let pts = _szDefiningPoint ub A.! i
                                                                                  validPts = [(pti,vi) | (pti,vi) <- pts, _ptPerf pti A.! cdir < _ptPerf pt A.! cdir]]
          child = ub & szU .~ childub
                     & szDefiningPoint .~ childdefpts
                     & szMaxProj .~ childmaxproj



       
{- Utils -}

             
computeMaxProj :: GlobalBounds -> Bound -> ((Int,Double), ProjDir)
computeMaxProj (yA,(Ideal yI)) ub = L.minimumBy (compare `on` fst) [(projVal yA yI ub $ ProjDir i, ProjDir i)  | i <- [1..p], ub A.! i /= (toBound yA A.! i + 1)]
    where (_,p) = A.bounds ub
          -- projVal is negated since we manipulates min heap
projVal yA yI ub (ProjDir i) = --negate $ sum $ logBase 2 <$> zipWith (-) (A.elems $ proj i yA) (A.elems $ proj i ub)
        (negate $ p-length definedComp,
       --  negate $ sum $ [logBase 2 $ (toBound yA A.! j) - (toBound ub A.! j)  | j <- definedComp, j /= i]) -- FAUX
         -- negate $ sum $ [logBase 2 $ (toBound ub A.! j) - (toBound yI A.! j)  | j <- definedComp, j /= i])  -- RE FAUX
         negate $ sum $ [logBase 2 $ (toBound ub A.! j) - (toBound yI A.! j)  | j <- [1..p], j /= i]) 
    where definedComp = [j | j <- [1..p], toBound ub A.! j < toBound yA A.! j]
          p = dimension ub



updateDefiningPoints :: Point -> SubOpt -> UB -> UB
updateDefiningPoints pt ptval zone = foldr f zone $ ProjDir <$> [1..dimension zone]
    where f pdir@(ProjDir i) acc  
                | proj pdir pt `domS` proj pdir zone = acc & szDefiningPoint . ix i %~ ((pt,ptval):) 
                | otherwise = acc

childKHasValidDefPoint (AntiIdeal yA,_) pt k z = and  [not $ null $ validPts | 
                                    i <- [1..p],
                                        i /= k,
                                        _szU z A.! i /= yA A.! i,
                                        let pts = _szDefiningPoint z A.! i
                                            validPts = [pti | (pti,_) <- pts, _ptPerf pti A.! k < _ptPerf pt A.! k]]
        where (_,p) = A.bounds $ _szU z

