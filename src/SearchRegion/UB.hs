{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module SearchRegion.UB where

import SearchRegion.Class
import Control.Lens
import qualified Data.Array.Unboxed as AU
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
    show ub = show (AU.elems $ _szU ub)

instance Boundary ExploredUB where
    toBound (ExploredUB ub) = toBound ub



mkZone :: GlobalBounds -> UB
mkZone (yA,yI) = UB initBnd (A.listArray (1,p) $ take p $ repeat []) ((-p,0),ProjDir 2) $ HyperOpt (-maxbound)
    where (1,p) = AU.bounds $ toBound yA
          initBnd = AU.listArray (1,p) $ fmap (+1) (AU.elems $ toBound yA)




{-| Generates the child *childir* from ub and pt after exploring zexp. Checks if the reduction rule applies -}
child :: GlobalBounds 
       -> Point -- The point that have been found (minimizing y_l over u_{-l})
       -> SubOpt 
       -> UB  -- The search zone to be subdivided
       -> ChildDir -- The direction of the subdivision (index of the child) 
       -> Maybe UB -- The result if it stil has a componnent to explore 
child (yA, ideal@(Ideal yI)) pt subopt ub (ChildDir cdir) 
      | _ptPerf pt AU.! cdir == yI AU.! cdir = Nothing -- Ideal contributing point found
      | and [not $ null $  _szDefiningPoint child AU.! i |  -- All unbounded components have at least one defining point
                      i <- [1..p], 
                      childub AU.! i /= (toBound yA AU.! i + 1)] 
                = Just child
      | otherwise = Nothing
    where p = snd $ AU.bounds childub
          childub = _szU ub AU.// [(cdir, _ptPerf pt AU.! cdir)]
          --childmaxproj = (projVal yA yI ub $ ProjDir cdir, ProjDir cdir)  --projdir = child dir
          childmaxproj = computeMaxProj (yA,ideal) childub  --hypervolume
          childdefpts = A.array (1,p) $ (cdir,[(pt,subopt)]):[(i, validPts) | i <- [1..p],
                                                                              i /= cdir, 
                                                                              let pts = _szDefiningPoint ub A.! i
                                                                                  validPts = [(pti,vi) | (pti,vi) <- pts, _ptPerf pti AU.! cdir < _ptPerf pt AU.! cdir]]
          child = ub & szU .~ childub
                     & szDefiningPoint .~ childdefpts
                     & szMaxProj .~ childmaxproj



       
{- Utils -}

             
computeMaxProj :: GlobalBounds -> Bound -> ((Int,Double), ProjDir)
computeMaxProj (yA,(Ideal yI)) ub = L.minimumBy (compare `on` fst) [(projVal yA yI ub $ ProjDir i, ProjDir i)  | i <- [1..p], ub AU.! i /= (toBound yA AU.! i + 1)]
    where (_,p) = AU.bounds ub
          -- projVal is negated since we manipulates min heap
projVal yA yI ub (ProjDir i) = --negate $ sum $ logBase 2 <$> zipWith (-) (A.elems $ proj i yA) (A.elems $ proj i ub)
        (negate $ p-length definedComp,
       --  negate $ sum $ [logBase 2 $ (toBound yA A.! j) - (toBound ub A.! j)  | j <- definedComp, j /= i]) -- FAUX
         -- negate $ sum $ [logBase 2 $ (toBound ub A.! j) - (toBound yI A.! j)  | j <- definedComp, j /= i])  -- RE FAUX
         negate $ sum $ [logBase 2 $ (toBound ub AU.! j) - (toBound yI AU.! j)  | j <- [1..p], j /= i]) 
    where definedComp = [j | j <- [1..p], toBound ub AU.! j < toBound yA AU.! j]
          p = dimension ub



updateDefiningPoints :: Point -> SubOpt -> UB -> UB
updateDefiningPoints pt ptval zone = foldr f zone $ ProjDir <$> [1..dimension zone]
    where f pdir@(ProjDir i) acc  
                | proj pdir pt `domS` proj pdir zone = acc & szDefiningPoint . ix i %~ ((pt,ptval):) 
                | otherwise = acc

childKHasValidDefPoint (AntiIdeal yA,_) pt k z = and  [not $ null $ validPts | 
                                    i <- [1..p],
                                        i /= k,
                                        _szU z AU.! i /= yA AU.! i,
                                        let pts = _szDefiningPoint z A.! i
                                            validPts = [pti | (pti,_) <- pts, _ptPerf pti AU.! k < _ptPerf pt AU.! k]]
        where (_,p) = AU.bounds $ _szU z

