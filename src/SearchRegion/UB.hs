{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module SearchRegion.UB where

import SearchRegion.Class
import Control.Lens
import qualified Data.Array as A
import Data.List
import Data.Maybe
import Data.Function

maxbound :: Double
maxbound = 100000000

{-| Wrapping some integers to avoid spending brain ticks on stupid mistakes -}
newtype ChildDir = ChildDir {fromChildDir :: Int}
    deriving (Show, Eq, Ord, Num, A.Ix)





data UB = UB {_szU :: !Bound,
              _szDefiningPoint :: ! (A.Array Int [Point]),
              _szMaxProj :: (Double, ProjDir)}

makeLenses ''UB


newtype ExploredUB = ExploredUB {fromExplored :: UB }
instance Show ExploredUB where
    show (ExploredUB ub) = "zexp=" ++ show ub

{-| TODO use a min heap -}
data SRUB = SRUB {_srub :: ![UB]}
instance Show SRUB where show (SRUB sr) = show $ fmap (A.elems ._szU ) sr

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
mkZone (yA,yI) = UB (fmap (+1) $ toBound yA) (A.listArray (1,p) $ take p $ repeat []) (0,ProjDir 2)
    where (1,p) = A.bounds $ toBound yA

updateSR :: GlobalBounds -> ExploredUB -> ProjDir -> Maybe Point -> Double -> SRUB -> SRUB
updateSR gbnds zexp pdir Nothing _ (SRUB sr) = SRUB $ catMaybes $ updateZoneNothing gbnds zexp pdir <$> sr
updateSR gbnds zexp pdir@(ProjDir k) (Just pt) estimation (SRUB sr) = SRUB $ sr >>= updateZoneJustWithRR gbnds zexp pdir lb pt estimation
    where lb = _ptPerf pt A.! k

{-| Updates a zone z and returns the potentially
      - Nothing (deleted by a reduction rule)
      - The singleton [z], the zone unchanged except potentially its defining points
      - A child
 -}     

{-
updateZone :: GlobalBounds -> NadDir -> ExploredUB -> ProjDir -> Maybe Point -> UB -> ChildDir -> Maybe UB
updateZone gbnds ndir zexp pdir pt ub cdir
    | isJust pt = updateZoneJustWithRR gbnds ndir zexp pdir ub (fromJust pt) cdir
    | otherwise = updateZoneNothing gbnds ndir zexp pdir ub cdir
-}

-- TODO strict evaluation
updateZoneNothing :: GlobalBounds -> ExploredUB -> ProjDir -> UB -> Maybe UB
updateZoneNothing gbnds (ExploredUB zexp) pdir  z 
    | proj pdir z `domL` proj pdir zexp = Nothing
    | otherwise = Just z
    


-- TODO strict evaluation
updateZoneJustWithRR :: GlobalBounds -> ExploredUB -> ProjDir -> Double ->Point -> Double -> UB -> [UB]
updateZoneJustWithRR gbnds zexp pdir lb_l pt estimation ub = useLocalLowerBound $ 
                                                             catMaybes $ 
                                                                 applyReductionRule zexp pdir lb_l <$> updateZoneJust gbnds ub pt 
    where useLocalLowerBound lz = [li | li <- lz, sum (A.elems $ toBound li) <= estimation]
updateZoneJust :: GlobalBounds -> UB -> Point -> [UB]
updateZoneJust gbnds ub  pt 
        | pt `domS` ub = catMaybes [child gbnds pt ub i | i <- ChildDir <$> [1..p]]
        | pt `domL` ub = [updateDefiningPoints pt ub]
        | otherwise = [ub]
    where p = dimension ub

{-| Applies the reduction rule if a point have been found y have been found after looking for 
    improving component k by searching in direction l:
    If child-l <= zexp-l (projection is included:
    and pt_l >= child_l 
    then, no point in child can improve the componnent k
 -}
applyReductionRule ::
           ExploredUB -- The zone
           -> ProjDir -- The projection that have been explored
           -> Double -- lower bound on projdir
           -> UB
           -> Maybe UB
applyReductionRule zexp pdir lb_l ub 
    | reductionRuleP zexp pdir lb_l ub = Nothing
    | otherwise = Just ub
   where l = fromProjDir pdir

reductionRuleP (ExploredUB zexp) pdir lb_l ub
        | proj pdir ub `domL` proj pdir zexp &&
          lb_l  >= _szU ub A.! l = True
        | otherwise = False
   where l = fromProjDir pdir

    



{-| Generates the child *childir* from ub and pt after exploring zexp. Checks if the reduction rule applies -}
child :: GlobalBounds 
       -> Point -- The point that have been found (minimizing y_l over u_{-l})
       -> UB  -- The search zone to be subdivided
       -> ChildDir -- The direction of the subdivision (index of the child) 
       -> Maybe UB -- The result if it stil has a componnent to explore 
child (yA, (Ideal yI)) pt ub (ChildDir cdir) 
      | _ptPerf pt A.! cdir == yI A.! cdir = Nothing -- Ideal contributing point found
      | and [not $ null $  _szDefiningPoint child A.! i |  -- All unbounded components have at least one defining point
                      i <- [1..p], 
                      childub A.! i /= (toBound yA A.! i + 1)] 
                = Just child
      | otherwise = Nothing
    where p = snd $ A.bounds childub
          childub = _szU ub A.// [(cdir, _ptPerf pt A.! cdir)]
          --childmaxproj = (projVal yA ub $ ProjDir cdir, ProjDir cdir) 
          childmaxproj = computeMaxProj yA childub 
          childdefpts = A.array (1,p) $ (cdir,[pt]):[(i, validPts) | i <- [1..p],
                                                                     i /= cdir, 
                                                                     let pts = _szDefiningPoint ub A.! i
                                                                         validPts = [pti | pti <- pts, _ptPerf pti A.! cdir < _ptPerf pt A.! cdir]]
          child = ub & szU .~ childub
                     & szDefiningPoint .~ childdefpts
                     & szMaxProj .~ childmaxproj


selectZone :: SRUB -> ExploredUB
selectZone (SRUB sr) = ExploredUB $ minimumBy (compare `on` (fst . view szMaxProj)) sr -- minimumBy since we negates the values

emptySR :: SRUB -> Bool
emptySR (SRUB sr) = null sr

srSize :: SRUB -> Int
srSize (SRUB sr) = length sr


       
{- Utils -}

             
computeMaxProj :: AntiIdeal -> Bound -> (Double, ProjDir)
computeMaxProj yA ub = minimumBy (compare `on` fst) [(projVal yA ub $ ProjDir i, ProjDir i)  | i <- [1..p], ub A.! i /= (toBound yA A.! i + 1)]
    where (_,p) = A.bounds ub
          -- projVal is negated since we manipulates min heap
projVal yA ub i = negate $ sum $ logBase 2 <$> zipWith (-) (A.elems $ proj i yA) (A.elems $ proj i ub)



updateDefiningPoints :: Point -> UB -> UB
updateDefiningPoints pt zone = foldr f zone $ ProjDir <$> [1..dimension zone]
    where f pdir@(ProjDir i) acc  
                | proj pdir pt `domS` proj pdir zone = acc & szDefiningPoint . ix i %~ (pt:) 
                | otherwise = acc

childKHasValidDefPoint (AntiIdeal yA,_) pt k z = and  [not $ null $ validPts | 
                                    i <- [1..p],
                                        i /= k,
                                        _szU z A.! i /= yA A.! i,
                                        let pts = _szDefiningPoint z A.! i
                                            validPts = [pti | pti <- pts, _ptPerf pti A.! k < _ptPerf pt A.! k]]
        where (_,p) = A.bounds $ _szU z
 
