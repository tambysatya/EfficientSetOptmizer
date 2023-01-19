{-# LANGUAGE TemplateHaskell #-}
module SearchRegion.Internals where

import SearchRegion.UB
import SearchRegion.Class
import MOIP
import IloCplex

import qualified Data.Array as A
import qualified Data.List as L
import Control.Lens
import Data.Function
import Data.Maybe
import Control.Monad.State




{-| TODO use a min heap -}
data SRUB = SRUB {_srUB :: ![UB]}
makeLenses ''SRUB
instance Show SRUB where show (SRUB sr) = show $ fmap _szU sr
type SRUBT = StateT SRUB

--mkSRUB :: IloEnv -> Domain -> FunCoefs -> GlobalBounds -> IO SRUB
--mkSRUB env dom funcoefs gbnds = SRUB <$> mkOptEff env  dom funcoefs <*> pure [mkZone gbnds]
mkSRUB :: GlobalBounds -> SRUB
mkSRUB gbnds = SRUB [mkZone gbnds]

{-
updateSR :: GlobalBounds -> ExploredUB -> ProjDir -> Maybe (Point,HyperOpt) -> SubOpt -> SRUB -> SRUB
updateSR gbnds zexp pdir Nothing _ (SRUB mdl sr) = SRUB mdl $ catMaybes $ updateZoneNothing gbnds zexp pdir <$> sr
updateSR gbnds zexp pdir@(ProjDir k) (Just (pt,hopt)) estimation (SRUB mdl sr) = SRUB mdl $ sr >>= updateZoneJustWithRR gbnds zexp hopt pdir lb pt estimation
    where lb = _ptPerf pt A.! k
-}
updateSR :: (MonadIO m) => GlobalBounds -> ExploredUB -> ProjDir -> Maybe (HyperOpt, Point) -> SubOpt -> SRUBT m ()
updateSR gbnds zexp pdir Nothing _ = do
    sr <- use srUB
    srUB .= catMaybes (updateZoneNothing gbnds zexp pdir <$> sr)
updateSR gbnds zexp pdir@(ProjDir l) (Just (hopt,pt)) estimation@(SubOpt s) = do --SRUB mdl $ sr >>= updateZoneJustWithRR gbnds zexp hopt pdir lb pt estimation
        sr <- use srUB
        ret <- forM sr $ \u -> updateZoneJustWithRR gbnds zexp pdir hopt lb_l pt estimation u

        srUB .= concat ret
    where lb_l = _ptPerf pt A.! l


updateSR_noRR :: (MonadIO m) => GlobalBounds -> Point -> SubOpt -> SRUBT m ()
updateSR_noRR gbnds pt (SubOpt estimation) = do
    sr <- use srUB 
    ret <- forM sr $ \u -> updateZoneJustReopt gbnds u pt
    let --result = filter f $ concat ret
        --f z = _szLB z < HyperOpt estimation
        fun z 
            | _szLB z >= HyperOpt estimation = do
                liftIO $ putStrLn $ "\t\t discarding " ++ show z ++ " [" ++ show (_szLB z) ++ "]"
                pure False
            | otherwise = pure True
    result <- filterM fun $ concat ret
    srUB .= result

    --SRUB mdl $ concatMap (\u -> updateZoneJust gbnds u pt) sr

-- TODO strict evaluation
updateZoneNothing :: GlobalBounds -> ExploredUB -> ProjDir -> UB -> Maybe UB
updateZoneNothing gbnds (ExploredUB zexp) pdir  z 
    | proj pdir z `domL` proj pdir zexp = Nothing
    | otherwise = Just z
    


-- TODO strict evaluation
updateZoneJustWithRR :: (MonadIO m) => GlobalBounds -> ExploredUB -> ProjDir -> HyperOpt -> Double ->Point -> SubOpt -> UB -> SRUBT m [UB]
updateZoneJustWithRR gbnds zexp pdir hopt lb_l pt estimation@(SubOpt cur) ub = do
            newzones <- updateZoneJustReopt gbnds ub pt
            ret <- forM newzones $ applyReductionRule zexp pdir hopt lb_l estimation
            pure $ catMaybes ret
            -- pure $ catMaybes $ applyReductionRule zexp pdir lb_l estimation <$> newzones
            --catMaybes $ applyReductionRule zexp pdir lb_l estimation <$> updateZoneJustHOpt gbnds zexp hopt ub pt 

{-| Before splitting a zone, computes a lowerbound using the point as a warmstart.
    This approximates the lowerbound of the children -}
updateZoneJust :: GlobalBounds -> UB -> Point -> [UB]
updateZoneJust gbnds ub pt
    | pt `domS` ub = catMaybes [child gbnds pt ub i| i <- ChildDir <$> [1..p]]
    | pt `domL` ub = [updateDefiningPoints pt ub]
    | otherwise = [ub]
  where p = dimension ub
updateZoneJustReopt :: (MonadIO m) => GlobalBounds -> UB -> Point -> SRUBT m [UB]
updateZoneJustReopt gbnds ub pt 
        | pt `domS` ub = do 
                liftIO $ putStrLn $ "\t\t Splitting " ++ show ub  ++ " lb=" ++ show (_szLB ub)
                pure $ catMaybes [child gbnds pt ub i | i <- ChildDir <$> [1..p]]
        | pt `domL` ub = do -- pure [updateDefiningPoints pt ub]
                liftIO $ putStrLn $ "\t\t Updating " ++ show ub ++ " lb=" ++ show (_szLB ub)
                pure [updateDefiningPoints pt ub]

        | otherwise = pure [ub]
  where p = dimension ub

{-| Applies the reduction rule if a point have been found y have been found after looking for 
    improving component k by searching in direction l:
    If child-l <= zexp-l (projection is included:
    and pt_l >= child_l 
    then, no point in child can improve the componnent k
 -}
applyReductionRule :: (MonadIO m) =>
           ExploredUB -- The zone
           -> ProjDir -- The projection that have been explored
           -> HyperOpt
           -> Double -- lower bound on projdir
           -> SubOpt
           -> UB
           -> SRUBT m (Maybe UB)
applyReductionRule (ExploredUB zexp) pdir (HyperOpt hopt) lb_l (SubOpt sopt) ub
        | localIdeal >= sopt = do
                liftIO $ putStrLn $ "\t\t discarding " ++ show ub ++ " [lb=" ++ show localIdeal++"]"
                pure Nothing
        | projPred && hopt >= sopt = pure Nothing
        | projPred  &&
          lb_l  >= _szU ub A.! l = do
                liftIO $ putStrLn $ "\t\t discarding " ++ show ub ++ " [reduction rule]"
                pure Nothing
        | projPred  = pure $ Just $ ub & szLB .~ (HyperOpt hopt)
        | otherwise = pure $  Just ub
   where l = fromProjDir pdir
         projPred = proj pdir ub `domL` proj pdir zexp
         (HyperOpt localIdeal) = _szLB ub

    
 
deleteZone :: UB -> SRUB -> SRUB
deleteZone ub (SRUB sr) = SRUB $ L.deleteBy ((==) `on` toBound) ub sr


selectZone :: SRUB -> ExploredUB
--selectZone (SRUB _ sr) = ExploredUB $ L.minimumBy (compare `on` (fst . view szMaxProj)) sr -- minimumBy since we negates the values
selectZone (SRUB sr) = ExploredUB $ L.maximumBy (compare `on` view szLB) sr -- selects the most promising zone (to find the optimum faster)

emptySR :: SRUB -> Bool
emptySR (SRUB sr) = null sr

srSize :: SRUB -> Int
srSize (SRUB sr) = length sr


