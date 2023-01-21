{-# LANGUAGE TemplateHaskell #-}
module SearchRegion.Internals where

import Utils
import SearchRegion.UB
import SearchRegion.Class
import SearchRegion.Archive
import MOIP
import IloCplex

import qualified Data.Array as A
import qualified Data.List as L
import Control.Lens
import Data.Function
import Data.Maybe
import Control.Monad.State




{-| TODO use a min heap -}
data SRUB = SRUB {_srUB :: ![UB] , _yArchive :: YArchive, _xeArchive :: XeArchive}
makeLenses ''SRUB
instance Show SRUB where show (SRUB sr _ _) = show $ fmap _szU sr
type SRUBT = StateT SRUB

mkSRUB :: GlobalBounds -> SRUB
mkSRUB gbnds = SRUB [mkZone gbnds] (YArchive []) (XeArchive [])

updateSR :: (MonadIO m) => GlobalBounds -> ExploredUB -> ProjDir -> Maybe (HyperOpt, Point, SubOpt, Double) -> SubOpt -> SRUBT m ()
updateSR gbnds zexp pdir Nothing _ = do
    sr <- use srUB
    -- TODO archive
    srUB .= catMaybes (updateZoneNothing gbnds zexp pdir <$> sr)
    yArchive %= insertYMdl (mkYMdl zexp Nothing)

updateSR gbnds zexp pdir@(ProjDir l) (Just (hopt,pt, ptval,lb_l)) estimation@(SubOpt s) = do --SRUB mdl $ sr >>= updateZoneJustWithRR gbnds zexp hopt pdir lb pt estimation
        sr <- use srUB
        ret <- forM sr $ \u -> updateZoneJustWithRR gbnds zexp pdir hopt lb_l (pt,ptval) estimation u

        yArchive %= insertYMdl (mkYMdl zexp (Just lb_l))
        srUB .= concat ret
    --where lb_l = _ptPerf pt A.! l


updateSR_noRR :: (MonadIO m) => GlobalBounds -> (Point,SubOpt) -> SubOpt -> SRUBT m ()
updateSR_noRR gbnds pt (SubOpt estimation) = do
    sr <- use srUB 
    yar <- use yArchive
    ret <- forM sr $ \u -> updateZoneJust gbnds u pt

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
updateZoneJustWithRR :: (MonadIO m) => GlobalBounds -> ExploredUB -> ProjDir -> HyperOpt -> Double -> (Point,SubOpt) -> SubOpt -> UB -> SRUBT m [UB]
updateZoneJustWithRR gbnds zexp pdir hopt lb_l pt estimation@(SubOpt cur) ub = do
            newzones <- updateZoneJustReopt gbnds ub pt
            ret <- forM newzones $ applyReductionRule zexp pdir hopt lb_l estimation
            yar <- use yArchive
            xar <- use xeArchive
    
            newzones' <- forM (catMaybes ret) $ \zi -> let yreqM = checkYMdl (ExploredUB zi) yar
                                                       in case yreqM of
                                                            Nothing -> do 
                                                                let xreqM = checkXeMdl zi estimation xar
                                                                case xreqM of
                                                                    Nothing -> pure $ Just zi
                                                                    Just mdl -> do 
                                                                        liftIO $ putStrLn $ "\t\t discarding " ++ show zi ++ " [Xarchive: " ++ show mdl ++ "]"
                                                                        pure Nothing
                                                                    
                                                            Just mdl -> do
                                                                liftIO $ putStrLn $ "\t\t discarding " ++ show zi ++ " [Yarchive: " ++ show mdl ++ "]"
                                                                pure Nothing
            pure $ catMaybes newzones'
            -- pure $ catMaybes $ applyReductionRule zexp pdir lb_l estimation <$> newzones
            --catMaybes $ applyReductionRule zexp pdir lb_l estimation <$> updateZoneJustHOpt gbnds zexp hopt ub pt 

updateZoneJust :: (MonadIO m) => GlobalBounds -> UB -> (Point,SubOpt) -> SRUBT m [UB]
updateZoneJust gbnds ub (pt,ptval)
    | pt `domS` ub = do
        yar <- use yArchive 
        let children = catMaybes [child gbnds pt ptval ub i| i <- ChildDir <$> [1..p]]
            filterFun ci = case checkYMdl (ExploredUB ci) yar of
                Nothing -> pure True
                Just ymdl -> do
                    logM $ "\t\t discarding " ++ show ci ++ " [YArchive: " ++ show ymdl ++ "]"
                    pure False
        filterM filterFun children
    | pt `domL` ub = do
        logM $ "\t\t Updating " ++ show ub
        pure $ [updateDefiningPoints pt ptval ub]
    | otherwise = pure $ [ub]
  where p = dimension ub

updateZoneJustReopt :: (MonadIO m) => GlobalBounds -> UB -> (Point,SubOpt) -> SRUBT m [UB]
updateZoneJustReopt gbnds ub (pt,ptval) 
        | pt `domS` ub = do 
                liftIO $ putStrLn $ "\t\t Splitting " ++ show ub  ++ " " ++ show (_szLB ub)
                pure $ catMaybes [child gbnds pt ptval ub i | i <- ChildDir <$> [1..p]]
        | pt `domL` ub = do -- pure [updateDefiningPoints pt ub]
                liftIO $ putStrLn $ "\t\t Updating " ++ show ub ++ " " ++ show (_szLB ub)
                pure [updateDefiningPoints pt ptval ub]

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
        | projPred && hopt >= sopt = do
                liftIO $ putStrLn $ "\t\t discarding " ++ show ub ++ " [" ++ show hopt ++ "]" 
                pure Nothing
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
deleteZone ub (SRUB sr yar xar) = SRUB (L.deleteBy ((==) `on` toBound) ub sr) yar xar


selectZone :: SRUB -> ExploredUB
--selectZone (SRUB sr) = ExploredUB $ L.minimumBy (compare `on` (fst . view szMaxProj)) sr -- maximumBy since we negates the values
--selectZone (SRUB sr) = ExploredUB $ L.minimumBy (compare `on` (fst . view szMaxProj)) sr -- minimumBy since we negates the values
-- selectZone (SRUB sr _) = ExploredUB $ L.minimumBy (compare `on` view szLB) sr -- selects the most promising zone (to find the optimum faster)
selectZone (SRUB sr _ _) = ExploredUB $ L.minimumBy f sr -- selects the most promising zone (to find the optimum faster)
    where f z1 z2
                | nbUndef1 /= nbUndef2 = compare nbUndef1 nbUndef2
                | hv1 /= hv2 = compare hv1 hv2
                -- | otherwise = compare hv1 hv2 
                -- | lb1 /= lb2 = compare lb1 lb2
                | otherwise = compare lb1 lb2


            where (lb1,lb2) = (_szLB z1, _szLB z2)
                  ((nbUndef1,hv1),(nbUndef2,hv2)) = (_szMaxProj z1, _szMaxProj z2)


emptySR :: SRUB -> Bool
emptySR (SRUB sr _ _) = null sr

srSize :: SRUB -> Int
srSize (SRUB sr _ _) = length sr


