{-# LANGUAGE TemplateHaskell #-}
module SearchRegion.Internals where

import Utils
import SearchRegion.UB
import SearchRegion.Class
import SearchRegion.Archive
import MOIP
import IloCplex

import qualified Data.Array.Unboxed as A
import qualified Data.List as L
import Control.Lens
import Data.Function
import Data.Maybe
import Control.Monad.State.Strict
import Control.DeepSeq




{-| TODO use a min heap -}
data SRUB = SRUB {_srUB :: ![UB] , _srStats :: !SRStats,  _yArchive :: !YArchive, _xeArchive :: !XeArchive, _srSize :: !Int}
data SRStats = SRStats {_nbCutRR :: !Int -- Number of children discarded using the reduction rules
                        ,_nbCutLB :: !Int -- number of zones discarded due to the lower bound estimation
                        ,_nbArchive :: !Int -- number of zones discarded due to the archive (postponed reduction rules)
                        ,_nbUpdated :: !Int -- Number of zones that have been updated (lowerbound or defining point)
                        ,_nbChildren :: !Int -- Number of created zones
                        }
makeLenses ''SRUB
makeLenses ''SRStats
instance Show SRUB where show (SRUB sr _ _ _ _) = show $ fmap _szU sr
type SRUBT = StateT SRUB

instance Show SRStats where show (SRStats nbcut nblb nbar updat child) = "rr=" ++ show nbcut ++ " lb=" ++ show nblb ++ " archive=" ++ show nbar ++ " updated=" ++ show updat ++ " children=" ++ show child

mkSRUB :: GlobalBounds -> SRUB
mkSRUB gbnds = SRUB [mkZone gbnds] mkSRStats (mkYArchive p) (mkXeArchive p) 1
    where p = dimension (fst gbnds)

mkSRStats :: SRStats
mkSRStats = SRStats 0 0 0 0 0

instance Semigroup SRStats where
    (SRStats a b c d e) <> (SRStats a' b' c' d' e') = SRStats (a+a') (b+b') (c+c') (d+d') (e+e')
instance Monoid SRStats where
    mempty = mkSRStats

updateSR :: (MonadIO m) => GlobalBounds -> ExploredUB -> ProjDir -> Maybe (HyperOpt, Point, SubOpt, Double) -> SubOpt -> SRUBT m ()
updateSR gbnds zexp pdir Nothing _ = do
    srStats .= mkSRStats
    srSize .= 0

    sr <- use srUB
    logM $ "\t\t [computing children]"
    -- TODO archive
    retM <- mapM (updateZoneNothing gbnds zexp pdir) sr 
    logM $ "\t\t [force evaluation]"
    srUB .= force (catMaybes retM)
    logM $ "\t\t [update archive]"
    yArchive %= force . insertYMdl zexp Nothing
    use srStats >>= \st -> logM ("\t\t [discard report] " ++ show st)

updateSR gbnds zexp pdir@(ProjDir l) (Just (hopt,pt, ptval,lb_l)) estimation@(SubOpt s) = do --SRUB mdl $ sr >>= updateZoneJustWithRR gbnds zexp hopt pdir lb pt estimation
        srStats .= mkSRStats
        srSize .= 0

        sr <- use srUB
        logM $ "\t\t [computing children]"
        ret <- forM sr $ \u -> updateZoneJustWithRR gbnds zexp pdir hopt lb_l (pt,ptval) estimation u
        logM $ "\t\t [force evaluation]"
        srUB .= force (concat ret)
        logM $ "\t\t [update archive]"
        yArchive %= force . insertYMdl zexp (Just lb_l)
        use srStats >>= \st -> logM ("\t\t [discard report] " ++ show st)
    --where lb_l = _ptPerf pt A.! l



-- TODO strict evaluation
updateZoneNothing :: (MonadIO m) => GlobalBounds -> ExploredUB -> ProjDir -> UB -> SRUBT m (Maybe UB)
updateZoneNothing gbnds (ExploredUB zexp) pdir  z 
    | proj pdir z `lDomL` proj pdir zexp = do 
        srStats.nbCutLB += 1
        pure Nothing
    | otherwise = do
        srSize += 1
        pure $ Just z
    


-- TODO strict evaluation
updateZoneJustWithRR :: (MonadIO m) => GlobalBounds -> ExploredUB -> ProjDir -> HyperOpt -> Double -> (Point,SubOpt) -> SubOpt -> UB -> SRUBT m [UB]
updateZoneJustWithRR gbnds zexp pdir hopt lb_l pt estimation@(SubOpt cur) ub = do
            newzones <- updateZoneJustReopt gbnds ub pt
            retM <- (forM newzones $ applyReductionRule (proj pdir zexp) pdir hopt lb_l estimation)
            yar <- use yArchive
            xar <- use xeArchive
    
            let ret = catMaybes retM
            newzones' <- if fmap toBound ret == [toBound ub]
                                then do
                                    srSize += 1
                                    pure retM
                                else forM ret $ \zi -> let yreqM = checkYMdl (ExploredUB zi) yar
                                                       in case yreqM of
                                                            Nothing -> do 
                                                                let xreqM = checkXeMdl zi estimation xar
                                                                case xreqM of
                                                                    Nothing -> do
                                                                        srSize += 1
                                                                        pure $ Just $ zi
                                                                    Just mdl -> do 
                                                                        srStats.nbArchive += 1
                                                                        --liftIO $ putStrLn $ "\t\t discarding " ++ show zi ++ " [Xarchive: " ++ show mdl ++ "]"
                                                                        pure Nothing
                                                                    
                                                            Just mdl -> do
                                                                srStats.nbArchive += 1
                                                                --liftIO $ putStrLn $ "\t\t discarding " ++ show zi ++ " [Yarchive: " ++ show mdl ++ "]"
                                                                pure Nothing
            pure $ catMaybes newzones'
            -- pure $ catMaybes $ applyReductionRule zexp pdir lb_l estimation <$> newzones
            --catMaybes $ applyReductionRule zexp pdir lb_l estimation <$> updateZoneJustHOpt gbnds zexp hopt ub pt 

updateZoneJustReopt :: (MonadIO m) => GlobalBounds -> UB -> (Point,SubOpt) -> SRUBT m [UB]
updateZoneJustReopt gbnds ub (pt,ptval) 
        | domSP = do 
                --liftIO $ putStrLn $ "\t\t Splitting " ++ show ub  ++ " " ++ show (_szLB ub)
                let maxchild = catMaybes [child gbnds pt ptval ub i | i <- ChildDir <$> [1..p]]
                    nchildren = length maxchild
                srStats.nbChildren += nchildren
                pure $ maxchild
        | domLP = do -- pure [updateDefiningPoints pt ub]
                --liftIO $ putStrLn $ "\t\t Updating " ++ show ub ++ " " ++ show (_szLB ub)
                srStats.nbUpdated += 1
                pure [updateDefiningPoints pt ptval ub]

        | otherwise = pure [ub]
  where p = dimension ub
        perfs = _ptPerf pt
        u = toBound ub
        domLP = and [perfs A.! i <= toBound u A.! i | i <- [1..p]]
        domSP = domLP && perfs /= u

{-| Applies the reduction rule if a point have been found y have been found after looking for 
    improving component k by searching in direction l:
    If child-l <= zexp-l (projection is included:
    and pt_l >= child_l 
    then, no point in child can improve the componnent k
 -}
applyReductionRule :: (MonadIO m) =>
           [Double] -- The projection that have been explored
           -> ProjDir -- The projection that have been explored
           -> HyperOpt
           -> Double -- lower bound on projdir
           -> SubOpt
           -> UB
           -> SRUBT m (Maybe UB)
applyReductionRule exproj pdir (HyperOpt hopt) lb_l (SubOpt sopt) ub
        | localIdeal >= sopt = do
                --liftIO $ putStrLn $ "\t\t discarding " ++ show ub ++ " [lb=" ++ show localIdeal++"]"
                srStats.nbCutLB += 1
                pure Nothing
        | projPred && hopt >= sopt = do
                --liftIO $ putStrLn $ "\t\t discarding " ++ show ub ++ " [" ++ show hopt ++ "]" 
                srStats.nbCutLB += 1
                pure Nothing
        | projPred  &&
          lb_l  >= _szU ub A.! l = do
               -- liftIO $ putStrLn $ "\t\t discarding " ++ show ub ++ " [reduction rule]"
                srStats.nbCutRR += 1
                pure Nothing
        | projPred  = pure $ Just $ ub & szLB .~ (HyperOpt hopt)
        | otherwise = pure $  Just $ ub
   where l = fromProjDir pdir
         --projPred = and [toBound ub A.! i <= toBound zexp A.! i | i <- [1..dimension ub], i /= l] --proj pdir ub `domL` proj pdir zexp
         projPred = and $ zipWith (<=) (proj pdir ub) exproj
         --projPred = and $ zipWith (<=) [_szU ub A.! i | i <- L.delete l [1..p]] exproj
         p = dimension ub
         (HyperOpt localIdeal) = _szLB ub

    
 


selectZone :: SRUB -> ExploredUB
--selectZone (SRUB sr) = ExploredUB $ L.minimumBy (compare `on` (fst . view szMaxProj)) sr -- maximumBy since we negates the values
--selectZone (SRUB sr) = ExploredUB $ L.minimumBy (compare `on` (fst . view szMaxProj)) sr -- minimumBy since we negates the values
-- selectZone (SRUB sr _) = ExploredUB $ L.minimumBy (compare `on` view szLB) sr -- selects the most promising zone (to find the optimum faster)
selectZone (SRUB sr _ _ _ _) = ExploredUB $ L.minimumBy f sr -- selects the most promising zone (to find the optimum faster)
    where f z1 z2
                | nbUndef1 /= nbUndef2 = compare nbUndef1 nbUndef2
                | hv1 /= hv2 = compare hv1 hv2
                -- | otherwise = compare hv1 hv2 
                -- | lb1 /= lb2 = compare lb1 lb2
                | otherwise = compare lb1 lb2


            where (lb1,lb2) = (_szLB z1, _szLB z2)
                  ((nbUndef1,hv1),(nbUndef2,hv2)) = (_szMaxProj z1, _szMaxProj z2)


emptySR :: SRUB -> Bool
emptySR (SRUB sr _ _ _ _) = null sr

{- deprecated -}
srLen :: SRUB -> Int
srLen (SRUB sr _ _ _ _) = length sr

checkEstimation :: (MonadIO m) => SubOpt -> UB -> SRUBT m (Maybe UB)
checkEstimation s u 
        | _szLB u <= HyperOpt (fromSubOpt s) = pure $ Just  $ u
        | otherwise = do 
            srStats.nbCutLB += 1
            pure Nothing

checkArchives :: (MonadIO m) => SubOpt -> UB -> SRUBT m (Maybe UB)
checkArchives estimation ub = do
    xar <- use xeArchive
    yar <- use yArchive
    if isJust (checkYMdl (ExploredUB ub) yar) || isJust (checkXeMdl ub estimation xar)
        then pure Nothing
        else pure $ Just $ ub

