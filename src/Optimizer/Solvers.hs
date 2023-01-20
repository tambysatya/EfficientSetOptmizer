module Optimizer.Solvers where

import Optimizer.Types
import MOIP
import SearchRegion

import qualified Data.Array as A
import qualified Data.List as L
import Data.Maybe
import Control.Lens
import Control.Monad.State



{-| Looks for a non-dominated point that dominates the argument -}
verifyDominance :: (MonadIO m) => Point -> AlgorithmT m Point
verifyDominance pt = zoom reoptMdl $ do
   reoptimizeFromM pt
   fromJust <$> solveFromPointM pt

   
{-| Explores the projection of the zone with the additional constraint to improve the current estimation
    A warmstart can be provided-}
exploreProjection :: (MonadIO m) => ExploredUB -> SubOpt -> Maybe Point -> AlgorithmT m (Maybe Point)
exploreProjection (ExploredUB zexp) (SubOpt estimation) warmstartM = do
        zoom exploreMdl $ do
            setProj pdir
            setLocalUpperBoundM zexp
            setCutUB estimation
            case warmstartM of
                Nothing -> solveM
                Just warmstart -> solveFromPointM warmstart
    where pdir = snd $ _szMaxProj $ zexp

effsetGetDominatingPoint :: (MonadIO m) => Point -> AlgorithmT m (Point, SubOpt)
effsetGetDominatingPoint pt = zoom optEffLB $ do
    reoptimizeFromM pt
    retM <- solveFromPointM pt
    case retM of
        Nothing -> error $ "[optEffLb] reoptimizing from " ++ show pt ++ " is infeasible."
        Just ret -> (,) <$> pure ret <*> (SubOpt <$> getObjValueM)

effsetOptimizeProjection :: (MonadIO m) => GlobalBounds -> ExploredUB -> Maybe Point -> AlgorithmT m (Maybe (Point,SubOpt))
effsetOptimizeProjection gbnds zexp warmstartM = zoom optEffLB $ do
                    setLocalUpperBoundM zexp
                    omitConstraintOnObjM l
                    let (AntiIdeal yA) = fst gbnds
                    ptM <-case warmstartM of 
                            Nothing -> do
                                logM $ "[WARNING] no warmstart while optimizing over projection " ++ show zexp
                                solveM
                            Just warmstart -> solveFromPointM warmstart -- (head $ _szDefiningPoint (fromExplored zexp) A.! l)

                    retM <- case ptM of
                                Nothing -> pure Nothing
                                Just pt -> do
                                    val <- getObjValueM
                                    pure $ Just $ (pt, SubOpt val)
                                    
                    addConstraintOnObjM l
                    pure retM
    where (ProjDir l) = snd $ _szMaxProj $ fromExplored zexp

