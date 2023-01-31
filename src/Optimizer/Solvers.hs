module Optimizer.Solvers where

import Optimizer.Types
import Optimizer.Models
import MOIP
import SearchRegion

import qualified Data.Array as A
import qualified Data.List as L
import Data.Maybe
import Control.Lens
import Data.Function
import Control.Monad.State.Strict



{-| Looks for a non-dominated point that dominates the argument -}
verifyDominance :: (MonadIO m) => Point -> AlgorithmT m Point
verifyDominance pt = zoom reoptMdl $ fromJust <$> exploreLargeM (_ptPerf pt) (Just pt)

   
{-| Explores the projection of the zone with the additional constraint to improve the current estimation
    A warmstart can be provided-}
exploreProjection :: (MonadIO m) => ExploredUB -> SubOpt -> Maybe Point -> AlgorithmT m (Maybe (Point,OptValue))
exploreProjection (ExploredUB zexp) (SubOpt estimation) warmstartM = do
        zoom exploreMdl $ do
               omitConstraintOnObjM pdir
               setObjectiveCoefM pdir 1

               exploreSetCutUB $ estimation - 0.5

               ret <- do
                    ptM <- exploreStrictM (toBound zexp) warmstartM
                    case ptM of
                        Nothing -> pure Nothing
                        Just pt -> do
                            val <- objValueM
                            pure $ Just (pt,val)
               

               addConstraintOnObjM pdir
               setObjectiveCoefM pdir 0
               pure ret

    where (ProjDir pdir) = snd $ _szMaxProj $ zexp

effsetComputeLBOnProj :: (MonadIO m) => ExploredUB -> AlgorithmT m (Maybe (Point, HyperOpt))
effsetComputeLBOnProj (ExploredUB zexp) = zoom optEff $ do
        -- exportModelM $ "test" ++ show it ++ ".lp"
        omitConstraintOnObjM pdir
        ret <- do
            ptM <- exploreStrictM (toBound zexp) warmstartM
            case ptM of 
                Nothing -> pure Nothing
                Just pt -> do
                    (OptValue v) <- objValueM
                    pure $ Just (pt, HyperOpt v)
        
        addConstraintOnObjM pdir
        pure ret
    where (ProjDir pdir) = snd $ _szMaxProj zexp
          warmstartM = case _szDefiningPoint zexp A.! pdir of
                            [] -> Nothing
                            pts -> Just $ fst $ L.minimumBy (compare `on` snd) pts

effsetGetDominatingPoint :: (MonadIO m) => Point -> AlgorithmT m (Point, SubOpt)
effsetGetDominatingPoint pt = zoom optEff $ do
    retM <- exploreLargeM (_ptPerf pt) (Just pt)
    case retM of
        Nothing -> error $ "[optEffLb] reoptimizing from " ++ show pt ++ " is infeasible."
        Just ret -> do
                (OptValue val) <- objValueM       
                pure (ret,SubOpt val)
