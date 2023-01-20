{-# LANGUAGE TemplateHaskell, TupleSections #-}
module Optimizer where

import Optimizer.Types
import Optimizer.Debug
import Optimizer.Solvers
import Optimizer
import SearchRegion
import MOIP

import IloCplex

import Control.Monad
import Control.Monad.State
import Control.Lens
import qualified Data.Array as A
import qualified Data.List as L
import Data.Function
import Data.Maybe

import qualified Data.Set as S



optimize :: StateT Algorithm IO ()
optimize = do
    gbnds <- use globalBounds

    sr <- use searchRegion
    if emptySR sr
        then pure ()
        else do
            stats.nbIt += 1
            estimation <- use bestVal
            let zexp = selectZone sr
                (_,pdir) = _szMaxProj $ fromExplored zexp
                (ProjDir l) = pdir
                srsize = srSize sr
            --let (SRUB sr') = sr                    
            --logM $ "SR=" ++ show [(zi,_szMaxProj zi) | zi <- sr']
            logM $ "exploring: " ++ show zexp ++  " " ++ show pdir ++ " " ++ show (_szLB $ fromExplored zexp) ++ " hv=" ++ show (fst $ _szMaxProj $ fromExplored zexp) ++ " size=" ++ show srsize  ++ " estimation=" ++ show estimation
            stats.lmax %= max srsize
            stats.ltotal += srsize
            -- DEBUG
            --zoneLBM <-  dbg_compute_zone_lb gbnds zexp estimation
            --logM $ "\t\t[DEBUG] lower bound on zone " ++ show zexp ++ " is " ++ show zoneLBM

            lbM <- zoom optEffLB $ do
                    setLocalUpperBoundM zexp
                    omitConstraintOnObjM l
                    let (AntiIdeal yA) = fst gbnds
                    ptM <- if (toBound zexp A.! l >= yA A.! l) 
                         then do
                                logM $ "[WARNING]\t no defining point"
                                solveM
                         else do 
                                let pts = _szDefiningPoint (fromExplored zexp) A.! l
                                    bestpt = fst $ L.minimumBy (compare `on` snd) pts
                                solveFromPointM bestpt -- (head $ _szDefiningPoint (fromExplored zexp) A.! l)

                    retM <- case ptM of
                                Nothing -> pure Nothing
                                Just pt -> do
                                    optval <- getObjValueM
                                    if SubOpt optval >= estimation 
                                        then pure Nothing --  No point in the projection improves the estimation
                                        else pure $ Just (optval,pt)
                    addConstraintOnObjM l
                    pure retM
            searchRegion.xeArchive %= insertXeMdl zexp (fmap HyperOpt $ fst <$> lbM) 
            case lbM of
                {- No feasible point in the projection -}
                Nothing -> do
                    logM $ "\t X [compute lb]"
                    stats.nbInfeasible += 1
                    zoom searchRegion $ updateSR gbnds zexp pdir Nothing estimation
                    optimize 
                Just (lb,lbPt) -> do
                    logM $ "\t lbPt=" ++ show lbPt ++ " " ++ show lb
                    weakND <- fromJust <$> (exploreProjection zexp estimation $ Just lbPt)
                    logM $ "\t weakND=" ++ show weakND
                    yND <- verifyDominance weakND
                    logM $ "\t yND=" ++ show yND
                    (bestsol, bestval) <- effsetGetDominatingPoint yND


                    logM $ "\t " ++ show bestsol ++ " val=" ++ show bestval ++ " [lb=" ++ show lb ++ "]"
                    logM "\t [updateSR with yND]"
                    bestVal %= min bestval
                    curval <- use bestVal
                    zoom searchRegion $ updateSR gbnds zexp pdir (Just (HyperOpt lb,bestsol,bestval)) curval

                    
                    optimize
                    
                    
                           

runAlgorithm' :: IloEnv -> Domain -> FunCoefs -> IO (SubOpt, Algorithm)
runAlgorithm' env dom fun = do
    algo <- mkAlgorithm env dom fun
    (_,final) <- runStateT optimize algo
    --touchMOIP (_exploreMdl final)
    --touchMOIP (_reoptMdl final)
    --touchMOIP (_optEff final)
    pure (_bestVal final, final)

runAlgorithm name log env dom fun = do
    let (obj, _, _,_) = dom
        (p,n) = (length obj, length $ head obj)
    ((opt, final),duration) <- time $ runAlgorithm' env dom fun 
    appendFile log $ show p ++ ";" ++ show n ++ ";" ++ name ++ ";" ++ show (_stats final) ++ ";" ++ show duration ++ ";" ++ show (S.size $ _ndpts final) ++ " # " ++ show opt ++ "\n"



