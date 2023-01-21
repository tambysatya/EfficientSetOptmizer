{-# LANGUAGE TemplateHaskell, TupleSections #-}
module Optimizer.Algorithm where

import Optimizer.Types
import Optimizer.Debug
import Optimizer.Solvers
import Optimizer.Models
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
    estimation <- use bestVal
    if emptySR sr
        then pure ()
        else do
            logM $ show estimation
            stats.nbIt += 1
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

            lbM <- effsetComputeLBOnProj zexp 
            when (isNothing lbM) $ error $ "[effsetComputeLBOnProj] infeasible zone: " ++ show zexp
            let (lbPt,lb) = fromJust lbM
            if lb >= (HyperOpt $ fromSubOpt estimation)
                {- No feasible point in the projection -}
                then do
                    logM $ "\t X [compute lb]"
                    stats.nbInfeasible += 1
                    -- searchRegion.xeArchive %= insertXeMdl zexp  Nothing
                    zoom searchRegion $ updateSR gbnds zexp pdir Nothing estimation
                    optimize 
                else do
                    logM $ "\t lbPt=" ++ show lbPt ++ " " ++ show lb
                    (weakND, OptValue weakNDl) <- fromJust <$> (exploreProjection zexp estimation $ Just lbPt)
                    logM $ "\t weakND=" ++ show weakND
                    yND <- verifyDominance weakND
                    logM $ "\t yND=" ++ show yND
                    (bestsol, bestval) <- effsetGetDominatingPoint yND


                    logM $ "\t " ++ show bestsol ++ " val=" ++ show bestval ++ " [lb=" ++ show lb ++ "]"
                    bestVal %= min bestval
                    curval <- use bestVal

                    when (not $ yND `domL` lbPt) $ do
                        logM "\t [updateSR with lbPtND]"
                        opt <- verifyDominance lbPt
                        (optsol,optval) <- effsetGetDominatingPoint opt
                        zoom searchRegion $ updateSR_noRR gbnds (optsol,optval) curval
                    logM "\t [updateSR with yND]"
                    --searchRegion.xeArchive %= insertXeMdl zexp  (Just lb)
                    zoom searchRegion $ updateSR gbnds zexp pdir (Just (lb,bestsol,bestval,weakNDl)) curval
                    
                    optimize
                    
                    
                           

runAlgorithm' :: IloEnv -> Domain -> FunCoefs -> IO (SubOpt, Algorithm)
runAlgorithm' env dom fun = do
    algo <- mkAlgorithm env dom fun
    (_,final) <- runStateT optimize algo
    --touchMOIP (_exploreMdl final)
    --touchMOIP (_reoptMdl final)
    --touchMOIP (_optEff final)
    --deleteMOIP (_exploreMdl algo)
    --deleteMOIP (_reoptMdl algo)
    --deleteMOIP (_optEff algo)
    pure (_bestVal final, final)

runAlgorithm name log env dom fun = do
    let (obj, _, _,_) = dom
        (p,n) = (length obj, length $ head obj)
    ((opt, final),duration) <- time $ runAlgorithm' env dom fun 
    appendFile log $ show p ++ ";" ++ show n ++ ";" ++ name ++ ";" ++ show (_stats final) ++ ";" ++ show duration ++ ";" ++ show (S.size $ _ndpts final) ++ " # " ++ show opt ++ "\n"



