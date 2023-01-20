{-# LANGUAGE TemplateHaskell, TupleSections #-}
module Optimizer where

import Optimizer.Types
import Optimizer.Debug
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
            cutval <- use bestVal
            let zexp = selectZone sr
                (_,pdir) = _szMaxProj $ fromExplored zexp
                (ProjDir l) = pdir
                srsize = srSize sr
            --let (SRUB sr') = sr                    
            --logM $ "SR=" ++ show [(zi,_szMaxProj zi) | zi <- sr']
            logM $ "exploring: " ++ show zexp ++  " " ++ show pdir ++ " " ++ show (_szLB $ fromExplored zexp) ++ " hv=" ++ show (fst $ _szMaxProj $ fromExplored zexp) ++ " size=" ++ show srsize  ++ " cutval=" ++ show cutval
            stats.lmax %= max srsize
            stats.ltotal += srsize
            -- DEBUG
            --zoneLBM <-  dbg_compute_zone_lb gbnds zexp cutval
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
                                    if SubOpt optval >= cutval
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
                    zoom searchRegion $ updateSR gbnds zexp pdir Nothing cutval
                    optimize 
                Just (lb,lbPt) -> do
                    logM $ "\t lbPt=" ++ show lbPt ++ " " ++ show lb
                    weakND <- zoom exploreMdl $ do 

                                setProj pdir
                                setLocalUpperBoundM zexp
                                setCutUB $ fromSubOpt cutval
                                fromJust <$> solveFromPointM lbPt
                    logM $ "\t weakND=" ++ show weakND
                    (yND,yNDval) <- zoom reoptMdl $ do
                                reoptimizeFromM weakND
                                ret <- fromJust <$> solveFromPointM weakND
                                val <- getObjValueM
                                pure (ret, SubOpt val)
                    logM $ "\t yND=" ++ show yND


                    bestSol <- zoom optEff $ fromJust <$> optEffExplore zexp pdir yND
                    bestval <- SubOpt <$> zoom optEff getObjValueM
                    logM $ "\t " ++ show bestSol ++ " val=" ++ show bestval ++ " [lb=" ++ show lb ++ "]"
                    logM "\t [updateSR with yND]"
                    bestVal %= min bestval
                    curval <- use bestVal
                    zoom searchRegion $ updateSR gbnds zexp pdir (Just (HyperOpt lb,yND,yNDval)) curval

                       -- We can also update from lbPT but we must check if it is non-dominated
                       -- and update the estimation accordingly
                    when (not $ yND `domL` lbPt) $ do
                        logM "\t [updateSR with lbPT]"
                        lbNDM <- zoom reoptMdl $ do
                            error "to implement"
                        bestSol <- zoom optEff $ fromJust <$> optEffExplore zexp pdir lbPt
                        zoom searchRegion $ updateSR_noRR gbnds (lbPt,SubOpt lb) curval


                    when (bestSol /= yND) $ do
                        logM "\t [updateSR with bestSol]"
                        zoom searchRegion $ updateSR gbnds zexp pdir (Just (HyperOpt lb,bestSol,bestval)) curval
                        ndpts %= S.insert (_ptPerf bestSol)
                    ndpts %= S.insert (_ptPerf yND)
                    
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



