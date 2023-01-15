{-# LANGUAGE TemplateHaskell, TupleSections #-}
module Optimizer where

import SearchRegion
import MOIP

import IloCplex

import Control.Monad
import Control.Monad.State
import Control.Lens
import qualified Data.Array as A
import Data.Maybe

import System.CPUTime
import qualified Data.Set as S



data Algorithm = Algorithm {
                    _globalBounds :: GlobalBounds,
                    _exploreMdl :: ExploreMdl,
                    _reoptMdl :: ReoptMdl,
                    _optEff :: OptEffCut,
                    --_optEff :: OptEff,

                    _searchRegion :: SRUB,
                    _xeArchive :: XeArchive,
                    _yArchive :: YArchive,
                    
                    _ndpts :: S.Set Bound,
                    _bestVal :: SubOpt,
                    _stats :: Stats
                 }
data Stats = Stats {_lmax :: Int,
                    _ltotal :: Int,
                    _nbIt :: Int,
                    _nbInfeasible :: Int
                    }

makeLenses ''Stats
makeLenses ''Algorithm

mkStats = Stats 0 0 0 0
instance Show Stats where show (Stats lm la it inf) = show lm ++ ";" ++ show (fromIntegral la/fromIntegral it) ++ ";" ++ show it ++ ";" ++ show inf

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
                srsize = srSize sr
            logM $ "exploring: " ++ show zexp ++  " " ++ show pdir ++ " size=" ++ show srsize  ++ " cutval=" ++ show cutval
            yar <- use yArchive
            case checkYMdl zexp yar of
                Just (YMdl azone ptM) -> do
                    logM $ "\t" ++ show ptM ++ " [known]"
                    searchRegion %= updateSR gbnds zexp pdir ((,_szLB $ fromExplored zexp) <$> ptM) cutval
                    optimize
                Nothing -> do
                    stats.lmax %= max srsize
                    stats.ltotal += srsize
                    ptM <- zoom exploreMdl $ do 
                            setProj pdir
                            setLocalUpperBoundM zexp
                            setCutUB $ fromSubOpt cutval
                            solveM
                    case ptM of
                        Nothing -> do
                            logM $ "\t X"
                            stats.nbInfeasible += 1
                            searchRegion %= updateSR gbnds zexp pdir Nothing cutval
                            yArchive %= insertYMdl (mkYMdl zexp Nothing)
                            optimize 
                        Just y -> do
                            -- found a feasible point improving the best value
                            logM $ "\t" ++ show y
                            yNDM <- zoom reoptMdl $ do
                                        reoptimizeFromM y
                                        solveFromPointM y
                            yArchive %= insertYMdl (mkYMdl zexp yNDM)
                            case yNDM of
                                Nothing -> error $ "reoptimizing was infeasible [should not happen]"
                                Just yND -> do
                                    logM $ "\t" ++ show yND
                                    searchRegion %= updateSR gbnds zexp pdir (Just (yND,HyperOpt $ negate maxval)) 0 --without lowerbound TODO

                                    -- explore over the projection
                                    xearchive@(XeArchive xear) <- use xeArchive
                                    let xereq = mkXeMdl zexp yND
                                    case checkXeMdl xereq xearchive of
                                        Just known -> do
                                                logM $ "\t" ++ show known ++ " [known]"
                                                pure ()
                                        Nothing -> do
                                            newsolutionM <- zoom optEff $ optEffExplore zexp pdir yND
                                            optval <- SubOpt <$> zoom optEff getObjValueM



                                            let sol = fromJust newsolutionM
                                                (ProjDir k) = pdir

                                            logM $ "\t" ++ show sol
                                            bestVal %= min optval
                                            newval <- use bestVal
                                            xeArchive %= insertXeMdl (mkXeMdl zexp sol)
                                            --logM $ "\t" ++ show y ++ " => " ++ show yND ++ " => " ++  show (fromJust newsolution) ++ " best=" ++ show newval                                                
                                            --searchRegion %= updateSR gbnds zexp pdir (Just (yND,HyperOpt $ negate maxval)) newval --without lowerbound
                                            searchRegion %= updateSR_noRR gbnds sol --without lowerbound
                                    --searchRegion %= updateSR gbnds zexp pdir (Just (sol,lb)) newval
                                            when (yND /= sol) $
                                                ndpts %= S.insert (_ptPerf sol)
                                                
                                    ndpts %= S.insert (_ptPerf yND)
                                    optimize 

                            
                    
logM :: (MonadIO m) => String -> StateT a m ()
logM text = liftIO $ putStrLn text



mkAlgorithm :: IloEnv -> Domain -> FunCoefs -> IO Algorithm
mkAlgorithm env dom funcoefs = do
       globalbounds <- computeGlobalBounds 
       let (AntiIdeal yA, Ideal yI) = globalbounds
       putStrLn $ "bounds of the domain:" ++ show (yA, yI)
       Algorithm <$> pure globalbounds
                 <*> mkExploreMdl env dom funcoefs
                 <*> mkReoptMdl env dom
                 <*> mkOptEffCut env dom funcoefs
                 -- <*> mkOptEff env dom funcoefs
                 <*> pure (SRUB [mkZone globalbounds])
                 <*> pure (XeArchive [])
                 <*> pure (YArchive [])
                 <*> pure S.empty
                 <*> pure (SubOpt maxval)
                 <*> pure mkStats
    where computeGlobalBounds = do
                moipmin <- mkMOIPScheme env dom
                moipmax <- mkMOIPScheme env dom
                _setMaximize moipmax
                bnds <- forM [1..nbCrits moipmin] $ \i -> do
                                when (i /= 1) $ do 
                                    _setObjectiveCoef moipmin (i-1) 0
                                    _setObjectiveCoef moipmax (i-1) 0
                                _setObjectiveCoef moipmax i 1
                                _setObjectiveCoef moipmin i 1
                                (optmax, optmin) <- (,) <$> _solve moipmax <*> _solve moipmin
                                when (isNothing optmax || isNothing optmin) $ error "Unable to compute the bounds of the domain: the domain is empty"
                                print (optmax,optmin)
                                pure (_ptPerf (fromJust optmax) A.! i, _ptPerf (fromJust optmin) A.! i)
                let (yA,yI) = unzip bnds
                pure $ (AntiIdeal $ A.listArray (1,nbCrits moipmin) yA,
                        Ideal $ A.listArray (1,nbCrits moipmin) yI)
                            

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



time :: (MonadIO m) => m a -> m (a, Double)
time act = do cpu <- liftIO getCPUTime
              r <- act
              cpu' <- liftIO getCPUTime
              pure (r,(fromIntegral $ cpu' - cpu) / (fromIntegral $ 10^12))
