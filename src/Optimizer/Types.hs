{-# LANGUAGE TemplateHaskell #-}
module Optimizer.Types where

import SearchRegion
import MOIP
import IloCplex

import qualified Data.Set as S
import Control.Monad
import Control.Monad.State
import Control.Lens
import qualified Data.Array as A
import qualified Data.List as L
import Data.Function
import Data.Maybe


data Algorithm = Algorithm {
                    _globalBounds :: GlobalBounds,
                    _exploreMdl :: ExploreMdl,
                    _reoptMdl :: ReoptMdl,
                    _optEff :: OptEffCut,
                    _optEffLB :: OptEff,

                    _searchRegion :: SRUB,
                    
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

type AlgorithmT = StateT Algorithm
logM :: (MonadIO m) => String -> StateT a m ()
logM text = liftIO $ putStrLn text



mkAlgorithm :: IloEnv -> Domain -> FunCoefs -> IO Algorithm
mkAlgorithm env dom funcoefs = do
       (globalbounds,yIPts) <- computeGlobalBounds 
       let (AntiIdeal yA, Ideal yI) = globalbounds
       putStrLn $ "bounds of the domain:" ++ show (yA, yI)
       Algorithm <$> pure globalbounds
                 <*> mkExploreMdl env dom funcoefs
                 <*> mkReoptMdl' yIPts env dom funcoefs
                 -- <*> mkReoptMdl env dom
                 <*> mkOptEffCut env dom funcoefs
                 <*> mkOptEff env dom funcoefs
                 <*> pure (mkSRUB globalbounds) --mkSRUB env dom funcoefs globalbounds
                 <*> pure S.empty
                 -- <*> pure (SubOpt 9218)
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
                                pure (_ptPerf (fromJust optmax) A.! i, _ptPerf (fromJust optmin) A.! i, fromJust optmin)
                let (yA,yI,yIPts) = unzip3 bnds
                pure $ ((AntiIdeal $ A.listArray (1,nbCrits moipmin) yA,
                        Ideal $ A.listArray (1,nbCrits moipmin) yI), yIPts)
 
