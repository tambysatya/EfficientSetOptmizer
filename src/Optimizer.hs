module Optimizer where

import SearchRegion
import MOIP

import IloCplex

import Control.Monad
import qualified Data.Array as A
import Data.Maybe



data Algorithm = Algorithm {
                    _exploreMdl :: ExploreMdl,
                    _reoptMdl :: ReoptMdl,
                    _optEff :: OptEff,

                    _sr :: SRUB,
                    
                    _ndpts :: [Point],
                    _bestVal :: Double,
                    _bestSol :: Maybe Point
                 }





mkAlgorithm :: IloEnv -> Domain -> FunCoefs -> IO Algorithm
mkAlgorithm env dom funcoefs = do
       globalbounds <- computeGlobalBounds 
       Algorithm <$> mkExploreMdl env dom funcoefs
                 <*> mkReoptMdl env dom
                 <*> mkOptEff env dom funcoefs
                 <*> pure (SRUB [mkZone globalbounds])
                 <*> pure []
                 <*> pure maxval
                 <*> pure Nothing
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
                                pure (_ptPerf (fromJust optmax) A.! i, _ptPerf (fromJust optmin) A.! i)
                let (yA,yI) = unzip bnds
                pure $ (AntiIdeal $ A.listArray (1,nbCrits moipmin) yA,
                        Ideal $ A.listArray (1,nbCrits moipmin) yI)
                            

