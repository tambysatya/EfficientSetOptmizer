{-# LANGUAGE TemplateHaskell #-}
module Optimizer where

import SearchRegion
import MOIP

import IloCplex

import Control.Monad
import Control.Monad.State
import Control.Lens
import qualified Data.Array as A
import Data.Maybe



data Algorithm = Algorithm {
                    _exploreMdl :: ExploreMdl,
                    _reoptMdl :: ReoptMdl,
                    _optEff :: OptEff,

                    _searchRegion :: SRUB,
                    
                    _ndpts :: [Point],
                    _bestVal :: Double
                 }
makeLenses ''Algorithm




optimize :: GlobalBounds -> StateT Algorithm IO ()
optimize gbnds = do
    sr <- use searchRegion
    if emptySR sr
        then pure ()
        else do
            cutval <- use bestVal
            --(exp,reopt,opteff) <- (,,) <$> use exploreMdl <*> use reoptMdl <*> use optEff
            let zexp = selectZone sr
                (_,pdir) = _szMaxProj $ fromExplored zexp
            ptM <- zoom exploreMdl $ do 
                    setProj pdir
                    setCut $ cutval - 0.5
                    setLocalUpperBoundM zexp
                    solveM
            case ptM of
                Nothing -> searchRegion %= updateSR gbnds zexp pdir ptM
                Just y -> do
                    -- found a feasible point improving the best value
                    yNDM <- zoom reoptMdl $ do
                                reoptimizeFromM y
                                solveFromPointM y
                    case yNDM of
                        Nothing -> error $ "reoptimizing was infeasible [should not happen]"
                        Just yND -> do
                            newsolution <- zoom optEff $ do
                                                setCut $ sum $ A.elems $ _ptPerf yND
                                                solveM
                            optval <- zoom optEff getObjValueM
                            bestVal %= max optval
                            searchRegion %= updateSR gbnds zexp pdir newsolution

                    
            




mkAlgorithm :: IloEnv -> Domain -> FunCoefs -> IO Algorithm
mkAlgorithm env dom funcoefs = do
       globalbounds <- computeGlobalBounds 
       Algorithm <$> mkExploreMdl env dom funcoefs
                 <*> mkReoptMdl env dom
                 <*> mkOptEff env dom funcoefs
                 <*> pure (SRUB [mkZone globalbounds])
                 <*> pure []
                 <*> pure maxval
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
                            

