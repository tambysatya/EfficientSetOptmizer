{-# LANGUAGE TemplateHaskell #-}
module Optimizer.Types where

import SearchRegion
import MOIP
import Optimizer.Models
import IloCplex (IloEnv)

import qualified Data.Set as S
import Control.Monad
import Control.Monad.State.Strict
import Control.Lens
import qualified Data.Array.Unboxed as A
import qualified Data.List as L
import Data.Function
import Data.Maybe


data Algorithm = Algorithm {
                    _globalBounds :: GlobalBounds,
                    _exploreMdl :: ExploreMdl,
                    _reoptMdl :: ReoptMdl,
                    _optEff :: LBMdl,

                    _searchRegion :: SRUB,
                    
                    _ndpts :: S.Set Point,
                    _bestVal :: SubOpt,
                    _stats :: Stats
                 }
data Stats = Stats {_lmax :: Int
                    ,_ltotal :: Int
                    ,_discarded :: SRStats
                    ,_nbInfeasible :: Int
                    ,_nbIt :: Int
                    }

makeLenses ''Stats
makeLenses ''Algorithm

mkStats = Stats 0 0 mempty 0 0
instance Show Stats where show (Stats lm la discarded inf it) = show lm ++ ";" ++ show (fromIntegral la/fromIntegral it) ++ ";" ++ dumpDiscarded ++ ";" ++ show inf ++ ";" ++ show it 
                                where dumpDiscarded = (show $ (fromIntegral $ _nbCutRR discarded) / fromIntegral (_nbChildren discarded)) ++ ";"
                                                    ++ (show  $ (fromIntegral $ _nbCutLB discarded ) / fromIntegral (_nbChildren discarded)) ++ ";"
                                                    ++ (show  $ (fromIntegral $ _nbArchive discarded ) / fromIntegral (_nbChildren discarded)) 
showStatsHeader = "lmax;lavg;rr;cutlb;archive;inf,nbit"

type AlgorithmT = StateT Algorithm
logM :: (MonadIO m) => String -> StateT a m ()
logM text = liftIO $ putStrLn text

maxval = 2^32


mkAlgorithm :: IloEnv -> Domain -> FunCoefs -> IO Algorithm
mkAlgorithm env dom funcoefs = do
       (globalbounds,yIPts) <- computeGlobalBounds  env dom
       let (AntiIdeal yA, Ideal yI) = globalbounds
       putStrLn $ "bounds of the domain:" ++ show (yA, yI)
       Algorithm <$> pure globalbounds
                 <*> mkExploreMdl env dom funcoefs
                 -- <*> mkReoptMdl' env dom funcoefs yIPts
                 <*> mkReoptMdl env dom
                 <*> mkLBMdl env dom funcoefs
                 <*> pure (mkSRUB globalbounds) --mkSRUB env dom funcoefs globalbounds
                 <*> pure S.empty
                 <*> pure (SubOpt maxval)
                 <*> pure mkStats

computeGlobalBounds :: IloEnv -> Domain -> IO (GlobalBounds, [Point])
computeGlobalBounds env dom = do
        moip <- mkReoptMdl env dom
        forM [1..p] $ \i -> setObjectiveCoef moip i 0
        (yAPts, yIPts) <- unzip <$> (forM [1..p] $ \i -> do
                                setObjectiveCoef moip i 1
                                when (i > 1) $ setObjectiveCoef moip (i-1) 0

                                
                                yIPtM <- setMinimize moip >> solve moip
                                yAPtM <- setMaximize moip >> solve moip
                                case (yAPtM, yIPtM) of
                                    (Just yAPt, Just yIPt) -> pure $ (yAPt, yIPt)
                                    _ -> error "computeGlobalBounds: infeasible domain")
        let yA = A.listArray (1,p) $ fmap (\(i,pti) -> _ptPerf pti A.! i) (zip [1..] yAPts)
            yI = A.listArray (1,p) $ fmap (\(i,pti) -> _ptPerf pti A.! i) (zip [1..] yIPts)
                                  
        pure ((AntiIdeal yA, Ideal yI), yIPts)
        
   where p = nbObjVars dom

