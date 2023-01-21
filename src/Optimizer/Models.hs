module Optimizer.Models

(FunCoefs(..)
,mkExploreMdl
,mkReoptMdl, mkReoptMdl'
,mkLBMdl
,ExploreMdl, ReoptMdl, LBMdl
,exploreSetCutUB
)


where


import MOIP
import SearchRegion
import qualified IloCplex as CPX

import qualified Data.Array as A
import Control.Monad
import Control.Monad.State


newtype FunCoefs = FunCoefs [(Int,Double)]

data ExploreMdl = ExploreMdl !MOIPScheme !CPX.IloRange
data ReoptMdl = ReoptMdl !MOIPScheme
data LBMdl = LBMdl !MOIPScheme

instance MOIP ExploreMdl where toMOIPScheme (ExploreMdl mdl _) = mdl
instance MOIP ReoptMdl where toMOIPScheme (ReoptMdl mdl) = mdl
instance MOIP LBMdl where toMOIPScheme (LBMdl mdl) = mdl



mkExploreMdl :: CPX.IloEnv -> Domain -> FunCoefs -> IO ExploreMdl
mkExploreMdl env dom (FunCoefs fcoefs) = do
    moip <- mkMOIPScheme env dom
    cut <- newIloObject moip
    forM fcoefs $ \(i,vi) -> CPX.setLinearCoef cut (_domvars moip A.! i) vi
    moip `add` cut
    pure $ ExploreMdl moip cut

mkReoptMdl :: CPX.IloEnv -> Domain -> IO ReoptMdl
mkReoptMdl env dom = do
    moip <- mkMOIPScheme env dom
    forM [1..p] $ \i -> setObjectiveCoef moip i 1
    pure $ ReoptMdl moip
 where p = nbObjVars dom

mkReoptMdl' :: CPX.IloEnv -> Domain -> FunCoefs -> [Point] -> IO ReoptMdl
mkReoptMdl' env dom fcoefs yIPts = do
    moip <- mkMOIPScheme env dom
    let vals = evaluatePoint fcoefs <$> yIPts
        total = sum vals
    forM (zip [1..p] vals) $ \(i,vi) -> setObjectiveCoef moip i (vi/total)
    pure $ ReoptMdl moip
 where p = nbObjVars dom

mkLBMdl :: CPX.IloEnv -> Domain -> FunCoefs -> IO LBMdl
mkLBMdl env dom (FunCoefs fcoefs) = do
    moip <- mkMOIPScheme env dom
    forM fcoefs $ \(i,vi) -> CPX.setLinearCoef (_objfun moip) (_domvars moip A.! i) vi
    pure $ LBMdl moip

exploreSetCutUB :: (MonadIO m) => Double -> StateT ExploreMdl m ()
exploreSetCutUB val = do
    (ExploreMdl _ cut) <- get
    liftIO $ CPX.setUB cut val
    


evaluatePoint :: FunCoefs -> Point -> Double
evaluatePoint (FunCoefs fcoefs) pt = sum $ [vi*(_ptSol pt A.! i) | (i,vi) <- fcoefs]
