module MOIP.SinglePhase(
maxval,
ExploreMdl, ReoptMdl, OptEff, FunCoefs (..),
mkExploreMdl, mkReoptMdl, mkOptEff,
setProj, setCut
)

where

import MOIP.Scheme
import MOIP.Domain
import MOIP.Class
import SearchRegion.Class
import IloCplex
import LP

import Control.Monad
import Control.Monad.State
import qualified Data.Array as A


data ExploreMdl = ExploreMdl MOIPScheme ProjDir IloRange -- Finds a point (not necessarily nondominated) that improves the best value found so far
data ReoptMdl = ReoptMdl MOIPScheme -- Verifies if a point is nondominated
data OptEff = OptEff MOIPScheme IloRange -- Optimise over the efficient solutions associated to a given weighted sum

class HasCut mdl where
    getCut :: mdl -> IloRange
instance HasCut ExploreMdl where getCut (ExploreMdl _ _ cut) = cut
instance HasCut OptEff where getCut (OptEff _ cut) = cut


newtype FunCoefs = FunCoefs [(Int,Double)]


instance SimpleMOIP ExploreMdl where
    toMOIPScheme (ExploreMdl mdl _ _) = mdl
instance SimpleMOIP ReoptMdl where
    toMOIPScheme (ReoptMdl mdl) = mdl
instance SimpleMOIP OptEff where
    toMOIPScheme (OptEff mdl _) = mdl


maxval :: Double
maxval = fromIntegral $ (2^(32 ::Int) :: Int)

mkExploreMdl :: IloEnv -> Domain -> FunCoefs -> IO ExploreMdl
mkExploreMdl env dom (FunCoefs funcoefs) = do
    moip <- mkMOIPScheme env dom

    {- Constructs the constraints on the value of the function to be optimized over the efficient set -}
    cut <- newIloObject env
    forM_ funcoefs $ \(i,ci) -> 
        setLinearCoef cut (_moipsDomvars moip A.! i) ci
    _moipsLP moip `lpAdd` cut
    setBounds cut (-maxval, maxval)

    {- Set the objective function to be minimized -}
    _setObjectiveCoef moip 1 1
    _ommitConstraintOnObj moip 1


    pure $ ExploreMdl moip (ProjDir 1) cut

mkReoptMdl :: IloEnv -> Domain -> IO ReoptMdl
mkReoptMdl env dom = do
    moip <- mkMOIPScheme env dom
    forM_ [1..nbCrits moip] $ \i -> _setObjectiveCoef moip i 1
    pure $ ReoptMdl moip 

mkOptEff :: IloEnv -> Domain -> FunCoefs -> IO OptEff
mkOptEff env dom (FunCoefs funcoefs) = do
    moip <- mkMOIPScheme env dom
    sumctr <- newIloObject env
    forM_ [1..nbCrits moip] $ \i -> setLinearCoef sumctr (_moipsObjvars moip A.! i) 1
    _moipsLP moip `lpAdd` sumctr

    forM_ funcoefs $ \(i,ci) -> 
        setLinearCoef (lpObj $ _moipsLP moip) (_moipsDomvars moip A.! i) ci
    pure $ OptEff moip sumctr
    
setProj :: (MonadIO m) => ProjDir -> StateT ExploreMdl m ()
setProj (ProjDir pdir) = do
    (ExploreMdl moip (ProjDir cur) cut) <- get
    when (pdir /= cur) $ liftIO $ do
            _setObjectiveCoef moip cur 0
            _setObjectiveCoef moip pdir 1

            _addConstraintOnObj moip cur 
            _ommitConstraintOnObj moip pdir
    put $ ExploreMdl moip (ProjDir pdir) cut

setCut :: (MonadIO m, HasCut mdl) => Double -> StateT mdl m ()
setCut val = do
    mdl <- get
    liftIO $ setUB (getCut mdl) val

