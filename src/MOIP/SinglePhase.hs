module MOIP.SinglePhase(
maxval,
ExploreMdl, ReoptMdl, OptEff, OptEffCut,
FunCoefs (..),
mkExploreMdl, mkReoptMdl, mkOptEff, mkOptEffCut,
setProj, setCutUB, setCutEq,
optEffExplore
)

where

import MOIP.Scheme
import MOIP.Domain
import MOIP.Class
import SearchRegion.Class
import SearchRegion.UB
import IloCplex
import LP

import Control.Monad
import Control.Monad.State
import qualified Data.Array as A
import Foreign.ForeignPtr


data ExploreMdl = ExploreMdl MOIPScheme ProjDir IloRange -- Finds a point (not necessarily nondominated) that improves the best value found so far
data ReoptMdl = ReoptMdl MOIPScheme -- Verifies if a point is nondominated
data OptEff = OptEff MOIPScheme -- Optimise over the efficient solutions associated to a given weighted sum
data OptEffCut = OptEffCut { _oecMOIP :: MOIPScheme,
                             _oecCutSum :: IloRange} -- imposes to be equal to the optimal sum in the projection

class HasCut mdl where
    getCut :: mdl -> IloRange
class HasProj mdl where
    getCurProj :: mdl -> ProjDir
    setCurProj :: mdl -> ProjDir -> mdl
instance HasCut ExploreMdl where getCut (ExploreMdl _ _ cut) = cut
instance HasCut OptEffCut where getCut (OptEffCut _ cut) = cut

instance HasProj ExploreMdl where
    getCurProj (ExploreMdl _ p _) = p
    setCurProj (ExploreMdl mdl _ cut) p = ExploreMdl mdl p cut
newtype FunCoefs = FunCoefs [(Int,Double)]


instance SimpleMOIP ExploreMdl where
    toMOIPScheme (ExploreMdl mdl _ _) = mdl
    touchMOIP (ExploreMdl mdl _ cut) = cleanMOIPScheme mdl >> (_moipsLP mdl `lpRemove` cut)
    --touchMOIP (ExploreMdl mdl _ cut) = touchMOIPScheme mdl >> touchForeignPtr (getImpl cut)
instance SimpleMOIP ReoptMdl where
    toMOIPScheme (ReoptMdl mdl) = mdl
    touchMOIP (ReoptMdl mdl) = cleanMOIPScheme mdl
    --touchMOIP (ReoptMdl mdl) = touchMOIPScheme mdl
instance SimpleMOIP OptEff where
    toMOIPScheme (OptEff mdl) = mdl
    touchMOIP (OptEff mdl) = cleanMOIPScheme mdl
    --touchMOIP (OptEff mdl) = touchMOIPScheme mdl
instance SimpleMOIP OptEffCut where
    toMOIPScheme (OptEffCut mdl _) = mdl
    touchMOIP (OptEffCut mdl _) = cleanMOIPScheme mdl 

maxval :: Double
maxval = fromIntegral $ (2^(32 ::Int) :: Int)

mkExploreMdl :: IloEnv -> Domain -> FunCoefs -> IO ExploreMdl
mkExploreMdl env dom (FunCoefs funcoefs) = do
    moip <- mkMOIPScheme env dom
    putStrLn "creating cut"

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
    forM_ funcoefs $ \(i,ci) -> 
        setLinearCoef (lpObj $ _moipsLP moip) (_moipsDomvars moip A.! i) ci
    pure $ OptEff moip 
    
{-| Searchs the optimal efficient solution that are optimal for y_k in the projection of the zone -}
mkOptEffCut :: IloEnv -> Domain -> FunCoefs -> IO OptEffCut
mkOptEffCut env dom (FunCoefs funcoefs) = do
    moip <- mkMOIPScheme env dom
    forM_ funcoefs $ \(i,ci) -> 
        setLinearCoef (lpObj $ _moipsLP moip) (_moipsDomvars moip A.! i) ci
    sumcut <- newIloObject env 
    _moipsLP moip `lpAdd` sumcut

    forM (A.elems $ _moipsObjvars moip) $ \oi -> setLinearCoef sumcut oi 1
    pure $ OptEffCut moip sumcut 

setProj :: (MonadIO m, HasProj mdl, SimpleMOIP mdl) => ProjDir -> StateT mdl m ()
setProj (ProjDir pdir) = do
    -- (ExploreMdl moip (ProjDir cur) cut) <- get
    me <- get
    let moip = toMOIPScheme me
        (ProjDir cur) = getCurProj me
    when (pdir /= cur) $ liftIO $ do
            _setObjectiveCoef moip cur 0
            _setObjectiveCoef moip pdir 1

            _addConstraintOnObj moip cur 
            _ommitConstraintOnObj moip pdir
    put $ setCurProj me (ProjDir pdir)
    --put $ ExploreMdl moip (ProjDir pdir) cut

setCutUB :: (MonadIO m, HasCut mdl) => Double -> StateT mdl m ()
setCutUB val = do
    mdl <- get
    liftIO $ setUB (getCut mdl) val
setCutEq :: (MonadIO m, HasCut mdl) => Double -> StateT mdl m ()
setCutEq val = do
    mdl <- get
    liftIO $ setBounds (getCut mdl) (val,val)
optEffExplore :: (MonadIO m) => ExploredUB -> ProjDir -> Point -> StateT OptEffCut m (Maybe Point)
optEffExplore zexp@(ExploredUB ub) pdir pt = do
    (OptEffCut moip sumcut) <- get
    setLocalUpperBoundM zexp
    let objlctr = _moipsobjctrs moip A.! l 
    liftIO $ setUB objlctr (_ptPerf pt A.! l)
    
    setCutEq $ sum $ A.elems (_ptPerf pt)

    ret <- solveFromPointM pt
    addConstraintOnObjM l
    pure ret
    
 where l = fromProjDir pdir


    
