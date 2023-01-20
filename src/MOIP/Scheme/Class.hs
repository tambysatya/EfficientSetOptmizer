
module MOIP.Scheme.Class

(newIloObject, newIloObjectM, add, addM, remove, removeM
,setObjectiveCoef, setObjectiveCoefM
,omitConstraintOnObj, omitConstraintOnObjM, addConstraintOnObj, addConstraintOnObjM
,strictUpperBound, strictUpperBoundM, largeUpperBound, largeUpperBoundM
,objValue, objValueM
,solve, solveM, solveFromPoint, solveFromPointM
,exploreStrict, exploreStrictM, exploreLarge, exploreLargeM
,MOIP.OptValue
)

where


import MOIP.Scheme.Type
import SearchRegion.Class

import qualified MOIP.Scheme.Methods as MOIP
import qualified IloCplex as CPX

import qualified Data.Array as A
import Control.Monad.State

class MOIP a where
    toMOIPScheme :: a -> MOIPScheme


{-| Pure version -}

newIloObject :: (MOIP a, CPX.IloObject b) => a -> IO b
newIloObject moip = liftIO $ MOIP.newIloObject (toMOIPScheme moip)

add :: (MOIP a, CPX.IloObject b, CPX.IloAddable b) => a -> b -> IO ()
moip `add` a = toMOIPScheme moip `MOIP.moipAdd` a
remove :: (MOIP a, CPX.IloObject b, CPX.IloAddable b) => a -> b -> IO ()
moip `remove` a = toMOIPScheme moip `MOIP.moipRemove` a

    {-| Objective function -}
setObjectiveCoef :: (MOIP a) => a -> Int -> Double -> IO ()
setObjectiveCoef mdl i v = MOIP.setObjectiveCoef (toMOIPScheme mdl) i v

    {-| Constraints -}
omitConstraintOnObj :: (MOIP a) =>  a -> Int -> IO ()
omitConstraintOnObj moip i = MOIP.omitConstraintOnObj (toMOIPScheme moip)  i

addConstraintOnObj :: (MOIP a) =>  a -> Int -> IO ()
addConstraintOnObj moip i = MOIP.addConstraintOnObj (toMOIPScheme moip) i

strictUpperBound :: MOIP a => a -> A.Array Int Double -> IO ()
strictUpperBound moip bnd = MOIP.strictUpperBound (toMOIPScheme moip) bnd
largeUpperBound :: MOIP a => a -> A.Array Int Double -> IO ()
largeUpperBound moip bnd = MOIP.largeUpperBound (toMOIPScheme moip) bnd


    {-| Solve -}
objValue :: MOIP a => a -> IO MOIP.OptValue
objValue moip = MOIP.objValue $ toMOIPScheme moip

solve :: MOIP a => a -> IO (Maybe Point)
solve moip = MOIP.solve $ toMOIPScheme moip

solveFromPoint :: MOIP a => a -> Point -> IO Point
solveFromPoint moip pt = MOIP.solveFromPoint (toMOIPScheme moip) pt


exploreStrict :: MOIP a => a -> A.Array Int Double -> Maybe Point -> IO (Maybe Point)
exploreStrict moip ub ptM = do
    strictUpperBound moip ub
    case ptM of
        Nothing -> solve moip
        Just pt -> Just <$> solveFromPoint moip pt
exploreLarge :: MOIP a => a -> A.Array Int Double -> Maybe Point -> IO (Maybe Point)
exploreLarge moip ub ptM = do
    largeUpperBound moip ub
    case ptM of
        Nothing -> solve moip
        Just pt -> Just <$> solveFromPoint moip pt
 
    


{-| Monadic Version -}
setObjectiveCoefM :: (MOIP a, MonadIO m) => Int -> Double -> StateT a m ()
setObjectiveCoefM i vi = do
    moip <- gets toMOIPScheme
    liftIO $ MOIP.setObjectiveCoef moip i vi

omitConstraintOnObjM :: (MOIP a, MonadIO m) => Int -> StateT a m ()
omitConstraintOnObjM i = do
    moip <- gets toMOIPScheme
    liftIO $ MOIP.omitConstraintOnObj moip i
addConstraintOnObjM :: (MOIP a, MonadIO m) => Int -> StateT a m ()
addConstraintOnObjM i = do
    moip <- gets toMOIPScheme
    liftIO $ MOIP.addConstraintOnObj moip i  

largeUpperBoundM :: (MOIP a, MonadIO m) => A.Array Int Double -> StateT a m ()
largeUpperBoundM bnd = do
    moip <- gets toMOIPScheme
    liftIO $ MOIP.largeUpperBound moip bnd
strictUpperBoundM :: (MOIP a, MonadIO m) => A.Array Int Double -> StateT a m ()
strictUpperBoundM bnd = do
    moip <- gets toMOIPScheme
    liftIO $ MOIP.strictUpperBound moip bnd
objValueM :: (MOIP a, MonadIO m) => StateT a m MOIP.OptValue
objValueM = do
    moip <- gets toMOIPScheme
    liftIO $ MOIP.objValue moip

solveM :: (MOIP a, MonadIO m) => StateT a m (Maybe Point)
solveM = do
    moip <- gets toMOIPScheme
    liftIO $ MOIP.solve moip
    
solveFromPointM :: (MOIP a, MonadIO m) => Point -> StateT a m Point
solveFromPointM pt = do
    moip <- gets toMOIPScheme
    liftIO $ MOIP.solveFromPoint moip pt

exploreStrictM :: (MOIP a, MonadIO m) => A.Array Int Double -> Maybe Point -> StateT a m (Maybe Point)
exploreStrictM ub ptM = do
    moip <- get
    liftIO $ exploreStrict moip ub ptM
exploreLargeM :: (MOIP a, MonadIO m) => A.Array Int Double -> Maybe Point -> StateT a m (Maybe Point)
exploreLargeM ub ptM = do
    moip <- get
    liftIO $ exploreLarge moip ub ptM



newIloObjectM :: (MOIP a, CPX.IloObject b, MonadIO m) => StateT a m b
newIloObjectM = do
    moip <- gets toMOIPScheme
    liftIO $ MOIP.newIloObject moip

addM :: (MOIP a, CPX.IloObject b, CPX.IloAddable b, MonadIO m) => b -> StateT a m ()
addM a = do
    moip <- gets toMOIPScheme
    liftIO $ moip `MOIP.moipAdd` a
removeM :: (MOIP a, CPX.IloObject b, CPX.IloAddable b, MonadIO m) => b -> StateT a m ()
removeM a = do
    moip <- gets toMOIPScheme
    liftIO $ moip `MOIP.moipRemove` a


