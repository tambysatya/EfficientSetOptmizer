
module MOIP.Class

(
MOIP(..), deleteMOIP, exportModel, exportModelM
,newIloObject, newIloObjectM, add, addM, remove, removeM
,setObjectiveCoef, setObjectiveCoefM, setMinimize, setMinimizeM, setMaximize, setMaximizeM
,omitConstraintOnObj, omitConstraintOnObjM, addConstraintOnObj, addConstraintOnObjM
,strictUpperBound, strictUpperBoundM, largeUpperBound, largeUpperBoundM
,objValue, objValueM
,solve, solveM, solveFromPoint, solveFromPointM
,exploreStrict, exploreStrictM, exploreLarge, exploreLargeM
, MOIPScheme, mkMOIPScheme, _objvars, _domvars, _objfun , _objbind
,MOIP.OptValue(..)

)

where


import MOIP.Type
import qualified MOIP.Methods as MOIP
import SearchRegion.Class

import qualified IloCplex as CPX

import qualified Data.Array as A
import qualified Data.Array.Unboxed as A
import Control.Monad.State

class MOIP a where
    toMOIPScheme :: a -> MOIPScheme
instance MOIP MOIPScheme where
    toMOIPScheme = id


{-| Pure version -}

deleteMOIP :: (MOIP a) => a -> IO ()
deleteMOIP moip = deleteMOIPScheme $ toMOIPScheme moip

exportModel :: MOIP a => a -> String -> IO ()
exportModel moip str = MOIP.exportModel (toMOIPScheme moip) str

newIloObject :: (MOIP a, CPX.IloObject b) => a -> IO b
newIloObject moip = liftIO $ MOIP.newIloObject $ toMOIPScheme moip
add :: (MOIP a, CPX.IloAddable b) => a -> b -> IO ()
add moip a = liftIO $ MOIP.moipAdd (toMOIPScheme  moip) a
remove :: (MOIP a, CPX.IloAddable b) => a -> b -> IO ()
remove moip a = liftIO $ MOIP.moipRemove (toMOIPScheme moip) a



    {-| Objective function -}
setObjectiveCoef :: (MOIP a) => a -> Int -> Double -> IO ()
setObjectiveCoef mdl i v = MOIP.setObjectiveCoef (toMOIPScheme mdl) i v

setMinimize :: (MOIP a) => a -> IO ()
setMinimize moip = MOIP.setMinimize (toMOIPScheme moip)
setMaximize :: (MOIP a) => a -> IO ()
setMaximize moip = MOIP.setMaximize (toMOIPScheme moip)

    {-| Constraints -}
omitConstraintOnObj :: (MOIP a) =>  a -> Int -> IO ()
omitConstraintOnObj moip i = MOIP.omitConstraintOnObj (toMOIPScheme moip)  i

addConstraintOnObj :: (MOIP a) =>  a -> Int -> IO ()
addConstraintOnObj moip i = MOIP.addConstraintOnObj (toMOIPScheme moip) i

strictUpperBound :: MOIP a => a -> Bound -> IO ()
strictUpperBound moip bnd = MOIP.strictUpperBound (toMOIPScheme moip) bnd
largeUpperBound :: MOIP a => a -> Bound -> IO ()
largeUpperBound moip bnd = MOIP.largeUpperBound (toMOIPScheme moip) bnd


    {-| Solve -}
objValue :: MOIP a => a -> IO MOIP.OptValue
objValue moip = MOIP.objValue $ toMOIPScheme moip

solve :: MOIP a => a -> IO (Maybe Point)
solve moip = MOIP.solve $ toMOIPScheme moip

solveFromPoint :: MOIP a => a -> Point -> IO Point
solveFromPoint moip pt = MOIP.solveFromPoint (toMOIPScheme moip) pt


exploreStrict :: MOIP a => a -> Bound -> Maybe Point -> IO (Maybe Point)
exploreStrict moip ub ptM = do
    strictUpperBound moip ub
    -- moip `exportModel` "exploring.lp"
    case ptM of
        Nothing -> solve moip
        Just pt -> Just <$> solveFromPoint moip pt
exploreLarge :: MOIP a => a -> Bound -> Maybe Point -> IO (Maybe Point)
exploreLarge moip ub ptM = do
    largeUpperBound moip ub
    case ptM of
        Nothing -> solve moip
        Just pt -> Just <$> solveFromPoint moip pt
 
    


{-| Monadic Version -}
exportModelM :: (MOIP a, MonadIO m) => String -> StateT a m ()
exportModelM str = do
    moip <- get
    liftIO $ moip `exportModel` str
newIloObjectM :: (MOIP a, CPX.IloObject b, MonadIO m) => StateT a m b
newIloObjectM = do
    moip <- get
    liftIO $ newIloObject moip
addM :: (MOIP a, CPX.IloAddable b, MonadIO m) => b -> StateT a m ()
addM a = do
    moip <- get
    liftIO $ moip `add` a
removeM :: (MOIP a, CPX.IloAddable b, MonadIO m) => b -> StateT a m ()
removeM a = do
    moip <- get
    liftIO $ moip `remove` a
setObjectiveCoefM :: (MOIP a, MonadIO m) => Int -> Double -> StateT a m ()
setObjectiveCoefM i vi = do
    moip <- gets toMOIPScheme
    liftIO $ MOIP.setObjectiveCoef moip i vi
setMaximizeM :: (MOIP a, MonadIO m) => StateT a m ()
setMaximizeM = do
    moip <- gets toMOIPScheme
    liftIO $ MOIP.setMaximize moip
setMinimizeM :: (MOIP a, MonadIO m) => StateT a m ()
setMinimizeM = do 
    moip <- gets toMOIPScheme
    liftIO $ MOIP.setMinimize moip


omitConstraintOnObjM :: (MOIP a, MonadIO m) => Int -> StateT a m ()
omitConstraintOnObjM i = do
    moip <- gets toMOIPScheme
    liftIO $ MOIP.omitConstraintOnObj moip i
addConstraintOnObjM :: (MOIP a, MonadIO m) => Int -> StateT a m ()
addConstraintOnObjM i = do
    moip <- gets toMOIPScheme
    liftIO $ MOIP.addConstraintOnObj moip i  

largeUpperBoundM :: (MOIP a, MonadIO m) => Bound -> StateT a m ()
largeUpperBoundM bnd = do
    moip <- gets toMOIPScheme
    liftIO $ MOIP.largeUpperBound moip bnd
strictUpperBoundM :: (MOIP a, MonadIO m) => Bound -> StateT a m ()
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

exploreStrictM :: (MOIP a, MonadIO m) => Bound -> Maybe Point -> StateT a m (Maybe Point)
exploreStrictM ub ptM = do
    moip <- get
    liftIO $ exploreStrict moip ub ptM
exploreLargeM :: (MOIP a, MonadIO m) => Bound -> Maybe Point -> StateT a m (Maybe Point)
exploreLargeM ub ptM = do
    moip <- get
    liftIO $ exploreLarge moip ub ptM



