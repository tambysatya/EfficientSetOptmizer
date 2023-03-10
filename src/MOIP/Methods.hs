{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MOIP.Methods
(OptValue(..)
, exportModel
, setObjectiveCoef, setMinimize, setMaximize
, largeUpperBound, strictUpperBound, omitConstraintOnObj, addConstraintOnObj
, objValue, solve, solveFromPoint
, newIloObject, moipAdd, moipRemove
)

where

import SearchRegion.Class
import MOIP.Type
import IloCplex hiding (solve, newIloObject, setMinimize, setMaximize, exportModel)
import qualified IloCplex as CPX (solve,newIloObject, setMinimize, setMaximize, exportModel)

import Control.Monad
import Control.Lens
import Data.Maybe
import qualified Data.Array as A
import qualified Data.Array.Unboxed as AU

newtype OptValue = OptValue Double
    deriving (Eq,Ord,Num)


exportModel :: MOIPScheme -> String -> IO ()
exportModel moip str = _cplex moip `CPX.exportModel` str
{-| Objective function -}
setObjectiveCoef :: MOIPScheme -> Int -> Double -> IO ()
setObjectiveCoef moip i val = setLinearCoef fun (_objvars moip A.! i) val
    where fun = _objfun moip

setMaximize :: MOIPScheme -> IO ()
setMaximize moip = CPX.setMaximize $ _objfun moip

setMinimize :: MOIPScheme -> IO ()
setMinimize moip = CPX.setMinimize $ _objfun moip

{-| Constraints -}
largeUpperBound :: MOIPScheme -> AU.UArray Int Double ->  IO ()
largeUpperBound moip ub = forM_ (AU.assocs ub) $ \(i,vi) -> setUB (_objctrs moip A.! i) vi

strictUpperBound :: MOIPScheme -> AU.UArray Int Double -> IO ()
strictUpperBound moip ub = forM_ (AU.assocs ub) $ \(i,vi) -> setUB (_objctrs moip A.! i) $ vi - 0.5

omitConstraintOnObj :: MOIPScheme -> Int -> IO ()
omitConstraintOnObj moip i= _model moip `remove`( _objctrs moip A.! i)

addConstraintOnObj :: MOIPScheme -> Int -> IO ()
addConstraintOnObj moip i = _model moip `add`( _objctrs moip A.! i)

{-| Solve -}
objValue :: MOIPScheme -> IO OptValue
objValue moip = OptValue .fromInteger.round<$> (getObjValue $ _cplex moip)

solve :: MOIPScheme -> IO (Maybe Point)
solve moip = do 
        resultB <- CPX.solve (_cplex moip) 
        if not resultB 
            then pure Nothing
            else do pt <- extractPoint moip
                    pure $ Just pt

        
    where dvars = _domctrs moip
          ovars = _objvars moip

solveFromPoint :: MOIPScheme -> Point -> IO Point
solveFromPoint moip ws = do
    mipstart <- addWarmStart moip ws
    ptM <- solve moip
    if isNothing ptM
        then error $ "reoptimizing from " ++ show ws ++ " has failed."
        else pure $ fromJust ptM


extractPoint :: MOIPScheme -> IO Point
extractPoint moip = Point <$> mkVect moip (_objvars moip) <*> mkVect moip (_domvars moip)


{-| MIPStart -}

addWarmStart :: MOIPScheme -> Point -> IO MIPStart
addWarmStart moip pt = do
    mipstart <- newIloObject moip :: IO MIPStart
    zipWithM (editMIPStart mipstart) (A.elems $ _domvars moip) (AU.elems $ _ptSol pt)
    addMIPStart (_cplex moip) mipstart
    pure mipstart
{-| Wrapper - object creation -}
newIloObject :: (IloObject a) => MOIPScheme -> IO a
newIloObject moip = CPX.newIloObject (getEnv moip) 

moipAdd :: (IloAddable a, IloObject a) => MOIPScheme -> a -> IO ()
moipAdd moip a = _model moip `add` a

moipRemove :: (IloAddable a, IloObject a) => MOIPScheme -> a -> IO ()
moipRemove moip a = _model moip `remove` a


-- utils
mkVect :: (IloVar a) => MOIPScheme -> A.Array Int a -> IO (AU.UArray Int Double)
mkVect moip vars = AU.listArray (1,length vars) . fmap (fromInteger. round) <$> forM (A.elems vars) (_cplex moip `getValue`)
