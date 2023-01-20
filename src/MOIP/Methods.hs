{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MOIP.Methods
(OptValue
, setObjectiveCoef
, largeUpperBound, strictUpperBound, omitConstraintOnObj, addConstraintOnObj
, objValue, solve, solveFromPoint
, newIloObject, moipAdd, moipRemove
)

where

import SearchRegion.Class
import MOIP.Type
import IloCplex hiding (solve, newIloObject)
import qualified IloCplex as CPX (solve,newIloObject)

import Control.Monad
import Control.Lens
import Data.Maybe
import qualified Data.Array as A

newtype OptValue = OptValue Double
    deriving (Eq,Ord,Num)


{-| Objective function -}
setObjectiveCoef :: MOIPScheme -> Int -> Double -> IO ()
setObjectiveCoef moip i val = setLinearCoef fun (_objvars moip A.! i) val
    where fun = _objfun moip

{-| Constraints -}
largeUpperBound :: MOIPScheme -> A.Array Int Double ->  IO ()
largeUpperBound moip ub = forM_ (A.assocs ub) $ \(i,vi) -> setUB (_objctrs moip A.! i) vi

strictUpperBound :: MOIPScheme -> A.Array Int Double -> IO ()
strictUpperBound moip ub = forM_ (A.assocs ub) $ \(i,vi) -> setUB (_objctrs moip A.! i) $ vi - 0.5

omitConstraintOnObj :: MOIPScheme -> Int -> IO ()
omitConstraintOnObj moip i= _model moip `remove`( _objctrs moip A.! i)

addConstraintOnObj :: MOIPScheme -> Int -> IO ()
addConstraintOnObj moip i = _model moip `add`( _objctrs moip A.! i)

{-| Solve -}
objValue :: MOIPScheme -> IO OptValue
objValue moip = OptValue <$> (getObjValue $ _cplex moip)

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
    zipWithM (editMIPStart mipstart) (A.elems $ _domvars moip) (A.elems $ _ptSol pt)
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
mkVect :: (IloVar a) => MOIPScheme -> A.Array Int a -> IO (A.Array Int Double)
mkVect moip vars = A.listArray (1,length vars) <$> forM (A.elems vars) (_cplex moip `getValue`)
