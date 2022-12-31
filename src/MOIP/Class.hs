{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
    {-| Interface for developping multiobjective models.

    Provides generic functions for exploring search zones, adding / removing budget constrains 
    or providing a warm start to the solver

    A multiobjective integer model contains a domain X which is described using linearly constrained binary variables x = (x_1,...,x_n) 
    and p objective functions that are stored in p variables y_i = f_i (x).
    Moreover, p constraints (so called budget constraints), each one constraining an objective y_i are introduced.    
. -}
module MOIP.Class where

import SearchRegion.Class
import IloCplex hiding (exportModel, getObjValue)
import qualified Data.Array as A
import Control.Monad.State
import MOIP.Scheme

          

{-| A multi-objective integer program -}  
class MOIP a where
    nbCrits :: a -> Int
    setLocalUpperBound :: (Boundary z) => a -> z -> IO () -- | Specify the local upper bound to explore
    solve     :: a -> IO (Maybe Point) -- | Solves and returns the optimal point if the domain is feasible
         
 {-| A simple interface for writing multi-objective programs. -}
class SimpleMOIP a where
    toMOIPScheme :: a -> MOIPScheme 

class AsymetricMOIP a where
   selectObjective :: a -> ProjDir -> IO a


instance SimpleMOIP MOIPScheme where
    toMOIPScheme = id
instance (SimpleMOIP a) => MOIP a where
    nbCrits moip = snd $ A.bounds $ (_moipsObjvars $ toMOIPScheme moip)
    setLocalUpperBound moip = _setLocalUpperBound (toMOIPScheme moip)
    solve = _solve . toMOIPScheme
   

startFrom :: (SimpleMOIP a) => a -> Point -> IO MIPStart -- | Uses the provided point as a warm start. Specifying a warm start usually significantly reduces the time spent to solve the model, since the evaluation tree can be pruned.
startFrom moip = _startFrom (toMOIPScheme moip)

setObjectiveCoef :: (SimpleMOIP a) => a -> Int -> Double -> IO () -- | Modify the weight of the objective f_k in the objective function
setObjectiveCoef moip i = _setObjectiveCoef (toMOIPScheme moip) i

ommitConstraintOnObj :: (SimpleMOIP a) => a -> Int -> IO ()
ommitConstraintOnObj moip = _ommitConstraintOnObj (toMOIPScheme moip)

addConstraintOnObj :: (SimpleMOIP a) => a -> Int -> IO ()
addConstraintOnObj moip = _addConstraintOnObj (toMOIPScheme moip)

exportModel :: (SimpleMOIP a) => a -> String -> IO ()
exportModel moip str = _exportModel (toMOIPScheme moip) str

reoptimizeFrom :: (SimpleMOIP a, Boundary z) => a -> z -> IO ()
reoptimizeFrom moip = _reoptimizeFrom (toMOIPScheme moip)

solveFromPoint :: (SimpleMOIP a) => a -> Point -> IO (Maybe Point)
solveFromPoint moip = _solveFromPoint (toMOIPScheme moip)

solveFromPointM :: (MonadIO m, SimpleMOIP a) => Point -> StateT a m (Maybe Point)
solveFromPointM pt = do
    me <- get
    liftIO $ solveFromPoint me pt

reoptimizeFromM :: (MonadIO m, SimpleMOIP a, Boundary z) => z -> StateT a m ()
reoptimizeFromM z = do
    me <- get
    liftIO $ reoptimizeFrom me z
setLocalUpperBoundM :: (MonadIO m, SimpleMOIP a, Boundary z) => z -> StateT a m ()
setLocalUpperBoundM z = do
    me <- get
    liftIO $ setLocalUpperBound me z

setEqualityConstraint :: (SimpleMOIP a) => a -> Int -> Double -> IO ()
setEqualityConstraint moip i val = _setEqualityConstraint (toMOIPScheme moip) i val

selectObjectiveM :: (MonadIO m, AsymetricMOIP a) => ProjDir -> StateT a m ()
selectObjectiveM k = do
    me <- get
    me' <- liftIO $ selectObjective me k
    put me'

solveM :: (MonadIO m, SimpleMOIP a) => StateT a m (Maybe Point)
solveM = do
    mdl <- get
    liftIO $ MOIP.Class.solve mdl

getObjValue :: (SimpleMOIP a) => a -> IO Double
getObjValue moip = _getObjValue (toMOIPScheme moip)
getObjValueM :: (MonadIO m, SimpleMOIP a) => StateT a m Double
getObjValueM = do
    me <- get
    liftIO $ getObjValue me
