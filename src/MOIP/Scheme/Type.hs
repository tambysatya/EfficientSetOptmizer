{-# LANGUAGE TemplateHaskell #-}
module MOIP.Scheme.Type where

import MOIP.Domain


import IloCplex


import qualified Data.Array as A
import Control.Monad
import Control.Lens

type IloRangeArray = A.Array Int IloRange
type IloNumVarArray = A.Array Int IloNumVar
type BoolVarArray = A.Array Int IloBoolVar


data MOIPScheme = MOIPScheme {_model :: IloModel
                              ,_cplex :: IloCplex
                              ,_objfun :: IloObjective

                              ,_objctrs :: IloRangeArray
                              ,_domctrs :: IloRangeArray

                              ,_objvars :: IloNumVarArray
                              ,_domvars :: BoolVarArray}
makeLenses ''MOIPScheme


{-| The objective function is NOT ASSIGNED -}
mkMOIPScheme :: IloEnv -> Domain -> IO MOIPScheme
mkMOIPScheme env dom@(objcoefs, lbcoefs, ctrcoefs, ubcoefs) = do
        mdl <- newIloObject env 
        cpx <- newIloObject env
        cpx `extract` mdl
        ovars <- A.listArray (1,p) <$> (forM [1..p] $ pure $ newIloObject env)
        dvars <- A.listArray (1,n) <$> (forM [1..n] $ pure $ newIloObject env)
        let mkObjBindCtr :: Int -> [Double] -> IO IloRange
            mkObjBindCtr k coefs = do
                ctr <- newIloObject env
                mdl `add` ctr
                setBounds ctr (0,0)
                forM (zip (A.elems dvars)  coefs) $ \(vi,ci) -> setLinearCoef ctr vi ci 
                setLinearCoef ctr (ovars A.! k) (-1) 
                pure ctr
            mkDomCtr :: Double -> [Double] -> Double -> IO IloRange
            mkDomCtr lb coefs ub = do
                ctr <- newIloObject env
                mdl `add` ctr
                setBounds ctr (lb,ub)
                forM (zip (A.elems dvars) coefs) $ \(vi,ci) -> setLinearCoef ctr vi ci
                pure ctr 
        objbindctrs <- zipWithM mkObjBindCtr [1..p] objcoefs -- builds y_i = f(x)
        dctrs <- forM (zip3 lbcoefs ctrcoefs ubcoefs) $ \(lb,coefs,ub) -> mkDomCtr lb coefs ub -- builds l <= AX <= u
        octrs <- forM [1..p] $ \i -> do
            ctr <- newIloObject env
            mdl `add` ctr
            setLinearCoef ctr (ovars A.! i) 1
            setBounds ctr (-(2^32),2^32)
            pure ctr
--        mipstart <- mkEmptyMIPStart env (A.elems domvars)
        ofun <- newIloObject env
        setMinimize ofun
        pure $ MOIPScheme mdl cpx ofun (A.listArray (1,p) octrs) (A.listArray (1,m) dctrs) ovars dvars

    where (m,n,p) = (nbDomCtrs dom, nbDomVars dom, nbObjVars dom)

