module MOIP.Scheme where

import IloCplex
import LP
import MOIP.Domain
import SearchRegion.Class
import qualified Data.Array as A
import Control.Monad
import Foreign.ForeignPtr

type IloBoolVarArray = A.Array Int IloBoolVar
type IloNumVarArray = A.Array Int IloNumVar
type IloRangeArray = A.Array Int IloRange


data MOIPScheme = MOIPScheme { _moipsLP :: !LP,
                               _moipsObjvars :: !IloNumVarArray,
                               _moipsDomvars :: !IloBoolVarArray,
                               _moipsobjctrs :: !IloRangeArray,
                               _moipsdomctrs :: ![IloRange]}

{-| Initialize a LP object with the correcponding variables, constraints.
    The objective function is to be minimized.
    The objective coefficients must be set manually -}



mkMOIPScheme env dom@(objcoefs, lbcoefs, ctrcoefs, ubcoefs) = do
        putStrLn "creating moip scheme"
        lp <- newLP env
        objvars <- A.listArray (1,p) <$> (forM [1..p] $ pure $ newIloObject env)
        domvars <- A.listArray (1,n) <$> (forM [1..n] $ pure $ newIloObject env)

        let mkObjBindCtr :: Int -> [Double] -> IO IloRange
            mkObjBindCtr k coefs = do
                ctr <- newIloObject env
                lpAdd lp ctr
                setBounds ctr (0,0)
                forM (zip (A.elems domvars)  coefs) $ \(vi,ci) -> setLinearCoef ctr vi ci 
                setLinearCoef ctr (objvars A.! k) (-1) 
                pure ctr
            mkDomCtr :: Double -> [Double] -> Double -> IO IloRange
            mkDomCtr lb coefs ub = do
                ctr <- newIloObject env
                lpAdd lp ctr
                setBounds ctr (lb,ub)
                forM (zip (A.elems domvars) coefs) $ \(vi,ci) -> setLinearCoef ctr vi ci
                pure ctr 
        objbindctrs <- zipWithM mkObjBindCtr [1..p] objcoefs -- builds y_i = f(x)
        domctrs <- forM (zip3 lbcoefs ctrcoefs ubcoefs) $ \(lb,coefs,ub) -> mkDomCtr lb coefs ub -- builds l <= AX <= u
        objctrs <- forM [1..p] $ \i -> do
            ctr <- newIloObject env
            lpAdd lp ctr
            setLinearCoef ctr (objvars A.! i) 1
	    setBounds ctr (-(2^32),2^32)
            pure ctr
--        mipstart <- mkEmptyMIPStart env (A.elems domvars)

        pure $ MOIPScheme lp objvars domvars (A.listArray (1,p) objctrs) (objbindctrs ++ domctrs) --mipstart
     where n = length (head objcoefs)
           p = length objcoefs




_setObjectiveCoef :: MOIPScheme -> Int -> Double -> IO ()
_setObjectiveCoef  (MOIPScheme lp ovars _ _ _ ) k objcoef = setLinearCoef (lpObj lp) (ovars A.! k) objcoef

_setLocalUpperBound :: (Boundary z) => MOIPScheme -> z -> IO ()
_setLocalUpperBound (MOIPScheme lp ovars dvars octrs _ ) ub = zipWithM_ setUB (A.elems octrs) (fmap (\x -> x -0.5) $ A.elems $ toBound ub)

_setEqualityConstraint :: MOIPScheme -> Int -> Double -> IO ()
_setEqualityConstraint (MOIPScheme lp ovars _ octrs _) i val = setBounds (octrs A.! i) (val,val)

_reoptimizeFrom :: (Boundary z) => MOIPScheme -> z -> IO ()
_reoptimizeFrom (MOIPScheme lp ovars dvars octrs _ ) ub = zipWithM_ setUB (A.elems octrs)  $ A.elems $ toBound ub



_solve :: MOIPScheme -> IO (Maybe Point)
_solve moip = do
    ret <- lpSolve $ _moipsLP moip
    case ret of
      False -> pure Nothing
      True -> Just <$> extractPoint
   where extractTab vartab = fmap (A.listArray (A.bounds vartab)) $ forM (A.elems vartab) $ \vi -> fromIntegral . round <$> lpValueOf (_moipsLP moip) vi
         extractPoint = Point <$> extractTab (_moipsObjvars moip) <*> extractTab (_moipsDomvars moip)

_startFrom :: MOIPScheme -> Point -> IO MIPStart
_startFrom moip pt = do
    mipstart <- lpNew (_moipsLP moip)
    zipWithM (editMIPStart mipstart) (A.elems $ _moipsDomvars moip) (A.elems $ _ptSol pt)
    addMIPStart (lpCpx $ _moipsLP moip) mipstart

    addMIPStart (lpCpx lp) mipstart

    pure mipstart
  where lp = _moipsLP moip


_solveFromPoint :: MOIPScheme -> Point -> IO (Maybe Point)
_solveFromPoint moip pt = do
    (MIPStart mip) <- _startFrom moip pt
    ret <- _solve moip
    touchForeignPtr mip
    pure ret

_ommitConstraintOnObj :: MOIPScheme -> Int -> IO ()
_ommitConstraintOnObj (MOIPScheme lp ovars dvars octrs _ ) k = lp `lpRemove` (octrs A.! k)

_addConstraintOnObj :: MOIPScheme -> Int -> IO ()
_addConstraintOnObj (MOIPScheme lp ovars dvars octrs _ ) k = lp `lpAdd` (octrs A.! k)

_exportModel :: MOIPScheme -> String -> IO ()
_exportModel moip name = exportModel (lpCpx $ _moipsLP moip) name

_setMaximize :: MOIPScheme -> IO ()
_setMaximize moip = setMaximize $ lpObj $ _moipsLP moip

_getObjValue :: MOIPScheme -> IO Double
_getObjValue moip = lpObjValue $ _moipsLP moip
