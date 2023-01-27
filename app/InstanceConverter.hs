module InstanceConverter where


import MOIP.Type
import MOIP.Domain
import MOIP.Class
import qualified Data.Array as A
import IloCplex  
import Control.Monad
import Foreign.ForeignPtr
import Control.Lens

mkDCM env dom@(objs,a,b,c) filename = do
    moip <- mkMOIPScheme env (init objs, a,b,c)
    forM [1..length objs - 1] $ \i ->  do
        moip `omitConstraintOnObj` i
        setLinearCoef (_objbind moip A.! i) (_objvars moip A.! i) 0
    let ctrs = _objbind moip :: IloRangeArray
    forM (zip [1..] $ last objs) $ \(i,vi) -> setLinearCoef (_objfun moip) (_domvars moip A.! i) vi 
    _cplex moip `IloCplex.exportModel` filename


mainDCM = do
  let ins = [(3,100), (4,100), (5,100)] -- (7,50)]
  --let ins = [(3,50),(4,20)]
  env@(IloEnv ptr) <- newIloEnv
  forM_ [(p,n,i) | (p,n) <- ins, i <- [1..10]]$ \(p,n,i) -> do
    let 
        srcname = "Instances/SatKP_random/" ++ mkKSName p n i ++ ".ins"
        --srcname = "Instances/MOAP/" ++ mkAPName p n i ++ ".dat"
        dstname = "Instances/MOKP/Boland_random/" ++ mkKSName p n i ++ ".lp"
    putStrLn $ "converting " ++ srcname ++ " => " ++ dstname
    dom <- read1KS srcname
    --dom <- readVC srcname
    mkDCM env dom dstname


mainDCMAP = do
  let ins = [(3,30), (4,30), (5,30)] -- (7,50)]
  --let ins = [(3,50),(4,20)]
  env@(IloEnv ptr) <- newIloEnv
  forM_ [(p,n,i) | (p,n) <- ins, i <- [1..10]]$ \(p,n,i) -> do
    let 
        srcname = "Instances/SatAP_random/" ++ mkAPName p n i ++ ".ins"
        --srcname = "Instances/MOAP/" ++ mkAPName p n i ++ ".dat"
        dstname = "Instances/MOAP/Boland_random/" ++ mkAPName p n i ++ ".lp"
    putStrLn $ "converting " ++ srcname ++ " => " ++ dstname
    dom <- readVC srcname
    mkDCM env dom dstname



mkKSName p n i = "SatKP_p-" ++ show p ++ "_n-" ++ show n ++ "_i-" ++ show i 
mkAPName p n i = "SatAP_p-" ++ show p ++ "_n-" ++ show n ++ "_i-" ++ show i 

