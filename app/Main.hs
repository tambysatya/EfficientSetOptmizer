module Main (main) where

import Optimizer
import MOIP
import IloCplex

import Control.Monad

main :: IO ()
main = void mainRandom


mainKS' =
    forM instances $ \(p,n) -> 
    --forM [2] $ \i -> do
    forM [1..10] $ \i -> do
        let name = mkKSName p n i             
        env <- newIloEnv
        dom@(objs,_,_,_) <- read1KS name
        let coefs = foldr1 (zipWith (+)) objs
            --funcoefs = FunCoefs $ zip [1..] $ fmap negate coefs
            funcoefs = FunCoefs [] $ zip [1..] $ take p (repeat (-1))
        --val <- runAlgorithm "nbdef-subopt-childhv-arfix" "kp.log" env dom funcoefs
        val <- runAlgorithm "reformulate-optcompile-weighted-EPSGAP+fixProjVal-refactor-[update+archive weakNDL]- [XE Archive]" "trash.log" env dom funcoefs
        --val <- runAlgorithm "weightedreopt-nbdef-subopt-childhv-arfix" "kp.log" env dom funcoefs
        --val <- runAlgorithm "cut-prdir=cdir-nolb" "kp.log" env dom funcoefs
        print val
         
  where  instances =  [(3,100), (4,100)]
        -- instances =  [(4,100)]
        --instances =  [(4,100), (5,100), (3,100)]
        --instances = [(5,100), (4,100),(3,100)] --[(3,100),(4,100)]

mainRandom =
    forM instances $ \(p,n) -> 
    --forM [2] $ \i -> do
    forM [1..10] $ \i -> do
        let name = mkKSName p n i             
        env <- newIloEnv
        dom@(objs,a,b,c) <- read1KS name
        let coefs = foldr1 (zipWith (+)) objs
            --funcoefs = FunCoefs $ zip [1..] $ fmap negate coefs
            --funcoefs = FunCoefs [] $ zip [1..] $ take p (repeat (-1))
            funcoefs = FunCoefs (zip [1..] (last objs)) []
            dom' = (init objs,a,b,c)
        --val <- runAlgorithm "nbdef-subopt-childhv-arfix" "kp.log" env dom funcoefs
        val <- runAlgorithm "refactor" "randomKS.log" env dom' funcoefs
        --val <- runAlgorithm "refactor" "randomKS.log" env dom' funcoefs
        --val <- runAlgorithm "weightedreopt-nbdef-subopt-childhv-arfix" "kp.log" env dom funcoefs
        --val <- runAlgorithm "cut-prdir=cdir-nolb" "kp.log" env dom funcoefs
        print val
         
  where  instances =  [(3,100), (4,100),(5,100)]
        --instances =  [(5,100)]
        --instances =  [(4,100), (5,100), (3,100)]
        --instances = [(5,100), (4,100),(3,100)] --[(3,100),(4,100)]
         mkKSName p n i = "/home/sat/git/EfficientSetOptimizer/Instances/SatKP_random/SatKP_p-" ++ show p ++ "_n-" ++ show n ++ "_i-" ++ show i ++ ".ins"



mkKSName :: Int -> Int -> Int -> String
mkKSName p n i = "/home/sat/git/NadirSolver/Instances/SatyaKP/Sat_KP_p-" ++ show p ++ "_n-" ++ show n ++ "_" ++ show i ++ ".dat"


