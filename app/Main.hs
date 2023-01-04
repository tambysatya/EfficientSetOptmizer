module Main (main) where

import Optimizer
import MOIP
import IloCplex

import Control.Monad

main :: IO ()
main = void mainKS


mainKS =
    forM instances $ \(p,n) -> 
    forM [1..10] $ \i -> do
        let name = mkKSName p n i             
        env <- newIloEnv
        dom@(objs,_,_,_) <- read1KS name
        let coefs = foldr1 (zipWith (+)) objs
            funcoefs = FunCoefs $ zip [1..] $ fmap negate coefs
        val <- runAlgorithm "vanilla-pdir_cdir" "kp_cut.log" env dom funcoefs
        print val
         
  where instances = [(5,100), (4,100),(3,100)] --[(3,100),(4,100)]



mkKSName :: Int -> Int -> Int -> String
mkKSName p n i = "/home/sat/git/NadirSolver/Instances/SatyaKP/Sat_KP_p-" ++ show p ++ "_n-" ++ show n ++ "_" ++ show i ++ ".dat"


