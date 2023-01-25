module Main (main) where

import Optimizer
import MOIP
import IloCplex

import Control.Monad
import System.Environment

main :: IO ()
main = void mainRandom'


mainRandom' = do
    args <- getArgs
    env <- newIloEnv
    case args of
        ["KP", insfile, logfile] -> do
                dom@(objs,a,b,c) <- read1KS insfile
                let coefs = foldr1 (zipWith (+)) objs
                    --funcoefs = FunCoefs $ zip [1..] $ fmap negate coefs
                    --funcoefs = FunCoefs [] $ zip [1..] $ take p (repeat (-1))
                    funcoefs = FunCoefs (zip [1..] (last objs)) []
                    dom' = (init objs,a,b,c)
                --val <- runAlgorithm "nbdef-subopt-childhv-arfix" "kp.log" env dom funcoefs
                runAlgorithm "refactor-noar-fixLeak-archive" logfile env dom' funcoefs
        _ -> putStrLn "syntax: ./main KP instance logfile"




