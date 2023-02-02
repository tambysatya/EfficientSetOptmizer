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
                    funcoefs = FunCoefs (zip [1..] (last objs)) []
                    dom' = (init objs,a,b,c)
                runAlgorithm "refactor-noar-O2" logfile env dom' funcoefs
        ["AP", insfile, logfile] -> do
                dom@(objs,a,b,c) <- readVC insfile
                let coefs = foldr1 (zipWith (+)) objs
                    funcoefs = FunCoefs (zip [1..] (last objs)) []
                    dom' = (init objs,a,b,c)
                runAlgorithm "refactor-noar-O2" logfile env dom' funcoefs

        _ -> putStrLn "syntax: ./main KP|AP instance logfile"




