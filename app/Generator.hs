module Generator where

import System.Random
import Control.Monad


generateKP :: Int -> Int -> Int -> Int -> String -> IO ()
generateKP vmin vmax p n name = do
    objs <- forM [1..p+1] $ \_ -> forM [1..n] $ \i -> randomRIO (vmin,vmax)
    weights <- forM [1..n] $ \_ -> randomRIO (vmin, vmax)
    let wmax = fromIntegral (sum weights) / 2
    writeFile name $ show p ++ "\n"
    appendFile name $ show n ++ "\n"
    appendFile name $ show wmax ++ "\n"
    appendFile name $ show objs ++ "\n"
    appendFile name $ show weights ++ "\n"





generateInstances = do
        forM [(3,100), (4,100), (5,100)] $ \(p,n) -> do
            forM [1..10] $ \i -> generateKP 1 100 p n $ mkName p n i

    where path= "Instances/SatKP_random/"
          mkName p n i = path ++  "SatKP_p-" ++ show p ++ "_n-" ++ show n ++ "_i-" ++ show i ++ ".ins"
