module Optimizer.Debug where


import System.CPUTime
import SearchRegion
import MOIP
import Optimizer.Types
import qualified Data.Array as A
import Data.Function
import qualified Data.List as L
import Control.Lens
import Data.Maybe
import Control.Monad.State


time :: (MonadIO m) => m a -> m (a, Double)
time act = do cpu <- liftIO getCPUTime
              r <- act
              cpu' <- liftIO getCPUTime
              pure (r,(fromIntegral $ cpu' - cpu) / (fromIntegral $ 10^12))


{- DEBUG -}

dbg_compute_zone_lb :: (MonadIO m) => GlobalBounds -> ExploredUB -> SubOpt -> StateT Algorithm m (Maybe (HyperOpt,Point))
dbg_compute_zone_lb gbnds zexp cutval = zoom optEffLB $ do
                            setLocalUpperBoundM zexp
                            let (AntiIdeal yA) = fst gbnds
                                (ProjDir l) = snd $ _szMaxProj $ fromExplored zexp
                            ptM <- if (toBound zexp A.! l >= yA A.! l) 
                                 then do
                                        logM $ "[WARNING]\t no defining point"
                                        solveM
                                 else do 
                                        let pts = _szDefiningPoint (fromExplored zexp) A.! l
                                            bestpt = fst $ L.minimumBy (compare `on` snd) pts
                                        solveFromPointM bestpt -- (head $ _szDefiningPoint (fromExplored zexp) A.! l)

                            retM <- case ptM of
                                        Nothing -> pure Nothing
                                        Just pt -> do
                                            optval <- getObjValueM
                                            if SubOpt optval >= cutval
                                                then pure Nothing --  No point in the projection improves the estimation
                                                else pure $ Just (HyperOpt optval,pt)
                            pure retM

