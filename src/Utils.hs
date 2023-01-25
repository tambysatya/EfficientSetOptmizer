module Utils where
import Control.Monad.State

logM :: (MonadIO m) => String -> StateT a m ()
logM str = liftIO $ putStrLn str
