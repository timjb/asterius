module Asterius.Internals.TimeIt
  ( timeIt
  ) where

import Control.Monad.IO.Class
import GHC.Clock

timeIt :: MonadIO m => String -> m a -> m a
timeIt tag m = do
  t0 <- liftIO getMonotonicTime
  r <- m
  liftIO $ do
    t1 <- getMonotonicTime
    putStrLn $ "timeIt: " <> tag <> " took " <> show (t1 - t0) <> " s"
    pure r
