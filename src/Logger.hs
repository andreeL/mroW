module Logger
( Logger
, Channel(..)
, withLogger
) where

import Control.Concurrent.MVar (MVar(..), newMVar, withMVar)
import Control.Monad (forM, forM_)
import System.IO (openFile, IOMode(AppendMode), hPutStrLn, hClose, hFlush, stdout)

data Channel = Console | File String
type Logger = String -> IO ()

withLogger :: [Channel] -> (Logger -> IO a) -> IO a
withLogger channels callback = do
    logChannels <- forM channels $ \channel -> case channel of
        Console -> pure $ (\message -> putStrLn message *> hFlush stdout, pure ())
        File filename -> do
            logFile <- openFile filename AppendMode
            pure $ (\message -> hPutStrLn logFile message *> hFlush logFile, hClose logFile)

    mutex <- newMVar ()

    result <- callback $ \message -> withMVar mutex $ const $ forM_ logChannels $ \(logger, _) -> logger message

    forM_ logChannels $ \(_, cleanup) -> cleanup

    pure result
