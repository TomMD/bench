{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | For POSIX systems with a /proc/[pid]/statm file in the proper form and
-- 4096 byte pages, provide tools to execute a process while tracking the memory use.
module BenchUtils
    ( readProcessMemory
    , memoryMonitor
    , pidOfProcessHandle
    ) where

import Data.Maybe (fromMaybe)
import Control.DeepSeq (rnf)
import Control.Monad
import Control.Exception as X
import Control.Concurrent
import Control.Concurrent.MVar
import System.Process
import System.Process.Internals
import System.IO
import System.IO.Error (mkIOError, ioeSetErrorString)
import System.Exit      ( ExitCode(..) )

import System.FilePath
import Foreign.C.Error
import GHC.IO.Exception ( ioException, IOErrorType(..), IOException(..) )
import System.Posix.Types
import System.Posix.Signals


readProcessMemory :: String               -- ^ Filename of the executable (see 'RawCommand' for details)
                  -> [String]             -- ^ arguments
                  -> String               -- ^ stdin
                  -> IO (String,[Double]) -- ^ stdout, memory use samples
readProcessMemory cmd args input =
  do let cp = (proc cmd args) {
                    std_in  = CreatePipe,
                    std_out = CreatePipe,
                    std_err = Inherit
                  }
     (ex,output,mem) <- withCreateProcess_ "readCreateProcessMemory" cp $
        \(Just inh) (Just outh) _ ph -> do
        mv <- memoryMonitor ph
        -- fork off a thread to start consuming the output
        output  <- hGetContents outh
        withForkWait (X.evaluate $ rnf output) $ \waitOut -> do

          -- now write any input
          unless (null input) $
            ignoreSigPipe $ hPutStr inh input
          -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
          ignoreSigPipe $ hClose inh

          -- wait on the output
          waitOut
          hClose outh

        -- wait on the process
        ex <- waitForProcess ph
        mem <- takeMVar mv
        return (ex, output, mem)

     case ex of
      ExitSuccess   -> return (output,mem)
      ExitFailure r -> processFailedException "readCreateProcess" cmd args r



  -- do (sin, sout, serr, ph) <- createProcess (proc bin args)
  --    mv  <- memoryMonitor ph
  --    maybe (return ()) (\h -> hPutStr h stdIn) sin
  --    err <- maybe (return "") hGetContents serr
  --    out <- maybe (return "") hGetContents sout
  --    X.evaluate (rnf err)
  --    X.evaluate (rnf out)
  --    _   <- waitForProcess ph
  --    (out ++ err,) <$> takeMVar mv

-- wrapper so we can get exceptions with the appropriate function name.
withCreateProcess_
  :: String
  -> CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess_ fun c action =
    X.bracketOnError (createProcess_ fun c) cleanupProcess
                     (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)

cleanupProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
               -> IO ()
cleanupProcess (mb_stdin, mb_stdout, mb_stderr, ph) = do
    terminateProcess ph
    -- Note, it's important that other threads that might be reading/writing
    -- these handles also get killed off, since otherwise they might be holding
    -- the handle lock and prevent us from closing, leading to deadlock.
    maybe (return ()) (ignoreSigPipe . hClose) mb_stdin
    maybe (return ()) hClose mb_stdout
    maybe (return ()) hClose mb_stderr
    -- terminateProcess does not guarantee that it terminates the process.
    -- Indeed on Unix it's SIGTERM, which asks nicely but does not guarantee
    -- that it stops. If it doesn't stop, we don't want to hang, so we wait
    -- asynchronously using forkIO.
    _ <- forkIO (waitForProcess ph >> return ())
    return ()

processFailedException :: String -> String -> [String] -> Int -> IO a
processFailedException fun cmd args exit_code =
      ioError (mkIOError OtherError (fun ++ ": " ++ cmd ++
                                     concatMap ((' ':) . show) args ++
                                     " (exit " ++ show exit_code ++ ")")
                                 Nothing Nothing)


ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = X.handle $ \e -> case e of
                                   IOError { ioe_type  = ResourceVanished
                                           , ioe_errno = Just ioe }
                                     | Errno ioe == ePIPE -> return ()
                                   _ -> throwIO e

withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `X.onException` killThread tid

-- XXX pageSize is a bad hard-coded assumption.  True for my use (this is
-- single shot code - if you are reading this then it has out-lived its designed
-- purpose).
pageSize :: Double
pageSize = 4096

memoryMonitor :: ProcessHandle -> IO (MVar [Double])
memoryMonitor ph =
 do pidm <- pidOfProcessHandle ph
    mv <- newEmptyMVar
    maybe (putMVar mv []) (void . forkIO . handler mv) pidm
    return mv
 where
    handler :: MVar [Double] -> PHANDLE -> IO ()
    handler fmv pid =
        do mv <- newMVar []
           X.handle (\(_::SomeException) -> putMVar fmv . map (*pageSize) . reverse =<< takeMVar mv) (go pid mv)
           -- XXX ^^^ lame use of exception for termination.
    go :: PHANDLE -> MVar [Double] -> IO ()
    go pid mv = do
        threadDelay 100000 -- XXX 1) We drift by not having proper scheduling
                           --     2) We have a race condition with
                           --        starting the process, depending on
                           --        the semantics of 'createProcess'.
        h  <- openFile ("/proc" </> show pid </> "statm") ReadMode
        cs <- hGetContents h
        let !mem = read (head (words cs))
        hClose h
        modifyMVar_ mv (return . (mem:))
        go pid mv

pidOfProcessHandle :: ProcessHandle -> IO (Maybe PHANDLE)
pidOfProcessHandle ph =
 do let ProcessHandle mv _ = ph
    hdl <- readMVar mv
    return $ case hdl of
                OpenHandle p -> Just p
                ClosedHandle _ -> Nothing
