{-                                                                               
 - ----------------------------------------------------------------------------  
 - "THE BEER-WARE LICENSE" (Revision 42):                                        
 - <code@gregorkopf.de> wrote this file. As long as you retain this notice you   
 - can do whatever you want with this stuff. If we meet some day, and you        
 - think this stuff is worth it, you can buy me a beer in return Gregor Kopf     
 - ----------------------------------------------------------------------------  
 -}


{-| This module implements a higher-level abstraction of an OpenBM service. -}
module Network.OpenBM.Service (OpenBMServiceContext, openBMServiceCreate,
                               openBMRegisterInterest, sendOpenBMHandle,
                               recvOpenBMHandle, OpenBMHandle) where

import           Network.OpenBM
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           Network
import           Control.Monad
import           Control.Concurrent
import           Data.IORef
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Control.Concurrent.MVar
import           System.IO
import qualified Control.Exception as EX

-- | A Handle to an OpenBM connection
type OpenBMHandle = TChan OpenBMMessage

-- | The context of an OpenBM service
data OpenBMServiceContext = OpenBMServiceContext {
      openBMHandle       :: Handle
    , openBMInterests    :: IORef [(   (OpenBMMessage -> Bool) 
                                     , OpenBMHandle
                                     , OpenBMHandle
                                  )]
    , openBMInterestLock :: MVar ()
    , openBMShutdown     :: MVar ()
    , openBMInterestSet  :: MVar ()
   }

-- | Pings the OpenBM server on a regular basis
openBMPingThread :: OpenBMServiceContext -> IO ()
openBMPingThread ctx = forever $ do
   sendOpenBM (openBMHandle ctx) openBMPing
   threadDelay $ 1000000 * 2 -- Send a ping every 2 seconds just in case..

-- | The main OpenBM service thread. It waits for OpenBM messages and dispatches
-- them to all interested parties.
openBMServiceThread :: OpenBMServiceContext -> IO ()
openBMServiceThread ctx = forever $ do
   msg       <- receiveOpenBM (openBMHandle ctx)
   interests <- readIORef $ openBMInterests ctx
   forM_ interests $ \(selector, chan, _) -> do
      when (selector msg) $ atomically $ writeTChan chan msg

-- | Multiplexes from a number of channels
selectSTM :: [TChan a] -> STM a
selectSTM [] = retry
selectSTM (chan:chans) = readTChan chan `orElse` selectSTM chans

-- | The OpenBM sender thread is for multiplexing between the individual output
-- channels from the interested parties.
openBMSenderThread :: OpenBMServiceContext -> IO ()
openBMSenderThread ctx = forever $ do
   interests <- readIORef $ openBMInterests ctx
   if length interests == 0
      then takeMVar (openBMInterestSet ctx)
      else do let chans = map (\(_, _, chan) -> chan) interests
              msg <- atomically $ selectSTM chans
              sendOpenBM (openBMHandle ctx) msg

-- | Runs an IO action and exit the whole process if an exception occurs.
-- This is most useful for multi-threaded code, where normally an exception
-- would only terminate the current thread.
forkIOCritical :: IO () -> IO ThreadId
forkIOCritical action = do
   tid <- myThreadId
   forkIO $ EX.catch action (handler tid)
   where handler :: ThreadId -> EX.SomeException -> IO ()
         handler tid ex = throwTo tid ex

-- | Creates an OpenBM service context. It will perform the low-level tasks
-- such as pinging the OpenBM server on a regular basis, so that you can
-- focus on the actual application logic.
openBMServiceCreate ::     String -- ^ The hostname of the OpenBM server
                        -> Int -- ^ The server's port number
                        -> IO (Either String OpenBMServiceContext)
openBMServiceCreate hostName port = do
   hdl'     <- connectTo hostName (PortNumber $ fromIntegral port)
   hsResult <- doOpenBMHandshake hostName hdl'
   hClose hdl'
   case hsResult of
      Left err  -> return $ Left err
      Right hdl -> do ior <- newIORef []
                      lck <- newMVar ()
                      mv  <- newEmptyMVar 
                      is  <- newEmptyMVar
                      let ct = OpenBMServiceContext {   openBMHandle    = hdl
                                                      , openBMInterests = ior
                                                      , openBMInterestLock = lck
                                                      , openBMShutdown = mv
                                                      , openBMInterestSet = is
                                                    }
                      forkIOCritical $ openBMPingThread    ct
                      forkIOCritical $ openBMServiceThread ct
                      forkIOCritical $ openBMSenderThread  ct
                      return $ Right ct

-- | Registers interest for a particular kind of OpenBM message and returns
-- a pair of Channels. The first one is for receiving data from the OpenBM
-- gateway, the second one is for sending data to the OpenBM gateway.
openBMRegisterInterest ::    OpenBMServiceContext -- ^ The service context
                          -> (OpenBMMessage -> Bool) -- ^ The selector
                          -> IO (OpenBMHandle, OpenBMHandle)
openBMRegisterInterest ctx sel = do
   rChan <- newTChanIO
   wChan <- newTChanIO
   takeMVar (openBMInterestLock ctx)
   interests <- readIORef (openBMInterests ctx)
   writeIORef (openBMInterests ctx) ((sel,rChan,wChan):interests)
   tryPutMVar (openBMInterestSet ctx) ()
   putMVar (openBMInterestLock ctx) ()
   return $ (rChan, wChan)

-- | Sends a message to an OpenBM Handle
sendOpenBMHandle :: OpenBMHandle -> OpenBMMessage -> IO ()
sendOpenBMHandle h m = atomically $ writeTChan h m

-- | Receives a message from an OpenBM Handle
recvOpenBMHandle :: OpenBMHandle -> IO OpenBMMessage
recvOpenBMHandle h = atomically $ readTChan h
