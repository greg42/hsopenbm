{-                                                                               
 - ----------------------------------------------------------------------------  
 - "THE BEER-WARE LICENSE" (Revision 42):                                        
 - <code@gregorkopf.de> wrote this file. As long as you retain this notice you   
 - can do whatever you want with this stuff. If we meet some day, and you        
 - think this stuff is worth it, you can buy me a beer in return Gregor Kopf     
 - ----------------------------------------------------------------------------  
 -}


{-| A simple Haskell binding for the OpenBM Protocol
(https://github.com/cgart/OpenBM). Thie module implements the basic functions
for interacting with an OpenBM server.
-}
module Network.OpenBM where

import           Data.Word
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           Network
import           Control.Applicative
import           System.IO


-- | A raw OpenBM message
data OpenBMMessage = OpenBMMessage {
      openBMSrc  :: Word8 -- ^ IBUS source
    , openBMDst  :: Word8 -- ^ IBUS destination
    , openBMLen  :: Word8 -- ^ Message length
    , openBMRes1 :: Word8 -- ^ Reserved
    , openBMPrio :: Word16 -- ^ Priority
    , openBMRes2 :: Word8 -- ^ Reserved
    , openBMRes3 :: Word8 -- ^ Reserved
    , openBMData :: ByteString -- ^ The message data
   } deriving (Eq, Show)

-- | Creates an OpenBM Message
mkOpenBMMessage ::     Word8 -- ^ The IBUS source address
                    -> Word8 -- ^ The IBUS destination address
                    -> Word16 -- ^ The priority
                    -> ByteString -- ^ The payload
                    -> OpenBMMessage
mkOpenBMMessage src dst prio msg = OpenBMMessage {
      openBMSrc  = src
    , openBMDst  = dst
    , openBMLen  = fromIntegral $ BS.length msg
    , openBMRes1 = 0x00
    , openBMPrio = prio
    , openBMRes2 = 0x00
    , openBMRes3 = 0x00
    , openBMData = msg
   }

-- | An OpenBM Hello message
openBMHelloMsg :: OpenBMMessage
openBMHelloMsg = mkOpenBMMessage 0x68 0x69 0x00 BS.empty

-- | An OpenBM Ping message
openBMPing :: OpenBMMessage
openBMPing = mkOpenBMMessage 0xaa 0xaa 0x00 BS.empty

-- | An OpenBM Disconnect message
openBMDisconnect :: OpenBMMessage
openBMDisconnect = mkOpenBMMessage 0x00 0x00 0x00 BS.empty

-- | Reads and parses one OpenBM message from a Handle (probably a network
-- connection).
receiveOpenBM :: Handle -> IO OpenBMMessage
receiveOpenBM h = do
   src  <- BS.head <$> BS.hGet h 1
   dst  <- BS.head <$> BS.hGet h 1
   len  <- BS.head <$> BS.hGet h 1
   res1 <- BS.head <$> BS.hGet h 1
   tmp <- BS.hGet h 2
   let prio = fromIntegral $ (fromIntegral $ BS.head tmp) * 256 + (fromIntegral $ BS.last tmp)
   res2 <- BS.head <$> BS.hGet h 1
   res3 <- BS.head <$> BS.hGet h 1
   msg  <- BS.hGet h (fromIntegral len)
   return $ OpenBMMessage {
         openBMSrc  = src
       , openBMDst  = dst
       , openBMLen  = len
       , openBMRes1 = res1
       , openBMPrio = prio
       , openBMRes2 = res2
       , openBMRes3 = res3
       , openBMData = msg
      }

-- | Sends an OpenBM message to a Handle (probably a network connection).
sendOpenBM :: Handle -> OpenBMMessage -> IO ()
sendOpenBM h msg = do
   let p1 = fromIntegral $ (openBMPrio msg) `div` 256
   let p2 = fromIntegral $ (openBMPrio msg) `mod` 256
   let bsMsg = BS.pack $ [
           openBMSrc  msg
         , openBMDst  msg
         , openBMLen  msg
         , openBMRes1 msg
         , p1
         , p2
         , openBMRes2 msg
         , openBMRes3 msg
        ]
   BS.hPut h (BS.append bsMsg (openBMData msg))

-- | Does the OpenBM handshake procedure and returns a new Handle or an error
doOpenBMHandshake ::     String -- ^ The remote host name
                      -> Handle -- ^ The current handle
                      -> IO (Either String Handle)
doOpenBMHandshake remoteHost h = do
   sendOpenBM h (openBMHelloMsg)
   ct <- receiveOpenBM h
   if (openBMSrc ct /= 0x63 || openBMDst ct /= 0x74)
      then return $ Left $ "Expected a connect message, but got " ++ show ct
      else Right <$> connectTo remoteHost (PortNumber $ fromIntegral $ openBMPrio ct)

