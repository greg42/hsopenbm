{-                                                                               
 - ----------------------------------------------------------------------------  
 - "THE BEER-WARE LICENSE" (Revision 42):                                        
 - <code@gregorkopf.de> wrote this file. As long as you retain this notice you   
 - can do whatever you want with this stuff. If we meet some day, and you        
 - think this stuff is worth it, you can buy me a beer in return Gregor Kopf     
 - ----------------------------------------------------------------------------  
 -}

{-| This module provides a number of high-level functions for dealing with
(known) events on the IBUS. -}

module Network.OpenBM.Events (IbusDevice(..), message, IbusEvent(..),
                              isFromDevice, messageToEvent, isToDevice) where

import           Network.OpenBM
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Maybe
import           Data.Tuple
import           Data.Word

-- | Known IBUS devices                                                          
data IbusDevice = CDPlayer | BordMonitor | SteeringWheel | Radio | Broadcast
                deriving(Eq)

instance Enum IbusDevice where 
    fromEnum = fromJust . flip lookup table 
    toEnum = fromJust . flip lookup (map swap table) 
table = [  (CDPlayer, 0x18), (BordMonitor, 0xF0), (SteeringWheel, 0x50) 
         , (Radio, 0x68), (Broadcast, 0xFF)] 

-- | An event on the IBUS.
data IbusEvent = SteeringWheelUpButtonReleased
               | SteeringWheelDownButtonReleased
               | NavigationKnobTurnedLeft Int
               | NavigationKnobTurnedRight Int
               | NavigationKnobReleased
               | ModeButtonReleased
               | LeftButtonReleased
               | RightButtonReleased
               | ReverseTapeButtonReleased
               | UnknownEvent OpenBMMessage
               deriving (Eq, Show)

-- | Checks the source address and the payload of an OpenBM message
srcDataEquals :: IbusDevice -> [Word8] -> OpenBMMessage -> Bool
srcDataEquals src payload msg =
   openBMSrc msg == (fromIntegral $ fromEnum src) && openBMData msg == BS.pack payload

-- | Parses an OpenBM message into an IbusEvent.
messageToEvent :: OpenBMMessage -> IbusEvent
messageToEvent msg
   | srcDataEquals SteeringWheel [0x3B, 0x21] msg = SteeringWheelUpButtonReleased
   | srcDataEquals SteeringWheel [0x3B, 0x28] msg = SteeringWheelDownButtonReleased
   | srcDataEquals BordMonitor [0x48, 0x94] msg = ReverseTapeButtonReleased
   | srcDataEquals BordMonitor [0x48, 0xA3] msg = ModeButtonReleased
   | srcDataEquals BordMonitor [0x48, 0xA3] msg = ModeButtonReleased
   | srcDataEquals BordMonitor [0x48, 0x85] msg = NavigationKnobReleased
   | srcDataEquals BordMonitor [0x49, 0x01] msg = NavigationKnobTurnedLeft 1
   | srcDataEquals BordMonitor [0x49, 0x02] msg = NavigationKnobTurnedLeft 2
   | srcDataEquals BordMonitor [0x49, 0x03] msg = NavigationKnobTurnedLeft 3
   | srcDataEquals BordMonitor [0x49, 0x04] msg = NavigationKnobTurnedLeft 4
   | srcDataEquals BordMonitor [0x49, 0x05] msg = NavigationKnobTurnedLeft 5
   | srcDataEquals BordMonitor [0x49, 0x06] msg = NavigationKnobTurnedLeft 6
   | srcDataEquals BordMonitor [0x49, 0x07] msg = NavigationKnobTurnedLeft 7
   | srcDataEquals BordMonitor [0x49, 0x08] msg = NavigationKnobTurnedLeft 8
   | srcDataEquals BordMonitor [0x49, 0x09] msg = NavigationKnobTurnedLeft 9
   | srcDataEquals BordMonitor [0x49, 0x81] msg = NavigationKnobTurnedRight 1
   | srcDataEquals BordMonitor [0x49, 0x82] msg = NavigationKnobTurnedRight 2
   | srcDataEquals BordMonitor [0x49, 0x83] msg = NavigationKnobTurnedRight 3
   | srcDataEquals BordMonitor [0x49, 0x84] msg = NavigationKnobTurnedRight 4
   | srcDataEquals BordMonitor [0x49, 0x85] msg = NavigationKnobTurnedRight 5
   | srcDataEquals BordMonitor [0x49, 0x86] msg = NavigationKnobTurnedRight 6
   | srcDataEquals BordMonitor [0x49, 0x87] msg = NavigationKnobTurnedRight 7
   | srcDataEquals BordMonitor [0x49, 0x88] msg = NavigationKnobTurnedRight 8
   | srcDataEquals BordMonitor [0x49, 0x89] msg = NavigationKnobTurnedRight 9
   | srcDataEquals BordMonitor [0x48, 0x90] msg = LeftButtonReleased
   | srcDataEquals BordMonitor [0x48, 0x80] msg = RightButtonReleased
   | otherwise = UnknownEvent msg

-- | Creates an OpenBM Message (simplified version)
message :: IbusDevice -> IbusDevice -> [Word8] -> OpenBMMessage
message src dst payload = 
   mkOpenBMMessage (fromIntegral $ fromEnum src) (fromIntegral $ fromEnum dst)
                   0x00 (BS.pack payload)

-- | Is an OpenBM message from a given device?
isFromDevice :: IbusDevice -> OpenBMMessage -> Bool
isFromDevice src msg = openBMSrc msg == (fromIntegral $ fromEnum src)

-- | Is an OpenBM message to a given device?
isToDevice :: IbusDevice -> OpenBMMessage -> Bool
isToDevice dst msg = openBMDst msg == (fromIntegral $ fromEnum dst)
