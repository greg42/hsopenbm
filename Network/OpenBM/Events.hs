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
                              isFromDevice, messageToEvent, isToDevice,
                              eventToMessage) where

import           Network.OpenBM
import           Data.ByteString (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Maybe
import           Data.Tuple
import           Data.Word

-- | Known IBUS devices 
data IbusDevice = CDPlayer | BordMonitor | SteeringWheel | Radio | Broadcast
                | IKE | NavigationVideo | UnknownDevice
                deriving(Eq)

instance Enum IbusDevice where 
    fromEnum = fromJust . flip lookup table 
    toEnum   = fromMaybe UnknownDevice . flip lookup (map swap table)

table = [  (CDPlayer, 0x18), (BordMonitor, 0xF0), (SteeringWheel, 0x50)
         , (Radio, 0x68), (IKE, 0x80), (Broadcast, 0xFF)
         , (NavigationVideo, 0x3B)]

-- | An event on the IBUS.
data IbusEvent = SteeringWheelUpButtonReleased
               | SteeringWheelDownButtonReleased
               | NavigationKnobTurnedLeft Int -- ^ Rotation by n positions
               | NavigationKnobTurnedRight Int -- ^ Rotation by n positions
               | NavigationKnobReleased
               | ModeButtonPressed
               | ModeButtonReleased
               | LeftButtonReleased
               | RightButtonReleased
               | ReverseTapeButtonReleased
               | ReverseTapeButtonPressed Bool -- ^ Reverse button pressed long (True) or short (False)
               | CdPing -- ^ Ping message from the radio to the CD player
               | CdGetState -- ^ Radio requests current state from CD player
               | CdPlay -- ^ Radio tells CD player to start playing
               | CdPause -- ^ Radio tells CD player to pause playing
               | CdStop -- ^ Radio tells CD player to stop playing
               | CdNextTrack -- ^ Radio tells CD player to play the next track
               | CdPrevTrack -- ^ Radio tells CD player to play the previous track
               | IgnitionOn -- ^ IKE signals that the ignition is active
               | IgnitionOff -- ^ IKE signals that the ignition is turned off
               | WriteIndexMK2 Int String -- ^ Write to the given index field on
                                          -- the screen (see
                                          -- http://web.comhem.se/mulle2/IBUSInsideDRAFTREV5.pdf). Only
                                          -- observed on MK2 systems.
               | WriteIndexMK34 Int String -- ^ Write to the fiven index field on the screen.
               | WriteTitle Int String -- ^ Write to the given title field on the screen.
               | UnknownEvent OpenBMMessage
               deriving (Eq, Show)

-- | Checks the source address and the payload of an OpenBM message
srcDataEquals :: IbusDevice -> [Word8] -> OpenBMMessage -> Bool
srcDataEquals src payload msg =
   openBMSrc msg == (fromIntegral $ fromEnum src) && openBMData msg == BS.pack payload

-- | Checks the source and destination address and the beginning of the payload
-- of an OpenBM message
srcDstDataStartsWith :: IbusDevice -> IbusDevice -> Int -> [Word8] -> OpenBMMessage -> Bool
srcDstDataStartsWith src dst minLen payload msg =
   openBMSrc msg == (fromIntegral $ fromEnum src) 
      && openBMDst msg == (fromIntegral $ fromEnum dst)
      && (BS.length $ openBMData msg) >= minLen 
      && (BS.take (length payload) $ openBMData msg) == BS.pack payload

-- | Simple IBUS messages that are fully known in advance
simpleMessages :: [((IbusDevice, IbusDevice, [Word8]), IbusEvent)]
simpleMessages = [
     ((SteeringWheel, Radio   , [0x3B, 0x21]      ), SteeringWheelUpButtonReleased   )
   , ((SteeringWheel, Radio   , [0x3B, 0x28]      ), SteeringWheelDownButtonReleased )
   , ((BordMonitor  , Radio   , [0x48, 0x94]      ), ReverseTapeButtonReleased       )
   , ((BordMonitor  , Radio   , [0x48, 0x23]      ), ModeButtonPressed               )
   , ((BordMonitor  , Radio   , [0x48, 0xA3]      ), ModeButtonReleased              )
   , ((BordMonitor  , NavigationVideo   , [0x48, 0x85]      ), NavigationKnobReleased          )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x01]      ), NavigationKnobTurnedLeft 1      )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x02]      ), NavigationKnobTurnedLeft 2      )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x03]      ), NavigationKnobTurnedLeft 3      )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x04]      ), NavigationKnobTurnedLeft 4      )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x05]      ), NavigationKnobTurnedLeft 5      )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x06]      ), NavigationKnobTurnedLeft 6      )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x07]      ), NavigationKnobTurnedLeft 7      )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x08]      ), NavigationKnobTurnedLeft 8      )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x09]      ), NavigationKnobTurnedLeft 9      )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x81]      ), NavigationKnobTurnedRight 1     )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x82]      ), NavigationKnobTurnedRight 2     )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x83]      ), NavigationKnobTurnedRight 3     )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x84]      ), NavigationKnobTurnedRight 4     )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x85]      ), NavigationKnobTurnedRight 5     )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x86]      ), NavigationKnobTurnedRight 6     )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x87]      ), NavigationKnobTurnedRight 7     )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x88]      ), NavigationKnobTurnedRight 8     )
   , ((BordMonitor  , NavigationVideo   , [0x49, 0x89]      ), NavigationKnobTurnedRight 9     )
   , ((BordMonitor  , Radio   , [0x48, 0x90]      ), LeftButtonReleased              )
   , ((BordMonitor  , Radio   , [0x48, 0x80]      ), RightButtonReleased             )
   , ((BordMonitor  , Radio   , [0x48, 0x54]      ), ReverseTapeButtonPressed True   )
   , ((BordMonitor  , Radio   , [0x48, 0x14]      ), ReverseTapeButtonPressed False  )
   , ((Radio        , CDPlayer, [0x01]            ), CdPing                          )
   , ((Radio        , CDPlayer, [0x38, 0x00, 0x00]), CdGetState                      )
   , ((Radio        , CDPlayer, [0x38, 0x03, 0x00]), CdPlay                          )
   , ((Radio        , CDPlayer, [0x38, 0x02, 0x00]), CdPause                         )
   , ((Radio        , CDPlayer, [0x38, 0x01, 0x00]), CdStop                          )
   , ((Radio        , CDPlayer, [0x38, 0x0a, 0x00]), CdNextTrack                     )
   , ((Radio        , CDPlayer, [0x38, 0x0a, 0x01]), CdPrevTrack                     )
  ]

-- | Generates an OpenBM message from an IbusEvent.
eventToMessage :: IbusEvent -> OpenBMMessage
eventToMessage event = 
   case lookup event (map swap simpleMessages) of
      Just (src, dst, payload) -> mkOpenBMMessage (fromIntegral $ fromEnum src) (fromIntegral $ fromEnum dst) 0 (BS.pack payload)
      Nothing -> case event of
                   UnknownEvent msg -> msg
                   _                -> error "Cannot serialize event to message."

-- | Parses an OpenBM message into an IbusEvent.
messageToEvent :: OpenBMMessage -> IbusEvent
messageToEvent msg =
   case lookup (toEnum $ fromIntegral $ openBMSrc msg, toEnum $ fromIntegral $ openBMDst msg, BS.unpack $ openBMData msg) simpleMessages of
      Just event -> event
      Nothing    | srcDataEquals IKE [0x11, 0x00] msg -> IgnitionOff
                 | srcDataEquals IKE [0x11, 0x01] msg -> IgnitionOn
                 | srcDstDataStartsWith Radio NavigationVideo 3 [0x23, 0x62, 0x10] msg -> WriteTitle 0 (C8.unpack $ BS.drop 3 $ openBMData msg)
                 | srcDstDataStartsWith Radio NavigationVideo 4 [0xA5, 0x62, 0x01] msg -> WriteTitle (toIndex $ (flip BS.index) 3 $ openBMData msg) (C8.unpack $ BS.drop 4 $ openBMData msg)
                 | srcDstDataStartsWith Radio NavigationVideo 4 [0x21, 0x60, 0x00] msg -> WriteIndexMK34 (toIndex $ (flip BS.index) 3 $ openBMData msg) (C8.unpack $ BS.drop 4 $ openBMData msg)
                 | srcDstDataStartsWith Radio NavigationVideo 4 [0xA5, 0x62, 0x00] msg -> WriteIndexMK34 (toIndex $ (flip BS.index) 3 $ openBMData msg) (C8.unpack $ BS.drop 4 $ openBMData msg)
                | otherwise -> UnknownEvent msg
   where toIndex 7 = 7
         toIndex n = fromIntegral $ n - 0x40

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
