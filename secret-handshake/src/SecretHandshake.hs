module SecretHandshake where

import Data.Bits
import Data.Monoid

data HandshakeAction
  = Wink
  | DoubleBlink
  | CloseYourEyes
  | Jump
  | Reverse
  deriving (Bounded, Enum, Eq, Ord, Show)

enumerate :: (Bounded a, Enum a) => [a]
enumerate = enumFromTo minBound maxBound

action :: HandshakeAction -> [String] -> [String]
action Wink = (++ ["wink"])
action DoubleBlink = (++ ["double blink"])
action CloseYourEyes = (++ ["close your eyes"])
action Jump = (++ ["jump"])
action Reverse = reverse

actions :: Int -> [HandshakeAction]
actions x = filter (testBit x . fromEnum) enumerate

handshake :: Int -> [String]
handshake = flip (appEndo . getDual . foldMap (Dual . Endo . action) . actions) []
