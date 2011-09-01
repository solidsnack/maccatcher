
{-|
A MAC address datatype, representing the six bytes of a MAC address, also
known as an OID, IAB or \"...Vendor Address, Vendor ID, NIC Address, Ethernet
Address and others.\", see <http://standards.ieee.org/faqs/OUI.html#q4>
-}

module Data.MAC where

import Numeric
import Data.Word
import Data.List
import Data.Bits
import Foreign.Ptr
import Foreign.Storable
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Text.Printf
import Text.Read
import qualified Text.Read.Lex as Rex
import qualified Text.ParserCombinators.ReadPrec as Rex
import qualified Text.ParserCombinators.ReadP as Rex


data MAC                     =  MAC
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
 deriving (Eq, Ord, Bounded)
instance Show MAC where
  show (MAC a b c d e f)
    = printf "%02x:%02x:%02x:%02x:%02x:%02x" a b c d e f
instance Read MAC where
  readPrec                   =  Rex.lift $ do
    a                       <-  Rex.readHexP
    [b, c, d, e, f]         <-  Rex.many $ Rex.char ':' >> Rex.readHexP
    return $ MAC a b c d e f
instance Binary MAC where
 -- Thanks to aslatter@gmail.com for this instance.
  put (MAC a b c d e f)      =  do
    putWord8 a
    putWord8 b
    putWord8 c
    putWord8 d
    putWord8 e
    putWord8 f
  get                        =  do
    a                       <-  getWord8
    b                       <-  getWord8
    c                       <-  getWord8
    d                       <-  getWord8
    e                       <-  getWord8
    f                       <-  getWord8
    return $ MAC a b c d e f
instance Storable MAC where
 -- Thanks to aslatter@gmail.com for this instance.
  sizeOf _                   =  6
  alignment _                =  1
  peek p                     =  do
    a                       <-  peek $ castPtr p
    b                       <-  peekByteOff p 1
    c                       <-  peekByteOff p 2
    d                       <-  peekByteOff p 3
    e                       <-  peekByteOff p 4
    f                       <-  peekByteOff p 5
    return $ MAC a b c d e f
  poke p (MAC a b c d e f)   =  do
    poke (castPtr p) a
    pokeByteOff p 1 b
    pokeByteOff p 2 c
    pokeByteOff p 3 d
    pokeByteOff p 4 e
    pokeByteOff p 5 f


