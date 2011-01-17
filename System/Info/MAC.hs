{-# OPTIONS_GHC -fno-cse #-}


{-| Obtain a MAC address for the host system, on *NIX and Windows.
 -}


module System.Info.MAC
  ( refresh
  , mac
  , macs
  ) where

import Data.MAC
import System.Info.MAC.Fetch

import Data.IORef
import System.IO
import System.IO.Unsafe
import Data.Maybe
import Control.Applicative


{-| Fetch MAC address, using a cached value if it is available.
 -}
mac                         ::  IO (Maybe MAC)
mac                          =  listToMaybe <$> macs


{-| Fetch MAC addresses, using a cached value if it is available.
 -}
macs                        ::  IO [MAC]
macs                         =  map snd <$> nics


{-| Fetch a name-MAC pair, using a cached value if it is available.
 -}
nic                         ::  IO (Maybe (String, MAC))
nic                          =  listToMaybe <$> nics


{-| Fetch name-MAC pairs, using a cached value if it is available.
 -}
nics                        ::  IO [(String, MAC)]
nics                         =  do
  val                       <-  readIORef fetched
  case val of [ ]           ->  refresh
              _:_           ->  return val


{-| Explicitly re-run the MAC reading operation.
 -}
refresh                     ::  IO [(String, MAC)]
refresh                      =  do
  res                       <-  fetchNICs
  writeIORef fetched res
  return res


{-# NOINLINE fetched #-}
fetched                      =  unsafePerformIO $ newIORef []


