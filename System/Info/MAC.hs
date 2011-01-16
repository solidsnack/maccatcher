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


{-| Explicitly re-run the MAC catching operation.
 -}
refresh                     ::  IO [MAC]
refresh                      =  do
  res                       <-  fetchMACs
  writeIORef fetched res
  return res


{-| Fetch MAC address, using a cached value if it is available.
 -}
mac                         ::  IO (Maybe MAC)
mac                          =  listToMaybe <$> macs


{-| Fetch MAC addresses, using a cached value if it is available.
 -}
macs                        ::  IO [MAC]
macs                         =  do
  val                       <-  readIORef fetched
  case val of [ ]           ->  refresh
              _:_           ->  return val


{-# NOINLINE fetched #-}
fetched                      =  unsafePerformIO $ newIORef []


