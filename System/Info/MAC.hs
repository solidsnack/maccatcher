{-# OPTIONS_GHC -fno-cse #-}


{-| Obtain a MAC address for the host system, on *NIX and Windows.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}


module System.Info.MAC
  ( new
  , mac
  ) where

import Data.MAC
import System.Info.MAC.Fetch

import Data.IORef
import System.IO
import System.IO.Unsafe


{-| Explicitly re-run the MAC catching operation.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
new                         ::  IO (Maybe MAC)
new                          =  fetch


{-| Return a host MAC address, using a cached value if it is available.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
mac                         ::  IO (Maybe MAC)
mac                          =  do
  val                       <-  readIORef fetched
  case val of
    Nothing                 ->  do
      res                   <-  new
      writeIORef fetched res
      return res
    _                       ->  return val


{-# NOINLINE fetched #-}
fetched                      =  unsafePerformIO $ newIORef Nothing


