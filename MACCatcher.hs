#!/usr/bin/env runhaskell

import System.Info.MAC

main                         =  do
  macs                      <-  macs
  case macs of _:_          ->  mapM_ print macs
               [ ]          ->  putStrLn "Failed to obtain MAC address."

