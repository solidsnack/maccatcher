#!/usr/bin/env runhaskell
 -- Catch your MAC Address!

import System.Info.MAC

main                         =  do
  mac                       <-  mac
  case mac of
    Just mac                ->  print mac
    Nothing                 ->  putStrLn "Failed to obtain MAC address."

