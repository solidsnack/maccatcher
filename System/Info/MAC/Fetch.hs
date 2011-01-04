

{-| System specific routines for determing the MAC address and macros to help
 -  sort things out at compile time.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}


module System.Info.MAC.Fetch where

import Data.MAC

import Control.Monad
import Data.List
import System.Process
import System.Info
import System.IO
import Text.ParserCombinators.Parsec


{-| Obtain the appropriate hardware MAC fetcher for the host operating system.
 -  This could be inlined, but in practice it is run only once.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
fetch =
    case os of              --  This feels like it should be in IO.
      "mingw32"             ->  win32
      _                     ->  nixen


{-| Obtain the hardware address on @*NIX@ of any kind, using a command line
 -  utility.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
nixen                       ::  IO (Maybe MAC)
nixen                        =  do
  (_, o, _, h)              <-  runInteractiveCommand "ifconfig"
  waitForProcess h
  outputs                   <-  hGetContents o
  return $ join $ ifconfig outputs 


 -- TODO Test this thing.
{-| Obtain the hardware address on Windows, using a command line utility. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
win32                       ::  IO (Maybe MAC)
win32                        =  do
  (_, o, _, h)              <-  runInteractiveCommand "ipconfig /all"
  outputs                   <-  hGetContents o
  seq (length outputs) (return ())
  waitForProcess h
  return $ join $ ipconfig outputs 
 where
   -- Maybe we don't need this?
  locations                  =  map (++ "\\ipconfig")
    [ "c:\\windows\\system32"
    , "c:\\winnt\\system32"
    ]


{-| Parses the output of Windows @ipconfig@, yielding a Maybe MAC on
 -  succesful parse.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
ipconfig                     =  parse' "ipconfig" $ do
  manyTill anyChar $ try $ string "Physical Address"
  manyTill anyChar $ char ':'
  spaces
  hexen                     <-  sepHex '-'
  return . maybeMAC . intercalate ":" $ hexen


{-| Parses the output of Linux or BSD @ifconfig@, yielding a Maybe MAC on
 -  succesful parse.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
ifconfig                     =  parse' "ifconfig" $ do
  manyTill anyChar markers
  spaces
  hexen                     <-  sepHex ':'
  return . maybeMAC . intercalate ":" $ hexen
 where
  markers = choice $ map (try . string) [ "ether", "HWaddr" ]




parse' source parser         =  eitherToMaybe . parse parser source
 where
  eitherToMaybe (Left _)     =  Nothing
  eitherToMaybe (Right r)    =  Just r 


maybeMAC                    ::  String -> Maybe MAC
maybeMAC s =
  case reads s of
    [(mac, _)]              ->  Just mac
    _                       ->  Nothing


sepHex                       =  sepBy (sequence [hexDigit, hexDigit]) . char


