{-| System specific routines for determing the MAC address and macros to help
    sort things out at compile time.
 -}


module System.Info.MAC.Fetch where

import Data.MAC

import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import System.Process
import System.Info
import System.IO
import Text.ParserCombinators.Parsec


{-| Obtain a list of all available MACs.
 -}
fetchMACs                   ::  IO [MAC]
fetchMACs                    =  parser <$> i_config


{-| Run @ifconfig@ or @ipconfig@, as appropriate, capturing its output.
 -}
i_config                    ::  IO String
i_config                     =  do
  (_, o, _, h)              <-  runInteractiveCommand cmd
  outputs                   <-  hGetContents o
  seq (length outputs) (return ())
  waitForProcess h
  return outputs
 where
  cmd | os == "mingw32"      =  "ipconfig /all"
      | otherwise            =  "ifconfig"


parser | os == "mingw32"     =  parse' "ipconfig" ipconfig
       | otherwise           =  parse' "ifconfig" ifconfig


{-| Parses the output of Windows @ipconfig@.
 -}
ipconfig                    ::  Parser [MAC]
ipconfig                     =  parseMACs ((try . string) "Physical Address")
                                          (manyAnyTill (char ':') >> spaces)
                                          '-'


{-| Parses the output of Linux or BSD @ifconfig@.
 -}
ifconfig                    ::  Parser [MAC]
ifconfig                     =  parseMACs markers spaces ':'
 where
  markers = choice $ map (try . string) [ "ether", "HWaddr" ]


parseMAC :: Parser t -> Parser t' -> Char -> Parser (Maybe MAC)
parseMAC preamble fill c     =  do
  preamble
  fill
  maybeMAC . intercalate ":" <$> sepHex (char c)


parseMACs                   ::  Parser t -> Parser t' -> Char -> Parser [MAC]
parseMACs preamble fill c    =  catMaybes <$> parseMACs'
 where
  parseMACs' = 
    (skipManyTill anyChar . choice) [ eof >> return []
                                    , do m <- parseMAC preamble fill c
                                         (m:) <$> parseMACs' ]


parse'                      ::  String -> Parser [t] -> String -> [t]
parse' source parser         =  either (const []) id . parse parser source


maybeMAC                    ::  String -> Maybe MAC
maybeMAC s =
  case reads s of
    [(mac, _)]              ->  Just mac
    _                       ->  Nothing


sepHex                       =  sepBy (sequence [hexDigit, hexDigit])


manyAnyTill                  =  manyTill anyChar


skipManyTill p end           =  choice [try end, p >> skipManyTill p end]

