{-# LANGUAGE TupleSections
  #-}
{-| System specific routines for determing the MAC address and macros to help
    sort things out at compile time.
 -}


module System.Info.MAC.Fetch where

import Data.MAC

import Control.Monad
import Control.Applicative ((<$>))
import Data.List
import Data.Maybe
import System.Process
import System.Info
import System.IO
import Text.ParserCombinators.Parsec


{-| Obtain a list containing the name and MAC of all NICs.
 -}
fetchNICs                   ::  IO [(String, MAC)]
fetchNICs                    =  parser <$> i_config


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
       | otherwise           =  parse' "ifconfig" ifconfig . ("\n\n" ++)


{-| Parses the output of Linux or BSD @ifconfig@.
 -}
ifconfig                    ::  Parser [(String, MAC)]
ifconfig                     =  parseNICs parseNIC_ifconfig


{-| Parses the output of Windows @ipconfig@.
 -}
ipconfig                    ::  Parser [(String, MAC)]
ipconfig                     =  parseNICs parseNIC_ipconfig


parseNIC_ifconfig           ::  Parser (Maybe (String, MAC))
parseNIC_ifconfig            =  do
  name                      <-  many1 alphaNum
  skipManyTill (satisfy (/= '\n')) markers
  char ' '
  ((name,) <$>) <$> parseMAC ':'
 where
  markers = choice $ map (try . string) [ "ether", "HWaddr" ]


parseNIC_ipconfig           ::  Parser (Maybe (String, MAC))
parseNIC_ipconfig            =  do
  name                      <-  do string "Ethernet adapter "
                                   manyTill (satisfy (/= '\n')) (char ':')
  (skipManyAnyTill . choice) [ try (nl >> nl) >> unexpected "\\r\\n\\r\\n"
                             , (try . string) "Physical Address" ]
  manyTill (satisfy (/= '\n')) (char ':')
  char ' '
  ((name,) <$>) <$> parseMAC '-'


parseNICs :: Parser (Maybe (String, MAC)) -> Parser [(String, MAC)]
parseNICs p                  =  catMaybes <$> parseNICs'
 where
  parseNICs'                 =  (skipManyAnyTill . choice)
                                          [ eof >> return []
                                          , do try (nl >> nl)
                                               nic <- p
                                               (nic:) <$> parseNICs' ]


parseMAC sepChar = maybeMAC . intercalate ":" <$> sepHex (char sepChar)


parse'                      ::  String -> Parser [t] -> String -> [t]
parse' source parser         =  either (const []) id . parse parser source


maybeMAC                    ::  String -> Maybe MAC
maybeMAC s =
  case reads s of
    [(mac, _)]              ->  Just mac
    _                       ->  Nothing


sepHex                       =  sepBy (sequence [hexDigit, hexDigit])


manyAnyTill                 ::  Parser Char -> Parser String
manyAnyTill                  =  manyTill anyChar


skipManyTill                ::  Parser a -> Parser b -> Parser b
skipManyTill p end           =  choice [try end, p >> skipManyTill p end]


skipManyAnyTill             ::  Parser a -> Parser a
skipManyAnyTill              =  skipManyTill anyChar


nl                           =  many (char '\r') >> char '\n'


