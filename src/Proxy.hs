module Proxy where

import Data.Word
import Data.ByteString(ByteString)
import Text.ParserCombinators.Parsec

data IP = IP Word8 Word8 Word8 Word8
  deriving Show

ipFromIntegers:: [Integer] ->Maybe IP
ipFromIntegers (w:x:y:z:[]) = Just $ IP w' x' y' z'
  where
    [w',x',y',z'] = fmap fromInteger [w,x,y,z]
ipFromIntegers _ = Nothing

parseIPIntegers :: Parser [Integer]
parseIPIntegers = do
  w <-many1 digit
  char '.'
  x <-many1 digit
  char '.'
  y <-many1 digit
  char '.'
  z <-many1 digit
  return $ fmap read [w, x, y, z]

readIP :: String ->Maybe IP
readIP str = do
  let t = parse parseIPIntegers "IP" str
  case t of
    Left _ ->Nothing
    Right ws ->
      if any (>=256) ws
              then Nothing
              else ipFromIntegers ws

newtype Port = Port Word16
  deriving Show

readPort :: String ->Maybe Port
readPort str = do
  let t = parse (many1 digit) "Port" str
  case t of
    Left _ ->Nothing
    Right w ->
      let w' = read w in
        if w' <= 1 || w' >= 65536
          then Nothing
          else return $ Port (fromInteger w')

data ProxyType = HTTP
               | HTTPS
               | HTTPBoth
               | SOCKS4
               | SOCKS5
               deriving Show

data AnonymityLevel = AnonNone
                    | AnonLow
                    | AnonMedium
                    | AnonHigh
                    deriving Show

type CountryName = ByteString

type Latency = Word16

data Proxy = Proxy IP Port CountryName Latency ProxyType AnonymityLevel
  deriving Show
