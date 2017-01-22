module Proxy where

import Data.Word
import Data.Char
import Text.ParserCombinators.Parsec

data IP = IP Word8 Word8 Word8 Word8

instance Show IP where
  show (IP w x y z) = foldr (++) "" [show w, ".", show x, ".", show y, ".", show z]

ipFromIntegers:: [Integer] -> Maybe IP
ipFromIntegers (w:x:y:z:[]) = Just $ IP w' x' y' z'
  where
    [w',x',y',z'] = fmap fromInteger [w,x,y,z]
ipFromIntegers _ = Nothing

parseIPIntegers :: Parser [Integer]
parseIPIntegers = do
  w <- many1 digit
  char '.'
  x <- many1 digit
  char '.'
  y <- many1 digit
  char '.'
  z <- many1 digit
  return $ fmap read [w, x, y, z]

readIP :: String -> Maybe IP
readIP str = do
  case parse parseIPIntegers "IP" str of
    Left _ -> Nothing
    Right ws ->
      if any (> fromIntegral (maxBound :: Word8)) ws
              then Nothing
              else ipFromIntegers ws

readPort :: String -> Maybe Word16
readPort str = do
  case parse (many1 digit) "Port" str of
    Left _ -> Nothing
    Right w ->
      let w' = read w in
        if w' < 1 || w' > (fromIntegral (maxBound :: Word16))
          then Nothing
          else return $ fromInteger w'

data ProxyType = HTTP
               | HTTPS
               | HTTPBoth
               | SOCKS4
               | SOCKS5
               | SOCKSBoth
               deriving Show

readProxyType :: String -> Maybe ProxyType
readProxyType str = do
  case str of
    "HTTP"           -> return HTTP
    "HTTPS"          -> return HTTPS
    "HTTP, HTTPS"    -> return HTTPBoth
    "SOCKS4"         -> return SOCKS4
    "SOCKS5"         -> return SOCKS5
    "SOCKS4, SOCKS5" -> return SOCKSBoth
    otherwise        -> Nothing

data AnonymityLevel = AnonNone
                    | AnonLow
                    | AnonMedium
                    | AnonHigh
                    deriving Show

readAnonymityLevel:: String -> Maybe AnonymityLevel
readAnonymityLevel str = do
  case str of
    "No"      -> return AnonNone
    "Low"     -> return AnonLow
    "Medium"  -> return AnonMedium
    "High"    -> return AnonHigh
    otherwise -> Nothing

data Proxy = Proxy
  { pIP :: IP
  , pPort :: Word16
  , pLocation :: String
  , pLatency :: Word16
  , pType :: ProxyType
  , pAnon :: AnonymityLevel
  }
  deriving Show

readProxy :: [String] -> Maybe Proxy
readProxy (ip:port:loc:_:lat:ptype:anon:_) = do
  ip'    <- readIP ip
  port'  <- readPort port
  ptype' <- readProxyType ptype
  anon'  <- readAnonymityLevel anon
  return Proxy { pIP = ip'
          , pPort = port'
          , pLocation = (init . drop 2) loc
          , pLatency = read $ filter isNumber lat
          , pType = ptype'
          , pAnon = anon'
          }
readProxy _ = Nothing
