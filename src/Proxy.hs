module Proxy where

import Data.Word
import Data.Char
import Data.List (intercalate)
import Text.ParserCombinators.Parsec
import Control.Monad (guard)

data IP = IP Word8 Word8 Word8 Word8

instance Show IP where
  show (IP w x y z) = intercalate "." . fmap show $ [w, x, y, z]

parseIPOctet :: Parser Word8
parseIPOctet = do
  w <- many1 digit
  let i = read w :: Int
  guard (i >= word8Min && i <= word8Max)
  return $ fromIntegral i
    where
      word8Min = fromIntegral (minBound :: Word8)
      word8Max = fromIntegral (maxBound :: Word8)

parseIP :: Parser IP
parseIP = do
  w <- parseIPOctet
  char '.'
  x <- parseIPOctet
  char '.'
  y <- parseIPOctet
  char '.'
  z <- parseIPOctet
  return $ IP w x y z

readIP :: String -> Maybe IP
readIP str = do
  case parse parseIP "IP" str of
    Left _  -> Nothing
    Right a -> Just a

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
