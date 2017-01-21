{-# LANGUAGE RecordWildCards #-}

module Main where

import Proxy
import Data.Maybe
import Data.List.Split(splitWhen)
import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM
import System.Random
import Network.Wreq(getWith,defaults,param,responseBody)
import qualified Data.Text as T
import Text.StringLike
import Text.HTML.TagSoup

isolateTable :: [Tag String] -> [Tag String]
isolateTable = (takeWhile (~/= "</tbody>")) . tail . (dropWhile (~/= "<tbody>"))

processTable :: [Tag String] -> [[String]]
processTable = fmap (fmap fromTagText . (filter isTagText)) . filter (not . null) . splitWhen (~== "</tr>")

parseProxyPage :: Integer -> IO [Proxy]
parseProxyPage s = do
  let opts = defaults & param (T.pack "start") .~ [T.pack $ show s]
  res <- getWith opts $ "http://hidemy.name/en/proxy-list/"
  let rawTags = parseTags (castString (res ^. responseBody) :: String)
  let cookedTags = processTable $ isolateTable rawTags
  return $ mapMaybe readProxy cookedTags

getterThread :: TChan Proxy -> Integer -> IO ()
getterThread ch s = do
  d <- randomRIO (halfSecond, halfSecond * 2)
  threadDelay d
  proxies <- parseProxyPage s
  sequence_ $ fmap (atomically . (writeTChan ch)) proxies

printerThread :: TChan Proxy -> IO ()
printerThread ch = do
  p <- atomically $ readTChan ch
  putStrLn $ prettyProxy p
  printerThread ch

main :: IO ()
main = do
  proxyChan <- atomically newTChan
  let threads = fmap (forkIO . (getterThread proxyChan)) [0, 64 ..]
  sequence_ $ take 20 threads
  forkIO $ printerThread proxyChan
  threadDelay $ 8 * halfSecond

halfSecond =  500000

prettyProxy :: Proxy -> String
prettyProxy Proxy{..} = foldr (++) "" [show pIP, ":", show pPort, " ", show pType]
