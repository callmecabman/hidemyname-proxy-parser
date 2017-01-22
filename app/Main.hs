{-# LANGUAGE RecordWildCards #-}

module Main where

import Proxy
import Data.Maybe
import Data.List.Split(splitWhen)
import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
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

-- 've had to introduce the delay because their server rejects
-- even a modest amount of simultaneous connections
getterThread :: TQueue [Proxy] -> Integer -> IO ()
getterThread ch s = do
  d <- randomRIO (0, oneSecond)
  threadDelay d
  proxies <- parseProxyPage s
  atomically $ writeTQueue ch proxies
    where
      oneSecond = 1000000

printerThread :: TQueue [Proxy] -> IO ()
printerThread ch = do
  ps <- atomically $ readTQueue ch
  sequence_ $ fmap (putStrLn . prettyProxy) ps
  printerThread ch

main :: IO ()
main = do
  proxyChan <- atomically newTQueue
  let threads = fmap (getterThread proxyChan) [0, 64 ..]
  race_ (sequence_ $ take 10 threads) (printerThread proxyChan)

prettyProxy :: Proxy -> String
prettyProxy Proxy{..} = foldr (++) "" [show pIP, ":", show pPort, " ", show pType]
