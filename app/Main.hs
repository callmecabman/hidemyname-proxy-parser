{-# LANGUAGE RecordWildCards #-}

module Main where

import Proxy
import Data.Maybe
import Data.List.Split(splitWhen)
import Control.Lens
import Network.Wreq(getWith,defaults,param,responseBody)
import qualified Data.Text as T
import Text.StringLike
import Text.HTML.TagSoup
import Text.ParserCombinators.Parsec

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

main :: IO ()
main = do
  firstPage <- parseProxyPage 0
  sequence_ $ fmap (putStrLn . prettyProxy) firstPage

prettyProxy :: Proxy -> String
prettyProxy Proxy{..} = foldr (++) "" [show pIP, ":", show pPort, " ", show pType]
