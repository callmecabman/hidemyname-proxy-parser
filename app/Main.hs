module Main where

import Proxy
import Data.List.Split(splitWhen)
import Control.Lens
import Network.Wreq
import Text.StringLike
import Text.HTML.TagSoup
import Text.ParserCombinators.Parsec

junkTagsRemoved :: [Tag String] -> [Tag String]
junkTagsRemoved = (takeWhile (~/= "</tbody>")) . tail . (dropWhile (~/= "<tbody>"))

onlyProxyTagsList :: [Tag String] -> [[Tag String]]
onlyProxyTagsList = filter (not . null). fmap (dropWhile (~== "<tr")) . splitWhen (~== "</tr>")

main :: IO ()
main = do
  res <-get "http://hidemy.name/ru/proxy-list/"
  let rawTags = parseTags (castString (res ^. responseBody) :: String)
  let cookedTagList = onlyProxyTagsList $ junkTagsRemoved rawTags
--  print $ fmap (parse parseIP "whatever") cookedTagList
  return ()

parseIP (_:t:ts) = (fmap readIP $ maybeTagText t, ts)
parseIP ts = (Nothing, ts)

parsePort (_:t:ts) = (fmap readPort $ maybeTagText t, ts)
parsePort ts = (Nothing, ts)
