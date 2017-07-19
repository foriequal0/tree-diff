{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Lib
    ( someFunc
    ) where

import Text.XML.HXT.Core
import Network.HTTP
import Network.URI
import Pipes
import qualified Pipes.Prelude as P
import Data.SuffixTree

urls :: [String]
urls = fmap template [0..]
    where
      template page = "http://www.todayhumor.co.kr/board/list.php?kind=total&table=total&page=" ++ show page

get :: String -> IO String
get rawuri = do
    uri <- case parseURI rawuri of
             Nothing -> ioError . userError $ "Invalid URL"
             Just uri -> pure $ uri

    eresp <- simpleHTTP (Request uri GET [] "")
    case eresp of
      Left _ -> ioError . userError $ "Failed to get" ++ show uri
      Right res -> do
          case (rspCode res, findHeader HdrLocation res) of
            ((3, 0, _), Nothing) -> ioError . userError $ "Redirect location not found"
            ((3, 0, _), Just loc) -> get loc
            _ -> return $ rspBody res

parseXML doc = readString [ withValidate no
                          , withRemoveWS yes
                          , withParseHTML yes
                          ] doc

atTag tag = deep (isElem >>> hasName tag)

getPages :: Producer XmlTrees IO ()
getPages = do
    for (each urls) $ \url -> do
        doc <- lift $ get url
        let xml = parseXML doc
        result <- lift $ runX (xml >>> atTag "body")
        yield result

repeatedSubelement :: [a] -> (Int, Int)
repeatedSubelement list =
    let st = construct

someFunc :: IO ()
someFunc = do
    runEffect $ do
        for (getPages >-> P.take 3) $ \page -> do
            lift $ print page
