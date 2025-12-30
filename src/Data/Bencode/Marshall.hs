module Data.Bencode.Marshall (marshallString, marshallInt, marshallElem) where

import Data.Bencode.Types (BencodeValue (..))
import Data.List (sortOn)

marshallElem :: BencodeValue -> Maybe String
marshallElem (BencodeString s) = Just $ marshallString s
marshallElem (BencodeInt i) = Just $ marshallInt i
marshallElem (BencodeDict d) = marshallDict d
marshallElem (BencodeList l) = marshallList l

marshallString :: String -> String
marshallString s = show (length s) ++ (':' : s)

marshallInt :: Int -> String
marshallInt i = "i" ++ show i ++ "e"

marshallList :: [BencodeValue] -> Maybe String
marshallList l = do
  marshalledElems <- traverse marshallElem l
  return ("l" ++ concat marshalledElems ++ "e")

marshallDict :: [(String, BencodeValue)] -> Maybe String
marshallDict d = do
  let pairs = sortOn fst d
  marshallPairs <- traverse marshallPair pairs
  return $ "d" ++ concat marshallPairs ++ "e"
  where
    marshallPair :: (String, BencodeValue) -> Maybe String
    marshallPair (k, v) = do
      let marshalledK = marshallString k
      marshalledV <- marshallElem v
      return (marshalledK ++ marshalledV)
