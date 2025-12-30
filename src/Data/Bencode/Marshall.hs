module Data.Bencode.Marshall (marshallString, marshallInt, marshall) where

import Data.Bencode.Types (BencodeValue (..))
import Data.List (sortOn)

marshall :: BencodeValue -> Maybe String
marshall (BencodeString s) = Just $ marshallString s
marshall (BencodeInt i) = Just $ marshallInt i
marshall (BencodeDict d) = marshallDict d
marshall (BencodeList l) = marshallList l

marshallString :: String -> String
marshallString s = show (length s) ++ (':' : s)

marshallInt :: Int -> String
marshallInt i = "i" ++ show i ++ "e"

marshallList :: [BencodeValue] -> Maybe String
marshallList l = do
  marshalledElems <- traverse marshall l
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
      marshalledV <- marshall v
      return (marshalledK ++ marshalledV)
