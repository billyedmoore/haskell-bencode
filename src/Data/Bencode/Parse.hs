module Data.Bencode.Parse (parseInt, parseStr, parseList, parseDict, BencodeValue (..)) where

import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Char (isNumber)
import Text.Read (readMaybe)

data BencodeValue
  = BencodeString String
  | BencodeInt Int
  | BencodeList [BencodeValue]
  | BencodeDict [(String, BencodeValue)]
  deriving (Eq, Show)

-- Return "" if s has leading zero otherwise return s
checkLeadingZeros :: String -> String
checkLeadingZeros "0" = "0" -- Special case for 0 allowed to start with 0
checkLeadingZeros "-0" = "" -- Special case for -0 not allowed
checkLeadingZeros ('0' : _) = ""
checkLeadingZeros ('-' : xs) = '-' : checkLeadingZeros xs
checkLeadingZeros s = s

parseElem :: String -> Maybe (BencodeValue, String)
parseElem ('i' : xs) = parseInt ('i' : xs)
parseElem ('l' : xs) = parseList ('l' : xs)
parseElem ('d' : xs) = parseDict ('d' : xs)
parseElem (x : xs) = parseStr (x : xs)
parseElem [] = Nothing

parseList :: String -> Maybe (BencodeValue, String)
parseList ('l' : str) = parseListInternal str []
  where
    parseListInternal :: String -> [BencodeValue] -> Maybe (BencodeValue, String)
    parseListInternal ('e' : xs) acc = Just ((BencodeList . reverse) acc, xs)
    parseListInternal s acc = case parseElem s of
      Just (e, rest) -> parseListInternal rest (e : acc)
      Nothing -> Nothing
parseList _ = Nothing

parseDict :: String -> Maybe (BencodeValue, String)
parseDict ('d' : str) = parseDictInternal str []
  where
    parseDictInternal :: String -> [(String, BencodeValue)] -> Maybe (BencodeValue, String)
    parseDictInternal ('e' : remaining) acc =
      let dict = reverse acc
       in if areDictKeysSortedAndUnique dict
            then Just (BencodeDict dict, remaining)
            else Nothing
    parseDictInternal s acc = do
      (kv, rest) <- parseKeyVal s
      parseDictInternal rest (kv : acc)

    -- Check if Dict keys are sorted and unique
    areDictKeysSortedAndUnique :: [(String, BencodeValue)] -> Bool
    areDictKeysSortedAndUnique [] = True
    areDictKeysSortedAndUnique [_] = True
    areDictKeysSortedAndUnique (x : y : xs) =
      fst x < fst y
        && areDictKeysSortedAndUnique (y : xs)
parseDict _ = Nothing

parseKeyVal :: String -> Maybe ((String, BencodeValue), String)
parseKeyVal s = do
  (BencodeString key, restInclValue) <- parseStr s
  (val, rest) <- parseElem restInclValue
  return ((key, val), rest)

parseInt :: String -> Maybe (BencodeValue, String)
parseInt ('i' : str) = fmap (first BencodeInt) (parseIntInternal str "")
  where
    parseIntInternal :: String -> String -> Maybe (Int, String)
    parseIntInternal ('e' : xs) acc = do
      n <- (readMaybe . checkLeadingZeros . reverse) acc
      return (n, xs)
    parseIntInternal (x : xs) acc = parseIntInternal xs (x : acc)
    parseIntInternal [] _ = Nothing
parseInt _ = Nothing

parseStr :: String -> Maybe (BencodeValue, String)
parseStr s = do
  (n, body) <- parseLength s ""
  guard (length body >= n)
  let (str, remainder) = splitAt n body
  return (BencodeString str, remainder)
  where
    parseLength :: String -> String -> Maybe (Int, String)
    parseLength (x : xs) acc
      | x == ':' = do
          n <- (readMaybe . checkLeadingZeros . reverse) acc
          return (n, xs)
      | isNumber x = parseLength xs (x : acc)
      | otherwise = Nothing
    parseLength [] _ = Nothing
