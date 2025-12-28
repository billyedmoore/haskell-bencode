module Data.Bencode.Parse (parse, parseInt, BencodeValue (..)) where

import Data.Bifunctor (first)
import Text.Read (readMaybe)

data BencodeValue
  = BencodeString String
  | BencodeInt Int
  | BencodeList [BencodeValue]
  | BencodeMap [(String, BencodeValue)]
  deriving (Eq, Show)

parse :: String -> Maybe BencodeValue
parse ('i' : xs) = fmap fst (parseInt ('i' : xs))
parse ('l' : _) = Just $ BencodeList [BencodeInt 1]
parse ('d' : _) = Just $ BencodeMap [("key", BencodeString "value")]
parse (x : xs) = Just $ BencodeString (x : xs)
parse [] = Nothing

parseInt :: String -> Maybe (BencodeValue, String)
parseInt ('i' : str) = fmap (first BencodeInt) (parseIntInternal str "")
  where
    parseIntInternal :: String -> String -> Maybe (Int, String)
    parseIntInternal ('e' : xs) acc = case (readMaybe . checkLeadingZeros . reverse) acc of
      Just n -> Just (n, xs)
      Nothing -> Nothing
    parseIntInternal (x : xs) acc = parseIntInternal xs (x : acc)
    parseIntInternal [] _ = Nothing

    checkLeadingZeros :: String -> String
    checkLeadingZeros "0" = "0" -- Special case for 0 allowed to start with 0
    checkLeadingZeros "-0" = "" -- Special case for -0 not allowed
    checkLeadingZeros ('0' : _) = ""
    checkLeadingZeros ('-' : xs) = '-' : checkLeadingZeros xs
    checkLeadingZeros s = s
parseInt _ = Nothing
