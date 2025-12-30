module Data.Bencode.Types (BencodeValue (..)) where

data BencodeValue
  = BencodeString String
  | BencodeInt Int
  | BencodeList [BencodeValue]
  | BencodeDict [(String, BencodeValue)]
  deriving (Eq, Show)
