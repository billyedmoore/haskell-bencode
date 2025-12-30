module TestMarshall (marshallUnitTests) where

import Data.Bencode.Marshall (marshallElem)
import Data.Bencode.Types (BencodeValue (..))
import Test.Tasty
import Test.Tasty.HUnit

marshallUnitTests :: TestTree
marshallUnitTests =
  testGroup
    -- Tests for the element marshallers, largely AI generated
    "Bencode Marshall Tests"
    [ testGroup
        "Marshalling Tests"
        [ testGroup
            "Strings"
            [ testCase "Empty string" $
                marshallElem (BencodeString "") @?= Just "0:",
              testCase "Short string" $
                marshallElem (BencodeString "hi") @?= Just "2:hi",
              testCase "Longer string" $
                marshallElem (BencodeString "thirteenchars") @?= Just "13:thirteenchars",
              testCase "String with symbols" $
                marshallElem (BencodeString "!@:$") @?= Just "4:!@:$"
            ],
          testGroup
            "Integers"
            [ testCase "Positive integer" $
                marshallElem (BencodeInt 42) @?= Just "i42e",
              testCase "Negative integer" $
                marshallElem (BencodeInt (-42)) @?= Just "i-42e",
              testCase "Zero" $
                marshallElem (BencodeInt 0) @?= Just "i0e"
            ],
          testGroup
            "Lists"
            [ testCase "Empty list" $
                marshallElem (BencodeList []) @?= Just "le",
              testCase "List of strings" $
                marshallElem (BencodeList [BencodeString "spam", BencodeString "eggs"]) @?= Just "l4:spam4:eggse",
              testCase "Mixed list" $
                marshallElem (BencodeList [BencodeString "spam", BencodeInt 42]) @?= Just "l4:spami42ee"
            ],
          testGroup
            "Dictionaries"
            [ testCase "Empty dictionary" $
                marshallElem (BencodeDict []) @?= Just "de",
              testCase "Simple dictionary" $
                marshallElem (BencodeDict [("bar", BencodeString "spam"), ("foo", BencodeInt 42)])
                  @?= Just "d3:bar4:spam3:fooi42ee",
              testCase "Nested dictionary" $
                marshallElem (BencodeDict [("inner", BencodeList [BencodeInt 1])])
                  @?= Just "d5:innerli1eee"
            ]
        ]
    ]
