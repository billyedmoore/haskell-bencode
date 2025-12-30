module TestMarshall (marshallUnitTests) where

import Data.Bencode.Marshall (marshall)
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
                marshall (BencodeString "") @?= Just "0:",
              testCase "Short string" $
                marshall (BencodeString "hi") @?= Just "2:hi",
              testCase "Longer string" $
                marshall (BencodeString "thirteenchars") @?= Just "13:thirteenchars",
              testCase "String with symbols" $
                marshall (BencodeString "!@:$") @?= Just "4:!@:$"
            ],
          testGroup
            "Integers"
            [ testCase "Positive integer" $
                marshall (BencodeInt 42) @?= Just "i42e",
              testCase "Negative integer" $
                marshall (BencodeInt (-42)) @?= Just "i-42e",
              testCase "Zero" $
                marshall (BencodeInt 0) @?= Just "i0e"
            ],
          testGroup
            "Lists"
            [ testCase "Empty list" $
                marshall (BencodeList []) @?= Just "le",
              testCase "List of strings" $
                marshall (BencodeList [BencodeString "spam", BencodeString "eggs"]) @?= Just "l4:spam4:eggse",
              testCase "Mixed list" $
                marshall (BencodeList [BencodeString "spam", BencodeInt 42]) @?= Just "l4:spami42ee"
            ],
          testGroup
            "Dictionaries"
            [ testCase "Empty dictionary" $
                marshall (BencodeDict []) @?= Just "de",
              testCase "Simple dictionary" $
                marshall (BencodeDict [("bar", BencodeString "spam"), ("foo", BencodeInt 42)])
                  @?= Just "d3:bar4:spam3:fooi42ee",
              testCase "Nested dictionary" $
                marshall (BencodeDict [("inner", BencodeList [BencodeInt 1])])
                  @?= Just "d5:innerli1eee"
            ]
        ]
    ]
