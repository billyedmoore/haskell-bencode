import Data.Bencode.Parse (BencodeValue (BencodeInt, BencodeString), parseInt, parseStr)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Bencode Parser Tests"
    [ -- Integer Tests
      testGroup
        "parseInt Tests"
        [ testGroup
            "Successful Parsings"
            [ testCase "Standard positive" $
                parseInt "i42eRest" @?= Just (BencodeInt 42, "Rest"),
              testCase "Followed by another int" $
                parseInt "i42ei42e" @?= Just (BencodeInt 42, "i42e"),
              testCase "Zero" $
                parseInt "i0e" @?= Just (BencodeInt 0, ""),
              testCase "Negative integer" $
                parseInt "i-15e" @?= Just (BencodeInt (-15), ""),
              testCase "Integer containing zero" $
                parseInt "i1000e" @?= Just (BencodeInt 1000, "")
            ],
          testGroup
            "Invalid Formats (Should return Nothing)"
            [ testCase "No leading zeros allowed" $
                parseInt "i03e" @?= Nothing,
              testCase "No leading zeros allowed, two zeros" $
                parseInt "i003e" @?= Nothing,
              testCase "No leading zeros allowed, negative" $
                parseInt "i-030e" @?= Nothing,
              testCase "No negative zero allowed" $
                parseInt "i-0e" @?= Nothing,
              testCase "Missing end 'e'" $
                parseInt "i42" @?= Nothing,
              testCase "Empty integer content" $
                parseInt "ie" @?= Nothing,
              testCase "Non-numeric junk" $
                parseInt "iabc6e" @?= Nothing
            ]
        ],
      -- String Tests
      testGroup
        "parseStr Tests"
        [ testGroup
            "Successful Parsings"
            [ testCase "Standard string" $
                parseStr "4:spam" @?= Just (BencodeString "spam", ""),
              testCase "String with remainder" $
                parseStr "3:abcRest" @?= Just (BencodeString "abc", "Rest"),
              testCase "Empty string" $
                parseStr "0:" @?= Just (BencodeString "", ""),
              testCase "String containing spaces" $
                parseStr "11:hello world" @?= Just (BencodeString "hello world", ""),
              testCase "String followed by integer" $
                parseStr "4:spami42e" @?= Just (BencodeString "spam", "i42e")
            ],
          testGroup
            "Invalid Formats (Should return Nothing)"
            [ testCase "Content shorter than length" $
                parseStr "4:abc" @?= Nothing,
              testCase "Missing colon" $
                parseStr "4abc" @?= Nothing,
              testCase "Negative length not allowed" $
                parseStr "-3:abc" @?= Nothing,
              testCase "Leading zeros in length not allowed" $
                parseStr "03:abc" @?= Nothing,
              testCase "Non-numeric length" $
                parseStr "a:abc" @?= Nothing,
              testCase "Empty input" $
                parseStr "" @?= Nothing,
              testCase "Missing length" $
                parseStr ":abc" @?= Nothing
            ]
        ]
    ]
