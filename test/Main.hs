import Data.Bencode.Parse (BencodeValue (..), parseDict, parseInt, parseList, parseStr)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    -- Tests for the element parsers, largely AI generated
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
        ],
      -- List tests
      testGroup
        "parseList Tests"
        [ testGroup
            "Successful Parsings"
            [ testCase "Empty list" $
                parseList "le" @?= Just (BencodeList [], ""),
              testCase "List of integers" $
                parseList "li1ei2ei3ee" @?= Just (BencodeList [BencodeInt 1, BencodeInt 2, BencodeInt 3], ""),
              testCase "List of strings" $
                parseList "l4:spam4:eggse" @?= Just (BencodeList [BencodeString "spam", BencodeString "eggs"], ""),
              testCase "Mixed content (Int and String)" $
                parseList "l4:spami42ee" @?= Just (BencodeList [BencodeString "spam", BencodeInt 42], ""),
              testCase "Nested list" $
                parseList "ll4:spamee" @?= Just (BencodeList [BencodeList [BencodeString "spam"]], ""),
              testCase "List with remainder" $
                parseList "leRest" @?= Just (BencodeList [], "Rest")
            ],
          testGroup
            "Invalid Formats (Should return Nothing)"
            [ testCase "Missing end 'e'" $
                parseList "l4:spam" @?= Nothing,
              testCase "Missing end 'e' (nested)" $
                parseList "ll4:spame" @?= Nothing,
              testCase "Junk inside list" $
                parseList "l4:spamXa" @?= Nothing,
              testCase "Only start char" $
                parseList "l" @?= Nothing,
              testCase "Empty input" $
                parseList "" @?= Nothing
            ]
        ],
      -- Dictionary Tests
      testGroup
        "parseDict Tests"
        [ testGroup
            "Successful Parsings"
            [ testCase "Empty dictionary" $
                parseDict "de" @?= Just (BencodeDict [], ""),
              testCase "Simple dictionary (one key)" $
                parseDict "d3:cow3:mooe" @?= Just (BencodeDict [("cow", BencodeString "moo")], ""),
              testCase "Multiple keys" $
                parseDict "d3:cow3:moo4:spam4:eggse" @?= Just (BencodeDict [("cow", BencodeString "moo"), ("spam", BencodeString "eggs")], ""),
              testCase "Nested dictionary" $
                parseDict "d4:dictd3:foo3:baree" @?= Just (BencodeDict [("dict", BencodeDict [("foo", BencodeString "bar")])], ""),
              testCase "Dictionary with list value" $
                parseDict "d4:listli1ei2eee" @?= Just (BencodeDict [("list", BencodeList [BencodeInt 1, BencodeInt 2])], ""),
              testCase "Dictionary with remainder" $
                parseDict "deRest" @?= Just (BencodeDict [], "Rest")
            ],
          testGroup
            "Invalid Formats (Should return Nothing)"
            [ testCase "Missing end 'e'" $
                parseDict "d3:cow3:moo" @?= Nothing,
              testCase "Key not a string (integer key)" $
                parseDict "di42e4:spame" @?= Nothing,
              testCase "Duplicate keys" $
                parseDict "d3:cow3:moo3:cow4:moo2e" @?= Nothing,
              testCase "Missing value" $
                parseDict "d3:cowe" @?= Nothing,
              testCase "Odd number of elements" $
                parseDict "d3:cow3:moo4:spame" @?= Nothing,
              testCase "Unsorted keys" $
                parseDict "d4:spam4:eggs3:cow3:mooe" @?= Nothing,
              testCase "Only start char" $
                parseDict "d" @?= Nothing,
              testCase "Junk between key and value" $
                parseDict "d3:cowX3:mooe" @?= Nothing
            ]
        ]
    ]
