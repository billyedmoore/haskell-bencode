import Data.Bencode.Parse (BencodeValue (BencodeInt), parseInt)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Bencode parseInt Tests"
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
    ]
