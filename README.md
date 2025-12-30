# haskell-bencode

A toy bencode parser in Haskell!

## Parse a string

```haskell
import Data.Bencode.Parse (parse)

main :: IO()
-- Prints "BencodeList [BencodeInt 1, BencodeInt 2, BencodeInt 3, BencodeInt 4]"
main = putStrLn $ parse "li1ei2ei3ei4ee" 
```
## Marshall to a string

```haskell
import Data.Bencode.Marshall (marshall)

main :: IO()
-- Prints "li1ei2ei3ei4ee"
main = putStrLn $ marshall (BencodeList [BencodeInt 1, BencodeInt 2, BencodeInt 3, BencodeInt 4])
```
