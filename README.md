# haskell-bencode

A toy bencode parser in Haskell!

Not spec compliant because the assumption is made each Haskell Char is
a single byte, Haskell infact uses UTF-32. I considered using ByteString
which would be the correct way to do this but I like the normal strings.

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
