module Euler059 where
import Data.Bits
import Data.Char

cipherTxt :: IO [Int]
cipherTxt =
  readFile "cipher1.txt" >>= return . read . ("[" ++) . (++ "]")

secret_key = map ord "god"

crypt key text = zipWith xor text (cycle key)

decrypt = crypt secret_key

main :: IO String
main = cipherTxt >>= return . show . sum . decrypt

