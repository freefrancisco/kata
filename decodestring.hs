import Control.Applicative
import Text.ParserCombinators.ReadP 

-- problemDescription = [here|
-- https://leetcode.com/problems/decode-string/

-- Given an encoded string, return its decoded string.
-- The encoding rule is: k[encoded_string], where the encoded_string inside the 
-- square brackets is being repeated exactly k times. 
-- Note that k is guaranteed to be a positive integer.

-- You may assume that the input string is always valid; 
-- No extra white spaces, square brackets are well-formed, etc.

-- Furthermore, you may assume that the original data does not contain any digits 
-- and that digits are only for those repeat numbers, k. 
-- For example, there won't be input like 3a or 2[4].

-- Examples:
-- s = "3[a]2[bc]", return "aaabcbc".
-- s = "3[a2[c]]", return "accaccacc".
-- s = "2[abc]3[cd]ef", return "abcabccdcdcdef".

inputs   = words "3[a]2[bc] 3[a2[c]] 2[abc]3[cd]ef"
expected = words "aaabcbc accaccacc abcabccdcdcdef"

solve :: String -> String
solve = decode . parse

data Encoded 
    = Base String 
    | Node Int Encoded 
    | List [Encoded] 
    deriving (Eq, Show)

decode :: Encoded -> String 
decode (Base s) = s
decode (Node k enc) = repeatTimes k (decode enc) 
decode (List e) = concatMap decode e

repeatTimes :: Int -> String -> String
repeatTimes times string = concat . take times $ repeat string


parse :: String -> Encoded
parse  = fst . head . readP_to_S encodedP

encodedP :: ReadP Encoded
encodedP = listP <* eof

listP :: ReadP Encoded
listP = List <$> many1 (nodeP <|> baseP)

nodeP :: ReadP Encoded
nodeP = Node <$> intP <*> between lp rp (nodeP <|> baseP <|> listP) where
    lp = char '['
    rp = char ']'

baseP :: ReadP Encoded
baseP = Base <$> many1 (satisfy isBase) where
    isBase i = elem i basechars
    basechars = ['a'..'z'] ++ ['A'..'Z']

intP :: ReadP Int
intP = read <$> many1 (satisfy isInt) where
    isInt i = elem i intchars
    intchars = concatMap show [0..9]

main :: IO ()
main = do
    putStrLn $ "inputs:   " ++ show inputs
    putStrLn $ "expected: " ++ show expected
    putStrLn $ "actual:   " ++ show (map solve inputs)
    

