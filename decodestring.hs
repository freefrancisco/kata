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
-- |]


data Encoded 
    = Base String 
    | Node Int Encoded 
    | List [Encoded] 
    deriving (Eq, Show)

decode :: Encoded -> String 
decode (Base s) = s
decode (Node k enc) = repeatTimes k (decode enc) where 
    repeatTimes times string = concat . take times $ repeat string
decode (List e) = concatMap decode e

parse :: String -> Encoded
parse  = fst . head . readP_to_S encoded

encoded :: ReadP Encoded
encoded = do 
    out <- node <|> list <|> base 
    eof 
    return out

base :: ReadP Encoded 
base = Base <$> letters

node :: ReadP Encoded 
node = Node <$> digits <*> (bracketed (base <|> node <|> list ))

list :: ReadP Encoded
list = List <$> many1 (node <|> base)

letters :: ReadP String 
letters = munch1 letter where
    letter ch = elem ch (['a'..'z'] ++ ['A'..'Z'])

digits :: ReadP Int
digits = read <$> many1 digit where 
    digit = satisfy (\ch -> ch >= '0' && ch <= '9')

bracketed :: ReadP a -> ReadP a
bracketed  = between left right where
    left = char '['
    right = char ']'


main = mapM_ test ["3[a]2[bc]", "3[a2[c]]", "2[abc]3[cd]ef"]
    
test str = do 
    let parsed = parse str 
        decoded = decode parsed 
    putStrLn "\nTesting: "
    putStrLn str
    print parsed
    putStrLn decoded


