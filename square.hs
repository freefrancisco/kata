-- finding the largest submatrix of all ones in an input, and returning 
-- the sum of all the elements in this submatrix

import Control.Arrow

readInput :: String -> [[Int]]
readInput = lines >>> map words >>> (map . map) read

subMatrix :: Int -> Int -> Int -> [[a]] -> [[a]]
subMatrix n i j = drop i >>> take n >>> map (drop j) >>> map (take n)

convolution :: Int -> Int -> Int -> [[Int]] -> Int
convolution n i j = subMatrix n i j >>> map sum >>> sum

sortedResults :: [[Int]] -> [Int]
sortedResults m = [ conv | k <- [0..length m]
                         , i <- [0..k]
                         , j <- [0..k]
                         , let n = length m - k
                         , let conv = convolution n i j m
                         , conv ==  n*n
                    ]

-- run like this: cat input.txt | runhaskell square.hs
main :: IO ()
main = interact ( readInput >>> sortedResults >>> show . head)
