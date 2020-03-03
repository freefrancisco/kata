{- Container with most water:
Given n non-negative integers a1, a2, ..., an , where each represents a point at coordinate (i, ai). 
n vertical lines are drawn such that the two endpoints of line i is at (i, ai) and (i, 0). 
Find two lines, which together with x-axis forms a container, such that the container contains the most water.
https://leetcode.com/problems/container-with-most-water/description/

This solution is the functional equivalent to the iterative solution with two pointers starting from each end. 

λ> solve [8,10,14,0,13,10,9,9,11,11]
80
λ> solve [1,8,6,2,5,4,8,3,7]
49

-}

solve :: [Int] -> Int
solve = solve' 0 where
    solve' best ls | length ls < 2 = best
    solve' best (x:xs) = 
        let 
            y = last xs
            best' = max best (length xs * min x y)
        in 
            if x < y
            then solve' best' xs
            else solve' best' (x: init xs)
        

main = do
    ls <- readLn :: IO [Int]
    print $ solve ls

