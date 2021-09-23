module Main where

ones = 1:ones
xs = take 10000 ones

f :: Int -> [Int]
f 0 = xs
f n = filter (\x -> True) (f (n - 1))

main :: IO ()
main = putStrLn $ show $ length $ f 100000
