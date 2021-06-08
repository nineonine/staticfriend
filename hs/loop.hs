module Loop where

loop :: Int -> Int
loop 0 = 909
loop n = loop (n-1)
