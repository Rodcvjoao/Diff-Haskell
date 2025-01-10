hammingaux :: (Eq a) => [a] -> [a] -> Int -> Int
hamming :: (Eq a) => [a] -> [a] -> Int

hammingaux [] [] cont = cont
hammingaux (xs) []  cont = cont + length xs
hammingaux  [] (ys) cont = cont + length ys
hammingaux (x:xs) (y:ys) cont
    | x /= y = hammingaux xs ys (cont + 1)
    | x == y = hammingaux xs ys cont

hamming (xs) (ys) = hammingaux xs ys 0