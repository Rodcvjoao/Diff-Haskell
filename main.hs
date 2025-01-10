hamming :: (Eq a) => [a] -> [a] -> Int -> Int

hamming [] [] cont = cont
hamming (x:xs) (y:ys) cont
    | x /= y = hamming xs ys (cont + 1)
    | x == y = hamming xs ys cont