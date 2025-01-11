hammingaux :: String -> String -> Int -> Int
hamming :: String -> String -> Int
rdtuples :: [(String, String)] -> [Float]

--Gets the hamming distance between two strings.
hammingaux [] [] cont = cont
hammingaux (xs) []  cont = cont + length xs
hammingaux  [] (ys) cont = cont + length ys
hammingaux (x:xs) (y:ys) cont
    | x /= y = hammingaux xs ys (cont + 1)
    | x == y = hammingaux xs ys cont

--A simple function to call hamming aux passing the correct parameters to cont
hamming (xs) (ys) = hammingaux xs ys 0

--Function to read a list of tuples and return their respective hamming distances in a list
rdtuples (xs) = [fromIntegral(hamming (str1) (str2)) | (str1, str2) <- xs]

{-
Testing with these parameters
rdtuples [("teste", "teste"), ("teste", "twstw"), ("teste", ""), ("", "teste"), ("teste", "haskell")]
-}