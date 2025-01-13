hammingaux :: String -> String -> Int -> Int
hamming :: String -> String -> Int
rdtuples :: [(String, String)] -> [Float]
runningAverage :: [Float] -> [Float]
rdfile :: FilePath -> IO [String]
isMod :: String -> String -> Bool

----------------------------------------------------
rdfile path = fmap lines (readFile path)
----------------------------------------------------

{-
A function to verify if a line is modified, i.e, the hamming distance from the two strings is <= than 70% of the size of the string
-}
isMod str1 str2 = 
    let limit = ceiling (0.7 * fromIntegral(length str1))
    in hamming str1 str2 <= limit

{-
A function that creates a list of tuples where the pairs are (x, 1) (x being the original value of the list) --zip.
Then, iterates the list adding the tuples to create a accumulative sum necessary for the average --scanl1.
Lastly, it iterates the output tuples from scanl1 operatin the division of the two numbers --map.

Example:
    [2, 7, 4, 1] -> [(2,1), (7,1), (4,1), (1,1)] -> [(2,1), (9,2), (13,3), (14,4)] -> [2.0, 4.5, 4.33, 3.5]
-}

runningAverage xs = map (\(sum, count) -> sum / count) $ scanl1 (\(s1, c1) (s2, c2) -> (s1 + s2, c1 + c2)) $ zip xs (repeat 1)

--Gets the hamming distance between two strings.
hammingaux xs ys cont
    | (null xs) || (null ys) = cont
    | (head xs) /= (head ys) = hammingaux (tail xs) (tail ys) (cont + 1)
    | (head xs) == (head ys) = hammingaux (tail xs) (tail ys) cont

--A simple function to call hamming aux passing the correct parameters to cont
hamming (xs) (ys) = hammingaux xs ys $ abs(length xs - length ys)

--Function to read a list of tuples and return their respective hamming distances in a list
rdtuples (xs) = [fromIntegral(hamming (str1) (str2)) | (str1, str2) <- xs]

{-
Testing with these parameters
rdtuples [("teste", "teste"), ("teste", "twstw"), ("teste", ""), ("", "teste"), ("teste", "haskell")]
-}

func =
    runningAverage $ rdtuples [("teste", "teste"), ("teste", "twstw"), ("teste", ""), ("", "teste"), ("teste", "haskell")]