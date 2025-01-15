module Functions (
    hammingaux,
    hamming,
    rdtuples,
    runningAverage,
    rdfile,
    isMod,
    isEqual,
    onlySpace,
    noSpaceList,
    removesfirstaux,
    removesfirst,
    tuplehamming,
    tuplemin,
    listremain,
    modified
) where

hammingaux :: String -> String -> Int -> Int
hamming :: String -> String -> Int
rdtuples :: [(String, String)] -> [Float]
runningAverage :: [Float] -> [Int] -> [Float]
rdfile :: FilePath -> IO [String]
isMod :: String -> String -> Bool
isEqual :: String -> (String, String) -> Bool
onlySpace :: String -> Bool
noSpaceList :: [String] -> [String]
removesfirstaux :: String -> [String] -> [String]
removesfirst :: [(String,String)] -> [String] -> Bool -> [String]
tuplehamming :: [(String, String)] -> [(String, String, Int)]
tuplemin :: (String, String, Int) -> [(String, String, Int)] -> (String, String)
listremain :: (String, String) -> [String] -> [String]
modified :: [String] -> [String] -> [(String, String)]

--Returns True if a string is composed only of spaces (called by the function "noSpaceList").
onlySpace xs
    | null xs = True
    | head xs == ' ' = onlySpace (tail xs)
    | otherwise = False

--Receives a list of strings and returns those that contain at least a non-space character.
noSpaceList [] = []
noSpaceList (x:xs)
    | not (onlySpace x) = x:noSpaceList(xs)
    | otherwise = noSpaceList(xs)

--Auxiliary function to "removesfirst". Receives a list of strings, a string, searches for the first occurrence of "str", and removes it.
removesfirstaux str (x:xs)
    | str == x = xs
    | otherwise = x:removesfirstaux (str) xs

--Receives a list of string tuples, a list of strings and a boolean that select which string in the tuple to evaluate. Then calls "removesfirst". ("Cond == True" evaluates the first item in the tuple / "Cond == False" evaluates the second item in the tuple)
removesfirst [] (ys) cond = ys
removesfirst ((x1, x2):xs) (ys) cond
    | cond = removesfirstaux x1 (removesfirst xs ys True)
    | otherwise = removesfirstaux x2 (removesfirst xs ys False)

--Creates a list of tuples containing two strings, in which the hamming distance is less than or equal to "isMod", and the hamming distance between them.
tuplehamming [] = []
tuplehamming (xs) = [(str1, str2, hamming str1 str2) | (str1, str2) <- xs, isMod str1 str2]

--Returns a tuple with two strings with the smallest hamming distance in a list.
tuplemin tup (x:xs)
    | null xs = if n <= n' then (str1, str2) else (str1', str2')
    | otherwise = if n <= n' then tuplemin tup xs else tuplemin x xs
    where (str1, str2, n) = tup
          (str1', str2', n') = x

--Receives a tuple and a list of strings, searches for the first occurrence of a string, and returns the sublist from that point onward.
listremain tup [] = []
listremain tup (x:xs)
    | str2 == x = xs
    | otherwise = listremain tup (xs)
    where (str1, str2) = tup

--"Modified" is a core function in this code. It receives two lists of strings and returns a list of tuples containing strings where the hamming distance is less than or equal to "isMod", i.e, the ones considered "modified" by the algorithm.
modified xs ys
    | null xs || null ys = []
    | otherwise =
        let th = tuplehamming (zip (repeat (head xs)) ys)
            tuplemod = tuplemin (head th) (th)
            remaininglist = listremain tuplemod ys
        in if not (null th) then tuplemod : modified (tail xs) remaininglist else modified (tail xs) ys

--Reads the files and returns a list containing only the lines
rdfile path = fmap lines (readFile path)

--A function to verify if a line is modified, i.e, the hamming distance from the two strings is <= than 70% of the size of the string
isMod str1 str2 = 
    let limit = truncate (0.5 * fromIntegral(length str1))
    in hamming str1 str2 <= limit

isEqual str (str1, str2) = str == str1 && str1 == str2

{-Calculates the average of numbers until that point.
    [2, 7, 4, 1] -> [2, 4.5, 4.33, 3.5]
    2.0 = 2/1
    4.5 = (2 + 7)/2
    4.33 = (2 + 7 + 4)/3
    3.5 = (2 + 7 + 4 + 1)/4
-}
runningAverage distances indices = zipWith (/) (scanl1 (+) distances) (map fromIntegral indices)

--Gets the hamming distance between two strings.
hammingaux xs ys cont
    | (null xs) || (null ys) = cont
    | (head xs) /= (head ys) = hammingaux (tail xs) (tail ys) (cont + 1)
    | (head xs) == (head ys) = hammingaux (tail xs) (tail ys) cont

--A simple function to call hamming aux passing the correct parameters to cont
hamming (xs) (ys) = hammingaux xs ys $ abs(length xs - length ys)

--Function to read a list of tuples and return their respective hamming distances in a list
rdtuples (xs) = [fromIntegral(hamming (str1) (str2)) | (str1, str2) <- xs]
