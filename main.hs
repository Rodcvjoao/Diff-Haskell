import Control.Monad (zipWithM_)
import Functions

main :: IO ()
main = do
    spacefile1 <- rdfile "arq1.txt"
    spacefile2 <- rdfile "arq2.txt"

    let spaces = length spacefile1 - length spacefile2

    let file1 = noSpaceList spacefile1 
    let file2 = noSpaceList spacefile2

    let modifiedResults = modified file1 file2

    let removed = removesfirst (modifiedResults) file1 True
    let added = removesfirst (modifiedResults) file2 False
    let unchanged = [line | line <- file1, any (isEqual line) modifiedResults]

    let hammingDistances = map (\(str1, str2) -> fromIntegral (hamming str1 str2)) modifiedResults
    let averages = runningAverage hammingDistances [1 .. length modifiedResults]
    
    putStrLn "Linhas removidas:"
    mapM_ putStrLn removed

    putStrLn "\nLinhas adicionadas:"
    mapM_ putStrLn added

    putStrLn "\nLinhas inalteradas:"
    mapM_ putStrLn unchanged

    putStrLn "\nLinhas modificadas:"
    zipWithM_ (\(str1, str2) avg -> 
        if str1 /= str2 
            then putStrLn $ str1 ++ " -> " ++ str2 ++ " :: Media: " ++ show avg 
            else return ()) 
        modifiedResults 
        averages

    if spaces >= 0
        then putStrLn $ "\nEspaços adicionados: " ++ show spaces
    else putStrLn $ "\nEspaços removidos: " ++ show (abs spaces)