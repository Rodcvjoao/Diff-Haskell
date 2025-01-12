import System.IO

lerArquivo :: FilePath -> IO [String]
lerArquivo caminho = fmap lines (readFile caminho)

main = do 
    lista1 <- lerArquivo "arq1.txt"
    lista2 <- lerArquivo "arq2.txt"
    mapM_ putStrLn lista1
    mapM_ putStrLn lista2