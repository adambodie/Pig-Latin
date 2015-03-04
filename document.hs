import System.Environment   
import System.Directory  
import System.IO  
import Data.List 
import Piglatin 
  
dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ]  
   
main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args  
  
add :: [String] -> IO ()  
add [fileName, pigItem] = appendFile fileName (pigItem ++ "\n")  
  
view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let pigLine = lines contents  
        numberedLines = zipWith (\n line -> show n ++ ".  " ++ (pigSentence line)) [1..] pigLine  
    putStr $ unlines numberedLines  
  
remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        pigLine = lines contents  
        newPigItems = delete (pigLine !! number) pigLine  
    hPutStr tempHandle $ unlines newPigItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName  