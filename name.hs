import Data.Char  
import Piglatin  
main = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    let pigFirstName = pigSentence firstName  
        pigLastName =  pigSentence lastName  
    putStrLn $ "Hi " ++ pigFirstName ++ " " ++ pigLastName ++ ", how are you doing today?"  