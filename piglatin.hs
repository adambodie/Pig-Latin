-- piglatin.hs

module Piglatin
( pigSentence
, pigWord
) where

import Data.List
import Data.Char

-- Sets up the first and last letters of a word
firstLetter :: [a] -> a
firstLetter xs = xs !! 0
lastLetter xs = xs !! (length xs - 1)

-- A list of all the letters of the alphabet considered vowels.  For simplicity sakes, y is included as a vowel.
vowels = ['a', 'e', 'i', 'o', 'u', 'y']
vowelsUpper = map (toUpper) vowels
allVowels = vowels ++ vowelsUpper

-- A list of all the letters in the alphabet, removing all the vowels to make a list of consonants
letters = ['a'..'z']
lettersUpper = map (toUpper) letters
allLetters = letters ++ lettersUpper
consonants = snd (partition (`elem` allVowels) allLetters)

-- A list of all the symbols
symbols = ['?', '.', ',', '!', ':', ';', '&', '*', '(', ')']

-- Separates the word and the symbol at the end of a word if found
firstSymbol = fst . break (`elem` symbols)
secondSymbol = snd . break (`elem` symbols)  

-- Checks to determine if the first letter in a word is a vowel, adding +"way" at the end if it is, keeping it the same if not.
pigVowel :: String -> String
pigVowel myWord 
		| firstLetter myWord `elem` allVowels = firstSymbol myWord ++ "way" ++ secondSymbol myWord
		| otherwise = myWord

-- Searches in a word until the first vowel appears, separating the consonant cluster before the vowel from the rest of the word.				
firstCluster = fst . span (`elem` consonants)
secondCluster = snd . span (`elem` consonants)   

-- Check to determine of the first letter in a word is a consonant, grabbing the consonant cluster and putting it after the rest of the word, suffix "ay" and any symbols at the end of the word
pigWord :: String -> String
pigWord myWord 
		| firstLetter myWord `elem` consonants = firstSymbol (secondCluster myWord) ++ firstCluster myWord ++ "ay" ++ secondSymbol myWord
		| otherwise = pigVowel myWord

-- Takes a sentence, separating it into a list of words, turning words into Pig Latin and making the list a sentence again.				
pigSentence  = unwords . map (pigWord) . words		
