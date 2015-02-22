-- piglatin.hs

import Data.List
import Data.Char

-- Sets up the first and last letters of a word
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
firstSymbol xs = fst (break (`elem` symbols) xs)
secondSymbol xs = snd (break (`elem` symbols) xs)  

-- Checks to determine if the first letter in a word is a vowel, adding +"way" at the end if it is, keeping it the same if not.
pigVowel myWord = if firstLetter myWord `elem` allVowels
				then firstSymbol myWord ++ "way" ++ secondSymbol myWord
				else myWord

-- Searches in a word until the first vowel appears, separating the consonant cluster before the vowel from the rest of the word.				
firstCluster xs = fst (span (`elem` consonants) xs)
secondCluster xs = snd (span (`elem` consonants) xs)   

-- Check to determine of the first letter in a word is a consonant, grabbing the consonant cluster and putting it after the rest of the word, suffix "ay" and any symbols at the end of the word
pigConsonant myWord = if firstLetter myWord `elem` consonants
				then firstSymbol (secondCluster myWord) ++ firstCluster myWord ++ "ay" ++ secondSymbol myWord
				else pigVowel myWord

-- Takes a sentence, separating it into a list of words, turning words into Pig Latin and making the list a sentence again.				
pigLatin xs = unwords (map (pigConsonant) (words xs))
