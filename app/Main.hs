module Main where

import System.IO     (IOMode(ReadMode), Handle, openFile, hGetChar, hGetLine, hIsEOF)
import Data.Map      (Map, empty, insertWith)

readWord :: Int -> Handle -> IO String
readWord 0         _    = pure []
readWord charCount file = do
    eof <- hIsEOF file
    if eof == False then
        do
            char <- hGetChar file
            string <- readWord (charCount - 1) file
            pure ([char] ++ string)
    else
        pure []

countWords :: FilePath -> IO (Map String Int)
countWords filepath = do
    genomHandle <- openFile filepath ReadMode
    _ <- hGetLine genomHandle
    words <- countWord Nothing empty genomHandle
    pure words

countWord :: Maybe String -> Map String Int -> Handle -> IO (Map String Int)
countWord Nothing _ handle = do
    firstWord <- readWord 6 handle
    countWord (Just firstWord) (insertWith (+) firstWord 1 empty) handle
countWord (Just word) list handle = do
    eof <- hIsEOF handle
    if not eof then do
        nucleotide <- hGetChar handle
        let newWord = (tail word) ++ [nucleotide]
        countWord
            (Just newWord)
            (insertWith (+) newWord 1 list)
            handle
    else
        pure list
main :: IO ()
main = do
    words <- countWords "genom.fna"
    print words