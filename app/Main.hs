module Main where

import System.IO            (IOMode(ReadMode), Handle, openFile, hGetChar, hGetLine, hIsEOF)
import Data.Map.Strict      (Map, empty, insertWith)

data Nucleotide = A | C | T | G deriving (Enum, Eq, Ord, Show)

type GWord = [Nucleotide]

readNucleotide :: Char -> Maybe Nucleotide
readNucleotide symbol =
    case symbol of
        'A' -> Just A
        'C' -> Just C
        'T' -> Just T
        'G' -> Just G
        _   -> Nothing

readWord :: Int -> Handle -> IO GWord
readWord 0         _    = pure []
readWord nuclCount file = do
    eof <- hIsEOF file
    if not eof then
        do
            symbol <- hGetChar file
            case readNucleotide symbol of
                Just nucleotide -> do
                    gWord <- readWord (nuclCount - 1) file
                    pure ([nucleotide] ++ gWord)
                Nothing -> do
                    gWord <- readWord nuclCount file
                    pure gWord
    else
        pure []

countWords :: FilePath -> IO (Map GWord Int)
countWords filepath = do
    genomeHandle <- openFile filepath ReadMode
    _ <- hGetLine genomeHandle
    firstWord <- readWord 6 genomeHandle
    gWords <- countWord firstWord empty genomeHandle
    pure gWords

countWord :: GWord -> Map GWord Int -> Handle -> IO (Map GWord Int)
countWord gWord list handle = do
    eof <- hIsEOF handle
    if not eof then do
        symbol <- hGetChar handle
        case readNucleotide symbol of
            Just nucleotide -> do
                let newWord = (tail gWord) ++ [nucleotide]
                countWord
                    newWord
                    (insertWith (+) gWord 1 list)
                    handle
            Nothing         ->
                countWord gWord list handle
    else
        pure (insertWith (+) gWord 1 list)

main :: IO ()
main = do
    word <- countWords "genome.fna"
    print word