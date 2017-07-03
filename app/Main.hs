module Main where

import System.IO            (IOMode(ReadMode), Handle, openFile, hGetChar, hGetLine, hIsEOF)
import Data.Map.Strict      (Map, empty, insertWith)
import Data.Vector.Unboxed  (Vector(..))
import qualified Data.Vector.Unboxed as Vec
import Data.Word            (Word16(..))

data Nucleotide = A | C | T | G deriving (Enum, Eq, Ord, Read, Show)

type GWord = [Nucleotide]

codeWord :: GWord -> Word16
codeWord gWord = fromIntegral (sum [4^i * (fromEnum nuc) | (i, nuc) <- zip [0..] gWord])

decodeWord :: Word16 -> GWord
decodeWord word = [toEnum (fromIntegral((div word (4^i)) `mod` 4)) | i <- [0..5]]

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

countWords :: FilePath -> IO (Vector Word16)
countWords filepath = do
    genomeHandle <- openFile filepath ReadMode
    _ <- hGetLine genomeHandle
    firstWord <- readWord 6 genomeHandle
    gWords <- countWord firstWord (Vec.replicate 4096 0) genomeHandle
    pure gWords

countWord :: GWord -> Vector Word16 -> Handle -> IO (Vector Word16)
countWord gWord vector handle = do
    eof <- hIsEOF handle
    if not eof then do
        symbol <- hGetChar handle
        case readMaybe symbol of
            Just nucleotide -> do
                let newWord = (tail gWord) ++ [nucleotide]
                countWord
                    newWord
                    (Vec.accum (+) vector [(fromIntegral $ codeWord gWord, 1)])
                    handle
            Nothing         ->
                countWord gWord vector handle
    else
        pure (Vec.accum (+) vector [(fromIntegral $ codeWord gWord, 1)])

main :: IO ()
main = do
    words <- countWords "genome.fna"
    print words
