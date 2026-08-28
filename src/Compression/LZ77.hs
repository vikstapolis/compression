{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- Some tips: https://codereview.stackexchange.com/questions/245329/lz77-encoding-and-decoding-in-haskell

module Compression.LZ77 (
    compressFile,
    uncompressFile
) where

{-
    Description of the format of the encoded file:

    Encoding begins directly as there is no need to pass extra information for decoding.

    Every non-repeated substring (or repeated substring less than `minPairSize`) is
    left as individual characters. All repeated substrings within the range of the buffer
    (whose size is specified by `searchBufferSize` and `lookAheadBufferSize`) are encoded
    by a null ('\0') character followed by 2 bytes representing the distance of the match,
    and 2 more bytes representing the length of the match.
-}

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Char (chr, ord)
import Data.Bits (Bits(shiftR, (.&.), (.|.), shiftL))

import System.FilePath.Windows (takeBaseName, takeExtension)
import GHC.IO.Encoding
import System.IO
import Data.Word (Word8)

data LZ77Unit = Pair Int Int | Single Char deriving (Eq, Show)

lz77ToString :: LZ77Unit -> String
lz77ToString (Pair x y) = '\0' : map chr [x1, x2, y1, y2]
    where x1 = x .&. 0xff
          x2 = (x .&. 0xff00) `shiftR` 8
          y1 = y .&. 0xff
          y2 = (y .&. 0xff00) `shiftR` 8
lz77ToString (Single c) = [c]


searchBufferSize, lookAheadBufferSize, minPairSize :: Int

-- Size of buffers in bytes
searchBufferSize    = 15000
lookAheadBufferSize = 250

-- Minimum number of bytes required for storing in a Pair
minPairSize = 4


compressFile :: FilePath -> IO ()
compressFile path = do
    contents <- BS.readFile path
    let newPath = takeBaseName path ++ ".lz77"

    BS.writeFile newPath (BS.pack $ concatMap lz77ToString . toLZ77Units 0 $ contents)

uncompressFile :: FilePath -> IO ()
uncompressFile path =
    if takeExtension path /= ".lz77"
        then error "File needs to be .lz77"
        else do
            contents <- BSL.readFile path
            let newPath = takeBaseName path ++ ".txt"

            handle <- openFile newPath WriteMode
            hSetEncoding handle utf8

            -- Carriage returns are printed as newlines, must be filtered out
            hPutStr handle (filter (/='\r') . fromLZ77Units "" $ parseLZ77Units contents)
            
            hClose handle


toLZ77Units :: Int -> ByteString -> [LZ77Unit]
toLZ77Units idx str
    | BS.length str == idx = []
    | otherwise = case findLongestMatch str idx of
                      pair@(Pair matchDist matchSize) ->
                                   pair : toLZ77Units (idx + matchSize) str
                      Single ch -> Single ch : toLZ77Units (idx + 1) str


findLongestMatch :: ByteString -> Int -> LZ77Unit
findLongestMatch str idx
    = let lookAheadBuffer   = BS.take lookAheadBufferSize $ BS.drop idx str
          searchBuffer      = BS.drop (idx - searchBufferSize) $ BS.take idx str
          maybeMatch        = safeLast
                            . takeWhile (not . BS.null . snd . fst)
                            . map (\x -> (lastMatch x searchBuffer, x))
                            . drop minPairSize $ BS.inits lookAheadBuffer
      in case maybeMatch of
             Nothing     -> Single $ BS.head lookAheadBuffer
             Just ((b4Match, _), match) -> let matchLen = BS.length match
                                           in Pair
                                                  (min idx searchBufferSize - BS.length b4Match)
                                                  matchLen

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

lastMatch :: ByteString -> ByteString -> (ByteString, ByteString)
lastMatch x s = case BS.breakSubstring x s of
                    (BS.null . snd -> True) -> ("", "")
                    (b4match, match)        -> case lastMatch x (BS.tail match) of
                                                   (BS.null -> True, BS.null -> True) -> (b4match, match)
                                                   (newB4, nextMatch)   -> ((b4match `BS.snoc` BS.head x) `BS.append` newB4, nextMatch)


parseLZ77Units :: BSL.ByteString -> [LZ77Unit]
parseLZ77Units (BSL.null -> True) = []
parseLZ77Units (BSL.stripPrefix "\0" -> Just rest)
    = Pair x y : parseLZ77Units rest'
    where x = ord (rest `BSL.index` 0) .|. (ord (rest `BSL.index` 1) `shiftL` 8)
          y = ord (rest `BSL.index` 2) .|. (ord (rest `BSL.index` 3) `shiftL` 8)
          rest' = BSL.drop 4 rest
parseLZ77Units str = Single (BSL.head str) : parseLZ77Units (BSL.tail str)

fromLZ77Units :: String -> [LZ77Unit] -> String
fromLZ77Units str (Single ch : ls) = fromLZ77Units (ch:str) ls
fromLZ77Units str (Pair x y  : ls) = fromLZ77Units (take y (drop (x - y) str) ++ str) ls
fromLZ77Units str []               = reverse str