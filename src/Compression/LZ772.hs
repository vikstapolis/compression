{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- Some tips: https://codereview.stackexchange.com/questions/245329/lz77-encoding-and-decoding-in-haskell

module Compression.LZ772 {-(
    compressFile,
    uncompressFile
)-} where

{-
    Description of the format of the encoded file:

    Encoding begins directly as there is no need to pass extra information for decoding.

    Every non-repeated substring (or repeated substring less than `minPairSize`) is
    left as individual characters. All repeated substrings within the range of the buffer
    (whose size is specified by `searchBufferSize` and `lookAheadBufferSize`) are encoded
    by a null ('\0') character followed by 2 bytes representing the distance of the match,
    and 2 more bytes representing the length of the match.
-}

import Compression.ByteArray (ByteArray)
import qualified Compression.ByteArray as BA

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL

import Data.Char (chr, ord)
import Data.Bits (Bits(shiftR, (.&.), (.|.), shiftL))

import System.FilePath.Windows (takeBaseName, takeExtension)
import System.IO (utf8, hClose, hSetEncoding, hPutStr, IOMode(WriteMode), openFile)
import Data.Word (Word8)

data LZ77Unit = Pair Int Int | Single Word8 deriving (Eq, Show)

lz77ToBSL :: LZ77Unit -> ByteString
lz77ToBSL (Pair x y) = BSL.pack $ map fromIntegral [0, x1, x2, y1, y2]
    where x1 = x .&. 0xff
          x2 = (x .&. 0xff00) `shiftR` 8
          y1 = y .&. 0xff
          y2 = (y .&. 0xff00) `shiftR` 8
lz77ToBSL (Single c) = BSL.singleton c


searchBufferSize, lookAheadBufferSize, minPairSize :: Int

-- Size of buffers in bytes
searchBufferSize    = 15000
lookAheadBufferSize = 250

-- Minimum number of bytes required for storing in a Pair
minPairSize = 4

compressFile :: FilePath -> IO ()
compressFile path = do
    contents <- BA.readFile path
    let newPath = takeBaseName path ++ ".lz77"

    BSL.writeFile newPath (BSL.concat . map lz77ToBSL . toLZ77Units 0 $ contents)

uncompressFile :: FilePath -> IO ()
uncompressFile path =
    if takeExtension path /= ".lz77"
        then error "File needs to be .lz77"
        else do
            contents <- BA.readFile path
            let newPath = takeBaseName path ++ ".txt"

            handle <- openFile newPath WriteMode
            hSetEncoding handle utf8

            -- Carriage returns are printed as newlines, must be filtered out
            hPutStr handle (filter (/='\r') . fromLZ77Units "" $ parseLZ77Units contents)
            
            hClose handle


toLZ77Units :: Int -> BA.ByteArray -> [LZ77Unit]
toLZ77Units idx ba
    | BA.length ba == idx = []
    | otherwise = case findLongestMatch ba idx of
                      pair@(Pair matchDist matchSize) ->
                                   pair : toLZ77Units (idx + matchSize) ba
                      Single ch -> Single ch : toLZ77Units (idx + 1) ba


findLongestMatch :: BA.ByteArray -> Int -> LZ77Unit
findLongestMatch ba idx
    = let lookAheadBuffer   = BA.take lookAheadBufferSize $ BA.drop idx ba
          searchBuffer      = BA.drop (idx - searchBufferSize) $ BA.take idx ba
          maybeMatch        = BA.longestMatchingSubarray lookAheadBuffer searchBuffer
      in case maybeMatch of
             Nothing     -> Single $ BA.head lookAheadBuffer
             Just (b4Match, match) -> let matchLen = BA.length match
                                          in Pair
                                                 (min idx searchBufferSize - BA.length b4Match)
                                                 matchLen


parseLZ77Units :: ByteArray -> [LZ77Unit]
parseLZ77Units (BA.null -> True) = []
parseLZ77Units ba
    | BA.head ba == 0
    = Pair x y : parseLZ77Units rest'
    where rest = BA.tail ba
          x = fromIntegral (rest BA.! 0) .|. (fromIntegral (rest BA.! 1) `shiftL` 8)
          y = fromIntegral (rest BA.! 2) .|. (fromIntegral (rest BA.! 3) `shiftL` 8)
          rest' = BA.drop 4 rest
parseLZ77Units str = Single (BA.head str) : parseLZ77Units (BA.tail str)

fromLZ77Units :: String -> [LZ77Unit] -> String
fromLZ77Units str (Single ch : ls) = fromLZ77Units (BA.byteToChar ch : str) ls
fromLZ77Units str (Pair x y  : ls) = fromLZ77Units (take y (drop (x - y) str) ++ str) ls
fromLZ77Units str []               = reverse str