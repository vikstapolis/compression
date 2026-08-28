
module Compression.ByteArray (
    module V,
    module Compression.ByteArray{-
    ByteArray,
    stringToByteArray,
    charToByte,
    byteToChar,
    longestMatchingSubarray,
    readFile,
    writeFile
-}) where

{-
    A substitue for ByteString and wrapper for Vector Word8, providing additional functionality for
    finding substrings

    This is NOT a general substitute for ByteString, it is on used for the specific purpose of
    matching substrings
-}

import Prelude hiding (readFile, writeFile)

import Data.Vector.Unboxed as V

import Data.Word (Word8)
import qualified Data.ByteString as B

import Data.ByteString.Internal (c2w, w2c)
import Control.Monad (msum)

type ByteArray = Vector Word8

stringToByteArray :: String -> ByteArray
stringToByteArray = V.fromList . Prelude.map c2w

charToByte :: Char -> Word8
charToByte = c2w

byteToChar :: Word8 -> Char
byteToChar = w2c

-- returns Maybe (subarrayBeforeMatch, lastOccurenceOfLongestMatch)
longestMatchingSubarray :: ByteArray -> ByteArray -> Maybe (ByteArray, ByteArray)
longestMatchingSubarray x a = msum 
                            . Prelude.map (`findLastMatch` a)
                            . Prelude.filter (not . V.null) 
                            . Prelude.drop 3
                            $ baInits x

baInits :: ByteArray -> [ByteArray]
baInits = baInits' []
    where baInits' :: [ByteArray] -> ByteArray -> [ByteArray]
          baInits' res b
              | V.null b = res
              | otherwise  = baInits' (b':res) b'
              where b' = V.init b

findLastMatch :: ByteArray -> ByteArray -> Maybe (ByteArray, ByteArray)
findLastMatch x a = findLastMatch' (V.length a - V.length x) x a
    where
        findLastMatch' :: Int -> ByteArray -> ByteArray -> Maybe (ByteArray, ByteArray)
        findLastMatch' i x a
            | i < 0 = Nothing
            | a ! i == x ! 0 = if isSubarrayAtLocation i x a
                                   then Just (V.take i a, x)
                                   else findLastMatch' (i - 1) x a
            | otherwise      = findLastMatch' (i - 1) x a

isSubarrayAtLocation :: Int -> ByteArray -> ByteArray -> Bool
isSubarrayAtLocation i x a = helper 0 i x a
    where helper :: Int -> Int -> ByteArray -> ByteArray -> Bool
          helper i j x a
              | V.length x == i = True
              | V.length a == j = False
              | otherwise       = x ! i == a ! j
                                && helper (i + 1) (j + 1) x a

writeFile :: ByteArray -> FilePath -> IO ()
writeFile arr path = B.writeFile path $ B.pack (V.toList arr)

readFile :: FilePath -> IO ByteArray
readFile fp =  V.fromList . B.unpack <$> B.readFile fp