{-# LANGUAGE ViewPatterns #-}

module Huffman (
    encodeFile,
    decodeFile
) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import System.IO (hGetLine, hClose, openFile, IOMode(ReadMode))
import System.FilePath.Windows (takeBaseName)

import Data.List (sortBy, insert, group, sort)
import Data.List.Extra (stripSuffix)
import Data.Maybe (fromJust)
import Data.Function (on)
import Data.Char (ord, chr)

-- Datatype to build Huffman Tree
data Huff = Node Int Huff Huff | Leaf Int Char deriving Show

weight :: Huff -> Int
weight (Node w _ _) = w
weight (Leaf w _)   = w

instance Eq Huff where
    Node w1 l1 r1 == Node w2 l2 r2 = w1 == w2 && l1 == l2 && r1 == r2
    Leaf w1 c1 == Leaf w2 c2       = w1 == w2 && c1 == c2

instance Ord Huff where
    compare = compare `on` weight


-- Takes a FilePath and compresses it, writing the compressed information
-- to a file with "-encoded" suffix
-- Eg. encodeFile "myFile.txt"
--     => myFile-encoded.txt
encodeFile :: FilePath -> IO ()
encodeFile path = do
    contents <- (++"\n<<END>>   ") <$> readFile path

    let freqs       = frequencies contents
        encoding    = encode contents freqs
        newContents = unwords $ map (\(x, y) -> show (ord x) ++ "|" ++ show y) freqs
        newFile     = takeBaseName path ++ "-encoded.txt"

    writeFile newFile (newContents ++ "\n")
    BS.appendFile newFile encoding

-- Opposite of encodeFile; file needs to end with "-encoded" suffix
-- Eg. decodeFile "myFile-encoded.txt"
--     => myFile.txt
decodeFile :: FilePath -> IO ()
decodeFile path = do
    handle <- openFile path ReadMode
    freqStr <- hGetLine handle
    encoded <- BS.hGetContents handle

    let freqs = map readFreq (words freqStr)
        decoded = decode encoded freqs
        newFile = case stripSuffix "-encoded" $ takeBaseName path of
                      Just newName -> newName ++ ".txt"

    writeFile newFile decoded
    hClose handle

    where readFreq :: String -> (Char, Int)
          readFreq x = let [(ch, _:freqStr)] = reads x
                       in (chr ch, read freqStr)

-- Eg. frequencies "hello,world"
--     => [(',',1),('d',1),('e',1),('h',1),('l',3),('o',2),('r',1),('w',1)]
frequencies :: [Char] -> [(Char, Int)]
frequencies = map (\list -> (head list, length list)) . group . sort

-- Composition of the following two functions
huffman :: [(Char, Int)] -> Map Char String
huffman = getCodes . buildHuff

-- Builds a Huffman Tree from a list of character frequencies,
-- such as the list returned by function `frequencies`
buildHuff :: [(Char, Int)] -> Huff
buildHuff = buildHuff' . map (uncurry $ flip Leaf) . sortBy (compare `on` snd)
    where
        buildHuff' :: [Huff] -> Huff
        buildHuff' (x:y:xs) = buildHuff' $ insert (Node (weight x + weight y) x y) xs
        buildHuff' [x]      = x

-- Traverses Huffman tree and returns a map of characters to their
-- corresponding Huffman codes
getCodes :: Huff -> Map Char String
getCodes = Map.map reverse . getCodes' ""
    where
        getCodes' :: String -> Huff -> Map Char String
        getCodes' path (Node _ l r) = getCodes' ('0':path) l `Map.union`
                                      getCodes' ('1':path) r
        getCodes' path (Leaf _ c)   = Map.singleton c path


-- Takes a string to encode and a list containing the frequency of each character,
-- returns a compressed Huffman-encoded ByteString
encode :: String -> [(Char, Int)] -> ByteString
encode str (huffman -> codes)
    = let encodedStr = concatMap (fromJust . (`Map.lookup` codes)) str
          bytes      = map binStrToWord8 $ splitEvery 8 encodedStr
          byteString = BS.pack bytes
      in byteString
    where
        binStrToWord8 :: String -> Word8
        binStrToWord8 = foldl (\byte bit -> byte * 2 + read [bit]) 0

        splitEvery :: Int -> [a] -> [[a]]
        splitEvery _ [] = []
        splitEvery n xs = let (chunk, rest) = splitAt n xs
                          in chunk : splitEvery n rest

-- Takes the frequencies of characters and a compressed ByteString,
-- returns an uncompressed String
decode :: ByteString -> [(Char, Int)] -> String
decode bs (buildHuff -> huffTree)
    = let bytes = BS.unpack bs
          binaryStr = concatMap word8toBinStr bytes
      in traverseTreeAndDecode binaryStr huffTree huffTree
    where
        word8toBinStr :: Word8 -> String
        word8toBinStr byte = reverse . take 8 $ toBinary byte ++ repeat '0'
            where toBinary 0 = ""
                  toBinary x
                      | odd x  = '1' : toBinary (x `div` 2)
                      | even x = '0' : toBinary (x `div` 2)


        traverseTreeAndDecode :: String -> Huff -> Huff -> String

        -- If leaf node is reached, add character and go back to root
        traverseTreeAndDecode str root (Leaf _ c)
            = c : traverseTreeAndDecode str root root

        -- Otherwise go left or right depending on code
        traverseTreeAndDecode (x:xs) root (Node _ l r)
            = case x of '0' -> traverseTreeAndDecode xs root l
                        '1' -> traverseTreeAndDecode xs root r

        traverseTreeAndDecode [] _ _ = ""