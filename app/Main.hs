module Main where

import Compression.Huffman
import Compression.LZ77
import Data.Char (toUpper)

main :: IO ()
main = do
    putStrLn "This program compresses files using LZ77 encoding."
    putStrLn "Enter the name of the file:"
    filename <- getLine
    putStrLn "Do you want to COMPRESS or UNCOMPRESS the file?"
    action <- map toUpper <$> getLine
    case action of "COMPRESS"   -> compressFile filename
                   "UNCOMPRESS" -> uncompressFile filename
                   _            -> error "Invalid option"
    putStrLn $ "File " ++ filename ++ " " ++ action ++ "ED."