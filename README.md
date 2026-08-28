# Compression

Implementations of Huffman coding and LZ77 compression in Haskell, originally written in 2021.

The project contains implementations of:

Huffman encoding/decoding
LZ77 compression/decompression

## Status

Archived project originally written in 2021 while learning Haskell.

## How to run

```
cabal run
```

The current executable demonstrates the LZ77 implementation; the Huffman implementation is available in the source.

## Implementation

### Huffman coding

The Huffman implementation builds a binary Huffman tree from character frequencies and traverses the tree to generate variable-length prefix codes. The encoded bitstream is packed into bytes and the frequency table is stored alongside it so that the original file can be reconstructed during decoding.

### LZ77

The LZ77 implementation uses a sliding-window scheme with a 15,000-byte search buffer and a 250-byte look-ahead buffer. Repeated substrings are represented as `(distance, length)` pairs, while short or non-repeated sequences are stored as individual bytes.

The project contains two LZ77 implementations. The second experiments with representing the input as an unboxed `Vector Word8` rather than `ByteString`, together with custom substring-matching routines.

### Haskell

The project uses `Data.Map`, `Data.Vector.Unboxed`, `ByteString`, and lazy `ByteString` for the different stages of compression and file handling.
