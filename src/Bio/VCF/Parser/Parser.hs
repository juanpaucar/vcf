{-# LANGUAGE OverloadedStrings #-}

module Bio.VCF.Parser.Parser where

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as AC8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8 (singleton, words)
import Data.Word (Word8)
import Bio.VCF.Internal.Types

tabOrSpace :: Word8 -> Bool
tabOrSpace c = c == 32 || c == 9

isTab :: Word8 -> Bool
isTab c = c == 9

endOfLine :: Word8 -> Bool
endOfLine c = c == 13 || c == 10

parseMetaInformation :: Parser B.ByteString 
parseMetaInformation = do
  AC8.char '#'
  AC8.char '#'
  rest <- takeByteString
  return rest

parseFormatLine :: Parser B.ByteString
parseFormatLine = do
  AC8.char '#'
  next <- fmap BS8.singleton (AC8.notChar '#')
  rest <- takeByteString
  return $ B.append next rest

parsePatients :: Parser [B.ByteString]
parsePatients = do
  AC8.char '#'
  string "CHROM"
  skipWhile tabOrSpace
  string "POS"
  skipWhile tabOrSpace
  string "ID"
  skipWhile tabOrSpace
  string "REF"
  skipWhile tabOrSpace
  string "ALT"
  skipWhile tabOrSpace
  string "QUAL"
  skipWhile tabOrSpace
  string "FILTER"
  skipWhile tabOrSpace
  string "INFO"
  skipWhile tabOrSpace
  string "FORMAT"
  skipWhile tabOrSpace
  --TODO use `sepBy` in this part instead of words to gain performance
  names <- fmap BS8.words $ takeTill endOfLine
  return names
