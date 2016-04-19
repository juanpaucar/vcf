{-# LANGUAGE OverloadedStrings #-}

module Bio.VCF.Parser.Parser where

import Control.Applicative (liftA2, (<|>))
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as AC8
import Data.Bits (shiftL)
import Data.Char (ord)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8 (singleton, words, unpack, pack, split)
import Data.Word (Word8)
import Bio.VCF.Internal.Types

tabOrSpace :: Word8 -> Bool
tabOrSpace c = isTab c || isSpace c

isTab :: Word8 -> Bool
isTab c = c == 9

isSpace :: Word8 -> Bool
isSpace c = c == 32

isNumber :: Word8 -> Bool
isNumber c = c >= 48 && c <= 57

isFloatNumber :: Word8 -> Bool
isFloatNumber c = isNumber c || c == 46 -- '.'

isBase :: Word8 -> Bool
isBase c = c == 65 || c == 97  || -- A or a
           c == 67 || c == 99  || -- C or c
           c == 71 || c == 103 || -- G or g
           c == 84 || c == 116 || -- T or t
           c == 78 || c == 110    -- N or n

isBaseOrDeletion :: Word8 -> Bool
isBaseOrDeletion c = isBase c || c == 42 || c == 44 -- or '*' and ','

endOfLine :: Word8 -> Bool
endOfLine c = c == 13 || c == 10

parseMetaInformation :: Parser B.ByteString
parseMetaInformation = AC8.char '#' *>
                       AC8.char '#' *>
                       takeByteString

parseFormatLine :: Parser B.ByteString
parseFormatLine = AC8.char '#' *>
                  liftA2 B.append
                         (BS8.singleton `fmap` AC8.notChar '#')
                         takeByteString

parsePatients :: Parser [Patient]
parsePatients = AC8.char '#' *>
                string "CHROM" *>
                skipWhile tabOrSpace *>
                string "POS" *>
                skipWhile tabOrSpace *>
                string "ID" *>
                skipWhile tabOrSpace *>
                string "REF" *>
                skipWhile tabOrSpace *>
                string "ALT" *>
                skipWhile tabOrSpace *>
                string "QUAL" *>
                skipWhile tabOrSpace *>
                string "FILTER" *>
                skipWhile tabOrSpace *>
                string "INFO" *>
                skipWhile tabOrSpace *>
                string "FORMAT" *>
                skipWhile tabOrSpace *>
--TODO use `sepBy` in this part instead of words to gain performance
                ((fmap . fmap) Patient $
                    BS8.words `fmap` takeTill endOfLine)

parseChrom :: Parser B.ByteString
parseChrom = try (string  "<ID>") <|> takeTill isSpace

{-We don't care about the additional characters, it should be delegated to
 - the whole parser-}
parsePosition :: Parser Int
parsePosition =  (read . BS8.unpack) `fmap` takeWhile1 isNumber

parseID :: Parser [B.ByteString]
parseID = (BS8.split ':') `fmap` takeTill isSpace

parseRef :: Parser B.ByteString
parseRef = takeWhile1 isBase

parseAlt :: Parser [B.ByteString]
parseAlt = try (makeList `fmap` string "<ID>") <|>
           (BS8.split ',') `fmap` takeWhile1 isBaseOrDeletion
  where makeList x = x : []

parseQual :: Parser Float
parseQual = (read . BS8.unpack) `fmap` takeWhile1 isFloatNumber
