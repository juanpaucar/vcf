{-# LANGUAGE OverloadedStrings #-}

module Bio.VCF.Parser.Parser where

import Control.Applicative (liftA2, (<|>))
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as AC8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8 (singleton, words, unpack, split)
import Bio.VCF.Internal.Types
import Bio.VCF.Parser.Helpers

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

parseFilter :: Parser [B.ByteString]
parseFilter = try (makeList `fmap` string "PASS") <|>
              (BS8.split ';') `fmap` takeTill isSpace
  where makeList x = x : []

parseInformation :: Parser [B.ByteString]
parseInformation = (BS8.split ';') `fmap` takeTill isSpace
