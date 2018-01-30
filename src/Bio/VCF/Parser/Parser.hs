{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bio.VCF.Parser.Parser where

import qualified Data.Attoparsec.ByteString.Char8 as AC8 (notChar, char)
import qualified Data.ByteString as B (ByteString, append)
import qualified Data.ByteString.Char8 as BS8 (singleton, words, unpack, split)
import Control.Applicative (liftA2, (<|>), (<$>))
import Data.Attoparsec.ByteString (try, takeWhile1, takeByteString, skipWhile, Parser, string, takeTill)
import Text.Read (readMaybe)

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
                ((fmap Patient . BS8.words) <$> takeTill endOfLine)

parseChrom :: Parser B.ByteString
parseChrom = try (string  "<ID>") <|> takeWhile1 notTabOrSpace

{-We don't care about the additional characters, it should be delegated to
 - the whole parser-}
parsePosition :: Parser Int
parsePosition =  read . BS8.unpack <$> takeWhile1 isNumber

parseID :: Parser [B.ByteString]
parseID = BS8.split ':' <$> takeWhile1 notTabOrSpace

parseRef :: Parser B.ByteString
parseRef = takeWhile1 isBase

parseAlt :: Parser [B.ByteString]
parseAlt = try (makeList <$> string "<ID>") <|>
           BS8.split ',' <$> takeWhile1 isBaseOrDeletion
  where makeList = pure

parseQual :: Parser (Maybe Float)
parseQual = readMaybe . BS8.unpack <$> takeWhile1 isFloatNumber

parseFilter :: Parser [B.ByteString]
parseFilter = try (makeList <$> string "PASS") <|>
              BS8.split ';' <$> takeWhile1 notTabOrSpace
  where makeList = pure

parseInformation :: Parser [B.ByteString]
parseInformation = BS8.split ';' <$> takeWhile1 notTabOrSpace

parseFormat :: Parser (Maybe [B.ByteString])
parseFormat = try ((Just . BS8.split ':') <$> takeWhile1 notTabOrSpace) <|>
                pure Nothing

parseGenotypes :: Parser [Genotypes]
parseGenotypes = fmap (BS8.split ':') . BS8.split ' ' <$> takeByteString

parseVariation :: Parser (Variation, [Genotypes])
parseVariation = do
  chrom <- parseChrom
  skipWhile tabOrSpace
  pos <- parsePosition
  skipWhile tabOrSpace
  idx <- parseID
  skipWhile tabOrSpace
  ref <- parseRef
  skipWhile tabOrSpace
  alt <- parseAlt
  skipWhile tabOrSpace
  qual <- parseQual
  skipWhile tabOrSpace
  filt <- parseFilter
  skipWhile tabOrSpace
  info <- parseInformation
  skipWhile tabOrSpace
  format <- parseFormat
  (,) <$> pure Variation{..}
      <*> getGenotypes format
    where
      getGenotypes Nothing = pure []
      getGenotypes (Just _) =
        skipWhile tabOrSpace *>
        parseGenotypes
