{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bio.VCF.Parser.Parser
( parseMetaInformation
, parseFormatLine
, parsePatients
, parseChrom
, parsePosition
, parseID
, parseRef
, parseAlt
, parseQual
, parseFilter
, parseInformation
, parseFormat
, parseGenotypes
, parseVariation
) where

import qualified Data.Attoparsec.ByteString.Char8 as AC8 (notChar, char)
import qualified Data.ByteString as B (ByteString, append)
import qualified Data.ByteString.Char8 as BS8 (singleton, words, unpack, split)
import Control.Applicative (liftA2, (<|>))
import Data.Attoparsec.ByteString (try, takeWhile1, takeByteString, skipWhile, Parser, string, takeTill)
import Data.ByteString (ByteString)
import Data.Bool (bool)
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
                skipSeparations *>
                string "POS" *>
                skipSeparations *>
                string "ID" *>
                skipSeparations *>
                string "REF" *>
                skipSeparations *>
                string "ALT" *>
                skipSeparations *>
                string "QUAL" *>
                skipSeparations *>
                string "FILTER" *>
                skipSeparations *>
                string "INFO" *>
                skipSeparations *>
                string "FORMAT" *>
                skipSeparations *>
-- TODO use `sepBy` in this part instead of words to gain performance
                ((fmap . fmap) Patient $
                    BS8.words `fmap` takeTill endOfLine)

parseChrom :: Parser B.ByteString
parseChrom = try (string  "<ID>") <|> takeWhile1 notTabOrSpace

{-We don't care about the additional characters, it should be delegated to
 - the whole parser-}
parsePosition :: Parser Int
parsePosition =  (read . BS8.unpack) `fmap` takeWhile1 isNumber

parseID :: Parser [B.ByteString]
parseID = (BS8.split ':') `fmap` takeWhile1 notTabOrSpace

parseRef :: Parser B.ByteString
parseRef = takeWhile1 isBase

parseAlt :: Parser [B.ByteString]
parseAlt = try (makeList `fmap` string "<ID>") <|>
           (BS8.split ',') `fmap` takeWhile1 isBaseOrDeletion
  where makeList x = x : []

parseQual :: Parser (Maybe Float)
parseQual = (readMaybe . BS8.unpack) `fmap` takeWhile1 isFloatNumber

parseFilter :: Parser [B.ByteString]
parseFilter = try (makeList `fmap` string "PASS") <|>
              (BS8.split ';') `fmap` takeWhile1 notTabOrSpace
  where makeList x = x : []

parseInformation :: Parser [B.ByteString]
parseInformation = (BS8.split ';') `fmap` takeWhile1 notTabOrSpace

parseFormat :: Parser (Maybe [B.ByteString])
parseFormat = try ((Just . BS8.split ':') `fmap` takeWhile1 notTabOrSpace) <|>
                pure Nothing

parseGenotypes :: Parser [Genotypes]
parseGenotypes = try (((fmap (BS8.split ':')) . BS8.split ' ') `fmap` takeByteString) <|>
                  pure []

parseVariation :: Parser (Variation, Genotypes)
parseVariation = (,) <$> Variation <*> 
  parseChrom <*>
  (skipSeparations *> parsePosition) <*>
  (skipSeparations *> parseID) <*>
  (skipSeparations *> parseRef) <*>
  (skipSeparations *> parseAlt) <*>
  (skipSeparations *> parseQual) <*>
  (skipSeparations *> parseFilter) <*>
  (skipSeparations *> parseInformation) <*>
  (skipSeparations *> parseFormat) <*>
  (skipSeparations *> parseGenotypes)

skipSeparations :: Parser ()
skipSeparations = skipWhile tabOrSpace

{-parseVariation :: Parser (Variation, ByteString) -> Parser (Variation, Genotypes)-}
{-parseVariation  Parser (Variation{..}, remaining) = undefined-}
  {-maybeFormat <- parseFormat-}
  {-let variation = Variation vChrom vPos vId vRef vAlt vQual vFilter vInfo Nothing-}
  {-case maybeFormat of-}
    {-Just formats -> do-}
      {-skipWhile tabOrSpace-}
      {-genotypes <- parseGenotypes-}
      {-return (variation{format = Just formats}, genotypes)-}
    {-Nothing -> return (variation, [])-}
