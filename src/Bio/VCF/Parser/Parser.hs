{-# LANGUAGE OverloadedStrings #-}
module Bio.VCF.Parser.Parser where

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import Data.ByteString.Char8 (singleton)
import Bio.VCF.Internal.Types

parseMetaInformation :: Parser B.ByteString 
parseMetaInformation = do
  char '#'
  char '#'
  rest <- takeByteString
  return rest

parseFormatLine :: Parser B.ByteString
parseFormatLine = do
  char '#'
  next <- fmap singleton (notChar '#')
  rest <- takeByteString
  return $ B.append next rest
