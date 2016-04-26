module Bio.VCF.Parser.Helpers
( tabOrSpace
, isTab
, isSpace
, notTabOrSpace
, isNumber
, isFloatNumber
, isBase
, isBaseOrDeletion
, endOfLine
) where

import Data.Word (Word8)

tabOrSpace :: Word8 -> Bool
tabOrSpace c = isTab c || isSpace c

isTab :: Word8 -> Bool
isTab c = c == 9

isSpace :: Word8 -> Bool
isSpace c = c == 32

notTabOrSpace :: Word8 -> Bool
notTabOrSpace = not . tabOrSpace

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
