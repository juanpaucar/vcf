{-# LANGUAGE OverloadedStrings #-}
module Bio.VCF.Parser.ParserSpec where

import Test.Hspec

import Bio.VCF.Parser.Parser
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)

spec :: Spec
spec = do
  describe "parseMetaInformation" $ do
    it "should return a ByteString with the content of the the line" $ do
      let result = parseOnly parseMetaInformation "##This is a line with metaInformation"
      result `shouldBe` Right "This is a line with metaInformation"

    it "should fail when passed an information line" $ do
      let result = parseOnly parseMetaInformation "#This is an information line"
      result `shouldBe` Left "#: Failed reading: satisfyWith"

    it "should fail when it is passed a line with no # at the beginning" $ do
      let result = parseOnly parseMetaInformation "Normal line"
      result `shouldBe` Left "#: Failed reading: satisfyWith"

  describe "parseFormatLine" $ do
    it "should return the content on an information line" $ do
      let result = parseOnly parseFormatLine
            "#CHROM  POS ID  REF ALT QUAL  FILTER  INFO  FORMAT"
      result `shouldBe` Right "CHROM  POS ID  REF ALT QUAL  FILTER  INFO  FORMAT"

    it "should fail when parsing a normal line" $ do
      let result = parseOnly parseFormatLine "Normal line"
      result `shouldBe` Left "#: Failed reading: satisfyWith"

  describe "parsePatients" $ do
    it "should return a list with the names of the patients" $ do
      let result = parseOnly parsePatients
           "#CHROM  POS ID  REF ALT QUAL  FILTER  INFO  FORMAT patient1 patient2 patient3"
      result `shouldBe` Right ["patient1", "patient2", "patient3"]

    it "should return an empty list if there is no information about a pacient" $ do
      let result = parseOnly parsePatients
              "#CHROM  POS ID  REF ALT QUAL  FILTER  INFO  FORMAT"
      result `shouldBe` Right []

    it "should ignore whitespaces at the end of the input" $ do
      let result = parseOnly parsePatients
           "#CHROM  POS ID  REF ALT QUAL  FILTER  INFO  FORMAT patient1 patient2 patient3        "
      result `shouldBe` Right ["patient1", "patient2", "patient3"]

    it "should ignore multiple spaces between during the parsing" $ do
      let result = parseOnly parsePatients
           "#CHROM  POS ID  REF ALT QUAL  FILTER  INFO  FORMAT    patient1   patient2        patient3        "
      result `shouldBe` Right ["patient1", "patient2", "patient3"]


