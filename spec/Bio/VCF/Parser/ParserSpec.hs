{-# LANGUAGE OverloadedStrings #-}
module Bio.VCF.Parser.ParserSpec where

import Test.Hspec

import Bio.VCF.Parser.Parser
import Bio.VCF.Internal.Types
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
      result `shouldBe` Right [Patient "patient1", Patient "patient2", Patient "patient3"]

    it "should return an empty list if there is no information about a pacient" $ do
      let result = parseOnly parsePatients
              "#CHROM  POS ID  REF ALT QUAL  FILTER  INFO  FORMAT"
      result `shouldBe` Right []

    it "should ignore whitespaces at the end of the input" $ do
      let result = parseOnly parsePatients
           "#CHROM  POS ID  REF ALT QUAL  FILTER  INFO  FORMAT patient1 patient2 patient3        "
      result `shouldBe` Right [Patient "patient1", Patient "patient2", Patient "patient3"]

    it "should ignore multiple spaces between during the parsing" $ do
      let result = parseOnly parsePatients
           "#CHROM  POS ID  REF ALT QUAL  FILTER  INFO  FORMAT    patient1   patient2        patient3        "
      result `shouldBe` Right [Patient "patient1", Patient "patient2", Patient "patient3"]

  describe "parseChrom" $ do
    it "should parse an <ID>" $ do
      let result = parseOnly parseChrom "<ID>"
      result `shouldBe` Right "<ID>"

    it "should take a string or an <ID>" $ do
      let result = parseOnly parseChrom "12"
      result `shouldBe` Right "12"

    it "should ignore whitespaces" $ do
      let result = parseOnly parseChrom "23 "
      result `shouldBe` Right "23"


