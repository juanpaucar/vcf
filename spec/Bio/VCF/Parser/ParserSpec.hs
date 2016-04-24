{-# LANGUAGE OverloadedStrings #-}

module Bio.VCF.Parser.ParserSpec where

import Test.Hspec

import Bio.VCF.Parser.Parser
import Bio.VCF.Internal.Types
import Data.Attoparsec.ByteString

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

  describe "parsePosition" $ do
    it "should parse a valid number" $ do
      let result = parseOnly parsePosition "1123123123  "
      result `shouldBe` Right 1123123123

    it "should fail when an invalid input is given" $ do
      let result = parseOnly parsePosition "12a"
      result `shouldBe` Right 12

    it "should fail when non numbers chars are passed" $ do
      let result = parseOnly parsePosition "hello"
      result `shouldBe` Left "Failed reading: takeWhile1"

  describe "parseID" $ do
    it "should parse a single ID" $ do
      let result = parseOnly parseID "rs123123"
      result `shouldBe` Right ["rs123123"]

    it "should parse a list of IDs" $ do
      let result = parseOnly parseID "rs123123:rs234234:rs345345"
      result `shouldBe` Right ["rs123123","rs234234","rs345345"]

  describe "parseRef" $ do
    it "should read only bases and N" $ do
      let result = parseOnly parseRef "ACTGN "
      result `shouldBe` Right "ACTGN"

    it "should be case insentitive" $ do
      let result = parseOnly parseRef "AcTgNCcAaTtGn  "
      result `shouldBe` Right "AcTgNCcAaTtGn"

    it "should fail when invalid caharacters for a base are passed" $ do
      let result = parseOnly parseRef "Hello"
      result `shouldBe` Left "Failed reading: takeWhile1"

    it "should parse single bases" $ do
      let result = parseOnly parseRef "A "
      result `shouldBe` Right "A"

  describe "parseAlt" $ do
    it "should parse an <ID>" $ do
      let result = parseOnly parseAlt "<ID> "
      result `shouldBe` Right ["<ID>"]

    it "should parse multiple alterations" $ do
      let result = parseOnly parseAlt "ACTG,GGT,AGTCCC  "
      result `shouldBe` Right ["ACTG", "GGT", "AGTCCC"]

    it "should parse a deletion '*'" $ do
      let result = parseOnly parseAlt "* "
      result `shouldBe` Right ["*"]

  describe "parseQual" $ do
    it "should parse valid numbers" $ do
      let result = parseOnly parseQual "123.123 "
      result `shouldBe` Right 123.123

    it "should parse integer values as float" $ do
      let result = parseOnly parseQual "123  "
      result `shouldBe` Right 123

    it "should fail to parse other than numbers" $ do
      let result = parseOnly parseQual "ads"
      result `shouldBe` Left "Failed reading: takeWhile1"

  describe "parseFilter" $ do
    it "should parse a PASS filter" $ do
      let result = parseOnly parseFilter "PASS "
      result `shouldBe` Right ["PASS"]

    it "should parse not passed filters" $ do
      let result = parseOnly parseFilter "q10;s50 "
      result `shouldBe` Right ["q10", "s50"]

  describe "parseInformation" $ do
    it "should stop at a whitespace" $ do
      let result = parseOnly parseInformation "AC=2;AF=1.00;AN=2;DB;DP=11 "
      result `shouldBe` Right ["AC=2", "AF=1.00", "AN=2", "DB", "DP=11"]

    it "should split all the results with the ';'" $ do
      let result = parseOnly parseInformation "AC=2;AF=1.00;AN=2;DB;DP=11; "
      result `shouldBe` Right ["AC=2", "AF=1.00", "AN=2", "DB", "DP=11", ""]

  describe "parseFormat" $ do
    it "should return a list with the genotypes" $ do
      let result = parseOnly parseFormat "GT:AD:DP:GQ:PL  "
      result `shouldBe` Right (Just ["GT", "AD", "DP", "GQ", "PL"])

    it "should return Nothing when there's no input" $ do
      let result = parseOnly parseFormat ""
      result `shouldBe` Right (Nothing)
