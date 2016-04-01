module Bio.VCF.Internal.Types where

data VCF = VCF
          { header     :: Header
          , records    :: [Record]
          }

data Header = Header
              { fileFormat              :: String
              , informationFields       :: [InformationField]
              , filterFields            :: [FilterField]
              , formatFields            :: [FormatField]
              , alternativeAlleleFields :: [AlternativeAlleleField]
              , assemblyField           :: String
              , contigFields            :: [ContigField]
              , sampleFields            :: [SampleField]
              , pedigreeFields          :: PedigreeInformation
              }

data Record = Record
              { chrom  :: String --no white space
              , pos    :: Integer
              , id     :: [String] --no white space or semicolon
              , ref    :: String
              , alt    :: [String]
              , qual   :: Float
              , filter :: [String]
              , info   :: [String]
              }

type InformationField = String
type FilterField = String
type FormatField = String
type AlternativeAlleleField = String
type ContigField = String
type SampleField = String
type PedigreeInformation = String
