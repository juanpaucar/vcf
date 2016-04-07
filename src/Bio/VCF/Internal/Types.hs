module Bio.VCF.Internal.Types where

import Data.ByteString (ByteString(..))

data VCF = VCF
          { header     :: Header
          , variations :: [Variation]
          } deriving Show

data Header = Header
              { fileFormat              :: ByteString
              , informationFields       :: [InformationField]
              , filterFields            :: [FilterField]
              , formatFields            :: [FormatField]
              , alternativeAlleleFields :: [AlternativeAlleleField]
              , assemblyField           :: ByteString
              , contigFields            :: [ContigField]
              , sampleFields            :: [SampleField]
              , pedigreeFields          :: PedigreeInformation
              } deriving Show

data Variation = Variation
              { chrom  :: ByteString --no white space
              , pos    :: Integer
              , idx    :: [ByteString] --no white space or semicolon
              , ref    :: ByteString
              , alt    :: [ByteString]
              , qual   :: Float
              , filt   :: [ByteString]
              , info   :: [ByteString]
              } deriving Show

type InformationField = ByteString
type FilterField = ByteString
type FormatField = ByteString
type AlternativeAlleleField = ByteString
type ContigField = ByteString
type SampleField = ByteString
type PedigreeInformation = ByteString
