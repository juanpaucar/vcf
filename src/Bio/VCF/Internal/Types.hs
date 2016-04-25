module Bio.VCF.Internal.Types
( VCF(..)
, Header(..)
, Variation(..)
, Genotypes
, Patient(..)
) where


import Data.ByteString (ByteString)

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
              { chrom  :: ByteString
              , pos    :: Int
              , idx    :: [ByteString]
              , ref    :: ByteString
              , alt    :: [ByteString]
              , qual   :: Maybe Float
              , filt   :: [ByteString]
              , info   :: [ByteString]
              , format :: Maybe [ByteString]
              } deriving (Show, Eq)

newtype Patient = Patient ByteString deriving (Eq, Show)
type InformationField = ByteString
type FilterField = ByteString
type FormatField = ByteString
type AlternativeAlleleField = ByteString
type ContigField = ByteString
type SampleField = ByteString
type PedigreeInformation = ByteString
type Genotypes = [ByteString]
