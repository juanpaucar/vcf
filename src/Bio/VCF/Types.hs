module Bio.VCF.Types where

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
              { chrom :: String --no white space
              , pos   :: Integer
              , id    :: [String] --no white space or semicolon
              , ref   :: 
              }
