module Bio.VCF.Types where

import Data.HashMap (Map(..))

data VCF = VCF
          { header  :: Header
          , records :: [Record]
          }

data Header = Header
              { fileFormat        :: String
              , informationFields :: Maybe (Map String InformationField)
              , filterFields      :: Map String FilterField
              , alternativeAlleleField :: Map String 
              }

