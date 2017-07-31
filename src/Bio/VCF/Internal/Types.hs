{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

module Bio.VCF.Internal.Types
( VCF(..)
, Header(..)
, Variation(..)
, Genotypes
, Patient(..)
) where


import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics

-------------------------------------------------------------------------------
-- Orphan instances for JSON from String, we are not really using BSON
-------------------------------------------------------------------------------

instance FromJSON ByteString where
  parseJSON (String a) = pure $ encodeUtf8 a
  parseJSON invalid = typeMismatch "ByteString" invalid

instance ToJSON ByteString where
  toJSON = String . decodeUtf8

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

data VCF = VCF
          { header     :: !Header
          , variations :: ![(Variation, [ByteString])]
          } deriving (Eq, Generic, Show)

instance FromJSON VCF

instance ToJSON VCF


data Header = Header
              { fileFormat              :: !ByteString
              , informationFields       :: ![InformationField]
              , filterFields            :: ![FilterField]
              , formatFields            :: ![FormatField]
              , alternativeAlleleFields :: ![AlternativeAlleleField]
              , assemblyField           :: !ByteString
              , contigFields            :: ![ContigField]
              , sampleFields            :: ![SampleField]
              , pedigreeFields          :: !PedigreeInformation
              } deriving (Eq, Generic, Show)

instance FromJSON Header

instance ToJSON Header


data Variation = Variation
              { chrom  :: !ByteString
              , pos    :: !Int
              , idx    :: ![ByteString]
              , ref    :: !ByteString
              , alt    :: ![ByteString]
              , qual   :: !(Maybe Float)
              , filt   :: ![ByteString]
              , info   :: ![ByteString]
              , format :: !(Maybe [ByteString])
              } deriving (Eq, Generic, Show)

instance FromJSON Variation

instance ToJSON Variation


newtype Patient = Patient ByteString deriving (Eq, Generic, Show)

instance FromJSON Patient

instance ToJSON Patient


-- TODO: Probably it's a good idea to put these type alias inside their own
-- newtypes or check for a better way to represent them
--
type InformationField = ByteString

type FilterField = ByteString

type FormatField = ByteString

type AlternativeAlleleField = ByteString

type ContigField = ByteString

type SampleField = ByteString

type PedigreeInformation = ByteString

type Genotypes = [ByteString]

