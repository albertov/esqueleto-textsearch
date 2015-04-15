{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module Database.Esqueleto.TextSearch.Types (
    TsQuery (..)
  , Words
  , Lexemes
  , TsVector
  , RegConfig
  , NormalizationOption
  , Weight (..)
) where

import Data.Monoid ((<>))
import Data.String (IsString(fromString))
import Data.Text (Text, singleton)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Database.Persist
import Database.Persist.Postgresql

data NormalizationOption
  = NormNone
  | Norm1LogLength
  | NormLength
  | NormMeanHarmDist
  | NormUniqueWords
  | Norm1LogUniqueWords
  | Norm1Self
  deriving (Eq, Show)

data Weight
  = Hightest
  | High
  | Medium
  | Low
  deriving (Eq, Show)

weightToChar :: Weight -> Char
weightToChar Hightest = 'A'
weightToChar High     = 'B'
weightToChar Medium   = 'C'
weightToChar Low      = 'D'

instance PersistField Weight where
  toPersistValue = PersistText . singleton . weightToChar
  fromPersistValue (PersistText "A") = Right Hightest
  fromPersistValue (PersistText "B") = Right High
  fromPersistValue (PersistText "C") = Right Medium
  fromPersistValue (PersistText "D") = Right Low
  fromPersistValue (PersistText _)
    = Left "TextSearch/Weight: Unexpected character"
  fromPersistValue f
    = Left $ "TextSearch/Weight: Unexpected Persist field: " <> tShow f
instance PersistFieldSql Weight where
  sqlType = const (SqlOther "char")

data QueryType = Words | Lexemes
type Lexemes = 'Lexemes
type Words = 'Words

data TsQuery (a :: QueryType) where
  Lexeme :: Text -> TsQuery Lexemes
  Word   :: Text -> TsQuery Words
  Raw    :: Text -> TsQuery Lexemes
  (:&)   :: TsQuery a -> TsQuery a -> TsQuery a
  (:|)   :: TsQuery a -> TsQuery a -> TsQuery a
  Not    :: TsQuery a -> TsQuery a

deriving instance Show (TsQuery a)

newtype TsVector = TsVector {unTsVector::Text} deriving (Eq, Show, IsString)


instance PersistField TsVector where
  toPersistValue = PersistDbSpecific . encodeUtf8 . unTsVector
  fromPersistValue (PersistDbSpecific bs) = Right $ TsVector $ decodeUtf8 bs
  fromPersistValue f
    = Left $ "TextSearch/TsVector: Unexpected Persist field: " <> tShow f
instance PersistFieldSql TsVector where
  sqlType = const (SqlOther "tsvector")


newtype RegConfig = RegConfig {unRegConfig::Text} deriving (Eq, Show, IsString)

instance PersistField RegConfig where
  toPersistValue = PersistDbSpecific . encodeUtf8 . unRegConfig
  fromPersistValue (PersistDbSpecific bs) = Right $ RegConfig $ decodeUtf8 bs
  fromPersistValue f
    = Left $ "TextSearch/RegConfig: Unexpected Persist field: " <> tShow f
instance PersistFieldSql RegConfig where
  sqlType = const (SqlOther "regconfig")

tShow :: Show a => a -> Text
tShow = fromString . show
