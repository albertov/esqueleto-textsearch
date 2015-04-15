{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Esqueleto.TextSearch.Language (
    TextSearch (..)
) where

import Data.String (IsString)
import Data.Text (Text)

import Database.Esqueleto (Esqueleto, SqlQuery, SqlExpr, SqlBackend, Value)
import Database.Esqueleto.Internal.Sql (unsafeSqlFunction)

import Database.Esqueleto.TextSearch.Types

class Esqueleto query expr backend => TextSearch query expr backend where

  (@@.)
    :: expr (Value TsVector) -> expr (Value (TsQuery Lexemes))
    -> expr (Value Bool)

  to_tsvector
    :: IsString a
    => expr (Value RegConfig) -> expr (Value a) -> expr (Value TsVector)

  to_tsquery
    :: expr (Value RegConfig) -> expr (Value (TsQuery Words))
    -> expr (Value (TsQuery Lexemes) )

  plainto_tsquery
    :: expr (Value RegConfig) -> expr (Value Text)
    -> expr (Value (TsQuery Lexemes))

  ts_rank
    :: expr (Value TsVector) -> expr (Value (TsQuery Lexemes))
    -> expr (Value [NormalizationOption]) -> expr (Value Float)

  ts_rank_cd
    :: expr (Value TsVector) -> expr (Value (TsQuery Lexemes))
    -> expr (Value [NormalizationOption]) -> expr (Value Float)

  setweight
    :: expr (Value TsVector) -> expr (Value Weight) -> expr (Value TsVector)

instance TextSearch SqlQuery SqlExpr SqlBackend where
  to_tsvector a b = unsafeSqlFunction "to_tsvector" (a, b)
  setweight a b = unsafeSqlFunction "setweight" (a, b)
