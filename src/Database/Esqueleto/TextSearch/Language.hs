{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Esqueleto.TextSearch.Language (
    TextSearch (..)
) where

import Database.Esqueleto
import Database.Esqueleto.TextSearch.Types
import Data.String (IsString)
import Data.Text (Text)

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
