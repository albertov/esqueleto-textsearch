{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Esqueleto.TextSearch.Types (
    TsQuery (..)
  , Words
  , Lexemes
  , TsVector
  , RegConfig
  , NormalizationOption
  , Weight (..)
  , Position (..)
  , word
  , queryToText
  , textToQuery
) where

import Control.Applicative (pure, many, optional, (<$>), (*>), (<*), (<|>))
import Data.Monoid ((<>))
import Data.String (IsString(fromString))

import Text.Parsec (
  ParseError, runParser, char, eof, between, choice, spaces, satisfy, many1)
import qualified Text.Parsec.Expr as P

import Data.Text (Text, singleton)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, toLazyText, fromText)
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
  = Highest
  | High
  | Medium
  | Low
  deriving (Eq, Show)

weightToChar :: Weight -> Char
weightToChar Highest = 'A'
weightToChar High    = 'B'
weightToChar Medium  = 'C'
weightToChar Low     = 'D'

instance PersistField Weight where
  toPersistValue = PersistText . singleton . weightToChar
  fromPersistValue (PersistText "A") = Right Highest
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

data Position = Prefix | Infix deriving (Show, Eq)

data TsQuery (a :: QueryType) where
  Lexeme :: Position -> [Weight] -> Text -> TsQuery Lexemes
  Word   :: Position -> [Weight] -> Text -> TsQuery Words
  (:&)   :: TsQuery a -> TsQuery a -> TsQuery a
  (:|)   :: TsQuery a -> TsQuery a -> TsQuery a
  Not    :: TsQuery a -> TsQuery a

infixr 3 :&
infixr 2 :|

deriving instance Show (TsQuery a)
deriving instance Eq (TsQuery a)

instance a ~ Words => IsString (TsQuery a) where
  fromString = word . fromString

word :: Text -> TsQuery Words
word = Word Infix []


queryToText :: TsQuery a -> Text
queryToText = toStrict . toLazyText . build . unsafeAsLexeme
  where
    build :: TsQuery Lexemes -> Builder
    build (Lexeme Infix [] s)    = "'" <> fromText s <> "'"
    build (Lexeme Infix ws s)    = "'" <> fromText s <> "':"  <> buildWeights ws
    build (Lexeme Prefix ws s)   = "'" <> fromText s <> "':*" <> buildWeights ws
    build (a :& b)               = parens a <> "&" <> parens b
    build (a :| b)               = parens a <> "|" <> parens b
    build (Not q)                = "!" <> parens q
    buildWeights                 = fromText . fromString . map weightToChar
    unsafeAsLexeme :: TsQuery a -> TsQuery Lexemes
    unsafeAsLexeme q@Lexeme{}    = q
    unsafeAsLexeme (Word p ws s) = Lexeme p ws s
    unsafeAsLexeme (a :& b)      = unsafeAsLexeme a :& unsafeAsLexeme b
    unsafeAsLexeme (a :| b)      = unsafeAsLexeme a :| unsafeAsLexeme b
    unsafeAsLexeme (Not q)       = Not (unsafeAsLexeme q)
    parens a@Lexeme{}            = build a
    parens a                     = "(" <> build a <> ")"

textToQuery :: Text -> Either ParseError (TsQuery Lexemes)
textToQuery = runParser (expr <* eof) () ""
  where
    expr    = spaced (P.buildExpressionParser table (spaced term))
    term    = parens expr
          <|> lexeme
    table   = [ [P.Prefix (char '!' *> pure Not)]
              , [P.Infix  (char '&' *> pure (:&)) P.AssocRight]
              , [P.Infix  (char '|' *> pure (:|)) P.AssocRight]
              ]
    lexeme = do
      s   <- fromString <$> quoted (many1 (satisfy (/='\'')))
      _   <- optional (char ':')
      pos <- char '*' *> pure Prefix <|> pure Infix
      ws  <- many weight
      return $ Lexeme pos ws s
    weight = choice [ char 'A' *> pure Highest
                    , char 'B' *> pure High
                    , char 'C' *> pure Medium
                    , char 'D' *> pure Low]
    spaced = between (optional spaces) (optional spaces)
    quoted = between (char '\'') (char '\'')
    parens = between (char '(') (char ')')

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
