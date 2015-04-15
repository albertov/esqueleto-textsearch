{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Esqueleto.TextSearch.Sql (
) where

import Database.Esqueleto.TextSearch.Language (TextSearch(..))
import Database.Esqueleto (SqlQuery, SqlExpr, SqlBackend)
import Database.Esqueleto.Internal.Sql (unsafeSqlFunction)

instance TextSearch SqlQuery SqlExpr SqlBackend where
  to_tsvector a b = unsafeSqlFunction "to_tsvector" (a, b)
  setweight a b = unsafeSqlFunction "setweight" (a, b)
