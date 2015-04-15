{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Esqueleto.TextSearchSpec (main, spec) where

import Control.Monad (forM_)
import Data.Maybe (isJust)
import Data.Text (Text)

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (MonadLogger(..), runStderrLoggingT)
import Control.Monad.Trans.Resource (
  MonadBaseControl, MonadThrow, ResourceT, runResourceT)
import Database.Esqueleto (
    SqlExpr, Value, update, set, val, just, (=.), (^.))
import Database.Persist (insert, get)
import Database.Persist.Postgresql (
    SqlPersistT, ConnectionString, runSqlConn, transactionUndo
  , withPostgresqlConn, runMigration)
import Database.Persist.TH (
  mkPersist, mkMigrate, persistUpperCase, share, sqlSettings)
import Test.Hspec (Spec, hspec, describe, it, shouldBe)

import Database.Esqueleto.TextSearch

connString :: ConnectionString
connString = "host=localhost port=5432 user=test dbname=test password=test"

-- Test schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
  Article
    title      Text
    content    Text
    textsearch TsVector Maybe
    deriving Eq Show

  WeightModel
    weight     Weight
    deriving Eq Show

  RegConfigModel
    config     RegConfig
    deriving Eq Show
|]


main :: IO ()
main = hspec spec

to_etsvector :: SqlExpr (Value Text) -> SqlExpr (Value TsVector)
to_etsvector = to_tsvector (val "english")

spec :: Spec
spec = do
  describe "TsVector column" $ do
    it "can set it" $ run $ do
      let article = Article "some title" "some content" Nothing
      arId <- insert article
      update  $ \a -> do
        set a [ArticleTextsearch =. just (to_etsvector (a^.ArticleContent))]
      Just ret <- get arId
      liftIO $ isJust (articleTextsearch ret) `shouldBe` True

    it "can set it with weight" $ run $ do
      let article = Article "some title" "some content" Nothing
      arId <- insert article
      update  $ \a -> do
        set a [  ArticleTextsearch
              =. just (setweight (to_etsvector (a^.ArticleContent))
                                 (val Highest))
              ]
      Just ret <- get arId
      liftIO $ isJust (articleTextsearch ret) `shouldBe` True

  describe "Weight column" $ do
    it "can de/serialize it" $ run $ do
      forM_ [Low, Medium, High, Highest] $ \w -> do
        let m = WeightModel w
        wId <- insert m
        ret <- get wId
        liftIO $ ret `shouldBe` Just m

  describe "RegConfig column" $ do
    it "can de/serialize it" $ run $ do
      forM_ ["english", "spanish"] $ \c -> do
        let m = RegConfigModel c
        wId <- insert m
        ret <- get wId
        liftIO $ ret `shouldBe` Just m
      

type RunDbMonad m
  = (MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m)

run :: (forall m. RunDbMonad m => SqlPersistT (ResourceT m) a) -> IO a
run act
  = runStderrLoggingT
  . runResourceT
  . withPostgresqlConn connString
  . runSqlConn
  $ (initializeDB >> act >>= \ret -> transactionUndo >> return ret)


initializeDB
  :: (forall m. RunDbMonad m
  => SqlPersistT (ResourceT m) ())
initializeDB  = do
  runMigration migrateAll
