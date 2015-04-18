{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Esqueleto.TextSearchSpec (main, spec) where

import Control.Monad (forM_)
import Data.Text (Text, pack)

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger (MonadLogger(..), runStderrLoggingT)
import Control.Monad.Trans.Resource (
  MonadBaseControl, MonadThrow, ResourceT, runResourceT)
import Database.Esqueleto (
    SqlExpr, Value(..), unValue, update, select, set, val, from, where_
  , (=.), (^.))
import Database.Persist (entityKey, insert, get, PersistField(..))
import Database.Persist.Postgresql (
    SqlPersistT, ConnectionString, runSqlConn, transactionUndo
  , withPostgresqlConn, runMigration)
import Database.Persist.TH (
  mkPersist, mkMigrate, persistUpperCase, share, sqlSettings)
import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import Test.QuickCheck (
    Arbitrary(..), property, elements, oneof, listOf, listOf1, choose)

import Database.Esqueleto.TextSearch

connString :: ConnectionString
connString = "host=localhost port=5432 user=test dbname=test password=test"

-- Test schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
  Article
    title      Text
    content    Text
    textsearch TsVector
    deriving Eq Show

  WeightsModel
    weights     Weights
    deriving Eq Show

  WeightModel
    weight     Weight
    deriving Eq Show

  RegConfigModel
    config     RegConfig
    deriving Eq Show

  QueryModel
    query     (TsQuery Lexemes)
    deriving Eq Show
|]


main :: IO ()
main = hspec spec

to_etsvector :: SqlExpr (Value Text) -> SqlExpr (Value TsVector)
to_etsvector = to_tsvector (val "english")

spec :: Spec
spec = do
  describe "TsVector" $ do
    it "can be persisted and retrieved" $ run $ do
      let article = Article "some title" "some content" def
      arId <- insert article
      update  $ \a -> do
        set a [ArticleTextsearch =. to_etsvector (a^.ArticleContent)]
      Just ret <- get arId
      liftIO $ articleTextsearch ret /= def `shouldBe` True

    it "can be persisted and retrieved with weight" $ run $ do
      let article = Article "some title" "some content" def
      arId <- insert article
      update  $ \a -> do
        set a [  ArticleTextsearch
              =. setweight (to_etsvector (a^.ArticleContent)) (val Highest)
              ]
      Just ret <- get arId
      liftIO $ articleTextsearch ret /= def `shouldBe` True

  describe "Weight" $ do
    it "can be persisted and retrieved" $ run $ do
      forM_ [Low, Medium, High, Highest] $ \w -> do
        let m = WeightModel w
        wId <- insert m
        ret <- get wId
        liftIO $ ret `shouldBe` Just m

  describe "Weights" $ do
    it "can be persisted and retrieved" $ run $ do
      let m = WeightsModel $ Weights 0.5 0.6 0.7 0.8
      wsId <- insert m
      ret <- get wsId
      liftIO $ ret `shouldBe` Just m

  describe "RegConfig" $ do
    it "can be persisted and retrieved" $ run $ do
      forM_ ["english", "spanish"] $ \c -> do
        let m = RegConfigModel c
        wId <- insert m
        ret <- get wId
        liftIO $ ret `shouldBe` Just m
      

  describe "TsQuery" $ do
    it "can be persisted and retrieved" $ run $ do
      let qm = QueryModel (lexm "foo" :& lexm "bar")
      qId <- insert qm
      Just ret <- get qId
      liftIO $ qm `shouldBe` ret

    describe "to_tsquery" $ do
      it "converts words to lexemes" $ run $ do
        [Value lq ] <- select $ return $ 
          to_tsquery (val "english") (val ("supernovae" :& "rats"))
        liftIO $ lq `shouldBe` (lexm "supernova" :& lexm "rat")

    describe "plainto_tsquery" $ do
      it "converts text to lexemes" $ run $ do
        [Value lq ] <- select $ return $ 
          plainto_tsquery (val "english") (val "rats in supernovae")
        liftIO $ lq `shouldBe` (lexm "rat" :& lexm "supernova")

    describe "queryToText" $ do
      it "can serialize infix lexeme" $
        queryToText (lexm "foo") `shouldBe` "'foo'"
      it "can serialize infix lexeme with weights" $
        queryToText (Lexeme Infix [Highest,Low] "foo") `shouldBe` "'foo':AD"
      it "can serialize prefix lexeme" $
        queryToText (Lexeme Prefix [] "foo") `shouldBe` "'foo':*"
      it "can serialize prefix lexeme with weights" $
        queryToText (Lexeme Prefix [Highest,Low] "foo") `shouldBe` "'foo':*AD"
      it "can serialize AND" $
        queryToText ("foo" :& "bar" :& "car") `shouldBe` "'foo'&('bar'&'car')"
      it "can serialize OR" $
        queryToText ("foo" :| "bar") `shouldBe` "'foo'|'bar'"
      it "can serialize Not" $
        queryToText (Not "bar") `shouldBe` "!'bar'"

    describe "textToQuery" $ do
      describe "infix lexeme" $ do
        it "can parse it" $
          textToQuery "'foo'" `shouldBe` Right (lexm "foo")
        it "can parse it surrounded by spaces" $
          textToQuery " 'foo' " `shouldBe` Right (lexm "foo")

      describe "infix lexeme with weights" $ do
        it "can parse it" $
          textToQuery "'foo':AB"
            `shouldBe` Right (Lexeme Infix [Highest,High] "foo")
        it "can parse it surrounded by spaces" $
          textToQuery " 'foo':AB "
            `shouldBe` Right (Lexeme Infix [Highest,High] "foo")

      describe "prefix lexeme" $ do
        it "can parse it" $
          textToQuery "'foo':*" `shouldBe` Right (Lexeme Prefix [] "foo")
        it "can parse it surrounded byb spaces" $
          textToQuery " 'foo':* " `shouldBe` Right (Lexeme Prefix [] "foo")

      describe "prefix lexeme with weights" $ do
        it "can parse it" $
          textToQuery "'foo':*AB" `shouldBe`
            Right (Lexeme Prefix [Highest,High] "foo")
        it "can parse it surrounded by spaces" $
          textToQuery " 'foo':*AB " `shouldBe`
            Right (Lexeme Prefix [Highest,High] "foo")

      describe "&" $ do
        it "can parse it" $
          textToQuery "'foo'&'bar'" `shouldBe`
            Right (lexm "foo" :& lexm "bar")
        it "can parse it surrounded by spaces" $ do
          textToQuery "'foo' & 'bar'" `shouldBe`
            Right (lexm "foo" :& lexm "bar")
          textToQuery "'foo'& 'bar'" `shouldBe`
            Right (lexm "foo" :& lexm "bar")
          textToQuery "'foo' &'bar'" `shouldBe`
            Right (lexm "foo" :& lexm "bar")
          textToQuery " 'foo'&'bar' " `shouldBe`
            Right (lexm "foo" :& lexm "bar")
        it "can parse several" $
          textToQuery "'foo'&'bar'&'car'" `shouldBe`
            Right (lexm "foo" :& lexm "bar" :& lexm "car")

      describe "|" $ do
        it "can parse it" $
          textToQuery "'foo'|'bar'" `shouldBe` Right (lexm "foo" :| lexm "bar")
        it "can parse several" $
          textToQuery "'foo'|'bar'|'car'" `shouldBe`
            Right (lexm "foo" :| lexm "bar" :| lexm "car")

      describe "mixed |s and &s" $ do
        it "respects precedence" $ do
          textToQuery "'foo'|'bar'&'car'" `shouldBe`
            Right (lexm "foo" :| lexm "bar" :& lexm "car")
          textToQuery "'foo'&'bar'|'car'" `shouldBe`
            Right (lexm "foo" :& lexm "bar" :| lexm "car")

      describe "!" $ do
        it "can parse it" $
          textToQuery "!'foo'" `shouldBe` Right (Not (lexm "foo"))

      describe "! and &" $ do
        it "can parse it" $ do
          textToQuery "!'foo'&'car'" `shouldBe`
            Right (Not (lexm "foo") :& lexm "car")
          textToQuery "!('foo'&'car')" `shouldBe`
            Right (Not (lexm "foo" :& lexm "car"))
        it "can parse it surrounded by spaces" $ do
          textToQuery "!'foo' & 'car'" `shouldBe`
            Right (Not (lexm "foo") :& lexm "car")
          textToQuery "!( 'foo' & 'car' )" `shouldBe`
            Right (Not (lexm "foo" :& lexm "car"))

    describe "textToQuery . queryToText" $ do
      it "is isomorphism" $ property $ \q ->
        (textToQuery . queryToText) q `shouldBe` Right q

  describe "@@" $ do
    it "works as expected" $ run $ do
      let article = Article "some title" "some content" def
      arId <- insert article
      update  $ \a -> do
        set a [ArticleTextsearch =. to_etsvector (a^.ArticleContent)]
      let query = to_tsquery (val "english") (val "content")
      result <- select $ from $ \a -> do
        where_ $ (a^. ArticleTextsearch) @@. query
        return a
      liftIO $ do
        length result `shouldBe` 1
        map entityKey result `shouldBe` [arId]
      let query2 = to_tsquery (val "english") (val "foo")
      result2 <- select $ from $ \a -> do
        where_ $ (a^. ArticleTextsearch) @@. query2
        return a
      liftIO $ length result2 `shouldBe` 0

  describe "ts_rank_cd" $ do
    it "works as expected" $ run $ do
      let vector  = to_tsvector (val "english") (val content)
          content = "content" :: Text
          query   = to_tsquery (val "english") (val "content")
          norm    = val []
      ret <- select $ return $ ts_rank_cd (val def) vector query norm
      liftIO $ map unValue ret `shouldBe` [0.1]

  describe "ts_rank" $ do
    it "works as expected" $ run $ do
      let vector  = to_tsvector (val "english") (val content)
          content = "content" :: Text
          query   = to_tsquery (val "english") (val "content")
          norm    = val []
      ret <- select $ return $ ts_rank (val def) vector query norm
      liftIO $ map unValue ret `shouldBe` [6.07927e-2]

  describe "NormalizationOption" $ do
    describe "fromPersistValue . toPersistValue" $ do
      let isEqual [] []         = True
          isEqual [NormNone] [] = True
          isEqual [] [NormNone] = True
          isEqual a  b          = a == b
          toRight (Right a)     = a
          toRight _             = error "unexpected Left"
      it "is isomorphism" $ property $ \(q :: [NormalizationOption]) ->
        isEqual ((toRight . fromPersistValue . toPersistValue) q) q
          `shouldBe` True

instance Arbitrary ([NormalizationOption]) where
  arbitrary = (:[]) <$> elements [minBound..maxBound]

instance a ~ Lexemes => Arbitrary (TsQuery a) where
  arbitrary = query 0
    where
      maxDepth :: Int
      maxDepth = 10
      query d
        | d<maxDepth  = oneof [lexeme, and_ d, or_ d, not_ d]
        | otherwise   = lexeme
      lexeme    = Lexeme <$> arbitrary <*> weights <*> lexString
      weights   = listOf arbitrary
      and_ d    = (:&) <$> query (d+1) <*> query (d+1)
      or_  d    = (:|) <$> query (d+1) <*> query (d+1)
      not_ d    = Not  <$> query (d+1)
      lexString = pack <$> listOf1 (oneof $ [ choose ('a','z')
                                            , choose ('A','Z')
                                            , choose ('0','9')
                                            ] ++ map pure "-_&|ñçáéíóú")

instance Arbitrary Position where
  arbitrary = oneof [pure Infix, pure Prefix]

instance Arbitrary Weight where
  arbitrary = oneof [pure Highest, pure High, pure Medium, pure Low]


lexm :: Text -> TsQuery Lexemes
lexm = Lexeme Infix []

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
