{-# LANGUAGE OverloadedLists #-}

module Main where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import qualified Text.XML as Xc
import qualified XmlUnscrambler as Xu
import Prelude hiding (assert)

main =
  defaultMain $
    testGroup "All tests" $
      [ testProperty "ByName/many" $ do
          bContents <- fmap (fromString . show) <$> listOf (chooseInt (0, 99))

          xml <-
            let childByName name =
                  Xc.Element name [] . pure . Xc.NodeContent
                root =
                  Xc.Element "a" [] . fmap (Xc.NodeElement)
             in fmap (documentByteString . elementDocument . root . join) $
                  forM bContents $ \bContent -> do
                    prefix <-
                      oneof
                        [ pure [],
                          listOf (childByName "c" <$> arbitrary)
                        ]
                    return (prefix <> [childByName "b" bContent])

          let parser =
                Xu.childrenByName $ many $ Xu.byName Nothing "b" $ Xu.children $ Xu.textNode $ Xu.textContent
              result = Xu.parseByteString parser xml

          return (result === Right bContents),
        testCase "Namespaces" $ do
          let parser = Xu.childrenByName $ Xu.byName (Just "B") "c" $ Xu.children $ Xu.textNode $ Xu.textContent
              result = Xu.parseByteString parser "<a xmlns:b=\"B\"><b:c>d</b:c></a>"
          assertEqual "" (Right "d") result,
        testCase "Error" $ do
          let parser = Xu.childrenByName $ Xu.byName Nothing "c" $ Xu.children $ Xu.textNode $ Xu.textContent
              result = Xu.parseByteString parser "<a><b>c</b><d/></a>"
          assertEqual "" (Left "/: None of following child element names found: [c]. Names available: [b, d]") result,
        testCase "QName content" $ do
          let input = "<root xmlns:abc='abc-uri'>abc:d</root>"
              parser = Xu.children $ Xu.textNode $ Xu.qNameContent
           in assertEqual "" (Right (Just "abc-uri", "d")) (Xu.parseByteString parser input)
      ]

documentByteString :: Xc.Document -> ByteString
documentByteString = LazyByteString.toStrict . Xc.renderLBS Xc.def

elementDocument :: Xc.Element -> Xc.Document
elementDocument x =
  Xc.Document (Xc.Prologue [] Nothing []) x []
