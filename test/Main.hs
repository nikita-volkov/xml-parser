module Main where

import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import qualified XmlUnscrambler
import Prelude hiding (assert)

main =
  defaultMain $
    testGroup "All tests" $
      []
