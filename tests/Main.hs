{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty as T
import Test.Tasty.QuickCheck as T
import Test.QuickCheck.Property
-- For Arbitrary instance for Text
import Data.Text.Arbitrary ()
import qualified Data.Text as Text

import Data.Text.Markup

propertyTests :: T.TestTree
propertyTests = T.testGroup "Markup"
    [ testProperty "no-op" $ property $ \t ->
      (fromMarkup $ toMarkup t ()) == [(t, ())]

    , testProperty "text preservation" $ property $ do
        txt <- arbitrary
        start <- arbitrary -- `suchThat` (>= 0)
        len <- arbitrary -- `suchThat` (<= Text.length txt)
        return $ counterexample (show (txt, start, len)) $
            let m1 = markRegion start len 2 m0
                m0 = toMarkup txt 1
            in (Text.concat $ fst <$> fromMarkup m1) == txt
    ]

main :: IO ()
main = T.defaultMain propertyTests
