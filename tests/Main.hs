{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty as T
import Test.Tasty.QuickCheck as T
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
        start <- arbitrary
        len <- arbitrary
        return $ counterexample (show (txt, start, len)) $
            let m1 = markRegion start len 2 m0
                m0 = toMarkup txt 1
            in (Text.concat $ fst <$> fromMarkup m1) == txt
    ]

main :: IO ()
main = T.defaultMain propertyTests
