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
    -- Markup with no markings shall preserve the original text in one
    -- chunk.
    [ testProperty "no marking" $ property $ \t ->
      (fromMarkup $ toMarkup t ()) == [(t, ())]

    , testProperty "text preservation" $ property $ do
        txt <- arbitrary
        start <- arbitrary
        len <- arbitrary
        return $ counterexample (show (txt, start, len)) $
            let m1 = markRegion start len 2 m0
                m0 :: Markup Int
                m0 = toMarkup txt 1
            in (Text.concat $ fst <$> fromMarkup m1) == txt

    -- A marking that is outside the text range shall have no effect.
    , testProperty "no-op marking" $ property $ do
        txt <- arbitrary
        -- Generate a no-op edit: it starts and ends outside the text
        -- range.
        start <- arbitrary `suchThat` (\s -> (s < 0) ||
                                             (s >= Text.length txt))
        len <- arbitrary `suchThat` (\l -> (l >= 0) &&
                                           ((start < 0 && start + l < 0) ||
                                            (start >= Text.length txt)))
        return $ counterexample (show (txt, start, len)) $
            let m1 = markRegion start len 2 m0
                m0 :: Markup Int
                m0 = toMarkup txt 1
            in m0 == m1
    ]

main :: IO ()
main = T.defaultMain propertyTests
