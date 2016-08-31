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
        let m1 = markRegion start len 2 m0
            m0 :: Markup Int
            m0 = toMarkup txt 1
        return $ counterexample (show (txt, start, len, m1)) $
            (Text.concat $ fst <$> fromMarkup m1) == txt

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

    -- Adjancent markings are merged.
    , testProperty "merged marking" $ property $ do
        txt <- arbitrary
        -- Generate the first range...
        r1Start <- arbitrary `suchThat` (\s -> (s >= 0))
        r1Len <- arbitrary `suchThat` (\l -> (l >= 0))
        -- then an adjacent one.
        let r2Start = r1Start + r1Len
        r2Len <- arbitrary `suchThat` (\l -> (l >= 0))

        let m1 = markRegion r1Start r1Len 2 m0
            m2 = markRegion r2Start r2Len 2 m1
            m0 :: Markup Int
            m0 = toMarkup txt 1
            m3 = markRegion r1Start (r1Len + r2Len) 2 m0
        return $ counterexample (show (txt, r1Start, r1Len, r2Len, fromMarkup m2, fromMarkup m3)) $
            fromMarkup m2 == fromMarkup m3

    -- Applying a small marking A followed by a containing marking B is
    -- equivalent to just applying B.
    , testProperty "containing marking" $ property $ do
        txt <- arbitrary
        -- Generate the containing range...
        r1Start <- arbitrary `suchThat` (\s -> (s >= 0))
        r1Len <- arbitrary `suchThat` (\l -> (l >= 3))
        -- then a contained one.
        let r2Start = r1Start + 1
            r2Len = r1Len - 1

        let m1 = markRegion r2Start r2Len 2 m0
            m2 = markRegion r1Start r1Len 3 m1
            m0 :: Markup Int
            m0 = toMarkup txt 1
            m3 = markRegion r1Start r1Len 3 m0
        return $ counterexample (show (txt, r1Start, r1Len, r2Len, fromMarkup m2, fromMarkup m3)) $
            fromMarkup m2 == fromMarkup m3
    ]

main :: IO ()
main = T.defaultMain propertyTests
