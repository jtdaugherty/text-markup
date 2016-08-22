{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty as T
import Test.Tasty.QuickCheck as T

import Data.Text.Markup

propertyTests :: T.TestTree
propertyTests = T.testGroup "Markup"
    [ testProperty "test" $ property $ (fromMarkup $ toMarkup "" ()) == [("", ())]
    ]

main :: IO ()
main = T.defaultMain propertyTests
