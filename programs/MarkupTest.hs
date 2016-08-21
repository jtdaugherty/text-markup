{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.Markup

main :: IO ()
main = do
    let t = "this is some text"
        val = ()
    print $ fromMarkup $ toMarkup t val
