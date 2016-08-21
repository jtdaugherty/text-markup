{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Data.Text.Markup

main :: IO ()
main = do
    let t = "this is some text"
        val :: Int
        val = 1
        m0 = toMarkup t val
        -- Set "this " to 2
        m1 = markRegion 0 5 2 m0
        -- Set "his" to 3
        m2 = markRegion 1 3 3 m1
        -- Set "is is so" to 4
        m3 = markRegion 2 8 4 m2
        -- Set "is" to 5
        m4 = markRegion 2 2 5 m3
        m5 = markRegion 0 2 5 m4
        m6 = markRegion 4 1 5 m5
        m7 = markRegion 5 5 5 m6
        m8 = markRegion 10 7 5 m7
    print $ fromMarkup m0
    print $ fromMarkup m1
    print $ fromMarkup m2
    print $ fromMarkup m3
    print $ fromMarkup m4
    print $ fromMarkup m5
    print $ fromMarkup m6
    print $ fromMarkup m7
    print $ fromMarkup m8
