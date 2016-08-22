{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Markup
import qualified Data.Array as A

import Text.Regex.Base.RegexLike (makeRegex, matchAll)
import Text.Regex.TDFA.String

emailPattern :: Regex
emailPattern = makeRegex ("[[:alnum:]\\+]+@([[:alnum:]]+\\.)+([[:alnum:]]+)":: String)

ipv4Pattern :: Regex
ipv4Pattern = makeRegex ("([[:digit:]]+\\.+){3}([[:digit:]]+)"::String)

findRegex :: T.Text -> Regex -> [(Int, Int)]
findRegex t r =
    concat $ A.elems <$> matchAll r (T.unpack t)

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

    let emailMatches = findRegex s emailPattern
        ipv4Matches = findRegex s ipv4Pattern
        s = "Email addresses look like email@domain.com or foo+bar@domain.net; " <>
            "IPv4 addresses look like 192.168.1.1."
        applyMatches matches tag markup =
            foldr (\(pos,len) m -> markRegion pos len tag m) markup matches

    print $ fromMarkup $ applyMatches emailMatches (Just "email") $
                         applyMatches ipv4Matches (Just "ipv4") $
                         toMarkup s Nothing
