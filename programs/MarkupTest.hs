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
    let emailMatches = findRegex s emailPattern
        ipv4Matches = findRegex s ipv4Pattern
        s = "Email addresses look like email@domain.com or foo+bar@domain.net; " <>
            "IPv4 addresses look like 192.168.1.1."
        applyMatches matches tag markup =
            foldr (\(pos,len) -> markRegion pos len tag) markup matches

    putStrLn $ "Original: " <> show s
    putStrLn "Markup:"
    print $ fromMarkup $ applyMatches emailMatches (Just "email") $
                         applyMatches ipv4Matches (Just "ipv4") $
                         (toMarkup s Nothing :: Markup (Maybe String))
