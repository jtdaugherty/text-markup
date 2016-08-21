module Data.Text.Markup
  ( Markup
  , toMarkup
  , fromMarkup
  )
where

import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Tree

data Descriptor a =
   Desc { seqStart :: Int
        , seqLength :: Int
        , seqData :: a
        }
        deriving (Show)

data Markup a =
    Markup { sourceText :: T.Text
           , markupMapping :: Tree (Descriptor a)
           }
           deriving (Show)

toMarkup :: T.Text -> a -> Markup a
toMarkup t a = Markup t (Node (Desc 0 (T.length t) a) [])

fromMarkup :: Markup a -> [(T.Text, a)]
fromMarkup (Markup txt tree) =
    -- Get all leave nodes from the tree in order, then use their
    -- descriptors to chop up the text
    let descs = leaves tree
        initialState :: (T.Text, [(T.Text, a)])
        initialState = (txt, [])
        nextChunk desc (remainingText, prevChunks) =
            let (thisText, remainingText') = T.splitAt (seqLength desc) remainingText
                thisChunk = (thisText, thisValue)
                thisValue = seqData desc
            in (remainingText', prevChunks <> [thisChunk])
        (_, chunks) = foldr nextChunk initialState descs
    in chunks

leaves :: Tree a -> [a]
leaves (Node l []) = [l]
leaves (Node _ cs) = concat $ leaves <$> cs
