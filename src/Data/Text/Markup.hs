module Data.Text.Markup
  ( Markup
  , toMarkup
  , fromMarkup
  , markRegion
  )
where

import Data.Monoid ((<>))
import qualified Data.Text as T

data SequenceTree a =
    Node Int Int [SequenceTree a]
    | Leaf Int Int a
    deriving (Show)

data Markup a =
    Markup { sourceText :: T.Text
           , markupMapping :: SequenceTree a
           }
           deriving (Show)

toMarkup :: T.Text -> a -> Markup a
toMarkup t a = Markup t (Leaf 0 (T.length t) a)

fromMarkup :: Markup a -> [(T.Text, a)]
fromMarkup (Markup txt tree) =
    -- Get all leave nodes from the tree in order, then use their
    -- descriptors to chop up the text
    let descs = leaves tree
        initialState :: (T.Text, [(T.Text, a)])
        initialState = (txt, [])
        nextChunk (remainingText, prevChunks) (_, len, val) =
            let (thisText, remainingText') = T.splitAt len remainingText
                thisChunk = (thisText, val)
            in (remainingText', prevChunks <> [thisChunk])
        (_, chunks) = foldl nextChunk initialState descs
    in chunks

markRegion :: (Show a) => Int -> Int -> a -> Markup a -> Markup a
markRegion start len val (Markup txt t0) = Markup txt t1
    where
        t1 = treeMarkRegion start len val t0

-- Need recursive algorithm to rebuild tree with nodes split up as
-- necessary
treeMarkRegion :: (Show a) => Int -> Int -> a -> SequenceTree a -> SequenceTree a
treeMarkRegion newStart newLen newVal leaf@(Leaf lStart lLen oldVal) =
    if not (startInLeaf || endInLeaf) then leaf
    else if length validLeaves == 1
         then validLeaves !! 0
         else if length validLeaves > 1
              then Node lStart lLen validLeaves
              else error "Ended up with zero valid leaves!"
    where
        end = newStart + newLen
        lEnd = lStart + lLen
        startInLeaf = newStart >= lStart && newStart <= lEnd
        endInLeaf = end >= lStart && end <= lEnd

        -- Clamp the new node leaf to the size of the current leaf since
        -- the request could be larger than this leaf
        newStart' = max lStart newStart
        newEnd = min lEnd (newStart + newLen)
        newLen' = newEnd - newStart'

        newLeaves = [ Leaf lStart (newStart - lStart) oldVal
                    , Leaf newStart' newLen' newVal
                    , Leaf newEnd (lEnd - newEnd) oldVal
                    ]
        validLeaves = filter isValidLeaf newLeaves
        isValidLeaf (Leaf _ l _) = l > 0

treeMarkRegion start len newVal node@(Node lStart lLen cs) =
    let end = start + len
        lEnd = lStart + lLen
        startInNode = start >= lStart && start <= lEnd
        endInNode   = end   >= lStart && end   <= lEnd
    -- If the start or end is somewhere in this node, we need to process
    -- the children
    in if startInNode || endInNode
       then Node lStart lLen $ treeMarkRegion start len newVal <$> cs
       else node

leaves :: SequenceTree a -> [(Int, Int, a)]
leaves (Leaf st len a) = [(st, len, a)]
leaves (Node _ _ cs) = concat $ leaves <$> cs
