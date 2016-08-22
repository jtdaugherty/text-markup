module Data.Text.Markup
  ( Markup
  , toMarkup
  , fromMarkup
  , markRegion
  )
where

import qualified Data.Sequence as S
import Data.Monoid ((<>))
import qualified Data.Text as T

data SequenceTree a =
    Node Int Int (S.Seq (SequenceTree a))
    | Leaf Int Int a
    deriving (Show, Eq)

data Markup a =
    Markup { sourceText :: T.Text
           , markupMapping :: SequenceTree a
           }
           deriving (Show, Eq)

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

markRegion :: (Eq a) => Int -> Int -> a -> Markup a -> Markup a
markRegion start len val (Markup txt t0) = Markup txt t1
    where
        t1 = treeMarkRegion start len val t0

-- Need recursive algorithm to rebuild tree with nodes split up as
-- necessary
treeMarkRegion :: (Eq a) => Int -> Int -> a -> SequenceTree a -> SequenceTree a
treeMarkRegion newStart newLen newVal leaf@(Leaf lStart lLen oldVal) =
    if newLen == 0 || not (startInLeaf || endInLeaf) then leaf
    else if length validLeaves == 1
         then S.index validLeaves 0
         else if S.length validLeaves > 1
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

        newLeaves = S.fromList [ Leaf lStart (newStart - lStart) oldVal
                               , Leaf newStart' newLen' newVal
                               , Leaf newEnd (lEnd - newEnd) oldVal
                               ]
        validLeaves = S.filter isValidLeaf newLeaves
        isValidLeaf (Leaf _ l _) = l > 0
        isValidLeaf _ = error "BUG: isValidLeaf got a Node!"

treeMarkRegion start len newVal node@(Node lStart lLen cs) =
    let end = start + len
        lEnd = lStart + lLen
        startInNode = start >= lStart && start <= lEnd
        endInNode   = end   >= lStart && end   <= lEnd
    -- If the start or end is somewhere in this node, we need to process
    -- the children
    in if startInNode || endInNode
       then let newChildren = treeMarkRegion start len newVal <$> cs
            in case mergeNodes newChildren of
                Left single -> single
                Right many -> Node lStart lLen many
       else node

mergeNodes :: (Eq a) => S.Seq (SequenceTree a) -> Either (SequenceTree a) (S.Seq (SequenceTree a))
mergeNodes s
  | S.null s = Right S.empty
  | S.length s == 1 = Left $ S.index s 0
  | otherwise =
      let a = S.index s 0
          b = S.index s 1
          rest = S.drop 2 s
      in case mergeNodePair a b of
        Just m -> mergeNodes $ m S.<| rest
        Nothing -> case mergeNodes $ b S.<| rest of
            Left l -> Right $ S.fromList [a, l]
            Right ls -> Right $ a S.<| ls

sInit :: S.Seq a -> S.Seq a
sInit s = S.take ((S.length s) - 1) s

sHead :: S.Seq a -> a
sHead s = S.index s 0

sTail :: S.Seq a -> S.Seq a
sTail = S.drop 1

sLast :: S.Seq a -> a
sLast s = S.index s ((S.length s) - 1)

mergeNodePair :: (Eq a) => SequenceTree a -> SequenceTree a -> Maybe (SequenceTree a)
mergeNodePair (Leaf aStart aLen aVal) (Leaf bStart bLen bVal)
  | aVal == bVal && bStart == aStart + aLen = Just $ Leaf aStart (aLen + bLen) aVal
  | otherwise = Nothing
mergeNodePair leaf@(Leaf aStart aLen _) (Node _ bLen bs) = do
    merged <- mergeNodePair leaf $ sHead bs
    case mergeNodes $ merged S.<| (sTail bs) of
        Left single -> return single
        Right many -> return $ Node aStart (aStart + aLen + bLen) many
mergeNodePair (Node aStart aLen as) leaf@(Leaf _ bLen _)
  | length as > 0 = do
    merged <- mergeNodePair (sLast as) leaf
    case mergeNodes $ sInit as <> S.singleton merged of
        Left single -> return single
        Right many -> return $ Node aStart (aStart + aLen + bLen) many
mergeNodePair (Node aStart aLen as) (Node _ bLen bs)
  | length as > 0 && length bs > 0 = do
    merged <- mergeNodePair (sLast as) (sHead bs)
    case mergeNodes $ sInit as <> S.singleton merged <> sTail bs of
        Left single -> return single
        Right many -> return $ Node aStart (aStart + aLen + bLen) many
mergeNodePair _ _ = Nothing

leaves :: SequenceTree a -> [(Int, Int, a)]
leaves (Leaf st len a) = [(st, len, a)]
leaves (Node _ _ cs) = concat $ leaves <$> cs
