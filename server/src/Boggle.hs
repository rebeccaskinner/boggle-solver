{-# LANGUAGE LambdaCase #-}
module Boggle (BogglePaths, mkTour, solveBoggle, dictTrie) where

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as PT
import qualified Data.Map.Strict as M
import qualified Data.Trie as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Debug.Trace
import Data.List
import Data.Maybe

-- | TourTrie defines a basic trie representation of a given tour of
-- | the boggle board.  We're using a custom trie implementation here
-- | because it makes it much easier for us to walk the tour
-- | character-by-character (that capability isn't provided by existing
-- | trie libraries).  We store the terminal character of a sequence
-- | with END, and each intermediary node as a TourTrie with the node
-- | value and all children stored as a list.
data TourTrie = End Char
              | TourTrie Char [TourTrie] deriving (Eq, Show)

-- | BogglePaths represents a forest of TourTries, containing the tours
-- | for every starting position in the boggle board.
type BogglePaths = [TourTrie]

-- | LookupState manages the different search states that allow us to
-- | terminate our search of the dictionary trie early if we find that
-- | there will be no matching children.
data LookupState = FoundCont String
                 | FoundTerminal String
                 | NotFoundCont
                 | NotFoundTerminal deriving (Eq, Show)

-- | mkTour generates a new graph representation of the boggle board
-- | by traversing the tree
mkTour :: Int -> String -> Maybe BogglePaths
mkTour n boardData =
  case boardGraph n boardData of
    Nothing -> Nothing
    Just g -> Just $ map (tour g) [0..(n ^ 2) - 1]

-- | Given a dictionary and a set of paths, solve the boggle board.
-- | This function also removes duplicate words and filters out short
-- | words since they aren't legal in boggle.
solveBoggle :: T.Trie () -> BogglePaths -> [String]
solveBoggle t p =
  let allResults = concatMap (dictMatches t) p
  in nub . filter ((>= 4) . length) $ allResults

-- | dictTrie generates a dictionary trie from a list of words
dictTrie :: [BS.ByteString] -> T.Trie ()
dictTrie s = foldl (\t s -> T.insert s () t) T.empty s

-- | dictMatches finds the matches in the dictionary from a single tour
dictMatches :: T.Trie () -> TourTrie -> [String]
dictMatches dict t =
  let initialPath = [getCh t]
      children = case t of
                   End _ -> []
                   TourTrie _ children -> children
  in concatMap (dictMatches' initialPath dict) children
  where
    dictMatches' :: String -> T.Trie() -> TourTrie -> [String]
    dictMatches' path dict t =
      let
        path' = path ++ [getCh t]
        t' = case t of
               End _ -> []
               TourTrie _ subtrees -> subtrees
        m = concatMap (dictMatches' path' dict) t'
      in
        case lookupDict dict path' of
          FoundCont s -> s : m
          FoundTerminal s -> [s]
          NotFoundCont -> m
          NotFoundTerminal -> []

lookupDict :: T.Trie () -> String -> LookupState
lookupDict dict s =
  let
    s' = BS.pack s
    dict' = T.submap s' dict
  in
    if T.null dict'
    then NotFoundTerminal
    else
      case T.lookup s' dict' of
        Nothing -> NotFoundCont
        Just _ ->
          if 1 == T.size dict'
          then FoundTerminal s
          else FoundCont s


neighbors' :: (Ord a, Num a) => a -> (a, a) -> [(a,a)]
neighbors' bounds (x,y) =
  let xs = [x - 1, x, x + 1]
      ys = [y - 1, y, y + 1]
  in  [(x',y') | x' <- xs
               , y' <- ys
               , (x',y') /= (x,y)
               , x' >= 0
               , x' < bounds
               , y' >= 0
               , y' < bounds ]

toIndex :: Int -> (Int, Int) -> Int
toIndex stride (x,y) =
  x + (y * stride)

fromIndex :: Int -> Int -> (Int, Int)
fromIndex stride idx =
  (idx `rem` stride, idx `div` stride)

neighbors :: Int -> Int -> [Int]
neighbors dimension idx =
  let idx' = fromIndex dimension idx
      neighborsInRange = neighbors' dimension idx'
  in map (toIndex dimension) neighborsInRange

mkNodes :: String -> [G.LNode Char]
mkNodes s = zip [0..] s

mkEdges :: Int -> [G.LEdge ()]
mkEdges n =
  nub $ concatMap idxEdges [0.. (n ^ 2) - 1]
  where
    idxEdges :: Int -> [G.LEdge ()]
    idxEdges idx =
      map (\a -> (idx, a, ())) $ neighbors n idx

boardGraph :: Int -> String -> Maybe (PT.Gr Char ())
boardGraph n s
  | (length s) == n ^ 2 = Just $ G.mkGraph (mkNodes s) (mkEdges n)
  | otherwise = Nothing

tour :: G.Graph gr => gr Char b -> Int -> TourTrie
tour g idx =
  let g' = G.delNode idx g
      n = nub $ G.neighbors g idx
      -- NB: unsafe partial function; since we won't export this
      -- method it's okay for the sake of expediency
      (Just l) = G.lab g idx
  in
    if G.isEmpty g'
    then
      End l
    else
      TourTrie l (map (tour g') n)

getCh :: TourTrie -> Char
getCh = \case
  (End c) -> c
  (TourTrie c _) -> c
