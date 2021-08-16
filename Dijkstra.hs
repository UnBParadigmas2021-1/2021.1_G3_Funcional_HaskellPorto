module Dijkstra 
(
  fromText,
  dijkstra,
  caminho,
  aresta,
  Edge(..),
  Node,
  Graph,
  Dnode
) where

import Data.List

data Edge = Edge { node::Node, weight::Float } deriving (Show)
type Node = String
type Graph = [(Node, [Edge])]
type Dnode = (Node, (Float, Node))

fromText :: String -> Bool -> Graph
fromText strLines isDigraph = 
  let readData [n1, n2, w] = ((n1, n2), read w :: Float)
      es = map (readData . words) $ lines strLines
      allEs = if isDigraph then es 
              else appendReversed es
  in fromList allEs

appendReversed :: [((String, String), Float)] -> [((String, String), Float)]
appendReversed es = es ++ map (\((n1,n2),w) -> ((n2,n1),w)) es

fromList :: [((String, String), Float)] -> Graph
fromList es =
  let nodes = nub . map (fst . fst) $ es
      aresta es node = 
        let connected = filter (\((n,_),_) -> node == n) $ es
        in map (\((_,n),wt) -> Edge n wt) connected 
  in map (\n -> (n, aresta es n)) nodes

-- Setando os Pesos nos nós
aresta :: Graph -> Node -> [Edge]
aresta g n = snd . head . filter (\(nd, _) -> nd == n) $ g

-- Given a node and a list of edges, one of which is incident on the node, return the weight
peso :: Node -> [Edge] -> Float
peso n = weight . head . filter (\e -> n == node e)

-- Given a list of edges, return their nodes
connectedNodes :: [Edge] -> [Node]
connectedNodes = map node

dnodeForNode :: [Dnode] -> Node -> Dnode
dnodeForNode dnodes n = head . filter (\(x, _) -> x == n) $ dnodes

-- Setando nó inicial
dijkstra :: Graph -> Node -> [Dnode]
dijkstra g start = 
  let dnodes = initD g start
      unchecked = map fst dnodes
  in  dijkstra' g dnodes unchecked

-- Construindo a lista a partir nó inicial
initD :: Graph -> Node -> [Dnode]
initD g start =
  let initDist (n, es) = 
        if n == start 
        then 0 
        else if start `elem` connectedNodes es
             then peso start es
             else 1.0/0.0
  in map (\pr@(n, _) -> (n, ((initDist pr), start))) g
