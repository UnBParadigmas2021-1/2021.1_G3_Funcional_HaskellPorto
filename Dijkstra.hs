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

aresta :: Graph -> Node -> [Edge]
aresta g n = snd . head . filter (\(nd, _) -> nd == n) $ g

-- Setando os Pesos nos nós
peso :: Node -> [Edge] -> Float
peso n = weight . head . filter (\e -> n == node e)

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
  
  -- Dijkstra's algoritmo
dijkstra' :: Graph -> [Dnode] -> [Node] -> [Dnode]
dijkstra' g dnodes [] = dnodes
dijkstra' g dnodes unchecked = 
  let dunchecked = filter (\dn -> (fst dn) `elem` unchecked) dnodes
      current = head . sortBy (\(_,(d1,_)) (_,(d2,_)) -> compare d1 d2) $ dunchecked
      c = fst current
      unchecked' = delete c unchecked
      edges = aresta g c
      cnodes = intersect (connectedNodes edges) unchecked'
      dnodes' = map (\dn -> update dn current cnodes edges) dnodes
  in dijkstra' g dnodes' unchecked' 

-- update melhor pontuação baseando-se no peso
update :: Dnode -> Dnode -> [Node] -> [Edge] -> Dnode
update dn@(n, (nd, p)) (c, (cd, _)) cnodes edges =
  let wt = peso n edges
  in  if n `notElem` cnodes then dn
      else if cd+wt < nd then (n, (cd+wt, c)) else dn

-- retorna o melhor caminho a partir do dikstra gerado
caminho :: [Dnode] -> Node -> [Node]
caminho dnodes dest = 
  let dn@(n, (d, p)) = dnodeForNode dnodes dest
  in if n == p then [n] else caminho dnodes p ++ [n]
