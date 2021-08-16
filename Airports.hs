module Airports (
  listAirports
) where

import Data.List
import Dijkstra
import System.IO
import Data.Set (Set)
import qualified Data.Set as Set

-- get airports names from field lines
getAirports [] listaDeAeroportos = listaDeAeroportos
getAirports (x:xs) listaDeAeroportos = do
    let linha = words x
    getAirports xs (listaDeAeroportos ++ [(linha !! 0)] ++ [(linha !! 1)])
    

printAirports [] = print "."
printAirports (x:xs) = do
  print x
  printAirports xs

-- list airports as a list
listAirports = do
    txt <- readFile "arquivo.csv"
    let linhas = lines txt
    let temp = getAirports linhas []
    let airportsOrdered = sort temp
    let airportsSet = Set.fromAscList airportsOrdered
    printAirports(Set.toList airportsSet)






