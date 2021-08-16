module Main where

import Data.List
import Dijkstra
import System.IO

main = do
    putStrLn "Starting..."
    txt <- readFile "arquivo.csv"
    let nos = fromText txt False
    let origem = dijkstra nos "Paris"
    print (caminho origem "Las_Vegas")
