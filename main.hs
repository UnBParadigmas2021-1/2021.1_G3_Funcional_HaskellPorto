module Main where

import Data.List
import Dijkstra
import System.IO
import Airports

main = do
    putStrLn "##################################"
    putStrLn "# Escolha, uma opção:            #"
    putStrLn "# 1 - Listar aeroports           #"
    putStrLn "# 2 - Verificar menor distância  #"
    putStrLn "# 3 - Encerrar programa          #"
    putStrLn "##################################"
    opcao <- getLine
    case opcao of
        "1" -> showAirports
        "2" -> findShortestPath
        "3" -> putStrLn "Fim do Programa"

findShortestPath = do
    putStrLn "Digite a origem:"
    origemStr <- getLine
    putStrLn "Digite o destino:"
    destinoStr <- getLine
    txt <- readFile "arquivo.csv"
    let nos = fromText txt False
    let origem = dijkstra nos origemStr
    print (caminho origem destinoStr)
    main

showAirports = do
    listAirports
    main

