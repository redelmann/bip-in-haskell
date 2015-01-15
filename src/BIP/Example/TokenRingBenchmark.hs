
module Main (main) where

import Criterion
import Criterion.Types
import Criterion.Main

import BIP
import BIP.Example.TokenRing

main :: IO ()
main = defaultMainWith opts [ bgroup "token ring"
    [ bgroup "100 atoms"
        [ bgroup "early execution" 
            [b 100 m True (show m ++ " token exchanged") | m <- [ 50000, 100000 .. 500000] ]
        , bgroup "normal execution" 
            [b 100 m False (show m ++ " token exchanged") | m <- [ 50000, 100000 .. 500000] ]
        ]
    , bgroup "100,000 token exchanged"
        [ bgroup "early execution" 
            [b n 100000 True (show n ++ " atoms") | n <- [10, 20.. 90] ++ [ 100, 200.. 1000] ]
        , bgroup "normal execution" 
            [b n 100000 False (show n ++ " atoms") | n <- [10, 20.. 90] ++ [ 100, 200.. 1000] ]
        ]
    ] ]
  where
    b n m early title = bench title
                      $ whnfIO
                      $ runSystemWith (defaultOptions { getEarlyInteractionExecution = early } )
                      $ tokenRing n m False

    opts = defaultConfig 
        { csvFile = Just "tokenring.csv"
        , reportFile = Just "tokenring.html" }