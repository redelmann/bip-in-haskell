
module Main (main) where

import Criterion
import Criterion.Types
import Criterion.Main

import BIP
import BIP.Example.ProducersConsumers

main :: IO ()
main = defaultMainWith opts [ bgroup "producers-consumers"
    [ bgroup "20 producers-consumers"
        [ bgroup "early execution" 
            [b 20 20 m 0 True (show m ++ " values produced") | m <- [ 50000, 100000 .. 500000] ]
        , bgroup "normal execution" 
            [b 20 20 m 0 False (show m ++ " values produced") | m <- [ 50000, 100000 .. 500000] ]
        ]
    , bgroup "100,000 values produced"
        [ bgroup "early execution" 
            [b n n 100000 0 True (show n ++ " producers-consumers") | n <- [ 10, 25, 50, 100, 250, 500, 1000] ]
        , bgroup "normal execution" 
            [b n n 100000 0 False (show n ++ " producers-consumers") | n <- [ 10, 25, 50, 100, 250, 500, 1000] ]
        ]
    , bgroup "100,000 values produced, 0.1 ms delay"
        [ bgroup "early execution" 
            [b n n 100000 100 True (show n ++ " producers-consumers") | n <- [ 2, 5, 10, 20, 50, 100] ]
        , bgroup "normal execution" 
            [b n n 100000 100 False (show n ++ " producers-consumers") | n <- [ 2, 5, 10, 20, 50, 100] ]
        ]
    ] ]
  where
    b n m k s early title = bench title
                          $ whnfIO
                          $ runSystemWith (defaultOptions { getEarlyInteractionExecution = early } )
                          $ producersConsumers n m k s

    opts = defaultConfig 
        { csvFile = Just "producersconsumers.csv"
        , reportFile = Just "producersconsumers.html" }