{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Control.Concurrent

import Criterion
import Criterion.Types
import Criterion.Main

import BIP
import BIP.Example.DiningPhilosophers

main :: IO ()
main = defaultMainWith opts [ bgroup "dining philosophers"
    [ bgroup "50 meals, early execution"
        [ bgroup "anyOf" 
            [b 1 n 50 0 True | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "manyOf" 
            [b 2 n 50 0 True | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "anyOf with priority" 
            [b 3 n 50 0 True | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "anyOf with shuffle" 
            [b 4 n 50 0 True | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "manyOf with shuffle"
            [b 5 n 50 0 True | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        ]
    , bgroup "50 meals, normal execution"
        [ bgroup "anyOf" 
            [b 1 n 50 0 False | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "manyOf" 
            [b 2 n 50 0 False | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "anyOf with priority" 
            [b 3 n 50 0 False | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "anyOf with shuffle" 
            [b 4 n 50 0 False | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "manyOf with shuffle"
            [b 5 n 50 0 False | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        ]
    , bgroup "50 meals, early execution, 1ms eating time"
        [ bgroup "anyOf" 
            [b 1 n 50 1000 True | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "manyOf" 
            [b 2 n 50 1000 True | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "anyOf with priority" 
            [b 3 n 50 1000 True | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "anyOf with shuffle" 
            [b 4 n 50 1000 True | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "manyOf with shuffle"
            [b 5 n 50 1000 True | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        ]
    , bgroup "50 meals, normal execution, 1ms eating time"
        [ bgroup "anyOf" 
            [b 1 n 50 1000 False | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "manyOf" 
            [b 2 n 50 1000 False | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "anyOf with priority" 
            [b 3 n 50 1000 False | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "anyOf with shuffle" 
            [b 4 n 50 1000 False | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "manyOf with shuffle"
            [b 5 n 50 1000 False | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        ]
    , bgroup "10 meals, manyOf versions, 0.1s eating time"
        [ bgroup "shuffled, early execution" 
            [b 5 n 10 100000 True | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "shuffled, normal execution" 
            [b 5 n 10 100000 False | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "early execution" 
            [b 2 n 10 100000 True | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        , bgroup "normal execution" 
            [b 2 n 10 100000 False | n <- [3, 5.. 11] ++ [21, 31 .. 91]]
        ]
    ] ]
  where
    b v n k s early = bench (show n ++ " philosophers")
                    $ whnfIO
                    $ runSystemWith (defaultOptions { getEarlyInteractionExecution = early } )
                    $ sys n k eat sleep
      where
        eat i j = if s > 0 then liftIO $ threadDelay s else return ()
        sleep i j = return ()

        sys = case v of
            1 -> diningPhilosophersAnyOf
            2 -> diningPhilosophersManyOf
            3 -> diningPhilosophersAnyOfPriority
            4 -> diningPhilosophersAnyOfShuffle
            5 -> diningPhilosophersManyOfShuffle


    opts = defaultConfig 
        { csvFile = Just "diningphilosophers.csv"
        , reportFile = Just "diningphilosophers.html" }