
module Main (main) where

import BIP
import BIP.Example.ProducersConsumers

-- | Runs the token ring example.
main :: IO ()
main = runSystemWith opts $ producersConsumers 20 20 30000 100

-- | System options.
opts :: SystemOptions
opts = defaultOptions {
    getEarlyInteractionExecution = True
}