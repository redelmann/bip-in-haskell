
module Main (main) where

import BIP
import BIP.Example.TokenRing

-- | Runs the token ring example.
main :: IO ()
main = runSystemWith opts $ tokenRing 100 10000 True

-- | System options.
opts :: SystemOptions
opts = defaultOptions {
    getEarlyInteractionExecution = True
}