
module Main (main) where

import BIP
import BIP.Example.DiningPhilosophers

-- | Runs the dining philosophers example.
main :: IO ()
main = runSystemWith opts $ diningPhilosophersAnyOfPriority 3 3 eat sleep
  where
    eat i j = liftIO $ do
        putStrLn $ "Philosopher " ++ show i ++ " eating (" ++ show j ++ ")"

    sleep i j = liftIO $ do
        putStrLn $ "Philosopher " ++ show i ++ " sleeping (" ++ show j ++ ")"

-- | System options.
opts :: SystemOptions
opts = defaultOptions {
    getEarlyInteractionExecution = False
}