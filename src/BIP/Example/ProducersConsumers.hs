
module BIP.Example.ProducersConsumers (producersConsumers) where

import Control.Applicative
import Control.Concurrent
import Control.Monad

import BIP

-- | Producers-consumers system.
--
--   * @n@ is the number of producers.
-- 
--   * @m@ is the number of consummers.
--
--   * @k@ is the number of values produced.
--     It should be a multiple of @n@.
--
--   * @s@ is the cost in microseconds to produce a value.
producersConsumers :: Int -> Int -> Int -> Int -> System s ()
producersConsumers n m k s = do
    -- Creation of the two ports.
    send    <- newPort  -- Port on which to send values produced.
    request <- newPort  -- Port on which to receive values.

    -- Creation of the producers.
    replicateM n $ newAtom $ replicateM (k `div` n) $ do
        when (s > 0) $ liftIO $ threadDelay s
        await send ()

    -- Creation of the consumers.
    replicateM m $ newAtom $ forever $ await request ()

    -- The connector of the system
    registerConnector $
        dynamic send  -- Picks any producer
        <*
        dynamic request  -- Picks any consumer