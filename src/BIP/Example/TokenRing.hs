{-# LANGUAGE RecursiveDo, BangPatterns #-}

module BIP.Example.TokenRing where

import Control.Monad
import Data.Vector (fromList, (!))

import BIP

-- | Describes a token ring.
--
--   * @n@ is the number of atoms.
--
--   * @m@ is the number of token exchanges.
--
--   * @display@ indicates if atoms should print to standard output or not.
tokenRing :: Int -> Int -> Bool -> System s ()
tokenRing n m display = mdo

    -- Port on which to send the token and its recipient.
    send <- newPort

    -- Port on which to receive the token.
    receive <- newPort

    -- Vector of atom identifiers
    let v = fromList as

    -- Returns the identifier of the atom that is
    -- supposed to receive message from the i^th atom.
    let next i | i == pred n = a
               | otherwise   = v ! i

    -- Behaviour of the i^th atom.
    let atom i = do
            -- Awaiting to receive a value.
            n <- await receive ()
            -- Printing the value received.
            when display $ liftIO $ do
                putStrLn $ "Atom " ++ show i ++ " received " ++ show n
            -- Computing the next value.
            let !n' = succ n
            -- Sending the value to the next atom.
            when (n' <= m) $ do
                await send (n', next i)
                atom i

    -- Creating the first atom.
    a <- newAtom $ do
        -- Printing the first message.
        when display $ liftIO $ putStrLn "Atom 0 ready to send."
        -- Sending the first message.
        await send (1, next 0)
        -- From now on, acts as a "normal" atom.
        atom 0

    -- Creating the n - 1 next atoms.
    as <- forM [ 1 .. pred n ] $ \ i ->
        newAtom $ atom i


    -- Connector of the system.
    registerConnector $ do
        -- Accepts any atom on the port send.
        (value, to) <- dynamic send
        -- Bind the receiver on the receive port
        -- and send it the given value.
        bind to receive # receiving value
