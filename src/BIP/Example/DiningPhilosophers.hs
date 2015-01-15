
module BIP.Example.DiningPhilosophers where

import Control.Applicative
import Control.Concurrent
import Control.Monad hiding (replicateM)
import Data.Monoid
import Data.Ord
import Data.Vector ((!), replicateM, fromList)
import System.Random.Shuffle

import BIP


-- | Dining philosophers problem.
--
--   * @n@ is the number of philosophers.
--
--   * @k@ is the number of meals per philosopher.
--
--   * @eat@ is the action to execute when a philosopher eats.
--
--   * @sleep@ is the action to execute when a philosopher sleeps.
diningPhilosophers :: Int -> Int
                   -> (Int -> Int -> Action s ())
                   -> (Int -> Int -> Action s ())
                   -> System s ([Connector s d1 Int], [Connector s d2 ()])
diningPhilosophers n k eat sleep = do
    
    allocateFork <- newPort  -- Indicates the allocation of a fork
    releaseFork  <- newPort  -- Indicates the release of a fork

    -- Creating the n forks
    forks <- replicateM n $ newAtom $ forever $ do
        -- the fork is free
        await allocateFork ()
        -- the fork is now taken
        await releaseFork ()

    startEating <- newPort  -- Indicates a desire to start eating
    endEating   <- newPort  -- Indicates a desire to stop eating

    -- Creating the n philosophers
    philosophers <- fmap fromList $ forM [1 .. n] $ \ i -> 
        newAtom $ forM_ [1 .. k] $ \ j -> do
            -- the philosopher is hungry
            await startEating j  -- j indicates how many times we've eaten
            -- the philosopher eats
            eat i j
            -- the philosopher has finished eating
            await endEating ()
            -- the philosopher sleeps
            sleep i j


    -- Indices of left and right forks
    let right i = (succ i) `rem` n
        left i = i

    -- For philosopher i to start eating,
    -- the following must synchronise.
    let enterEating i =
            bind (forks ! left i) allocateFork
            *>
            bind (forks ! right i) allocateFork
            *>
            bind (philosophers ! i) startEating

    -- For philosopher i to stop eating,
    -- the following must synchronise.
    let leaveEating i =
            bind (forks ! left i) releaseFork
            <>
            bind (forks ! right i) releaseFork
            <>
            bind (philosophers ! i) endEating

    -- Returning the connectors.
    return ([enterEating i | i <- [0 .. pred n]]
           ,[leaveEating i | i <- [0 .. pred n]])

diningPhilosophersManyOf n k eat sleep = do
    (es, ls) <- diningPhilosophers n k eat sleep

    registerConnector $ allOf
        [ manyOf [sending () e | e <- es]
        , manyOf ls ]

diningPhilosophersAnyOf n k eat sleep = do
    (es, ls) <- diningPhilosophers n k eat sleep

    registerConnector $ anyOf
        [ anyOf [ sending () e | e <- es ]
        , anyOf ls ]

diningPhilosophersAnyOfPriority n k eat sleep = do
    (es, ls) <- diningPhilosophers n k eat sleep
    
    registerConnector $ anyOf
        [ sending () $ minimal $ anyOf [ e | e <- es ]
        , anyOf ls ]

diningPhilosophersManyOfPriority n k eat sleep = do
    (es, ls) <- diningPhilosophers n k eat sleep
    
    registerConnector $ manyOf
        [ sending [] $ minimalBy (comparing sum)
                     $ ensuring (not . null)
                     $ manyOf [ e | e <- es ]
        , manyOf ls ]

diningPhilosophersAnyOfShuffle n k eat sleep = do
    (es, ls) <- diningPhilosophers n k eat sleep
    
    shuffled <- newPort
    shuffler <- newAtom $ forever $ do
        es' <- liftIO $ shuffleM [close e | e <- es]
        await shuffled es'

    let entering = do
            es' <- bind shuffler shuffled
            anyOf es'

    let leaving = anyOf ls

    registerConnector $ anyOf
        [ entering
        , leaving ]

diningPhilosophersManyOfShuffle n k eat sleep = do
    (es, ls) <- diningPhilosophers n k eat sleep
    
    shuffled <- newPort
    shuffler <- newAtom $ forever $ do
        es' <- liftIO $ shuffleM [close e | e <- es]
        await shuffled es'

    let entering = do
            es' <- bind shuffler shuffled
            ensuring (not . null) $ manyOf es'

    let leaving = manyOf ls

    registerConnector $ manyOf
        [ entering
        , leaving ]
