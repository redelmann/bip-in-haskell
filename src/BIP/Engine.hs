{-# LANGUAGE GADTs, RankNTypes, BangPatterns #-}

module BIP.Engine where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Operational
import Control.Monad.State
import Data.List (foldl')

import BIP.Atom
import BIP.Connector
import BIP.Interaction
import BIP.Port
import BIP.Semantic
import BIP.System
import BIP.WaitingList

-- | Contains mutable information about the state of the running system.
data SystemState s = SystemState
    { getNextIdVar      :: MVar Integer
    -- ^ Variable holding the integer to use as next identifier.
    , getRunningVar     :: MVar Integer
    -- ^ Variable counting the number of currently executing atoms.
    , getWaitingListVar :: MVar (WaitingList s)
    -- ^ Variable containing the waiting list.
    , getSemaphore      :: MVar ()
    -- ^ Semaphore used to block the main thread while the system is running.
    }

-- | Options of the system.
data SystemOptions = SystemOptions
    { getInteractionPicker :: forall s. [Interaction s] -> IO (Interaction s)
    -- ^ Picks an interaction from a non-empty list of interactions.
    , getEarlyInteractionExecution :: Bool
    -- ^ Indicates if stable interactions can be executed earlier.
    }

-- | Default set of options.
defaultOptions :: SystemOptions
defaultOptions = SystemOptions
    { getInteractionPicker = return . head
    , getEarlyInteractionExecution = True }

-- | Executes the given BIP system.
runSystem :: (forall s. System s ()) -> IO ()
runSystem s = runSystemWith defaultOptions s

-- | Excutes the given BIP system using custom options.
runSystemWith :: SystemOptions -> (forall s. System s ()) -> IO ()
runSystemWith opts s = do
    -- Obtaining the connector, the actions to execute, as well
    -- as the next id to use, from the system description.
    let i  = execState (getSystem s)
                (InitialSystemState (ClosedConnector Failure) [] 0)
        c  = getConnector i
        ts = getTasks i
        n  = getNextId i

    -- Initialising the state of the system.
    nextIdVar      <- newMVar n
    runningVar     <- newMVar 0
    waitingListVar <- newMVar emptyWaitingList
    semaphore      <- newEmptyMVar

    let state = SystemState
            { getNextIdVar      = nextIdVar
            , getRunningVar     = runningVar
            , getWaitingListVar = waitingListVar
            , getSemaphore      = semaphore }

    -- Executing the engine, starting with the given tasks.
    when (not $ null ts) $ engineLoop opts c state ts

-- | Main loop of the engine.
engineLoop :: SystemOptions -> ClosedConnector s -> SystemState s
           -> [Task s] -> IO ()
engineLoop opts c state as = do

    when (not $ null as) $ do
        --  Updates the number of running atoms.
        incrementBy (getRunningVar state) (toInteger (length as))

        -- Spawning all continuations of atoms.
        forM_ as (spawnAtom opts state)

    -- Waiting on the semaphore to be notified.
    -- This means that we can now try to execute the next interaction.
    wait (getSemaphore state)

    -- Reading the current state of the waiting list.
    w <- takeMVar (getWaitingListVar state)

    -- Snapshot of the number of running threads.
    -- If n is 0, then we know for sure that there
    -- aren't any atoms left running.
    -- If n is larger,
    -- we are garranteed to be signaled at least once more.
    n <- readMVar (getRunningVar state)

    let stable = n == 0

    -- Picks the semantic depending on whether the system is stable or not.
    let semantic = if stable
        then getClosedInteractions 
        else getStableClosedInteractions

    -- Trying to get an interaction
    case filter (not . null) $ semantic c w of
        [] -> if stable
            then do
                return ()   -- No non-empty interactions to perform.
                            -- Might be a deadlock, or not, depending on
                            -- the state of the waiting list.
            else do
                -- We loop back, since the system wasn't yet in a stable state.
                -- We do not schedule any new tasks however.
                -- Before we do so, we restore the waiting list.
                putMVar (getWaitingListVar state) w
                engineLoop opts c state []

        is -> do
            -- We have possibly multiple interactions.
            -- We let the interaction picker decide.
            i <- getInteractionPicker opts is

            -- Update the waiting list.
            -- All atoms of the interactions are no longer waiting.
            putMVar (getWaitingListVar state) $
                foldl' (flip setRunning) w $ fmap getAtom i

            -- There might be another possible interaction,
            -- we signal ourself to try to get an interaction yet again on next
            -- iteration of the loop.
            when (getEarlyInteractionExecution opts) $
                signal (getSemaphore state)

            -- We loop using the given interaction as our list
            -- of continuations to execute.
            engineLoop opts c state i


-- | Spawns an atom on its own lightweight thread.
--
--   The number of threads MUST be adapted accordingly before calls
--   to this function are made.
spawnAtom :: SystemOptions -> SystemState s -> Task s -> IO ()
spawnAtom opts state (Task a x) =
    void $ flip forkFinally terminateThread $ interpret $ getInstructions x
  where
    terminateThread _ = do
        -- We decrement the number of running atoms.
        n' <- decrementAndFetch (getRunningVar state)

        -- If there isn't any atom left running,
        -- or if early evaluation of stable interactions is enabled,
        -- we notify the engine.
        when (n' == 0 || getEarlyInteractionExecution opts) $
            signal (getSemaphore state)

    interpret p = do
        -- Obtaining the next atom instruction.
        !v <- viewT p
        case v of
            Return _ -> do
                return ()  -- Done executing.

            Spawn x' :>>= f -> do
                -- We are instructed to create a new atom.

                -- Getting and updating the next identifier.
                i <- fetchAndIncrement (getNextIdVar state)

                -- Updating the number of running atoms.
                increment (getRunningVar state)

                -- Creating the identifier of the atom.
                let a' = AtomId i

                -- Spawn the new atom.
                spawnAtom opts state $ Task a' (void x')

                -- Execute the rest of the computation of the parent,
                -- with the identifier of the created atom made available.
                interpret $ f a'

            Await cs :>>= f ->
                -- We are instructed to wait on some ports.

                when (not $ null cs) $ do

                    -- Defines what to do for each single WaitCase.
                    let go w (AwaitCase p u g) = 
                            setWaiting a p u (Action . f . g) w

                    -- Modify the waiting list to take into account all WaitCase's.
                    w <- takeMVar (getWaitingListVar state)
                    let w' = foldl' go w cs
                    putMVar (getWaitingListVar state) w'

            GetId :>>= f -> do
                -- We are instructed to return the identifier of the atom.

                -- Simply gives the identifier to the rest of the computation.
                interpret $ f a

-- | Increments the value inside of an 'MVar'.
--   Returns the value present before the increment takes place.
--
--   Will block on empty 'MVar's.
fetchAndIncrement :: MVar Integer -> IO Integer
fetchAndIncrement v = do
    n <- takeMVar v
    let !n' = succ n
    putMVar v n'
    return n

-- | Increments the value inside of an 'MVar'.
--
--   Will block on empty 'MVar's.
increment :: MVar Integer -> IO ()
increment v = incrementBy v 1

-- | Increments the value inside of an 'MVar' by a certain amount.
--
--   Will block on empty 'MVar's.
incrementBy :: MVar Integer -> Integer -> IO ()
incrementBy v k = do
    n <- takeMVar v
    let !n' = n + k
    putMVar v n'

-- | Decrements the value inside of an 'MVar'.
--   Returns the value after the decrement takes place.
--
--   Will block on empty 'MVar's.
decrementAndFetch :: MVar Integer -> IO Integer
decrementAndFetch v = do
    n <- takeMVar v
    let !n' = pred n
    putMVar v n'
    return n'

-- | Waits until the 'MVar' is full.
--
--   Will block on empty 'MVar's.
wait :: MVar () -> IO ()
wait v = takeMVar v

-- | Ensures that an 'MVar' is full.
signal :: MVar () -> IO ()
signal v = void $ tryPutMVar v ()
