{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, RecursiveDo, RankNTypes #-}

module BIP.System where

import Control.Applicative
import Control.Monad
import Control.Monad.State

import BIP.Atom
import BIP.Connector
import BIP.Port

-- | Describes a BIP system.
-- 
--   The type 's' ensures that identifiers of a system
--   may not leave the scope of the system.
newtype System s a = System
    { getSystem :: State (InitialSystemState s) a }
    deriving (Functor, Applicative, Monad, MonadFix)

-- | Contains the initial state of a system, that is
--   the state of a system before it is actually started.
data InitialSystemState s = InitialSystemState
    { getConnector :: ClosedConnector s
    -- ^ Indicates which connector is to be used by the system.
    , getTasks     :: [Task s]
    -- ^ Indicates, for each atom which action must be executed.
    , getNextId    :: Integer
    -- ^ Indicates the next free identifier.
    }

-- | Creates a new atom.
newAtom :: Action s a -> System s (AtomId s)
newAtom x = System $ do
    s <- get
    let n  = getNextId s   -- Next Id to use.
        as = getTasks s  -- Actions to execute so far.
        i  = AtomId n      -- Id of the newly created atom.
        s' = s
            { getNextId = succ n  -- Specifying the next Id to use.
            , getTasks = Task i (void x) : as  -- New task.
            }
    put s'
    return i

-- | Creates a new port.
newPort :: System s (PortId s d u)
newPort = System $ do
    s <- get
    let n = getNextId s
    put $ s { getNextId = succ n }
    return $ PortId n

-- | Registers a connector for the system.
--
--   Any previous call to 'registerConnector' will get overwritten.
registerConnector :: Connector s a a -> System s ()
registerConnector c = System $ do
    s <- get
    put $ s { getConnector = ClosedConnector c }

