{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}

module BIP.Atom where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Operational

import BIP.Port

-- | Atom identifier.
newtype AtomId s = AtomId Integer
    deriving (Eq, Ord, Show)

-- | Task to be executed by an atom.
data Task s = Task
    { getAtom   :: AtomId s
    -- ^ The atom responsible to execute the action.
    , getAction :: Action s ()
    -- ^ The action to execute.
    }

-- | Action to be performed by an atom.
newtype Action s a = Action
    { getInstructions :: ProgramT (Instruction s) IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

-- | Possible instructions to be performed within actions.
data Instruction s a where
    Spawn :: Action s a -> Instruction s (AtomId s)
    Await :: [AwaitCase s a] -> Instruction s a
    GetId :: Instruction s (AtomId s)

-- | Contains information about a case of an 'awaitAny' instruction.
--
--   Each record contains the identifier of the port on which to wait,
--   the value to be sent, and a function to apply on the downwards function.
data AwaitCase s a where
    AwaitCase :: PortId s d u -> u -> (d -> a) -> AwaitCase s a

instance Functor (AwaitCase s) where
    fmap f (AwaitCase p x g) = AwaitCase p x (f . g) 

-- | Sends a value on the given port and waits to receive a value.
await :: PortId s d u -> u -> Action s d
await p x = Action $ singleton $ Await [AwaitCase p x id]

-- | Sends values on the given ports and waits to receive a value on any of the ports.
awaitAny :: [AwaitCase s a] -> Action s a
awaitAny as = Action $ singleton $ Await as

-- | Specifies what to send on the particular port.
onPort :: PortId s d u -> u -> AwaitCase s d
onPort p x = AwaitCase p x id

-- | Spawns a new atom.
spawn :: Action s d -> Action s (AtomId s)
spawn a = Action $ singleton $ Spawn a

-- | Returns the identifier of the atom.
getSelfId :: Action s (AtomId s)
getSelfId = Action $ singleton $ GetId