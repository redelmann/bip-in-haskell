{-# LANGUAGE GADTs #-}

module BIP.WaitingList where

import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Unsafe.Coerce (unsafeCoerce)

import BIP.Atom
import BIP.Interaction
import BIP.Port

-- For clarity, we differentiate between integers corresponding to
-- AtomIds and to PortIds.
type AtomInteger = Integer
type PortInteger = Integer

-- | Data structure containing information about waiting atoms.
data WaitingList s = WaitingList
    { getFromAtoms :: Map AtomInteger (Map PortInteger (WaitState s))
    -- ^ For each atom, indicates which ports are active.
    --   For each active port, it also indicates what value was sent
    --   and what to do with the received value.
    , getFromPorts :: Map PortInteger (Set AtomInteger)
    -- ^ For each port, indicates which atoms are waiting on the port.
    }

-- | Contains an upwards value of some unknown type and
--   a downwards function whose domain is an unknown type.
data WaitState s where
    WaitState :: u -> (d -> Action s ()) -> WaitState s

-- | An empty waiting list.
emptyWaitingList :: WaitingList s
emptyWaitingList = WaitingList Map.empty Map.empty

-- | Registers in the waiting list that an atom is waiting on a given port.
setWaiting :: AtomId s -> PortId s d u -> u -> (d -> Action s ())
           -> WaitingList s -> WaitingList s
setWaiting (AtomId a) (PortId p) u d (WaitingList as ps) = WaitingList as' ps'
  where
    as' = Map.insertWith Map.union a (Map.singleton p $ WaitState u d) as
    ps' = Map.insertWith Set.union p (Set.singleton a) ps

-- | Registers in the waiting list that an atom is no longer waiting.
setRunning :: AtomId s -> WaitingList s -> WaitingList s
setRunning (AtomId a) (WaitingList as ps) = WaitingList as' ps'
  where
    -- Getting all ports active for the given atom.
    activePorts = Map.keysSet $ Map.findWithDefault Map.empty a as 
    -- Remove the entry for the atom.
    as' = Map.delete a as
    -- Updates the entries of all ports previously associated to the atom.
    ps' = foldl' removeAtom ps activePorts
      where
        -- Indicates that the atom is no longer waiting on the
        -- port p, by removing it from the set of active atoms 
        -- for the given port.
        -- If the port is no longer active for any atom,
        -- its entry in the map is removed.
        removeAtom m p = Map.update shrink p m
        shrink s = let s' = Set.delete a s in
            if Set.null s' then Nothing else Just s'   

-- | Returns the interaction, if any, related to a specific atom and port pair.
getBoundInteraction :: AtomId s -> PortId s d u -> WaitingList s
                    -> Maybe (OpenInteraction s d u)
getBoundInteraction (AtomId a) (PortId p) (WaitingList as _) = do
    ps <- Map.lookup a as
    WaitState u d <- Map.lookup p ps
    -- Coercing upwards value and downwards functions to the correct type.
    -- This is necessary as WaitState loses all type information.
    -- Fortunately, those types are recovered from the type of the port.
    let tu = unsafeCoerce u :: u
        td = unsafeCoerce d :: (d -> Action s ())
    return $ OpenInteraction
        { getUpwards   = tu 
        , getDownwards = \ x -> [Task (AtomId a) (td x)]
        , getInvolved  = Set.singleton (AtomId a) }

-- | Returns all interactions related to the given port.
getBoundInteractions :: PortId s d u -> WaitingList s
                     -> [OpenInteraction s d u]
getBoundInteractions (PortId p) w = do
    a <- Set.toList $ Map.findWithDefault Set.empty p $ getFromPorts w
    return $ fromMaybe e $ getBoundInteraction (AtomId a) (PortId p) w
  where
    e = error "getBoundInteractions: No interaction for an active atom-port."
         