
module BIP.Interaction where

import Data.Set (Set)
import qualified Data.Set as Set

import BIP.Atom

-- | Interaction.
type Interaction s = [Task s]

-- | Open interaction.
data OpenInteraction s d u = OpenInteraction
    { getUpwards   :: u
    -- ^ Value propagated upwards by the interaction.
    , getDownwards :: d -> [Task s]
    -- ^ Given a downwards value, returns all tasks to perform.
    , getInvolved  :: Set (AtomId s)
    -- ^ The set of all atoms involved in the interaction.
    }

-- | Obtains an interaction from an open interaction.
--
--   This function feeds the upwards value of the open interaction
--   to its downwards function.
close :: OpenInteraction s a a -> Interaction s
close (OpenInteraction x f _) = f x