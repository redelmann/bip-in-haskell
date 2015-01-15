
module BIP.Port where

-- | Port identifier.
--
--   * 's' is a phantom type ensuring that identifiers
--     do not escape the scope of the system.
--
--   * 'd', for downwards, is the type of values that
--     can be received by the port.
--
--   * 'u', for upwards, is the type of values that
--     can be sent through the port.
newtype PortId s d u = PortId Integer
    deriving (Eq, Ord, Show)