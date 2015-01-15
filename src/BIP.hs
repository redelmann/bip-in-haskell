
module BIP (
    -- * Systems
      System
    , runSystem
    , runSystemWith
    -- ** System instructions
    , newPort
    , newAtom
    , registerConnector
    -- ** Options
    , SystemOptions(..)
    , defaultOptions
    -- * Behavior layer
    , PortId
    , AtomId
    , Action
    -- ** Atom instructions
    , liftIO
    , spawn
    , getSelfId
    , await
    , awaitAny
    , AwaitCase
    , onPort
    -- * Glue layer
    , Connector
    -- ** Binding ports
    , bind
    , dynamic
    -- ** Combining combinators
    , allOf
    , anyOf
    , manyOf
    -- ** Data manipulation combinators
    , upwards
    , downwards
    , sending
    , receiving
    , ensuring
    , feedback
    , close
    -- ** Priority combinators
    , firstOf
    , maximal
    , minimal
    , maximalBy
    , minimalBy
    -- * Utilities
    , (#)
    -- * Internal details
    , Interaction
    , Task(..)
    ) where

import Control.Monad.Trans

import BIP.Atom
import BIP.Connector
import BIP.Engine
import BIP.Interaction hiding (close)
import BIP.Port
import BIP.System

infixl 0 #
-- | Function application operator.
(#) :: a -> (a -> b) -> b
x # f = f x