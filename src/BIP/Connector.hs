{-# LANGUAGE GADTs #-}

-- | This module describes `Connector`s, which are
--   used to connect together the different ports
--   and atoms of a system in a meaningful way.
module BIP.Connector where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Ord
import Data.Profunctor

import BIP.Atom (AtomId)
import BIP.Port (PortId)

-- | Contains a single connector whose upwards and downwards type,
--   whatever they may be, are the same.
data ClosedConnector s where
    ClosedConnector :: Connector s a a -> ClosedConnector s

-- | Connects together the different ports of a system.
--
--   * 's' corresponds to the type phantom type of the system,
--     which ensures that identifiers do not escape the system
--     in which they are defined.
--
--   * 'd' is the type of values propagated downwards by the connector.
--
--   * 'u' is the type of values progagated upwards by the connector.
data Connector s d u where
    -- Core combinators
    Bind    :: AtomId s -> PortId s d u -> Connector s d u
    Success :: u -> Connector s d u
    Failure :: Connector s d u
    OneOf   :: Connector s d u -> Connector s d u -> Connector s d u
    BothOf  :: Connector s d u -> Connector s d v -> Connector s d (u, v)

    -- Data combinators
    Mapped       :: (u -> v) -> Connector s d u -> Connector s d v
    ContraMapped :: (e -> d) -> Connector s d u -> Connector s e u
    Guarded      :: (u -> Bool) -> Connector s d u -> Connector s d u
    Feedback     :: Connector s (d, u) u -> Connector s d u

    -- Priority combinators
    Maximal :: (u -> u -> Ordering) -> Connector s d u -> Connector s d u
    FirstOf :: Connector s d u -> Connector s d u -> Connector s d u

    -- Dynamic combinators
    Dynamic :: PortId s d u -> Connector s d u
    Joined  :: Connector s d (Connector s d u) -> Connector s d u

-- ** Type class instances

instance Functor (Connector s d) where
    fmap f c = Mapped f c

instance Profunctor (Connector s) where
    lmap f c = ContraMapped f c
    rmap f c = Mapped f c

instance Applicative (Connector s d) where
    pure x = Success x
    c1 <*> c2 = Mapped (uncurry ($)) $ BothOf c1 c2

instance Alternative (Connector s d) where
    empty = Failure
    c1 <|> c2 = OneOf c1 c2
    many c = pure [] <|> some c
    some c = (:) <$> c <*> many c

instance Monad (Connector s d) where
    return x = Success x
    c >>= f = Joined (Mapped f c)
    c1 >> c2 = Mapped snd $ BothOf c1 c2
    fail _ = Failure

instance MonadPlus (Connector s d) where
    mzero = Failure
    mplus c1 c2 = OneOf c1 c2

instance Monoid u => Monoid (Connector s d u) where
    mempty = Success mempty
    mappend c1 c2 = Mapped (uncurry mappend) $ BothOf c1 c2


-- ** Library functions

-- | Connects the given port to the specified atom.
bind :: AtomId s -> PortId s d u -> Connector s d u
bind a p = Bind a p

-- | Chooses one of the given connector non-deterministically.
anyOf :: [Connector s d u] -> Connector s d u
anyOf cs = foldr OneOf Failure cs

-- | Synchronizes all underlying connectors and collects all upwards values.
allOf :: [Connector s d u] -> Connector s d [u]
allOf cs = mconcat $ fmap (fmap (: [])) cs

-- | Involves any number of the underlying connectors.
manyOf :: [Connector s d u] -> Connector s d [u]
manyOf cs = mconcat [fmap (: []) c <|> pure [] | c <- cs] 

-- | Behaves as the first enabled connector of the given list.
firstOf :: [Connector s d u] -> Connector s d u
firstOf cs = foldr FirstOf Failure cs

-- | Applies a function of upwards values.
upwards :: (u -> v) -> Connector s d u -> Connector s d v
upwards f c = Mapped f c

-- | Always uses the specified upwards value.
sending :: v -> Connector s d u -> Connector s d v
sending x c = upwards (const x) c

-- | Applies a function of downwards values.
downwards :: (e -> d) -> Connector s d u -> Connector s e u
downwards f c = ContraMapped f c

-- | Always uses the specified downwards value.
receiving :: d -> Connector s d u -> Connector s e u
receiving x c = downwards (const x) c

-- | Ensures that a given predicate holds on upwards values.
ensuring :: (u -> Bool) -> Connector s d u -> Connector s d u
ensuring f c = Guarded f c

-- | Feeds the upwards value downwards.
feedback :: Connector s (d, u) u -> Connector s d u
feedback c = Feedback c

-- | Closes the connector, using the upwards value as downwards value.
close :: Connector s a a -> Connector s d ()
close c = sending () $ feedback $ downwards snd c

-- | Ensures that the upwards value is maximal amongst the possible
--   upwards values.
maximal :: Ord u => Connector s d u -> Connector s d u
maximal c = Maximal compare c

-- | Ensures that the upwards value is minimal amongst the possible
--   upwards values.
minimal :: Ord u => Connector s d u -> Connector s d u
minimal c = Maximal (comparing Down) c

-- | Ensures that the upwards value is maximal amongst the possible
--   upwards values.
maximalBy :: (u -> u -> Ordering) -> Connector s d u -> Connector s d u
maximalBy f c = Maximal f c

-- | Ensures that the upwards value is minimal amongst the possible
--   upwards values.
minimalBy :: (u -> u -> Ordering) -> Connector s d u -> Connector s d u
minimalBy f c = Maximal (\ a b -> inverse $ f a b) c
  where
    inverse LT = GT
    inverse EQ = EQ
    inverse GT = LT

-- | Connects the given port non-deterministically to any atom.
dynamic :: PortId s d u -> Connector s d u
dynamic p = Dynamic p


