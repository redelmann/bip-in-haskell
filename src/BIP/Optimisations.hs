{-# LANGUAGE GADTs #-}

module BIP.Optimisations where

import Data.Set (Set)
import qualified Data.Set as Set

import BIP.Atom
import BIP.Connector

-- | Eliminates from the combinator all binds that are considered dead.
deadBindElimination :: Set (AtomId s) -> Connector s d u -> Connector s d u
deadBindElimination as c = case c of
    -- Eliminates the binding in case the atom is declared dead.
    Bind a p -> if Set.member a as then Failure else c

    -- In other cases, simply recursively call all branches.
    OneOf c1 c2 -> OneOf (deadBindElimination as c1) 
                         (deadBindElimination as c2)
    BothOf c1 c2 -> BothOf (deadBindElimination as c1)
                           (deadBindElimination as c2)
    Mapped f c1 -> Mapped f $ deadBindElimination as c1
    ContraMapped f c1 -> ContraMapped f $ deadBindElimination as c1
    Guarded f c1 -> Guarded f $ deadBindElimination as c1
    Feedback c1 -> Feedback $ deadBindElimination as c1
    FirstOf c1 c2 -> FirstOf (deadBindElimination as c1)
                             (deadBindElimination as c2)
    Maximal f c1 -> Maximal f $ deadBindElimination as c1
    Joined c1 -> Joined $ deadBindElimination as c1

    -- For all other leaf cases (Success, Failure, Dynamic), we do nothing.
    _ -> c

-- | Propagates failure and success up the connector.
simplify :: Connector s d u -> Connector s d u
simplify c = case c of

    -- In binary combinator cases.
    OneOf c1 c2 -> case (simplify c1, simplify c2) of
        (Failure, c2') -> c2'
        (c1', Failure) -> c1'
        (c1', c2') -> OneOf c1' c2'
    BothOf c1 c2 -> case (simplify c1, simplify c2) of
        (Failure, _) -> Failure
        (_, Failure) -> Failure
        (Success x1, Success x2) -> Success (x1, x2)
        (c1', c2') -> BothOf c1' c2'
    FirstOf c1 c2 -> case (simplify c1, simplify c2) of
        (Success x, _) -> Success x
        (Failure, c2') -> c2'
        (c1', Failure) -> c1'
        (c1', c2') -> FirstOf c1' c2'

    -- In unary combinator cases.
    Mapped f c1 -> case simplify c1 of
        Failure -> Failure
        Success x -> Success $ f x  -- f x is not forced here.
        c1' -> Mapped f c1'
    ContraMapped f c1 -> case simplify c1 of
        Failure -> Failure
        Success x -> Success x
        c1' -> ContraMapped f c1'
    Guarded f c1 -> case simplify c1 of
        Failure -> Failure
        -- Note that we do not want to evaluate the condition
        -- in here in the case of success. This might not terminate,
        -- and we can not return the connector lazily.
        c1' -> Guarded f c1'
    Feedback c1 -> case simplify c1 of
        Failure -> Failure
        Success x -> Success x
        c1' -> Feedback c1'
    Maximal f c1 -> case simplify c1 of
        Failure -> Failure
        Success x -> Success x
        c1' -> Maximal f c1'
    Joined c1 -> case simplify c1 of
        Failure -> Failure
        -- We might be tempted here to handle the Success case
        -- differently. However, we must refrain from doing so
        -- because evaluating the value contained in success
        -- might be expensive, or even not terminate.
        c1' -> Joined c1'

    -- In all other cases (Success, Failure, Bind, Dynamic), we do nothing.
    _ -> c





