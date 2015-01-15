{-# LANGUAGE TupleSections, GADTs #-}

module BIP.Semantic where

import Prelude hiding (any)

import Control.Monad
import Data.Foldable (any)
import Data.Maybe (maybeToList)
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set

import BIP.Atom
import BIP.Connector hiding (close)
import BIP.Interaction
import BIP.WaitingList

getClosedInteractions :: ClosedConnector s -> WaitingList s -> [Interaction s]
getClosedInteractions (ClosedConnector c) w = fmap close $ getInteractions c w

-- | Gets the list of possible interactions. The list is computed lazily,
--   element by element.
getInteractions :: Connector s d u -> WaitingList s -> [OpenInteraction s d u]
getInteractions c w = getInteractionsWithout Set.empty c w

-- | Gets the list of possible interactions that do not involve
--   a specific set of atoms. The list is computed lazily,
--   element by element.
getInteractionsWithout :: Set (AtomId s) -> Connector s d u -> WaitingList s
                       -> [OpenInteraction s d u]
getInteractionsWithout as c w = case c of 
    -- Core combinators
    Success x    -> return (OpenInteraction x (const []) (Set.empty))
    Failure      -> mzero
    Bind a p     -> if Set.member a as
        then mzero 
        else maybeToList $ getBoundInteraction a p w
    OneOf c1 c2  -> mplus
        (getInteractionsWithout as c1 w)
        (getInteractionsWithout as c2 w)
    BothOf c1 c2 -> do
        o1 <- getInteractionsWithout as c1 w
        -- We specify in the recursive call that using atoms involved in the
        -- open interaction o1 is prohibited.
        o2 <- getInteractionsWithout (Set.union as $ getInvolved o1) c2 w
        let o3 = OpenInteraction
                { getUpwards   = (getUpwards o1, getUpwards o2)
                , getDownwards = \ x -> getDownwards o1 x ++ getDownwards o2 x
                , getInvolved  = Set.union (getInvolved o1) (getInvolved o2)
                }
        return o3

    -- Data combinators
    Mapped f c1 -> do
        o1 <- getInteractionsWithout as c1 w
        return $ o1 { getUpwards = f (getUpwards o1) }
    ContraMapped f c1 -> do
        o1 <- getInteractionsWithout as c1 w
        return $ o1 { getDownwards = getDownwards o1 . f }
    Guarded f c1 -> do
        o1 <- getInteractionsWithout as c1 w
        guard (f $ getUpwards o1)
        return o1
    Feedback c1 -> do
        o1 <- getInteractionsWithout as c1 w
        return $ o1 { getDownwards = 
            \ x -> getDownwards o1 (x, getUpwards o1) }

    -- Priority combinators

    -- Note that the first recursive call in the case of FirstOf and Maximal
    -- is made with an empty set of prohibited atoms. This is by design.
    -- It is not because an interaction might be considered downwards
    -- incompatible later on that it should loose its priority here.
    FirstOf c1 c2 -> case getInteractionsWithout Set.empty c1 w of
        []  -> getInteractionsWithout as c2 w
        os1 -> mfilter (not . any (flip Set.member as) . getInvolved) os1
    Maximal f c1 ->
        let os1 = getInteractionsWithout Set.empty c1 w
            upwardsValues = fmap getUpwards os1
            biggerThan x = \ y -> f y x == GT
            isMaximal x = not $ any (biggerThan x) upwardsValues
        in
        mfilter (isMaximal . getUpwards) $
        mfilter (not . any (flip Set.member as) . getInvolved) os1

    -- Dynamic combinators
    Dynamic p -> do
        o1 <- getBoundInteractions p w
        -- Filter out any interaction that involves an prohibited atom.
        guard $ not $ any (flip Set.member as) (getInvolved o1)
        return o1 
    Joined c1 -> do
        o1 <- getInteractionsWithout as c1 w
        -- We specify in the recursive call that using atoms involved in the
        -- open interaction o1 is prohibited.
        o2 <- getInteractionsWithout (Set.union as $ getInvolved o1)
                (getUpwards o1) w
        let o3 = OpenInteraction
                { getUpwards   = getUpwards o2
                , getDownwards = \ x -> getDownwards o1 x ++ getDownwards o2 x
                , getInvolved  = Set.union (getInvolved o1) (getInvolved o2)
                }
        return o3

-- | Returns for the given connector and given waiting list
--   a list of provably stable interactions.
getStableClosedInteractions :: ClosedConnector s -> WaitingList s
                            -> [Interaction s]
getStableClosedInteractions (ClosedConnector c) w =
    fmap close $ getStableInteractions c w

-- | Returns for the given connector and given waiting list
--   a list of provably stable interactions.
getStableInteractions :: Connector s d u -> WaitingList s
                      -> [OpenInteraction s d u]
getStableInteractions c w = fst $ getStableInteractionsWithout Set.empty c w

-- | Returns for the given connector and given waiting list
--   a list of provably stable interactions that do not use any
--   of the specified atoms.
--
--   Also indicates if the list of interactions is exhaustive,
--   that is if the state is maximal.
getStableInteractionsWithout :: Set (AtomId s)
                             -> Connector s d u
                             -> WaitingList s
                             -> ([OpenInteraction s d u], Bool)
getStableInteractionsWithout as c w = case c of
    -- Core combinators
    Success x    -> ([OpenInteraction x (const []) (Set.empty)], True)
    Failure      -> ([], True)
    Bind a p     -> case getBoundInteraction a p w of
        Nothing -> ([], False)
        Just o  -> (if Set.member a as then [] else [o], True)
    OneOf c1 c2  -> 
        let (os1, m1) = getStableInteractionsWithout as c1 w
            (os2, m2) = getStableInteractionsWithout as c2 w
        in (os1 ++ os2, m1 && m2)
    BothOf c1 c2 ->
        let (os1, m1) = getStableInteractionsWithout as c1 w
            (oss2, m2s) = unzip [ getStableInteractionsWithout 
                                    (Set.union as $ getInvolved o1) c2 w
                                | o1 <- os1 ]
        in (, m1 && and m2s) $ do
            -- The state is maximal for BothOf if it is maximal
            -- for c1 and maximal for c2.
            (o1, os2) <- zip os1 oss2  -- Zips together the open interactions
                                       -- of c1 with the corresponding open
                                       -- interactions of c2. 
            o2 <- os2
            let o3 = OpenInteraction
                    { getUpwards   = (getUpwards o1, getUpwards o2)
                    , getDownwards =
                        \ x -> getDownwards o1 x ++ getDownwards o2 x
                    , getInvolved  =
                        Set.union (getInvolved o1) (getInvolved o2)
                    }
            return o3

    -- Data combinators
    Mapped f c1 ->
        let (os1, m1) = getStableInteractionsWithout as c1 w
        in (, m1) $ do
            o1 <- os1
            return $ o1 { getUpwards = f (getUpwards o1) }
    ContraMapped f c1 -> 
        let (os1, m1) = getStableInteractionsWithout as c1 w
        in (, m1) $ do
            o1 <- os1
            return $ o1 { getDownwards = getDownwards o1 . f }
    Guarded f c1 ->
        let (os1, m1) = getStableInteractionsWithout as c1 w
        in (, m1) $ do
            o1 <- os1
            guard (f $ getUpwards o1)
            return o1
    Feedback c1 ->
        let (os1, m1) = getStableInteractionsWithout as c1 w
        in (, m1) $ do
            o1 <- os1
            return $ o1 { getDownwards = 
                \ x -> getDownwards o1 (x, getUpwards o1) }

    -- Priority combinators
    FirstOf c1 c2 -> case getStableInteractionsWithout Set.empty c1 w of
        -- No stable interaction, and no possibility to
        -- have such an interaction in the future.
        -- Therefore we respect the priority enabling c2.
        ([], True) -> getStableInteractionsWithout as c2 w
        -- There are, or they might be in the future, interactions
        -- coming from c1, therefore we propagate them.
        (os1, m1) ->
            (mfilter (not . any (flip Set.member as) . getInvolved) os1, m1)
    Maximal f c1 ->
        let (os1, m1) = getStableInteractionsWithout Set.empty c1 w
        in if m1 then
            -- The state is maximal. Therefore there won't
            -- possibly be any interaction in the future with
            -- higher priority.
            let upwardsValues = fmap getUpwards os1
                biggerThan x = \ y -> f y x == GT
                isMaximal x = not $ any (biggerThan x) upwardsValues
            in (mfilter (not . any (flip Set.member as) . getInvolved)
               $ mfilter (isMaximal . getUpwards) os1, True)
        else
            -- Their might be interactions with higher priority
            -- in the future, thus there is no (provably) stable
            -- interactions at this point.
            ([], False)

    -- Dynamic combinators
    Dynamic p -> (mfilter (not . any (flip Set.member as) . getInvolved) $
                    getBoundInteractions p w, False)
    Joined c1 ->
        let (os1, m1) = getStableInteractionsWithout as c1 w
            (oss2, m2s) = unzip [ getStableInteractionsWithout 
                                    (Set.union as $ getInvolved o1)
                                    (getUpwards o1) w
                                | o1 <- os1 ]
        in (, m1 && and m2s) $ do
            -- The state is maximal for Joined if it is maximal
            -- for c1 and maximal for all upwards values, which are
            -- themselves connectors.
            (o1, os2) <- zip os1 oss2  -- Zips together the open interactions
                                       -- of c1 with the open interactions 
                                       -- of the connector in
                                       -- its upwards value.
            o2 <- os2
            let o3 = OpenInteraction
                    { getUpwards   = getUpwards o2
                    , getDownwards =
                        \ x -> getDownwards o1 x ++ getDownwards o2 x
                    , getInvolved  =
                        Set.union (getInvolved o1) (getInvolved o2)
                    }
            return o3
