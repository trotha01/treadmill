module Zipper
    exposing
        ( Zipper(..)
        , singleton
        , fromList
        , fromListWithFocus
        , withDefault
        , appendList
        , appendItem
        , delete
        , length
        , before
        , current
        , after
        , toList
        , map
        , indexedMap
        , mapBefore
        , mapCurrent
        , mapAfter
        , foldl
        , first
        , previous
        , next
        , safeNext
        , last
        , find
        )

{-| A zipper for `List`.

# The `Zipper` type
@docs Zipper

# Constructing a `Zipper`
@docs singleton, fromList, fromListWithFocus, withDefault, appendItem, appendList

# Destructing a `Zipper`
@docs delete

# Attributes
@docs length

# Accessors
@docs before, current, after, toList

# Mapping
@docs map, indexedMap, mapBefore, mapCurrent, mapAfter, foldl

# Moving around
@docs first, previous, next, safeNext, last, find

-}

import List exposing (reverse)


{-| The `Zipper` type.
-}
type Zipper a
    = Zipper (List a) a (List a)


{-| Construct a `Zipper` focussed on the first element of a singleton list.
-}
singleton : a -> Zipper a
singleton x =
    Zipper [] x []


{-| Construct a `Zipper` from a list. The `Zipper` will focus on the first element (if there is a first element).
-}
fromList : List a -> Maybe (Zipper a)
fromList xs =
    case xs of
        [] ->
            Nothing

        y :: ys ->
            Just (Zipper [] y ys)


{-| Construct a `Zipper` from a list with an initial item as the focus
-}
fromListWithFocus : a -> List a -> Zipper a
fromListWithFocus x xs =
    Zipper [] x xs


{-| Provide an alternative when constructing a `Zipper` fails.
-}
withDefault : a -> Maybe (Zipper a) -> Zipper a
withDefault x =
    Maybe.withDefault (singleton x)


{-| Append an item after the focus
-}
appendItem : a -> Zipper a -> Zipper a
appendItem y (Zipper ls x rs) =
    Zipper ls x (y :: rs)


{-| Append a list to the zipper
-}
appendList : List a -> Zipper a -> Zipper a
appendList xs (Zipper ls x rs) =
    Zipper ls x (rs ++ xs)


{-| Delete the current item, move cursor right
-}
delete : Zipper a -> Maybe (Zipper a)
delete (Zipper ls x rs) =
    case rs of
        r :: rs ->
            Just <| Zipper ls r rs

        rs ->
            case ls of
                l :: ls ->
                    Just <| Zipper ls l rs

                ls ->
                    Nothing


length : Zipper a -> Int
length (Zipper ls x rs) =
    (List.length ls) + 1 + (List.length rs)


{-| Returns all elements before the element the `Zipper` is focussed on.
-}
before : Zipper a -> List a
before (Zipper ls _ _) =
    reverse ls


{-| Returns the element the `Zipper` is currently focussed on.
-}
current : Zipper a -> a
current (Zipper _ x _) =
    x


{-| Returns all elements after the element the `Zipper` is focussed on
-}
after : Zipper a -> List a
after (Zipper _ _ rs) =
    rs


{-| Reconstruct the list.
-}
toList : Zipper a -> List a
toList z =
    before z ++ [ current z ] ++ after z


{-| Apply a function to every element in the `Zipper`.
-}
map : (a -> b) -> Zipper a -> Zipper b
map f (Zipper ls x rs) =
    Zipper (List.map f ls) (f x) (List.map f rs)


{-| Apply a function to every element in the `Zipper` with an index.
-}
indexedMap : (Int -> a -> b) -> Zipper a -> Zipper b
indexedMap f (Zipper ls x rs) =
    let
        newLs =
            (List.indexedMap f ls)

        ( _, newRs ) =
            List.foldr
                (\r ( i, nrs ) -> ( i + 1, (f i r) :: nrs ))
                ( ((List.length ls) + 1), [] )
                rs
    in
        Zipper newLs (f (List.length ls) x) newRs


{-| Apply a function to all elements before the element the `Zipper` is focussed on.
-}
mapBefore : (List a -> List a) -> Zipper a -> Zipper a
mapBefore f ((Zipper _ x rs) as zipper) =
    let
        elementsBefore =
            before zipper

        mappedElementsBefore =
            f elementsBefore
    in
        Zipper (reverse mappedElementsBefore) x rs


{-| Apply a function to the element the `Zipper` is focussed on.
-}
mapCurrent : (a -> a) -> Zipper a -> Zipper a
mapCurrent f (Zipper ls x rs) =
    Zipper ls (f x) rs


{-| Apply a function to all elements after the element the `Zipper` is focussed on.
-}
mapAfter : (List a -> List a) -> Zipper a -> Zipper a
mapAfter f (Zipper ls x rs) =
    Zipper ls x (f rs)


{-| Reduce the zipper, starting with the focus and moving right
-}
foldl : (Zipper a -> b -> b) -> b -> Zipper a -> b
foldl f z ((Zipper ls x rs) as zip) =
    case next zip of
        Nothing ->
            (f zip z)

        Just nzip ->
            foldl f (f zip z) (nzip)


{-| Move the focus to the first element of the list.
-}
first : Zipper a -> Zipper a
first ((Zipper ls x rs) as zipper) =
    case reverse ls of
        [] ->
            zipper

        y :: ys ->
            Zipper [] y (ys ++ [ x ] ++ rs)


{-| Move the focus to the element before the element the `Zipper` is currently focussed on (if there is such an element).
-}
previous : Zipper a -> Maybe (Zipper a)
previous (Zipper ls x rs) =
    case ls of
        [] ->
            Nothing

        y :: ys ->
            Just <| Zipper ys y (x :: rs)


{-| Move the focus to the element after the element the `Zipper` is currently focussed on (if there is such an element).
-}
next : Zipper a -> Maybe (Zipper a)
next (Zipper ls x rs) =
    case rs of
        [] ->
            Nothing

        y :: ys ->
            Just <| Zipper (x :: ls) y ys


{-| Move the focus to the element after the element the `Zipper` is currently focussed on. If the focus is at the end, it goes to the beginning
-}
safeNext : Zipper a -> Zipper a
safeNext (Zipper ls x rs) =
    case rs of
        [] ->
            first (Zipper ls x rs)

        y :: ys ->
            Zipper (x :: ls) y ys


{-| Move the focus to the last element of the list.
-}
last : Zipper a -> Zipper a
last ((Zipper ls x rs) as zipper) =
    case reverse rs of
        [] ->
            zipper

        y :: ys ->
            Zipper (ys ++ [ x ] ++ ls) y []


{-| Returns a `Zipper` focussed on the first element for which the predicate returns `True` (starting from a given `Zipper`).
-}
find : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
find predicate ((Zipper ls x rs) as zipper) =
    if predicate x then
        Just zipper
    else
        case next zipper of
            Just nextZipper ->
                find predicate nextZipper

            Nothing ->
                Nothing
