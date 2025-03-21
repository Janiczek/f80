module NonemptyList exposing
    ( NonemptyList
    , cons
    , fromCons
    , fromList
    , head
    , length
    , mapHead
    , singleton
    , tail
    )


type alias NonemptyList a =
    ( a, List a )


singleton : a -> NonemptyList a
singleton x =
    ( x, [] )


fromCons : a -> List a -> NonemptyList a
fromCons x xs =
    ( x, xs )


cons : a -> NonemptyList a -> NonemptyList a
cons x ( y, ys ) =
    ( x, y :: ys )


head : NonemptyList a -> a
head ( x, _ ) =
    x


length : NonemptyList a -> Int
length ( _, xs ) =
    1 + List.length xs


fromList : List a -> Maybe (NonemptyList a)
fromList xs =
    case xs of
        [] ->
            Nothing

        y :: ys ->
            Just ( y, ys )


tail : NonemptyList a -> Maybe (NonemptyList a)
tail ( _, xs ) =
    fromList xs


mapHead : (a -> a) -> NonemptyList a -> NonemptyList a
mapHead f ( x, xs ) =
    ( f x, xs )
