module Brainfuck.Tape exposing (Tape, empty, toList, get, set, increment, decrement, shiftLeft, shiftRight)


type Tape
    = Tape Int (List Int) (List Int)


type Byte
    = Byte Int


empty : Tape
empty =
    Tape 0 [] []


toList : Tape -> List Int
toList (Tape value left right) =
    List.concat
        [ List.reverse left
        , [ value ]
        , right
        ]


get : Tape -> Int
get (Tape value _ _) =
    value


set : Int -> Tape -> Tape
set value (Tape _ left right) =
    Tape (value % 255) left right


increment : Tape -> Tape
increment (Tape value left right) =
    Tape ((value + 1) % 255) left right


decrement : Tape -> Tape
decrement (Tape value left right) =
    Tape ((value - 1) % 255) left right


shiftLeft : Tape -> Tape
shiftLeft (Tape value left right) =
    case left of
        x :: xs ->
            Tape x xs (value :: right)

        [] ->
            Tape 0 [] (value :: right)


shiftRight : Tape -> Tape
shiftRight (Tape value left right) =
    case right of
        x :: xs ->
            Tape x (value :: left) xs

        [] ->
            Tape 0 (value :: left) []
