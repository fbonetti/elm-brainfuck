module Brainfuck.Parser exposing (Command(..), parser)

import Parser exposing (..)


type Command
    = Increment
    | Decrement
    | ShiftLeft
    | ShiftRight
    | OutputByte
    | InputByte
    | Loop (List Command)


parser : Parser (List Command)
parser =
    repeat zeroOrMore anyCommand


anyCommand : Parser Command
anyCommand =
    lazy
        (\_ ->
            oneOf
                [ increment
                , decrement
                , shiftLeft
                , shiftRight
                , outputByte
                , inputByte
                , loop
                ]
        )


increment : Parser Command
increment =
    map (always Increment) (symbol "+")


decrement : Parser Command
decrement =
    map (always Decrement) (symbol "-")


shiftLeft : Parser Command
shiftLeft =
    map (always ShiftLeft) (symbol "<")


shiftRight : Parser Command
shiftRight =
    map (always ShiftRight) (symbol ">")


outputByte : Parser Command
outputByte =
    map (always OutputByte) (symbol ".")


inputByte : Parser Command
inputByte =
    map (always InputByte) (symbol ",")


loop : Parser Command
loop =
    lazy
        (\_ ->
            succeed Loop
                |. symbol "["
                |= parser
                |. symbol "]"
        )
