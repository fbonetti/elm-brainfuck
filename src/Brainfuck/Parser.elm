module Brainfuck.Parser exposing (Command(..), parse)

import Parser exposing (..)
import Set exposing (Set)


type Command
    = Increment
    | Decrement
    | ShiftLeft
    | ShiftRight
    | OutputByte
    | InputByte
    | Loop (List Command)


parse : String -> Result Error (List Command)
parse code =
    run parser code


parser : Parser (List Command)
parser =
    commands |. end


commands : Parser (List Command)
commands =
    lazy (\_ -> repeat zeroOrMore command)


command : Parser Command
command =
    lazy
        (\_ ->
            succeed identity
                |. ignoreableChars
                |= oneOf
                    [ increment
                    , decrement
                    , shiftLeft
                    , shiftRight
                    , outputByte
                    , inputByte
                    , loop
                    ]
                |. ignoreableChars
        )


ignoreableChars : Parser ()
ignoreableChars =
    let
        commandChars =
            Set.fromList [ '+', '-', '<', '>', '.', ',', '[', ']' ]

        isIgnorableChar char =
            not (Set.member char commandChars)
    in
        ignore zeroOrMore isIgnorableChar


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
                |= commands
                |. symbol "]"
        )
