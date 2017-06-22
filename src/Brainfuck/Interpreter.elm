module Brainfuck.Interpreter exposing (run)

import Brainfuck.Parser exposing (Command(..))
import Brainfuck.Tape as Tape exposing (Tape)
import Char


type alias State =
    { tape : Tape
    , output : String
    , input : String
    }


run : String -> String -> Result String State
run code input =
    Brainfuck.Parser.parse code
        |> Result.mapError toString
        |> Result.map (List.foldl eval (init input))


init : String -> State
init input =
    State Tape.empty "" input


eval : Command -> State -> State
eval command ({ tape, output, input } as state) =
    case command of
        Increment ->
            { state | tape = Tape.increment tape }

        Decrement ->
            { state | tape = Tape.decrement tape }

        ShiftLeft ->
            { state | tape = Tape.shiftLeft tape }

        ShiftRight ->
            { state | tape = Tape.shiftRight tape }

        OutputByte ->
            { state | output = output ++ getTapeByte tape }

        InputByte ->
            case String.uncons input of
                Just ( byte, rest ) ->
                    { state | tape = Tape.set (Char.toCode byte) tape, input = rest }

                Nothing ->
                    state

        Loop commands ->
            if Tape.get tape == 0 then
                state
            else
                eval (Loop commands) (List.foldl eval state commands)


getTapeByte : Tape -> String
getTapeByte =
    Tape.get >> Char.fromCode >> String.fromChar
