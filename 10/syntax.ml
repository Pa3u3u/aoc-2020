open Printf
open Toolbox
open Toolbox.Operators


module Parser = struct
    type state =
        | OK
        | Incomplete
        | Unexpected of char


    let state_str = function
        | OK -> "OK"
        | Incomplete -> "Incomplete"
        | Unexpected c -> sprintf "Unexpected '%c'" c


    module PairingsT = struct
        type t = char
        let compare = Char.compare
    end


    module Pairings = Map.Make(PairingsT)
    let pairs = Pairings.(empty
        |> add '(' ')'
        |> add '{' '}'
        |> add '<' '>'
        |> add '[' ']')


    let is_opening c =
        Pairings.mem c pairs


    let get_closing =
        Fun.flip Pairings.find pairs


    let parse: char list -> state =
        let rec descend (input: char list) (closing: char list): (state * char list) =
            match (input, closing) with
            | ([], []) -> (OK, [])
            | ([], rest) -> (Incomplete, rest)
            | (i::is, c::cs) when i == c -> descend is cs
            | (i::rest,  _::_) when not(is_opening i) -> (Unexpected i, rest)
            | (i::rest, cs) -> descend rest (get_closing i::cs) in


        let rec parse_chunk l: state = match descend l [] with
            | (OK, []) -> OK
            | (OK, rest) -> parse_chunk rest
            | (Incomplete, _) -> Incomplete
            | (Unexpected c, _) -> Unexpected c in

        parse_chunk
end


let syntax_error_score: (Parser.state Seq.t) -> int =
    let char_score = function
        | ')' -> 3
        | ']' -> 57
        | '}' -> 1197
        | '>' -> 25137
        | c   -> raise (Failure (sprintf "BUG: Parser returned an invalid state 'Unexpected %c'" c)) in

    let get_score acc: Parser.state -> int = function
        | Unexpected c -> acc + char_score c
        | _ -> acc in

    Seq.fold_left get_score 0


let () =
    let argc = Array.length Sys.argv - 1 in

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    File.as_seq Sys.argv.(1)
        |> Seq.map (peek (printf "# input %s\n"))
        |> Seq.map String.to_chars
        |> Seq.map Parser.parse
        |> Seq.map (peek (Parser.state_str >> printf "# %s\n"))
        |> syntax_error_score
        |> printf "%d\n"
