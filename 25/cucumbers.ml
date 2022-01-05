open Printf
open Toolbox
open Toolbox.Core
open Toolbox.Extensions
open Toolbox.Operators

module Pos = struct
    let to_string (a, b) = sprintf "[%d, %d]" a b
end

module Tile = struct
    type t = E | R | D

    let init = E

    let parse = function
        | '.' -> E
        | '>' -> R
        | 'v' -> D
        | c -> raise (Failure (sprintf "Invalid character: %c" c))

    let parse_line =
        String.to_chars >> List.map parse

    let to_string = function
        | E -> "\x1b[97m·\x1b[0m"
        | R -> "\x1b[93m▶\x1b[0m"
        | D -> "\x1b[96m▼\x1b[0m"

    let to_string_u = function
        | E -> "."
        | R -> ">"
        | D -> "v"
end

module Plan = struct
    module M = Matrix.Make(Tile)
    type t = M.m

    let create: Tile.t list list -> t =
        M.from_lists

    let print m =
        let print_row r =
            printf "";
            Array.iter (Tile.to_string >> printf "%s") r;
            printf "\n" in

        printf "# --- Plan ---\n";
        Array.iter print_row m

    module IntPair = struct
        type t = int * int
        let compare (a, b) (c, d) =
            List.zip_with Int.compare [a; b] [c; d]
                |> List.find_opt ((!=) 0) |> Option.value ~default:0
    end

    module K = Map.Make(IntPair)
    type tk = Tile.t K.t

    let to_map m =
        let fold p k = function
            | Tile.E -> k
            | D -> K.add p Tile.D k
            | R -> K.add p Tile.R k in
        (M.foldi_left fold K.empty m, (M.width m, M.height m))

    let from_map (k, (w, h)) =
        let extract pos p = match K.find_opt pos k with
            | None | Some Tile.E -> p
            | Some v -> v in

        M.create w h
            |> M.mapi extract

    let next (k, (w, h)) =
        let ppos (a, b) (c, d) =
            ((a + c + w) mod w, (b + d + h) mod h) in

        let move_r (k, (w, h)) =
            let move (pos, v) = match v with
                | Tile.R -> let npos = ppos pos (1, 0) in
                    if not (K.mem npos k)
                    then (npos, v)
                    else (pos, v)
                | x -> (pos, x) in
            (K.bindings k |> List.map move |> List.to_seq |> K.of_seq, (w, h)) in

        let move_d (k, (w, h)) =
            let move (pos, v) = match v with
                | Tile.D -> let npos = ppos pos (0, 1) in
                    if not (K.mem npos k)
                    then (npos, v)
                    else (pos, v)
                | x -> (pos, x) in
            (K.bindings k |> List.map move |> List.to_seq |> K.of_seq, (w, h)) in

        move_r (k, (w, h)) |> move_d

    let next_n n m =
        (1 &-- n) |> List.fold_left (fun m' _ -> next m') m

    let move_until_stop m =
        let rec mus n m =
            let m2 = next m in
            if m2 = m then (n + 1, m) else mus (n + 1) m2 in

        mus 0 m
end

let () =
    let argc = Array.length Sys.argv - 1 in

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    Printexc.record_backtrace true;
    File.as_seq Sys.argv.(1)
        |> Seq.map Tile.parse_line
        |> List.of_seq
        |> Plan.create
        |> Fun.peek Plan.print
        |> (Plan.to_map >> Plan.move_until_stop >> Pair.second Plan.from_map)
        |> fun (n, m) ->
                Plan.print m; printf "%d\n" n;
