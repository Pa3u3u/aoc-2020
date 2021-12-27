open Printf
open Toolbox.Core
open Toolbox.Extensions
open Toolbox.Operators
open Toolbox.Parser

module Input = struct
    include CharParser

    let ppos =
        many (skip_until any (str ": ") @>> integer |> then_skip eol)

    let parse =
        Seq.resident >> run ppos () >> function
            | Ok v -> v
            | Error (s, _) -> raise (Failure s)
end

module Conf = struct
    let rolls = 3
    let plan_size = 10
    let win = 21
end

module Dice = struct
    let condense l =
        let rec condense' (v, c) = function
            | [] -> [(v, c)]
            | w::xs when v = w -> condense' (v, c + 1) xs
            | w::xs -> (v, c) :: condense' (w, 1) xs in
        condense' (List.hd l, 1) (List.tl l)


    class dice = object
        val rolls =
            List.product (List.product (1 &-- 4) (1 &-- 4)) (1 &-- 4)
                |> List.map (fun ((a, b), c) -> a + b + c)
                |> List.sort Int.compare
                |> condense

        method rolls =
            rolls
    end
end

class dice = Dice.dice

module Player = struct
    type t = int * int

    let create p = (0, p - 1)

    let score = fst
    let pos = snd

    let move ((s, p): t) n =
        let new_p = (p + n) mod Conf.plan_size in
        (s + new_p + 1, new_p)

    let won ((s, _): t) = s >= Conf.win
end

type player = Player.t

module Game = struct
    type round =
        | P1 | P2

    let next_round = function
        | P1 -> P2
        | P2 -> P1

    type t = player * player * round

    let create: int list -> t =
        List.map Player.create >> function
            | [p1; p2] -> (p1, p2, P1)
            | _ -> raise (Failure "Invalid number of players")

    let equal (p1, q1, f1) (p2, q2, f2) =
        f1 = f2 && p1 = p2 && q1 = q2

    let advance dice ((p, q, f), c) =
        let do_roll (wins, games) (move, card) =
            let new_p = Player.move p move in
            if Player.won new_p
            then wins + (c * card), games
            else wins, ((q, new_p, next_round f), c * card) :: games in

        dice#rolls |> List.fold_left do_roll (0, []) |> function
            | (w, g) when f = P1 -> (w, 0, g)
            | (w, g) -> (0, w, g)

    let join games =
        let rec join_game all (g, c) = match all with
            | [] -> [(g, c)]
            | (g', c')::rest when equal g g' -> (g', c + c')::rest
            | w::rest -> w :: join_game rest (g, c) in
        List.fold_left join_game games

    let play (game: t): (int * int) =
        let rec play' (wp, wq) d l =
            (*printf "# Remaining %d games\n" (List.length l);
            Stdlib.(flush stdout); *)
            match l with
            | [] -> (wp, wq)
            | g::rest ->
                let (dp, dq, new_games) = advance d g in
                play' (wp + dp, wq + dq) d (join rest new_games) in
        play' (0, 0) (new dice) [(game, 1)]

    let result (wp, wq) =
        let max a b = if a > b then a else b in
        printf "# Player 1 won %d times\n" wp;
        printf "# Player 2 won %d times\n" wq;
        printf "%d\n" (List.fold_left max 0 [wp; wq])
end

let () =
    let argc = Array.length Sys.argv - 1 in

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    Printexc.record_backtrace true;

    File.as_char_seq Sys.argv.(1)
        |> Input.parse
        |> Game.create
        |> Game.play
        |> Game.result
