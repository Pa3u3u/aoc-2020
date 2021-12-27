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
    let win = 1000
end

class dice = object
    val mutable next = 0
    val mutable rolls = 0

    method roll =
        let r = next + 1 in
            rolls <- rolls + 1;
            next <- (next + 1) mod 100;
            r

    method rolls =
        rolls
end

class player (start: int) = object (self)
    val mutable score = 0
    val mutable pos = start - 1

    method private add_score n =
        score <- score + n

    method move rolls =
        let total = List.sum rolls in
        pos <- (pos + total) mod Conf.plan_size;
        self#add_score (pos + 1);

    method pos =
        pos + 1

    method score =
        score
end

module Game = struct
    type t = player * player

    let create: int list -> t =
        List.map (new player) >> function
            | [p1; p2] -> (p1, p2)
            | _ -> raise (Failure "Invalid number of players")

    let play (players: t): (int * int * dice * t) =
        let count = 2 in
        let rec play' round pidx d (p, q) =
            let next_round = round + (pidx + 1) / count
            and next_pidx = (pidx + 1) mod count in

            printf "# Round %d player %d: " (round + 1) (pidx + 1);
            0 &-- Conf.rolls
                |> List.map (fun _ -> d#roll) |> p#move
                |> Fun.peek (fun _ -> printf "move to %d, score %d\n" p#pos p#score)
                |> function
                    | _ when p#score >= Conf.win -> (round, pidx, d, (p, q))
                    | _ -> play' next_round next_pidx d (q, p) in

        play' 0 0 (new dice) players

    let result (round, player, dice, (winner, loser)) =
        printf "# Player %d won in round %d\n" player round;
        printf "#   Winner: position %d score %d\n" winner#pos winner#score;
        printf "#   Loser:  position %d score %d\n" loser#pos loser#score;
        printf "# Dice has been rolled %d times\n" dice#rolls;
        printf "%d\n" (dice#rolls * loser#score)
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
