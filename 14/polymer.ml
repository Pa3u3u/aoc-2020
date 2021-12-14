open Printf
open Toolbox
open Toolbox.Operators

exception Invalid_input of string

module Rule = struct
    type t =  (char * char) * char
end


module Polymer = struct
    module RuleMap = Map.Make(struct
        type t = char * char
        let compare (a1, a2) (b1, b2) = match Char.compare a1 b1 with
            | 0 -> Char.compare a2 b2
            | n -> n
    end)

    type rules_t = char RuleMap.t

    let expand_once (pseq: char Seq.t) (rules: rules_t) =
        let apply a b rm = match RuleMap.find_opt (a, b) rm with
            | Some c -> [a; c]
            | None -> [a] in

        let term = Char.chr 0 in
        let unfolder ((a, s): (char * char Seq.t)) = match s () with
            | Cons (b, rest) -> Some (apply a b rules, (b, rest))
            | Nil when a != term -> Some ([a], (term, Seq.empty))
            | Nil -> None in

        match pseq () with
            | Cons (v, rest) -> Seq.unfold unfolder (v, rest)
            | Nil -> Seq.empty

    let fold_expand_once n poly =
        printf "# Expansion %d\n" (n + 1); Stdlib.(flush stdout);
        expand_once poly >> Seq.flat_map List.to_seq

    let expand (n: int) (poly: char Seq.t) (rules: Rule.t list): char Seq.t =
        let rule_map = List.fold_left (fun m (k, v) -> RuleMap.add k v m) RuleMap.empty rules in
        List.fold_left (fun p n -> fold_expand_once n p rule_map) poly (0 &-- n)

    module CharMap = Map.Make(struct
        type t = char
        let compare = Char.compare
    end)

    type stat_t = int CharMap.t

    let statistics: char Seq.t -> stat_t =
        let counter = ref 0 in
        let update_stats stats c =
            if !counter mod 1000 == 0 then
                printf "# F %d\x1b[K\r" !counter;
            counter := !counter + 1;

            let inc_count = function
                | None -> Some 1
                | Some n -> Some (n + 1) in
            CharMap.update c inc_count stats in
        Seq.fold_left update_stats (CharMap.empty)

    let select_elements: stat_t -> ((char * int) * (char * int)) =
        let select_max_min l =
            (List.hd l, List.rev l |> List.hd) in
        CharMap.bindings >> List.sort (fun (_, x) (_, y) -> Int.compare y x)
            >> select_max_min
end


let parse_input: string Seq.t -> (char list * Rule.t list) =
    let parse_header (seq: string Seq.t): (char list * string Seq.t) = match seq () with
        | Cons (str, rest) -> (String.to_chars str, rest)
        | Nil -> raise (Invalid_input "Empty input") in

    let parse_rules: string Seq.t -> Rule.t list =
        let regex = Str.regexp "\\([A-Z]+\\) -> \\([A-Z]\\)" in
        let unpack = function
            | [a; b] -> (a, b)
            | _ -> raise (Invalid_input "Invalid number of resources in the rule") in
        let parse_rule line =
            if not (Str.string_match regex line 0) then
                None
            else
                Some (Str.matched_group 1 line |> String.to_chars |> unpack,
                        Str.matched_group 2 line |> Fun.flip String.get 0) in
        Seq.filter_map parse_rule >> List.of_seq in

    parse_header >> second parse_rules


let () =
    let argc = Array.length Sys.argv - 1 in

    if argc < 1 || argc > 2 then begin
        Printf.eprintf "usage: %s FILE [N]\n" Sys.argv.(0);
        exit 1;
    end;

    let repeats = if argc == 1 then 40 else Num.parse_int_exn Sys.argv.(2) in

    File.as_seq Sys.argv.(1)
        |> parse_input
        |> first List.to_seq
        |> uncurry (Polymer.expand repeats)
        |> Polymer.statistics
        |> Polymer.select_elements
        |> peek (fun ((a, ac), (b, bc)) -> printf "# (%c: %d) (%c: %d)\n" a ac b bc)
        |> both snd |> uncurry (-) |> printf "%d\n"
