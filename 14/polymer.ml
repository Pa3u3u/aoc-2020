open Printf
open Toolbox
open Toolbox.Operators
open Toolbox.Pair

exception Invalid_input of string

module Rule = struct
    type t =  (char * char) * char
end


module Polymer = struct
    module CharPairMap = Map.Make(struct
        type t = char * char
        let compare (a1, a2) (b1, b2) = match Char.compare a1 b1 with
            | 0 -> Char.compare a2 b2
            | n -> n
    end)

    type rules_t = char CharPairMap.t
    type poly_t = int CharPairMap.t


    let rec pairs = function
        | a::b::rest -> (a, b) :: pairs (b::rest)
        | a::[] -> [(a, Char.chr 0)]
        | [] -> []

    let inc_opt (n: int) = function
        | Some k -> Some (k + n)
        | None -> Some n

    let fold (rules: rules_t): poly_t -> poly_t =
        let apply_rules ((x, y), n) rules = match CharPairMap.find_opt (x, y) rules with
            | Some c -> [((x, c), n); ((c, y), n)]
            | None -> [((x, y), n)] in

        CharPairMap.bindings >> List.map (Fun.flip apply_rules rules)
            >> List.flatten
            >> List.fold_left (fun m (p, v) -> CharPairMap.update p (inc_opt v) m) CharPairMap.empty


    let fold_stats (n: int) (rules: rules_t): char list -> poly_t =
        pairs >> List.fold_left (fun m p -> CharPairMap.update p (inc_opt 1) m) CharPairMap.empty
            >> fun p -> List.fold_left (fun p _ -> fold rules p) p (0 &-- n)


    let compile_rules: Rule.t list -> rules_t =
        List.fold_left (fun m (k, v) -> CharPairMap.add k v m) CharPairMap.empty


    module CharMap = Map.Make(struct
        type t = char
        let compare = Char.compare
    end)

    type stat_t = int CharMap.t

    let get_stats: poly_t -> stat_t =
        CharPairMap.bindings
            >> List.fold_left (fun m ((x, _), v) -> CharMap.update x (inc_opt v) m) CharMap.empty


    let select_elements: stat_t -> ((char * int) * (char * int)) =
        let select_max_min l =
            (List.hd l, List.rev l |> List.hd) in
        CharMap.bindings
            >> List.sort (fun (_, x) (_, y) -> Int.compare y x)
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
        |> second Polymer.compile_rules
        |> swap |> uncurry (Polymer.fold_stats repeats)
        |> Polymer.get_stats
        |> Polymer.select_elements
        |> Fun.peek (fun ((a, ac), (b, bc)) -> printf "# (%c: %d) (%c: %d)\n" a ac b bc)
        |> both snd |> uncurry (-) |> printf "%d\n"
