open Printf
open Toolbox
open Toolbox.Operators
open Toolbox.Pair


exception Invalid_input of string

module DispEnc = struct
    type t = char list array


    let create = Array.make 10 []


    type test =
        | Length of int
        | Contains of int
        | ContainsNot of int
        | IsContainedIn of int
        | IsNotContainedIn of int


    type category = test list


    let rec subseq (sub: char list) (super: char list): bool =
        match (sub, super) with
            | ([], _) -> true
            | (_, []) -> false
            | (s::xs, p::xp) when s == p -> subseq xs xp
            | (xs, _::xp) -> subseq xs xp


    let nsubseq a b = not (subseq a b)


    let join_chars seq =
        String.init (List.length seq) (List.nth seq)


    let print (enc: t): unit =
        printf "#";
        Array.iteri (fun n seq -> printf " %d:%-7s" n (join_chars seq)) enc;
        printf "\n"


    let assign categories (enc, inputs) n =
        let check (t: test) (enc: t) (seq: char list) = match t with
            | Length n -> List.length seq == n
            | Contains ix -> subseq enc.(ix) seq
            | ContainsNot ix -> not (subseq enc.(ix) seq)
            | IsContainedIn ix -> subseq seq enc.(ix)
            | IsNotContainedIn ix -> not (subseq seq enc.(ix)) in

        let rec matches (cat: category) (seq: char list) = match cat with
            | [] -> true
            | t::rest -> check t enc seq && matches rest seq in

        let rec select_matching cat = function
            | [] -> raise (Invalid_input "No matching sequence found")
            | s::xs when matches cat s -> (s, xs)
            | s::xs -> let (m, xm) = select_matching cat xs
                            in (m, s::xm) in

        let (sequence, rest) =
            select_matching (List.nth categories n) inputs in

        enc.(n) <- sequence;
        (enc, rest)


    let learn (enc: t) (inputs: char list list): t =
        let categories: test list list = [
            (* 0 *) [ Length 6; Contains 7 ];
            (* 1 *) [ Length 2 ];
            (* 2 *) [ Length 5; IsNotContainedIn 6 ];
            (* 3 *) [ Length 5; Contains 7 ];
            (* 4 *) [ Length 4 ];
            (* 5 *) [ Length 5; IsContainedIn 6 ];
            (* 6 *) [ Length 6; ContainsNot 7 ];
            (* 7 *) [ Length 3 ];
            (* 8 *) [ Length 7 ];
            (* 9 *) [ Length 6; Contains 4 ];
        ] in
        let order = [1; 4; 7; 8; 9; 0; 6; 3; 2; 5] in
        List.fold_left (assign categories) (enc, inputs) order
            |> fst


    let decode_sequence (enc: t) (inputs: char list list): int =
        print enc;

        let equal_seq = List.equal Char.equal in

        let find_sequence (enc: t) (seq: char list) =
            List.fold_left (fun b ix -> if equal_seq enc.(ix) seq then ix else b) (-1)
                    (List.init 10 Fun.id) in

        List.map (find_sequence enc) inputs
            |> List.fold_left (fun a n -> 10 * a + n) 0
end


let parse_input: string -> (char list list * char list list) =
    let cut_string =
        String.to_seq >> List.of_seq >> List.sort Char.compare in

    String.split_on_char '|'
        >> List.map String.trim
        >> List.map (String.split_on_char ' ' >> List.map cut_string)
        >> Fun.peek (fun l -> assert(List.length l == 2))
        >> fork List.hd (List.tl >> List.hd)


let () =
    let argc = Array.length Sys.argv - 1 in

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    File.as_seq Sys.argv.(1)
        |> Seq.map parse_input
        |> Seq.map (first (DispEnc.learn DispEnc.create))
        |> Seq.map (uncurry DispEnc.decode_sequence)
        |> Seq.fold_left (+) 0
        |> printf "%d\n"
