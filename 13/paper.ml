open Printf
open Toolbox
open Toolbox.Operators
open Toolbox.Pair

exception Invalid_input of string


module Origami = struct
    type pt = int * int
    type ptl = pt list

    module Paper = struct
        module PaperMatrix = Matrix.Make(struct
            type t = bool
            let init = false
        end)

        type t = PaperMatrix.m

        let create (input: ptl): t =
            let ptmax (wmax, hmax) (x, y) =
                (max wmax (x + 1), max hmax (y + 1)) in
            let (w, h) = List.fold_left ptmax (0, 0) input in
            let assign_pts m (x, y) =
                m.(y).(x) <- true; m in

            List.fold_left assign_pts (PaperMatrix.create w h) input

        let mirror (paper: t): t =
            let width = PaperMatrix.width paper in
            PaperMatrix.mapi (fun (x, y) _ -> paper.(y).(width - x - 1)) paper

        let print l: unit =
            let print_row row =
                printf "# ";
                Array.iter (fun p -> printf "%s" (Misc.ifv p "\x1b[96m█\x1b[0m" "\x1b[37m·\x1b[0m")) row;
                printf "\n" in
            let w = PaperMatrix.width l in
                printf "# ";
                List.iter (fun _ -> printf "-") (List.init w Fun.id);
                printf "\n";
            Array.iter print_row l; Stdlib.(flush stdout)
    end

    type axis = X | Y

    type ins = axis * int
    type insl = ins list

    let empty: ptl = []

    let debug_dump: ptl -> unit =
        Paper.create >> Paper.print

    let compare_dots (x1, y1) (x2, y2) =
        match Int.compare x1 x2 with
            | 0 -> Int.compare y1 y2
            | n -> n

    let fold (paper: ptl): insl -> ptl =
        let apply_instr (i: ins) ((x, y): pt): pt option = match i with
            | (X, n) when x < n -> Some (x, y)
            | (X, n) when x > n -> Some (2 * n - x, y)
            | (X, _) -> None
            | (Y, n) when y < n -> Some (x, y)
            | (Y, n) when y > n -> Some (x, 2 * n - y)
            | (Y, _) -> None in
        let fold_once (paper: ptl) (i: ins): ptl =
            List.filter_map (apply_instr i) paper in
        List.fold_left fold_once paper >> List.sort_uniq compare_dots

    let fold_n (n: int) (paper: ptl): insl -> ptl =
        List.take n >> fold paper
end


let parse_input: string Seq.t -> (Origami.ptl * Origami.insl) =
    let rec parse_points_rec (acc: Origami.ptl) (seq: string Seq.t): (Origami.ptl * string Seq.t) =
        let unpack = function
            | [x; y] -> (x, y)
            | _ -> raise (Invalid_input "Invalid point specification") in
        let parse_point: string -> Origami.pt =
            String.split_on_char ',' >> List.map Num.parse_int_exn >> unpack in
        match seq () with
        | Nil -> (acc, Seq.empty)
        | Cons ("", rest) -> (acc, rest)
        | Cons (pt, rest) -> parse_points_rec (parse_point pt::acc) rest in
    let parse_points: string Seq.t -> (Origami.ptl * string Seq.t) =
        parse_points_rec [] >> first List.rev in

    let rec parse_instructions_rec (acc: Origami.insl) (seq: string Seq.t): Origami.insl =
        let regex = Str.regexp "fold along \\([xy]\\)=\\([0-9]+\\)" in
        let parse_instr i: Origami.ins =
            if not (Str.string_match regex i 0) then
                raise (Invalid_input "Invalid instruction");
            let amount = Str.matched_group 2 i |> Num.parse_int_exn in
                match Str.matched_group 1 i with
                | "x" -> (X, amount)
                | "y" -> (Y, amount)
                | _ -> raise (Invalid_input "Invalid axis in instruction") in

        match seq () with
        | Nil -> acc
        | Cons (i, rest) -> parse_instructions_rec (parse_instr i::acc) rest in
    let parse_instructions: string Seq.t -> Origami.insl =
        parse_instructions_rec [] >> List.rev in

    parse_points >> second parse_instructions


let count_dots = List.length


let () =
    let argc = Array.length Sys.argv - 1 in

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    File.as_seq Sys.argv.(1)
        |> parse_input
        |> uncurry Origami.fold
        |> Origami.Paper.(create >> print)
