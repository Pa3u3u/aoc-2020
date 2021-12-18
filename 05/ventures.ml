open Printf
open Toolbox.Core
open Toolbox.Core.Pair
open Toolbox.Operators


exception Invalid_input of string

module Point = struct
    type t = { x: int; y: int }


    let make (x: int) (y: int): t = { x; y }


    let to_string (p: t): string =
        sprintf "[%d %d]" p.x p.y
end

type point = Point.t


module Segment = struct
    type t = Point.t * Point.t


    let make (a: Point.t) (b: Point.t): t = (a, b)


    let is_basic (s: t): bool =
        (fst s).x == (snd s).x || (fst s).y == (snd s).y


    let to_string (s: t) =
        let a = fst s and b = snd s in
        sprintf "%s -> %s" (Point.to_string a) (Point.to_string b)


    let get_range lens (s: t) =
        let (a, b) = (fork fst snd >> both lens) s in
        if a == b then
            Seq.repeat a
        else
            Range.make_inc a b |> Range.as_seq

    let range_x = get_range (fun (p: point) -> p.x)
    let range_y = get_range (fun (p: point) -> p.y)


    let get_points (s: t): Point.t list =
        let raw_values =
            fork range_x range_y
                >> uncurry Seq.zip
                >> List.of_seq in
        raw_values s
            |> List.map (uncurry Point.make)
end

type segment = Segment.t


module Diagram = struct
    type t = int array array


    let make (n: int): t =
        Array.make_matrix n n 0


    let add_point (diagram: t) (p: point): t =
        try
            diagram.(p.y).(p.x) <- diagram.(p.y).(p.x) + 1;
            diagram
        with _ ->
            printf "Cannot add point %s\n" (Point.to_string p);
            diagram


    let add_segment (diagram: t) (s: segment): t =
        Segment.get_points s
            |> List.fold_left add_point diagram


    let print (diagram: t) =
        let print_row row =
            printf "# ";
            Array.iter (fun cell -> if cell = 0 then printf "· " else printf "%d " cell) row;
            printf "\n"; in
        let size = Array.length diagram in
        printf "# Diagram [%d × %d]:\n" size size;
        Array.iter print_row diagram;
end


let regexp_str = "\\([0-9]+\\),\\([0-9]+\\) -> \\([0-9]+\\),\\([0-9]+\\)"


let parse_segment (line: string): segment option =
    let regexp = Str.regexp regexp_str in

    if not (Str.string_match regexp line 0) then begin
        Printf.eprintf "Line '%s' does not match expected format\n" line;
        None
    end else
        List.init 5 (Fun.flip Str.matched_group line)
            (* Group 0 matches the entire string, we don't need that. *)
            |> List.tl
            |> List.filter_map Num.parse_int
            |> List.map_pairs Point.make
            |> List.map_pairs Segment.make
            |> fun l -> Some (List.hd l)


let find_max_dim (segment: segment list): int =
    List.concat_map (fun ((a: point), (b: point)) -> [a.x; a.y; b.x; b.y]) segment
        |> List.fold_left max 0
        |> (+) 1


let create_diagram =
    fork find_max_dim Fun.id
        >> first Diagram.make
        >> uncurry (List.fold_left Diagram.add_segment)
        (* >> Fun.peek Diagram.print *)


let count_dangers =
    let fold_row acc row =
        Array.fold_left (fun a n -> a + if n > 1 then 1 else 0) acc row in
    Array.fold_left fold_row 0


let () =
    if Array.length Sys.argv - 1 <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    File.as_seq Sys.argv.(1)
        |> Seq.filter_map parse_segment
        |> List.of_seq
        |> create_diagram
        |> count_dangers
        |> printf "%d\n"
