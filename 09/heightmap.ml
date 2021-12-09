open Printf
open Toolbox
open Toolbox.Operators


exception Invalid_input of string

module HeightMap = struct
    type t = int array array


    let print (m: t) =
        let print_row row =
            printf "# ";
            Array.iter (printf "%d") row;
            printf "\n" in
        printf "# -- Height Map --\n";
        Array.iter print_row m


    let create width height =
        Array.make_matrix height width 0


    let height: t -> int = Array.length
    let width: t -> int = Fun.flip Array.get 0 >> Array.length


    let load (m: t): int list list -> t =
        let fill_row y =
            List.iteri (fun x v -> m.(y).(x) <- v) in
        List.iteri fill_row >> const m


    let safe_get (m: t) (x: int) (y: int) =
        let h = height m and w = width m in

    if x < 0 || x >= w || y < 0 || y >= h then 10 else m.(y).(x)
end


let process_input: string Seq.t -> int list list =
    let rec contains_same_values = function
        | [] -> true
        | [_] -> true
        | x::y::rest -> x == y && contains_same_values (y::rest) in

    let verify l =
        if not (contains_same_values l) then
            raise (Invalid_input "Map does not contain rows of same width") in

    Seq.map String.to_chars >> List.of_seq
        >> peek (List.map List.length >> verify)
        >> List.map (List.map (Char.code >> Fun.flip (-) (Char.code '0')))


let get_height_map: int list list -> HeightMap.t =
    dup >> first (fork (List.hd >> List.length) (List.length))
        >> first (uncurry HeightMap.create)
        >> uncurry HeightMap.load


let neighbours x y =
    [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]


let get_vales (m: HeightMap.t): int list =
    let scan_cell y x acc depth: int list =
        neighbours x y
            |> List.map (uncurry (HeightMap.safe_get m))
            |> List.for_all ((<) depth)
            |> fun b -> ifv b (depth::acc) acc in

    let fold_row y acc row =
        Array.foldi_left (scan_cell y) acc row in

    Array.foldi_left fold_row [] m


let get_risk_factor = List.map ((+) 1) >> List.sum


let () =
    let argc = Array.length Sys.argv - 1 in

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    File.as_seq Sys.argv.(1)
        |> process_input
        |> get_height_map
        |> peek HeightMap.print
        |> get_vales
        |> get_risk_factor
        |> printf "%d\n"
