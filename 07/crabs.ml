open Printf
open Toolbox.Core
open Toolbox.Core.Pair
open Toolbox.Operators

let process_input =
    Seq.flat_map (String.split_on_char ',' >> List.filter_map Num.parse_int >> List.to_seq)
        >> List.of_seq


let average =
    let avg = function
        | (sum, count) when sum mod count == 0 -> [sum / count]
        | pair -> both Float.of_int pair |> uncurry (/.)
            |> List.flist [Float.floor; Float.ceil] |> List.map Int.of_float in
    fork List.sum List.length >> avg


let rec median nums =
    let rec drop_last = function
        | [] -> raise (Failure "Invalid list for median")
        | [_] -> []
        | p::rest-> p::drop_last rest in
    match nums with
    | [] -> []
    | [x] -> [x]
    | [x; y] -> [x; y]
    | _::rest -> median (drop_last rest)


let count_fuel crabs value =
    let seq_sum n = ((n + 1) * n) / 2 in
    let distance v =  seq_sum (Int.abs (v - value)) in
    List.map distance crabs |> List.sum


let select_result =
    List.sort (fun a b -> Int.compare (snd b) (snd a))
        >> List.rev
        >> List.hd


let print_result (position, fuel) =
    printf "# position = %d\n%d\n" position fuel


let () =
    let argc = Array.length Sys.argv - 1 in

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    File.as_seq Sys.argv.(1)
        |> process_input
        |> fork Fun.id average
        |> fun (crabs, averages) -> List.map (fork Fun.id (count_fuel crabs)) averages
        |> select_result
        |> print_result
