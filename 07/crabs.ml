open Printf
open Toolbox
open Toolbox.Operators

let process_input =
    Seq.flat_map (String.split_on_char ',' >> List.filter_map Num.parse_int >> List.to_seq)
        >> List.of_seq


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
    let distance v = Int.abs (v - value) in
    List.map distance crabs |> List.sum


let select_result =
    List.sort (fun a b -> Int.compare (snd a) (snd b))
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
        |> fork Fun.id (List.sort Int.compare >> median)
        |> fun (crabs, medians) -> List.map (fork Fun.id (count_fuel crabs)) medians
        |> select_result
        |> print_result
