open Printf
open Toolbox.Core
open Toolbox.Core.Pair
open Toolbox.Operators


module String = struct
    include String

    let to_list str =
        List.init (String.length str) (String.get str)
end


exception Invalid_input of string

let bit_value = function
    | '0' -> 0
    | '1' -> 1
    | c -> raise (Invalid_input (sprintf "Invalid bit character %c" c))


let rec get_stats stats bits =
    let bit_weight v =
        2 * v - 1 in

    match (stats, bits) with
    | (_, []) -> []
    | ([], l) -> get_stats [0] l
    | (stat::xs, bit::xb) -> (stat + bit_weight bit) :: get_stats xs xb


let reconstruct_number =
    let rec reconstruct acc = function
        | bit::xn -> reconstruct ((Int.shift_left acc 1) lor bit) xn
        | [] -> acc in

    reconstruct 0


let analyze pos values =
    List.map (Fun.flip List.nth pos >> Fun.flip List.cons []) values
        |> List.fold_left get_stats []
        |> List.hd


let prune_values crit pos values =
    let keep = if crit (analyze pos values) then 1 else 0 in
        List.filter (fun l -> (List.nth l pos) == keep) values


let select criterion values =
    let rec select_n crit pos = function
        | [] -> raise (Invalid_input "No viable candidate left")
        | [v] -> v
        | xv -> select_n crit (pos + 1) (prune_values crit pos xv) in

    select_n criterion 0 values


let () =
    if Array.length Sys.argv - 1 <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    File.as_list Sys.argv.(1)
        |> List.map (String.to_list >> (List.map bit_value))
        |> fork (select (Fun.flip (>=) 0)) (select (Fun.flip (<) 0))
        |> both reconstruct_number
        |> fun (o2, co2) -> printf "# [O₂=%d CO₂=%d]\n%d\n" o2 co2 (o2 * co2)
