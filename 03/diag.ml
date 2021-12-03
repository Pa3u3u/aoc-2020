open Printf
open Toolbox.Operators


module String = struct
    include String

    let to_list str =
        List.init (String.length str) (String.get str)
end

exception Invalid_value of string

let bit_value = function
    | '0' -> 0
    | '1' -> 1
    | c -> raise (Invalid_value (sprintf "Invalid bit character %c" c))


let rec get_stats stats bits =
    let bit_weight v =
        1 - 2 * v in

    match (stats, bits) with
    | (_, []) -> []
    | ([], l) -> get_stats [0] l
    | (s::xs, b::xb) -> (s + bit_weight b) :: get_stats xs xb


let reconstruct_number =
    let rec reconstruct a = function
        | n::xn -> reconstruct ((Int.shift_left a 1) lor n) xn
        | [] -> a in

    reconstruct 0


let () =
    if Array.length Sys.argv - 1 <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 0;
    end;

    Toolbox.File.as_seq Sys.argv.(1)
        |> Seq.map (String.to_list >> (List.map bit_value) >> List.rev)
        |> Seq.fold_left get_stats []
        |> List.map (fun stat -> if stat > 0 then 1 else 0)
        |> Toolbox.fork Fun.id (List.map ((-) 1))
        |> Toolbox.both (List.rev >> reconstruct_number)
        |> fun (a, b) -> printf "# [γ=%d, ε=%d]\n%d\n" a b (a * b)
