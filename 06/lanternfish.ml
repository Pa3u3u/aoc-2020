open Printf
open Toolbox
open Toolbox.Operators


let process_input =
    Seq.flat_map (String.split_on_char ',' >> List.filter_map Num.parse_int >> List.to_seq)
        >> List.of_seq


module Population = struct
    type generation = int
    type t = generation list


    let rec add (pop: t) (gen: int) (v: int): t = match pop with
        | n::rest when gen == 0 -> (n + v)::rest
        | [] when gen == 0 -> [v]
        | n::rest -> n::(add rest (gen - 1) v)
        | [] -> 0::(add [] (gen - 1) v)


    let from_ages = List.fold_left (fun p g -> add p g 1)  []


    let grow = function
        | n::rest -> add rest 6 n |> fun pop -> add pop 8 n
        | [] -> []


    let count (p: t) = List.fold_left (+) 0 p


    let print title l =
        let gen_str (timer, count) =
            sprintf "[%d × (%d)]" count timer in

        printf "# %s:\n#  " title;
        List.iteri (fun i p -> printf "  %s" (gen_str (i, p))) l;
        printf "\n"
end


let sim_run (days: int) (pop: Population.t): Population.t =
    let sim_step pop day =
        Population.grow pop
            |> Fun.peek (Population.print (sprintf "After day %3d" (day + 1))) in

    List.init days Fun.id (* Generate list [0; 1; … (days - 1)] *)
        |> List.fold_left sim_step pop


let get_days str =
    match Num.parse_int str with
        | Some n -> n
        | None -> raise (Failure (sprintf "%s: Not a valid integer" str))


let () =
    let argc = Array.length Sys.argv - 1 in
    if argc <> 1 && argc <> 2 then begin
        Printf.eprintf "usage: %s FILE [DAYS]\n" Sys.argv.(0);
        exit 1;
    end;

    let days = if argc = 1 then 256 else get_days Sys.argv.(2) in

    Toolbox.File.as_seq Sys.argv.(1)
        |> process_input
        |> Population.from_ages
        |> Fun.peek (Population.print "Initial state")
        |> sim_run days
        |> Population.count
        |> printf "%d\n"
