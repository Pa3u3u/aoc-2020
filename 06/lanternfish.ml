open Printf
open Toolbox
open Toolbox.Operators


let process_input =
    Seq.flat_map (String.split_on_char ',' >> List.filter_map Num.parse_int >> List.to_seq)
        >> List.of_seq


module Population = struct
    type generation = (int * int)
    type t = generation list


    let compare (a: generation) (b: generation): int =
         Int.compare (fst a) (fst b)


    let normalize =
        let rec join = function
            | (a, na)::(b, nb)::rest when a == b -> join ((a, na + nb) :: rest)
            | r1::rest -> r1 :: join rest
            | [] -> [] in
        List.sort compare >> join


    let from_ages =
        List.map (fun n -> (n, 1))
            >> normalize


    let grow  =
        let rec grow_gens = function
            | (0, count)::rest -> (6, count)::(8, count) :: grow_gens rest
            | (n, count)::rest -> (n - 1, count) :: grow_gens rest
            | [] -> [] in
        grow_gens >> normalize


    let count =
        List.map snd >> List.fold_left (+) 0


    let print title l =
        let gen_str (timer, count) =
            sprintf "[%d × (%d)]" count timer in

        printf "# %s:\n#  " title;
        List.iter (fun p -> printf "  %s" (gen_str p)) l;
        printf "\n"
end


let sim_run (days: int) (pop: Population.t): Population.t =
    let sim_step pop day =
        Population.grow pop
            |> peek (Population.print (sprintf "After day %3d" day)) in

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
        |> peek (Population.print "Initial state")
        |> sim_run days
        |> Population.count
        |> printf "%d\n"
