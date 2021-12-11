open Printf
open Toolbox
open Toolbox.Operators

exception Invalid_input of string


module LightMap = struct
    include Matrix.Make(struct
        type t = int
        let init = 0
    end)
end


module SimBoard = struct
    include Matrix.Make(struct
        type t = int * bool
        let init = (0, false)
    end)


    let from_matrix = LightMap.map (fun v -> (v, false))
    let to_matrix = map (fun (v, _) -> v)


    let print (m: m) =
        let print_row row =
            printf "# ";
            Array.iter (fun (v, _) -> printf (if v == 0 then "\x1b[97m%d\x1b[0m" else "%d") v) row;
            printf "\n" in
        Array.iter print_row m
end


let run_simulation (steps: int): LightMap.m -> int * LightMap.m =
    let neighbours m (x, y) =
        let offsets = Range.(make_inc (-1) (1) |> as_list) in

        List.product offsets offsets
            |> List.map (fun (dx, dy) -> (x + dx, y + dy))
            |> List.filter (SimBoard.valid_point m)
            |> List.filter (fun (x1, y1) -> x != x1 || y != y1) in

    let reset = SimBoard.map (second (const false)) in

    let rec flash m (x, y) =
        let (v, flashed) = m.(y).(x) in

        if not flashed then begin
            if v + 1 <= 9 then begin
                m.(y).(x) <- (v + 1, flashed);
            end else begin
                m.(y).(x) <- (0, true);
                neighbours m (x, y) |> List.iter (flash m)
            end
        end in

    let count_flashes =
        let sum_flashed acc = function
            | (_, true) -> acc + 1
            | (_, false) -> acc in
        SimBoard.fold_left sum_flashed 0 in

    let sim_step (acc, m) step_num =
        printf "# --- Step %d ---\n" step_num;
        reset m |> (fun m -> SimBoard.iteri (fun p _ -> flash m p) m; m)
            |> fork count_flashes Fun.id |> first ((+) acc)
            |> peek (snd >> SimBoard.print) in

    let range = List.init steps Fun.id in

    SimBoard.from_matrix >> (fun m -> List.fold_left sim_step (0, m) range)
        >> second (SimBoard.to_matrix)


let process_input =
    Seq.map String.to_chars
        >> Seq.map (List.map (Char.code >> Fun.flip (-) (Char.code '0')))
        >> List.of_seq
        >> LightMap.from_lists


let get_steps str =
    match Num.parse_int str with
        | Some n -> n
        | None -> raise (Failure (sprintf "%s: Not a valid integer" str))


let () =
    let argc = Array.length Sys.argv - 1 in

    if argc < 1 || argc > 2 then begin
        Printf.eprintf "usage: %s FILE [STEPS]\n" Sys.argv.(0);
        exit 1;
    end;

    let steps = if argc = 1 then 100 else get_steps Sys.argv.(2) in

    File.as_seq Sys.argv.(1)
        |> process_input
        |> run_simulation steps
        |> fst |> printf "%d\n"
