open Printf
open Toolbox
open Toolbox.Pair
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


let run_simulation: LightMap.m -> int * LightMap.m =
    let neighbours m (x, y) =
        let offsets = Range.(make_inc (-1) (1) |> as_list) in

        List.product offsets offsets
            |> List.map (fun (dx, dy) -> (x + dx, y + dy))
            |> List.filter (SimBoard.valid_point m)
            |> List.filter (fun (x1, y1) -> x != x1 || y != y1) in

    let reset = SimBoard.remap (second (Fun.const false)) in

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

    let sim_step m step_num =
        printf "# --- Step %d ---\n" step_num;
        reset m |> (fun m -> SimBoard.iteri (fun p _ -> flash m p) m; m)
            |> Fun.peek SimBoard.print
            |> SimBoard.for_all snd in

    (* Steps are counted from 1 *)
    let range = Range.(infinite 1 1 |> as_seq) in

    SimBoard.from_matrix >> (fun m -> (Seq.find_opt (sim_step m) range, m))
        >> bimap Option.get SimBoard.to_matrix


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

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    File.as_seq Sys.argv.(1)
        |> process_input
        |> run_simulation
        |> fst |> printf "%d\n"
