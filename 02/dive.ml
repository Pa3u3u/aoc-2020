open Printf


let convert_input = function
    | a::b::[] -> Some (a, Toolbox.Num.parse_int b)
    | _ -> None


let extract_pairs = function
    | (_, None) -> None
    | (c, Some v) -> Some (c, v)


let process_input seq =
    Seq.map (String.split_on_char ' ') seq
        |> Seq.filter_map convert_input
        |> Seq.filter_map extract_pairs


type position = {
    hpos : int;
    depth : int;
    aim : int;
}


let dive offset position =
    { position with depth = position.depth + offset }


let move offset position =
    { position with hpos = position.hpos + offset }


let rotate offset position =
    { position with aim = position.aim + offset }


let interpret = function
    | ("forward", v) -> fun p -> move v p |> dive (p.aim * v)
    | ("up", v) -> rotate (-v)
    | ("down", v) -> rotate v
    | _ -> Fun.id


let print_position r =
    printf "# position: [h = %d, d = %d]\n%d\n" r.hpos r.depth
            (r.hpos * r.depth)


let initial_position =
    { hpos = 0; depth = 0; aim = 0 }


let () =
    if Array.length Sys.argv - 1 <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 0;
    end;

    Toolbox.File.as_seq Sys.argv.(1)
        |> process_input
        |> Seq.fold_left (Fun.flip interpret) initial_position
        |> print_position
