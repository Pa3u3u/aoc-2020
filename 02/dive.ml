open Printf


let file_as_seq file_name =
    let unfolder handle =
        try let line = input_line handle in
            Some (line, handle)
        with _ ->
            close_in_noerr handle;
            None

    and file = open_in file_name in
        Seq.unfold unfolder file


let read_int str =
    try Some (int_of_string str)
    with _ -> None


let convert_input = function
    | a::b::[] -> Some (a, read_int b)
    | _ -> None


let extract_pairs = function
    | (_, None) -> None
    | (c, Some v) -> Some (c, v)


let process_input seq =
    Seq.map (String.split_on_char ' ') seq
        |> Seq.filter_map convert_input
        |> Seq.filter_map extract_pairs


let uncurry f (a, b) = f a b


type position = {
    h : int;
    d : int;
}


let dive offset = function
    | { h = vh; d = vd } -> { h = vh; d = vd + offset }


let move offset = function
    | { h = vh; d = vd } -> { h = vh + offset; d = vd }


let interpret = function
    | ("forward", v) -> move v
    | ("up", v) -> dive (-v)
    | ("down", v) -> dive v
    | _ -> Fun.id


let print_position r =
    printf "position: [h = %d, d = %d]\nanswer: %d\n" r.h r.d (r.h * r.d)


let () =
    if Array.length Sys.argv - 1 <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 0;
    end else
        file_as_seq Sys.argv.(1)
            |> process_input
            |> Seq.fold_left (Fun.flip interpret) { h = 0; d = 0 }
            |> print_position

