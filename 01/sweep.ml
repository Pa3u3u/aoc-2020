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


let count_increases (state, counter) next = match (state, next) with
    | (None, next) -> (Some next, counter)
    | (Some prev, next) -> (Some next, counter + if prev < next then 1 else 0)


let read_int str =
    try Some (int_of_string str)
    with _ -> None


let sum = List.fold_left (+) 0


let window_add window size value =
    List.append window [value]
        |> if List.length window + 1 > size then List.tl else Fun.id


let seq_append_value value seq =
    Seq.append seq (Seq.return value)


let transform_data winsz old_seq =
    let folder (window, seq) value =
        let new_window = window_add window winsz value in
        let new_seq =
            if List.length new_window == winsz then
                seq_append_value (sum new_window) seq
            else seq in

        (new_window, new_seq) in
    Seq.fold_left folder ([], Seq.empty) old_seq |> snd


let () =
    if Array.length Sys.argv - 1 <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 0;
    end else
        file_as_seq Sys.argv.(1)
            |> Seq.filter_map read_int
            |> transform_data 3
            |> Seq.fold_left count_increases (None, 0)
            |> snd
            |> printf "%d\n"
