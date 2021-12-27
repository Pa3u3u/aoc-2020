module Fun = struct
    include Fun

    let const a _ = a
    let peek f v = f v; v
    let shift f _ = f
end

module Misc = struct
    let ifv (c: bool) (if_true: 'a) (if_false: 'a): 'a =
        if c then if_true else if_false

    let guard ex (f: 'a -> 'b option) (v: 'a): 'b = match f v with
        | Some x -> x
        | None -> (ex v)
end

module File = struct
    let as_seq' read file_name =
        let unfolder handle =
            try let e = read handle in
                Some (e, handle)
            with _ ->
                close_in_noerr handle;
                None
        and file = open_in file_name in
            Seq.unfold unfolder file

    let as_seq =
        as_seq' input_line

    let as_char_seq =
        as_seq' input_char

    let as_list file_name =
        as_seq file_name |> List.of_seq
end

module Num = struct
    exception Parse_error of string

    let parse_int str =
        try Some (int_of_string str)
        with _ -> None

    let parse_int_exn =
        Misc.guard (fun s -> raise (Parse_error (Printf.sprintf "%s: Not a valid number" s))) parse_int
end


module Range = struct
    type 'a t = 'a * 'a * ('a -> bool)

    let sign = function
        | n when n > 0 ->  1
        | n when n < 0 -> -1
        | _ -> 0

    let make_step (a: 'a) (z: 'a) (step: 'a): 'a t =
        assert (step != 0);
        let f = if step > 0 then Fun.flip (<) z else Fun.flip (>) z in
        (a, step, f)

    let make (a: 'a) (z: 'a): 'a t =
        let step = sign (z - a) in
        assert (step != 0);
        make_step a z step

    let make_inc (a: 'a) (z: 'a): 'a t =
        let step = sign (z - a) in
        assert (step != 0);
        make_step a (z + step) step

    let infinite (a: 'a) (step: 'a): 'a t =
        assert (step != 0);
        (a, step, fun _ -> true)

    let as_seq ((a, step, f): 'a t): 'a Seq.t =
        let unfolder = function
            | n when f n -> Some (n, n + step)
            | _ -> None in
        Seq.unfold unfolder a

    let as_list (r: 'a t): 'a list =
        as_seq r |> List.of_seq
end
