let pair a b = (a, b)
let both f (x, y) = (f x, f y)
let fork f g a = (f a, g a)

let curry f a b = f (a, b)
let uncurry f (a, b) = f a b

let first f (a, b) = (f a, b)
let second f (a, b) = (a, f b)

let peek f v = f v; v


module File = struct
    let as_seq file_name =
        let unfolder handle =
            try let line = input_line handle in
                Some (line, handle)
            with _ ->
                close_in_noerr handle;
                None
        and file = open_in file_name in
            Seq.unfold unfolder file

    let as_list file_name =
        as_seq file_name |> List.of_seq
end


module Num = struct
    let parse_int str =
        try Some (int_of_string str)
        with _ -> None
end


module Operators = struct
    let (|<) g f x = g (f x)
    let (>>) f g x = g (f x)
end


module List = struct
    include List


    let rec map_pairs (f : 'a -> 'a -> 'b) = function
        | [] -> []
        | a::b::rest -> (f a b) :: map_pairs f rest
        | _ -> raise (Failure "The list does not have even length")
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


    let as_seq ((a, step, f): 'a t): 'a Seq.t =
        let unfolder = function
            | n when f n -> Some (n, n + step)
            | _ -> None in
        Seq.unfold unfolder a


    let as_list (r: 'a t): 'a list =
        as_seq r |> List.of_seq
end
