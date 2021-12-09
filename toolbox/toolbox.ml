let pair a b = (a, b)
let both f (x, y) = (f x, f y)
let fork f g a = (f a, g a)

let curry f a b = f (a, b)
let uncurry f (a, b) = f a b

let first f (a, b) = (f a, b)
let second f (a, b) = (a, f b)

let peek f v = f v; v

let const a _ = a
let dup a = (a, a)

let ifv (c: bool) (if_true: 'a) (if_false: 'a): 'a =
    if c then if_true else if_false

let guard ex (f: 'a -> 'b option) (v: 'a): 'b = match f v with
    | Some x -> x
    | None -> (ex v)


module Array = struct
    include Array


    let foldi_left (f: int -> 'a -> 'b -> 'a) (acc: 'a) (arr: 'b array): 'a =
        List.init (Array.length arr) Fun.id
            |> List.fold_left (fun (ac, ar) i -> (f i ac arr.(i), ar)) (acc, arr)
            |> fst
end


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
    exception Parse_error of string

    let parse_int str =
        try Some (int_of_string str)
        with _ -> None


    let parse_int_exn =
        guard (fun s -> raise (Parse_error (Printf.sprintf "%s: Not a valid number" s))) parse_int
end


module List = struct
    include List


    let rec equal (cmp: 'a -> 'a -> bool) (l: 'a list) (r: 'a list): bool =
        match (l, r) with
            | ([], []) -> true
            | (vl::xl, vr::xr) -> cmp vl vr && equal cmp xl xr
            | _, _ -> false


    let rec map_pairs (f : 'a -> 'a -> 'b) = function
        | [] -> []
        | a::b::rest -> (f a b) :: map_pairs f rest
        | _ -> raise (Failure "The list does not have even length")


    let sum: int list -> int  = List.fold_left (+) 0


    let flist (functions: ('a -> 'b) list) (value: 'a): 'b list =
        List.map (fun f -> f value) functions


    let rec take (n: int): 'a list -> 'a list = function
        | [] -> []
        | _ when n == 0 -> []
        | x::xs -> x :: take (n - 1) xs
end


module Operators = struct
    let (|<) g f x = g (f x)
    let (>>) f g x = g (f x)

    let (&--) a z = List.init (z - a) ((+) a)
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


module Seq = struct
    include Seq


    let repeat (v: 'a): 'a Seq.t  =
        Seq.unfold (fun x -> Some (x, x)) v


    let zip_with (f: 'a -> 'b -> 'c) (left: 'a Seq.t) (right: 'b Seq.t): 'c Seq.t =
        let zipper (l, r) = match (l (), r ()) with
            | (Cons (va, xa), Cons (vb, xb)) -> Some (f va vb, (xa, xb))
            | _ -> None in
        unfold zipper (left, right)


    let zip (left: 'a Seq.t) (right: 'b Seq.t): ('a * 'b) Seq.t =
        zip_with pair left right
end


module String = struct
    include String


    let to_chars: String.t -> char list =
        Operators.(>>) String.to_seq List.of_seq
end
