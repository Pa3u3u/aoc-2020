let pair a b = (a, b)
let both f (x, y) = (f x, f y)
let fork f g a = (f a, g a)
let bimap f g (x, y) = (f x, g y)

let curry f a b = f (a, b)
let uncurry f (a, b) = f a b

let first f (a, b) = (f a, b)
let second f (a, b) = (a, f b)

let peek f v = f v; v

let const a _ = a
let dup a = (a, a)

let ifv (c: bool) (if_true: 'a) (if_false: 'a): 'a =
    if c then if_true else if_false

let shift f _ = f

let guard ex (f: 'a -> 'b option) (v: 'a): 'b = match f v with
    | Some x -> x
    | None -> (ex v)


module Operators = struct
    let (|<) g f x = g (f x)
    let (>>) f g x = g (f x)

    let (&--) a z = List.init (z - a) ((+) a)
end


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


module type MatrixType = sig
    type t
    val init: t
end


module Matrix = struct
    let rec contains_same_values = function
        | [] -> true
        | [_] -> true
        | x::y::rest -> x == y && contains_same_values (y::rest)


    let verify l =
        if not (contains_same_values l) then
            raise (Failure "Map does not contain rows of same width")


    module Make (MT: MatrixType) = struct
        open Operators

        type v = MT.t
        type 'a t = 'a array array
        type m = v t
        type pos = int * int


        let create width height =
            Array.make_matrix height width MT.init


        let load (m: m): v list list -> m =
            let fill_row y =
                List.iteri (fun x v -> m.(y).(x) <- v) in

            List.iteri fill_row >> const m


        let from_lists =
            peek (List.map List.length >> verify)
                >> dup >> first (fork (List.hd >> List.length) (List.length))
                >> first (uncurry create)
                >> uncurry load


        let height: m -> int = Array.length
        let width: m -> int = Fun.flip Array.get 0 >> Array.length


        let _f2di fin fout f =
            fout (fun y -> fin (fun x -> f (x, y)))


        let mapi (f: pos -> 'a -> 'b): 'a t -> 'b t =
            _f2di Array.mapi Array.mapi f


        let map (f: 'a -> 'b): 'a t -> 'b t =
            mapi (shift f)


        let foldi_left (f: pos -> 'a -> 'b -> 'a): 'a -> 'b t -> 'a =
            _f2di Array.foldi_left Array.foldi_left f


        let fold_left (f: 'a -> 'b -> 'a): 'a -> 'b t -> 'a =
            foldi_left (shift f)


        let iteri (f: pos -> 'a -> unit): 'a t -> unit =
            _f2di Array.iteri Array.iteri f


        let remap (f: 'a -> 'a) (m: 'a t): 'a t =
            iteri (fun (x, y) v -> m.(y).(x) <- f v) m; m


        let for_all (f: 'a -> bool): 'a t -> bool =
            fold_left (fun a v -> a && (f v)) true


        let valid_point (m: m) ((x, y): pos): bool =
            x >= 0 && y >= 0 && y < height m && x < width m
    end
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


    let rec product (la: 'a list) (lb: 'b list): ('a * 'b) list =
        let prod v = List.map (fun b -> (v, b)) in

        match la with
        | [] -> []
        | a::rest -> List.append (prod a lb) (product rest lb)
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


    let rec find_opt (p: 'a -> bool) (s: 'a Seq.t): 'a option = match s() with
        | (Cons (a, _)) when p a -> Some a
        | (Cons (_, t)) -> find_opt p t
        | Nil -> None
end


module String = struct
    include String


    let to_chars: String.t -> char list =
        Operators.(>>) String.to_seq List.of_seq
end
