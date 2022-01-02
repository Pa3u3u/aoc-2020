module Array = struct
    include Array

    let foldi_left (f: int -> 'a -> 'b -> 'a) (acc: 'a) (arr: 'b array): 'a =
        List.init (Array.length arr) Fun.id
            |> List.fold_left (fun (ac, ar) i -> (f i ac arr.(i), ar)) (acc, arr)
            |> fst
end

module List = struct
    open Operators
    include List

    let all = List.for_all Fun.id
    let any = List.exists Fun.id

    let index f l =
        let rec index' n = function
            | a::_ when f a -> Some n
            | _::r -> index' (n + 1) r
            | [] -> None in
        index' 0 l

    let replicate (n: int) (v: 'a): 'a list =
        List.init n (Fun.const v)

    let reject (f: 'a -> bool): 'a list -> 'a list =
        List.filter (f >> not)

    let rec equal (cmp: 'a -> 'a -> bool) (l: 'a list) (r: 'a list): bool =
        match (l, r) with
            | ([], []) -> true
            | (vl::xl, vr::xr) -> cmp vl vr && equal cmp xl xr
            | _, _ -> false

    let rec map_pairs (f : 'a -> 'a -> 'b) = function
        | [] -> []
        | a::b::rest -> (f a b) :: map_pairs f rest
        | _ -> raise (Failure "The list does not have even length")

    let flist (functions: ('a -> 'b) list) (value: 'a): 'b list =
        List.map (fun f -> f value) functions

    let rec take (n: int): 'a list -> 'a list = function
        | [] -> []
        | _ when n == 0 -> []
        | x::xs -> x :: take (n - 1) xs

    let rec drop (n: int): 'a list -> 'a list = function
        | [] -> []
        | l when n == 0 -> l
        | _::xs -> drop (n - 1) xs

    let rec product (la: 'a list) (lb: 'b list): ('a * 'b) list =
        let prod v = List.map (fun b -> (v, b)) in

        match la with
        | [] -> []
        | a::rest -> List.append (prod a lb) (product rest lb)


    let rec nproduct: 'a list list -> 'a list list =
        let rec merge la lr copy = match la, lr with
            | [], _ -> []
            | _::xa, [] -> merge xa copy copy
            | a::xa, l::xl -> (a::l) :: merge (a::xa) xl copy in
        function
        | [] -> []
        | l::[] -> List.map (fun v -> [v]) l
        | a::l -> let nl = nproduct l in merge a nl nl

    let rec populate p f v =
        if p v then v :: populate p f (f v)
            else []

    let rec zip_with (f: 'a -> 'b -> 'c) (left: 'a list) (right: 'b list): 'c list =
        match (left, right) with
        | [], _ -> []
        | _, [] -> []
        | a::xa, b::xb -> (f a b) :: zip_with f xa xb

    let zip (a: 'a list) (b: 'b list): ('a * 'b) list =
        zip_with Pair.make a b

    let sum: int list -> int = List.fold_left (+) 0
    let prod: int list -> int = List.fold_left ( * ) 1
end

module Seq = struct
    open Operators

    include Seq

    let take (n: int) (a: 'a Seq.t): 'a Seq.t =
        let unfolder (n, a) =
            if n = 0
            then None
            else match a () with
                | Nil -> None
                | Cons (v, rest) -> Some (v, (n - 1, rest)) in
        Seq.unfold unfolder (n, a)

    let concat (a: 'a Seq.t Seq.t): 'a Seq.t =
        Seq.flat_map Fun.id a

    let repeat (v: 'a): 'a Seq.t =
        Seq.unfold (fun x -> Some (x, x)) v

    let populate (p: 'a -> bool) (f: 'a -> 'a) (v: 'a): 'a Seq.t =
        let unfolder v = if p v then Some (v, f v) else None in
        Seq.unfold unfolder v

    let zip_with (f: 'a -> 'b -> 'c) (left: 'a Seq.t) (right: 'b Seq.t): 'c Seq.t =
        let zipper (l, r) = match (l (), r ()) with
            | (Cons (va, xa), Cons (vb, xb)) -> Some (f va vb, (xa, xb))
            | _ -> None in
        unfold zipper (left, right)

    let zip (left: 'a Seq.t) (right: 'b Seq.t): ('a * 'b) Seq.t =
        zip_with Pair.make left right

    let rec find_opt (p: 'a -> bool) (s: 'a Seq.t): 'a option = match s() with
        | (Cons (a, _)) when p a -> Some a
        | (Cons (_, t)) -> find_opt p t
        | Nil -> None

    let filter_result (f: 'a -> ('b, 'e) Result.t): 'a Seq.t -> 'b Seq.t =
        Seq.map f >> Seq.filter_map Result.to_option

    let resident (s: 'a t): 'a t =
        List.of_seq s |> List.to_seq
end

module String = struct
    include String

    let to_chars: String.t -> char list =
        Operators.(>>) String.to_seq List.of_seq
end
