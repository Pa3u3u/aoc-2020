module type ParseType = sig
    type token
    type user
end

module Make(P: ParseType) = struct
    open Operators

    type t = P.token
    type s = t Seq.t
    type u = P.user
    type pin = (u * s)
    type 'r pout = ('r * u * s) option
    type 'r p = pin -> 'r pout

    let (>>=) = Option.bind
    let (>=>) (f: 'a -> 'b option) (g: 'b -> 'c option): 'a -> 'c option =
        f >> Option.map g >> Option.join

    let rec choose (pl: 'a p list): 'a p =
        let nmap g v = function
            | None -> g v
            | v -> v in
        fun (u, s) -> match pl with
        | p::rest -> (p (u, s)) |> nmap (choose rest) (u, s)
        | [] -> None

    let choose2 (a: 'a p) (b: 'a p): 'a p =
        choose [a; b]

    let (<|>) = choose2

    let accept (f: 'a -> bool) (p: 'a p): 'a p =
        p >=> fun (v, u', s') -> if f v then Some (v, u', s') else None

    let return (v: 'a): 'a p =
        fun (u, s) -> Some (v, u, s)

    let read: t p = fun (u, s) -> match s () with
        | Cons (x, rest) -> Some (x, u, rest)
        | Nil -> None

    let rec fold (f: 'a -> 'b -> 'a) (iv: 'a) (p: 'b p): 'a p = fun (u, s) ->
        match p (u, s) with
        | Some (v, u', s') -> fold f (f iv v) p (u', s')
        | None -> Some (iv, u, s)

    let map (f: 'a -> 'b) (p: 'a p): 'b p =
        p >=> fun (a, u', s') -> Some (f a, u', s')

    let bind (p: 'a p) (f: 'a -> 'b p): 'b p =
        p >=> fun (v, u', s') -> (f v) (u', s')

    let seq (p1: 'a p) (p2: 'b p): 'b p =
        bind p1 (fun _ -> p2)

    let (@>>=) = bind
    let (@>>)  = seq

    let many (p: 'a p): 'a list p =
        fold (Fun.flip List.cons) [] p |> map List.rev

    let many1 (p: 'a p): 'a list p =
        fold (Fun.flip List.cons) [] p
            |> accept (fun l -> List.length l > 0)
            |> map List.rev

    let ignore (p: 'a p): unit p =
        map (Fun.const ()) p

    let sep_by (s: 's p) (p: 'r p): 'r list p =
        p >=> (fun (v1, u', s') ->
            (many (ignore s @>> p) |> map (List.cons v1)) (u', s'))

    let between (l: 'l p) (r: 'r p) (p: 'a p): 'a p =
        ignore l @>> p @>>= (fun a -> ignore r @>> return a)

    let run (p: 'r p) (u: u) (s: s): 'r option =
        p (u, s) |> Option.map (fun (r, _, _) -> r)
end

module CharParser = struct
    include Make(struct
        type token = char
        type user = unit
    end)

    open Operators
    open Core

    let any_of (str: string): char p =
        read |> accept (String.contains str)

    let digit: char p = any_of "0123456789"

    let integer: int p =
        many1 digit |> map (List.to_seq >> String.of_seq >> Num.parse_int_exn)

    let symbol (t: t): t p =
        read |> accept (fun c -> c = t)
end
