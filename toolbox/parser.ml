module type ParseType = sig
    type token
    type user
end

module Make(P: ParseType) = struct
    open Extensions
    open Operators

    type t = P.token
    type s = t Seq.t
    type u = P.user
    type h = u * s * int
    type pin = h
    type 'r pout = ('r * h, string * h) result
    type 'r p = pin -> 'r pout

    let (>>=) = Result.bind
    let (>=>) (f: 'a -> ('b, 'e) result) (g: 'b -> ('c, 'e) result): 'a -> ('c, 'e) result =
        f >> Result.map g >> Result.join

    let err' s hs = Error (s, hs)

    let rec choose (pl: 'a p list): 'a p =
        let nmap g v = function
            | Error _ -> g v
            | v -> v in
        fun hs -> match pl with
        | p::rest -> (p hs) |> nmap (choose rest) hs
        | [] -> err' "choose" hs

    let choose2 (a: 'a p) (b: 'a p): 'a p =
        choose [a; b]

    let accept (f: 'a -> bool) (p: 'a p): 'a p =
        p >=> fun (v, hs) -> if f v then Ok (v, hs) else err' "accept" hs

    let reject (f: 'a -> bool): 'a p -> 'a p =
        accept (f >> not)

    let return (v: 'a): 'a p =
        fun hs -> Ok (v, hs)

    let bind (p: 'a p) (f: 'a -> 'b p): 'b p =
        p >=> fun (v, hs') -> (f v) hs'

    let seq (p1: 'a p) (p2: 'b p): 'b p =
        bind p1 (fun _ -> p2)

    let (@>>=) = bind
    let (@>>)  = seq

    let read: t p = fun (u, s, n) -> match s () with
        | Cons (x, rest) -> Ok (x, (u, rest, n + 1))
        | Nil -> err' "read" (u, s, n)

    let eof: unit p = fun hs -> match read hs with
        | Error _ -> Ok ((), hs)
        | Ok (_, hs') -> err' "eof" hs'

    let eofv (p: 'a p): 'a p =
        p @>>= (fun v -> eof @>> return v)

    let (<|>) = choose2
    let (<?>) (p: 'a p) (s: string): 'a p =
        p >> Result.map_error (Pair.first (fun s' -> String.concat ": " [s; s']))

    let rec fold_count (f: 'a -> 'b -> 'a) (iv: 'a) (n: int option) (p: 'b p): 'a p = fun hs ->
        if n = Some 0
        then Ok (iv, hs)
        else let n1 = Option.map (Fun.flip (-) 1) n in
            match p hs with
            | Ok (v, hs') -> fold_count f (f iv v) n1 p hs'
            | Error _ -> Ok (iv, hs)

    let fold (f: 'a -> 'b -> 'a) (iv: 'a) (p: 'b p): 'a p =
        fold_count f iv None p

    let map (f: 'a -> 'b) (p: 'a p): 'b p =
        p >=> fun (a, hs') -> Ok (f a, hs')

    let perhaps (a: 'a) (p: 'a p): 'a p =
        choose [p; return a]

    let optional (s: 'a p): unit p =
        choose [s @>> return (); return ()]

    let sequence (pl: 'a p list): 'a list p =
        let rec sequence' acc l = fun hs -> match l with
            | [] -> Ok (acc, hs)
            | p::rest -> match p hs with
                | Ok (v, hs') -> sequence' (v::acc) rest hs'
                | Error e -> Error e in
        sequence' [] pl |> map List.rev

    let count (n: int) (p: 'a p): 'a list p =
        sequence (List.replicate n p)

    let ignore (p: 'a p): unit p =
        p @>> return ()

    let many (p: 'a p): 'a list p =
        fold (Fun.flip List.cons) [] p |> map List.rev

    let many1 (p: 'a p): 'a list p =
        fold (Fun.flip List.cons) [] p
            |> accept (fun l -> List.length l > 0)
            |> map List.rev

    let many_until_before (p: 'a p) (e: 'b p): 'a list p =
        let rec mub' acc = fun hs -> match e hs with
            | Ok (_, _) -> Ok (acc, hs)
            | Error _ -> (p @>>= (fun v -> mub' (v::acc))) hs in
        mub' [] |> map List.rev

    let many_until (p: 'a p) (e: 'b p): 'a list p =
        many_until_before p e @>>= (fun v -> ignore e @>> return v)

    let rec skip_until_before (p: 'a p) (e: 'b p): unit p = fun hs ->
        match e hs with
            | Ok (_, _) -> Ok ((), hs)
            | Error _ -> (p @>> skip_until_before p e) hs

    let skip_until (p: 'a p) (e: 'b p): unit p =
        skip_until_before p e @>> ignore e

    let sep_by (s: 's p) (p: 'r p): 'r list p =
        p >=> (fun (v1, hs') ->
            (many (ignore s @>> p) |> map (List.cons v1)) hs')

    let between (l: 'l p) (r: 'r p) (p: 'a p): 'a p =
        ignore l @>> p @>>= (fun a -> ignore r @>> return a)

    let then_skip (s: 'b p) (p: 'a p): 'a p =
        p @>>= (fun v -> s @>> return v)

    let combine (a: 'a p) (b: 'b p): ('a * 'b) p =
        a @>>= (fun va -> b @>>= (fun vb -> return (va, vb)))

    let (@>>&) = combine

    let run (p: 'r p) (u: u) (s: s): ('r, string * h) result =
        p (u, s, 0) |> Result.map (fun (r, _) -> r)
            |> Result.map_error (fun (s, hs) -> (s, hs))
end

module CharParser = struct
    include Make(struct
        type token = char
        type user = unit
    end)

    open Extensions
    open Operators
    open Core

    let any: char p =
        read

    let any_of (str: string): char p =
        read |> (accept (String.contains str)) <?> "any_of"

    let none_of (str: string): char p =
        read |> reject (String.contains str)

    let digit: char p = any_of "0123456789"

    let symbol (t: t): t p =
        read |> accept (fun c -> c = t) <?> (Printf.sprintf "symbol %c" t)

    let str: string -> unit p =
        String.to_chars >> List.map symbol >> sequence >> ignore

    let integer: int p =
        let convert n = map (List.to_seq
            >> String.of_seq
            >> Num.parse_int_exn >> ( * ) n) in

        (
            (symbol '-' @>> many1 digit |> convert (-1))
            <|>
            (optional (symbol '+') @>> many1 digit |> convert 1)
        ) <?> "integer"

    let eol: t p =
        symbol '\n' <?> "eol"
end
