open Printf
open Toolbox.Core
open Toolbox.Operators
open Toolbox.Parser

module SfNum = struct
    type t =
        | N of int
        | L of t * t

    let rec to_string: t -> string = function
        | N v -> sprintf "%d" v
        | L (a, b) -> sprintf "[%s, %s]" (to_string a) (to_string b)

    type transformation =
        | Ok
        | Sp
        | Ex of int * int

    let rec add_left n  = function
        | v when n = 0 -> v
        | N v -> N (v + n)
        | L (la, lb) -> L (add_left n la, lb)

    let rec add_right n = function
        | v when n = 0 -> v
        | N v -> N (v + n)
        | L (la, lb) -> L (la, add_right n lb)

    let explode =
        let rec explode' d = function
            | N v -> N v, Ok
            | L (N a, N b) when d >= 4 -> N 0, Ex (a, b)
            | L (la, lb) -> match explode' (d + 1) la with
                | (na, Ex (da, db)) -> L (na, add_left db lb), Ex (da, 0)
                | (na, _) -> match explode' (d + 1) lb with
                    | (nb, Ex (da, db)) -> L (add_right da na, nb), Ex (0, db)
                    | (nb, _) -> L (na, nb), Ok in
        explode' 0

    let split =
        let rec split' = function
            | N v when v >= 10 -> L (N (v / 2), N (v - (v / 2))), Sp
            | N v -> N v, Ok
            | L (a, b) -> match split' a with
                | na, Sp -> L (na, b), Sp
                | na,  _ -> match split' b with
                    | nb, r -> L (na, nb), r in
        split'

    let rec reduce e1 = match explode e1 with
        | e2, Ex (_, _) -> reduce e2
        | e2,  _ -> match split e2 with
            | e2, Sp -> reduce e2
            | e3,  _ -> e3

    let sum: t list -> t =
        let sum' a b = match a with
            | N _ -> b
            | l -> reduce (L (l, b)) in
        List.fold_left sum' (N 0)

    let rec magnitude: t -> int = function
        | N v -> v
        | L (a, b) -> 3 * (magnitude a) + 2 * (magnitude b)

    let maximum_magnitude (l: t list): ((t * t) * int) =
        let len = List.length l in
        List.product (0 &-- len) (0 &-- len)
            |> List.filter (fun (a, b) -> a != b)
            |> List.to_seq
            |> Seq.map (fun (ia, ib) ->
                    let a = List.nth l ia and b = List.nth l ib in
                    ((a, b), sum [a; b] |> magnitude))
            |> Seq.fold_left (fun (a, ar) (n, nr) ->
                    if ar > nr then (a, ar) else (n, nr))
                    ((N 0, N 0), 0)
end

type snail_int = SfNum.t

module Parser = struct
    include CharParser

    let parse_line: string -> snail_int option =
        let unwrap_n (n: int): snail_int =
            N n in
        let unwrap_l: snail_int list -> snail_int = function
            | x::y::[] -> L (x, y)
            | _ -> raise (Failure "Parser accepted wrong list") in

        let rec snp (us: pin): SfNum.t pout =
            ((integer |> map unwrap_n)
            <|>
            (between (symbol '[') (symbol ']')
                (sep_by (symbol ',') snp
                |> accept (fun l -> List.length l == 2)
                |> map unwrap_l))) us in
        String.to_seq >> run snp ()
end

let () =
    let argc = Array.length Sys.argv - 1 in

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    Printexc.record_backtrace true;
    File.as_seq Sys.argv.(1)
        |> Seq.filter_map Parser.parse_line
        |> List.of_seq
        |> SfNum.maximum_magnitude
        |> Fun.peek (fun ((a, b), v) ->
                printf "# %s + %s -> %d\n" (SfNum.to_string a) (SfNum.to_string b) v)
        |> (fun (_, v) -> printf "%d\n" v)
