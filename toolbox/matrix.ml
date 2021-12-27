open Core

module type MatrixType = sig
    type t
    val init: t
end

let rec contains_same_values = function
    | [] -> true
    | [_] -> true
    | x::y::rest -> x == y && contains_same_values (y::rest)

let verify l =
    if not (contains_same_values l) then
        raise (Failure "Map does not contain rows of same width")

module Make (MT: MatrixType) = struct
    open Extensions
    open Fun
    open Operators
    open Pair

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
