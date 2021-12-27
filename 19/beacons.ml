open Printf
open Toolbox.Core
open Toolbox.Extensions
open Toolbox.Operators
open Toolbox.Parser

module Position = struct
    type t = int * int * int

    let dist_sqr ((a1, a2, a3): t) ((b1, b2, b3): t): int =
        let pow a = a * a in
        List.zip_with (-) [a1; a2; a3] [b1; b2; b3]
            |> List.map pow
            |> List.sum

    let compare ((a1, a2, a3):t ) ((b1, b2, b3): t): int =
        List.zip_with Int.compare [a1; a2; a3] [b1; b2; b3]
            |> List.find_opt ((!=) 0)
            |> Option.value ~default:0

    let equal p q = (compare p q) == 0

    let hash (a1, a2, a3) =
        ((a1 * 2819 + a2) * 1723) + a3
end

type pos = Position.t

module IntMatrix = struct
    include Toolbox.Matrix.Make(struct
        type t = int
        let init = 0
    end)

    let qsin n = match n mod 4 with
        | 1 -> 1
        | 3 -> -1
        | _ -> 0

    let qcos n = qsin (n + 1)

    let r_x p = from_lists [
        [        1;      0;         0];
        [        0; qcos p; -(qsin p)];
        [        0; qsin p;   qcos p ]
    ]

    let r_y p = from_lists [
        [   qcos p;      0;   qsin p ];
        [        0;      1;        0 ];
        [-(qsin p);      0;   qcos p ]
    ]

    let r_z p = from_lists [
        [ qcos p; -(qsin p);       0 ];
        [ qsin p;   qcos p ;       0 ];
        [      0;        0 ;       1 ]
    ]

    let from_pos (x, y, z): m =
        from_lists [[x]; [y]; [z]]

    let to_pos m =
        assert (width m == 1);
        assert (height m == 3);
        (m.(0).(0), m.(1).(0), m.(2).(0))

    let add a b =
        let setv (x, y) _ =
            a.(y).(x) + b.(y).(x) in

        assert (width a == width b);
        assert (height a == height b);
        create (width a) (height a)
            |> mapi setv

    let mul a b =
        let m = width a in
        let setv (x, y) _ =
            0 &-- m |> List.map (fun i -> a.(y).(i) * b.(i).(x))
                    |> List.sum in

        assert (m == height b);
        create (width b) (height a)
            |> mapi setv

    let smul_l s =
        map (( * ) s)

    let smul_r =
        Fun.flip smul_l

    let ( +:: ) = add
    let ( *:: ) = mul
    let ( *$: ) = smul_l
    let ( *:$ ) = smul_r
    let ( -:: ) a b = a +:: (-1 *$: b)

    let rot ax ay az =
        let rx = r_x ax
        and ry = r_y ay
        and rz = r_z az in
        rx *:: ry *:: rz

    let to_list =
        Array.to_list >> List.concat_map Array.to_list

    let compare ma mb =
        List.zip_with Int.compare (to_list ma) (to_list mb)
            |> List.find_opt ((!=) 0)
            |> Option.value ~default:0

    let rotations =
        let q = 0 &-- 4 in
        List.product q q |> List.product q
            |> List.map (fun (a, (b, c)) -> rot a b c)
            |> List.sort_uniq compare

    let rotate_around (p: m) (axis: m) (r: m) =
        let shift   = (+::) (-1 *$: axis)
        and unshift = (+::) (axis) in
        r *:: (shift p) |> unshift

    let print m =
        let print_row row =
            Array.to_list row |> List.map (sprintf "%2d")
                |> String.concat " "
                |> printf "   [%s]\n" in
        printf "[\n";
        Array.iter print_row m;
        printf "]\n";
end

module Beacon = struct
    type t = pos

    let compare = Position.compare

    let to_string (a, b, c)
        = sprintf "[%4d, %4d, %4d]" a b c
end

type beacon = Beacon.t

module Conf = struct
    let dists = 80
    let candidate = 6
    let minimum_common = 12
end

module Beacons = Set.Make(Beacon)
type beacons = Beacons.t

module Scanner = struct

    module IntSet = Set.Make(Int)
    module DistMap = Hashtbl.Make(Position)

    type dmap = IntSet.t DistMap.t

    type t = {
        n: int;
        pos: pos;
        beacons: beacons;
        dists: dmap
    }

    let empty: t = {
        n = 0;
        pos = (0, 0, 0);
        beacons = Beacons.empty;
        dists = DistMap.create 16
    }

    let rotate (p: pos) (r: IntMatrix.m) (s: t) =
        let rotate_beacon axis r b =
            IntMatrix.(rotate_around (from_pos b) (from_pos axis) r |> to_pos) in
        { s with beacons = Beacons.map (rotate_beacon p r) s.beacons;
            dists = DistMap.create 1 }

    let translate v (s: t) =
        let translate_beacon v b =
            IntMatrix.((from_pos b) +:: v |> to_pos) in
        { s with beacons = Beacons.map (translate_beacon v) s.beacons;
            dists = DistMap.create 1 }

    let compute_distances (s: t): t =
        let create_pts (p: pos) =
            s.beacons |> Beacons.to_seq
                |> Seq.map (Position.dist_sqr p)
                |> Seq.filter ((!=) 0)
                |> List.of_seq
                |> List.sort Int.compare
                |> List.take Conf.dists
                |> List.fold_left (Fun.flip IntSet.add) IntSet.empty
                |> DistMap.add s.dists p in
        Beacons.iter create_pts s.beacons; s


    let candidates (s: t) (p: t): (beacon * beacon) Seq.t =
        let select pt =
            let pset = DistMap.find p.dists pt in
            Beacons.to_seq s.beacons
                |> Seq.filter (fun sp ->
                        IntSet.cardinal (IntSet.inter pset (DistMap.find s.dists sp))
                            >= Conf.candidate)
                |> Seq.map (fun sp -> (sp, pt))
            in
        let unfolder (x: beacon Seq.t) = match x () with
            | Nil -> None
            | Cons (p, xs) -> Some (select p, xs) in
        p.beacons |> Beacons.to_seq
            |> Seq.unfold unfolder
            |> Seq.concat


    let print s =
        let print_beacon b =
            printf "    %s\n" (Beacon.to_string b);
        in
        printf "--- scanner %d ---\n[\n" s.n;
        Beacons.iter (print_beacon) s.beacons;
        printf "]\n"
end

type scanner = Scanner.t

module Parser = struct
    include CharParser

    let pos: pos p =
        let unwrap = function
            | [x; y; z] -> (x, y, z)
            | _ -> raise (Failure "Parser accepted a wrong list of integers") in
        sep_by (symbol ',') (integer <?> "integer")
            |> accept (fun l -> List.length l == 3) <?> "three values"
            |> map unwrap

    let header = str "--- scanner " @>> integer |> skip (skip_until any eol)

    let input =
        let make_scanner (n, beacons): scanner = {
            Scanner.empty with n; beacons = beacons |> List.to_seq |> Beacons.of_seq
        } in
        let parse_scanner = header @>>& sep_by eol pos |> map make_scanner in
        (sep_by eol (parse_scanner |> skip eol)) |> eofv

    let parse =
        run input () >> function
            | Ok v -> v
            | Error (s, (_, _, n)) -> raise (Failure (sprintf "Parse error: %s at character %d\n" s n))
end


let fold_scanners (sl: scanner list): scanner =
    let split: scanner list -> (scanner * scanner list) = function
        | x::rest -> { x with n = -1 }, rest
        | _ -> raise (Failure "Scanner list is empty") in

    let try_join_r (s: scanner) x (ps, px) r =
        let v  = IntMatrix.((from_pos ps) -:: (from_pos px)) in
        let x' = Scanner.rotate px r x |> Scanner.translate v in
        let common_beacons = Beacons.inter s.beacons x'.beacons in

        if Beacons.cardinal common_beacons >= Conf.minimum_common
        then begin
            printf "#   Match on %s → %s\n" (Beacon.to_string px) (Beacon.to_string ps);
            { s with beacons = Beacons.union s.beacons x'.beacons }
            |> Scanner.compute_distances |> Option.some
        end else None in

    let try_join s x pts =
        IntMatrix.rotations |> List.to_seq
            |> Seq.map (try_join_r s x pts)
            |> Seq.find_opt Option.is_some
            |> Option.join in
    let join_scanner (s: scanner) (x: scanner) =
        printf "# Join scanner %d\n" x.n;
        Scanner.candidates s x
            |> Seq.map (try_join s x)
            |> Seq.find_opt Option.is_some
            |> Option.join in
    let rec join_scanners s flag stash work = match work, stash with
        | [], [] -> printf "# Done\n"; s
        | [], _ when not flag -> raise (Failure "Stalemate detected")
        | [], stash -> printf "# --- Next cycle\n"; join_scanners s false [] stash
        | x::xs, stash -> match join_scanner s x with
            | None -> join_scanners s flag (x::stash) xs
            | Some s' -> join_scanners s' true stash xs in

    let (s, l) = split sl in
    join_scanners s false [] l


let () =
    let argc = Array.length Sys.argv - 1 in

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    Printexc.record_backtrace true;

    File.as_char_seq Sys.argv.(1)
        |> Seq.resident
        |> Parser.parse
        |> List.map Scanner.compute_distances
        |> fold_scanners
        |> Fun.peek Scanner.print
        |> fun s -> printf "%d\n" (Beacons.cardinal s.beacons)
