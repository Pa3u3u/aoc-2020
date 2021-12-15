open Printf
open Toolbox
open Toolbox.Operators
open Toolbox.Pair

exception Invalid_input of string

module DangerMap = struct
    include Matrix.Make(struct
        type t = int
        let init = 0
    end)

    let parse = Seq.map String.to_chars
        >> Seq.map (List.map (Char.code >> Fun.flip (-) (Char.code '0')))
        >> List.of_seq
        >> from_lists
end

module Point = struct
    type t = { x: int; y: int }
    let to_tup p = (p.x, p.y)
    let from_tup (x, y) = {x; y}

    let compare p1 p2 = match Int.compare p1.x p2.x with
        | 0 -> Int.compare p1.y p2.y
        | n -> n

    let equals p1 p2 = p1.x = p2.x && p1.y = p2.y
end


type pt = Point.t

module PointSet = Set.Make(Point)
type ptset = PointSet.t


module SearchMap = struct
    type value_type = {
        dist: int;
        source: pt option
    }

    let empty_value _ = {dist = Int.max_int; source = None}

    include Matrix.Make(struct
        type t = value_type
        let init = empty_value ()
    end)

    let lift =
        let blank _ = { dist = Int.max_int; source = None } in
        map (fun _ -> blank ())
end


type map = DangerMap.m
type search_map = SearchMap.m


module Search = struct
    module Unvisited = struct
        type t = pt * SearchMap.v
        let compare ((p1, vt1): t) ((p2, vt2): t) =
            match Int.compare vt1.dist vt2.dist with
                | 0 -> Point.compare p1 p2
                | n -> n
    end

    module UnvisitedSet = struct
        include Set.Make(Unvisited)

        let pt_mem (p: pt) =
            exists (fun (p', _) -> Point.equals p' p)

        let pt_get (p: pt) =
            partition (fun (p', _) -> Point.equals p p')
                >> fst >> choose

        let pt_update (p: pt) (f: SearchMap.v -> SearchMap.v) s =
            let (sg, rest) = partition (fun (p', _) -> Point.equals p p') s in
            let (_, v) = choose sg in
            add (p, f v) rest
    end

    type unvisited = UnvisitedSet.t

    class dijkstra (m: map) = object (self)
        val mutable sm = SearchMap.lift m

        method private mark (p: pt) (u: unvisited) =
            let (_, r) = UnvisitedSet.pt_get p u in
            sm.(p.y).(p.x) <- r;
                UnvisitedSet.remove (p, r) u

        method private select_next (u: unvisited): pt * unvisited =
            let next = UnvisitedSet.min_elt u in
            (fst next, u)

        method private neighbours (p: pt) =
           let offsets = [(-1, 0); (1, 0); (0, -1); (0, 1)] in
           List.map (lift2 (+) (p.x, p.y)) offsets
               |> List.filter (SearchMap.valid_point sm)
               |> List.map Point.from_tup

        method uset: unvisited =
            List.product (0 &-- DangerMap.width m) (0 &-- DangerMap.height m)
                |> List.map Point.from_tup
                |> List.map (fun p -> (p, SearchMap.empty_value ()))
                |> UnvisitedSet.of_list
                |> UnvisitedSet.pt_update {x=0; y=0} (fun _ -> {dist = 0; source = None})

        method private search' (target: pt) (start: pt) (u: unvisited) =
            sm.(start.y).(start.x) <- UnvisitedSet.pt_get start u |> snd;
            let cur_d = sm.(start.y).(start.x).dist in

            let update_distance (u: unvisited) (n: pt) =
                let cost = m.(n.y).(n.x) in
                let updater (tentative: SearchMap.value_type): SearchMap.value_type =
                    if cur_d + cost < tentative.dist
                    then { source = Some start; dist = cur_d + cost}
                    else tentative in
                UnvisitedSet.pt_update n updater u in

            if Point.equals start target
            then begin self#mark start u |> ignore; sm end
            else self#neighbours start
                    |> List.filter (Fun.flip UnvisitedSet.pt_mem u)
                    |> List.fold_left update_distance u
                    |> self#mark start
                    |> self#select_next
                    |> uncurry (self#search' target)

        method search (start: pt) (target: pt) =
            self#search' target start self#uset
    end


    let run (m: map): (int * ptset) =
        let target = fork DangerMap.width DangerMap.height m
                |> both (Fun.flip (-) 1)
                |> Point.from_tup in

        let final_cost (sm: SearchMap.m) =
            sm.(target.y).(target.x).dist in

        let reconstruct_path (sm: SearchMap.m) =
            let unfolder: pt option -> (pt * pt option) option = function
                | Some p -> Some (p, sm.(p.y).(p.x).source)
                | None -> None in
            Seq.unfold unfolder (Some target)
                |> Seq.fold_left (Fun.flip PointSet.add) PointSet.empty in

        let d = new dijkstra m in

        d#search {x=0; y=0} target
           |> fork final_cost reconstruct_path
end

let print_path (map: map) (path: ptset) =
    let fmt p =
        if PointSet.mem (Point.from_tup p) path
        then format_of_string "\x1b[97m%d\x1b[0m"
        else format_of_string "%d" in
    let print_row y row =
        printf "# ";
        Array.iteri (fun x v -> printf (fmt (x, y)) v) row;
        printf "\n";
        Stdlib.(flush stdout) in
    Array.iteri print_row map


let () =
    let argc = Array.length Sys.argv - 1 in

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    Printexc.record_backtrace true;
    File.as_seq Sys.argv.(1)
        |> DangerMap.parse
        |> fun map -> map
        |> Search.run
        |> Fun.peek (snd >> print_path map)
        |> fst |> printf "%d\n"
