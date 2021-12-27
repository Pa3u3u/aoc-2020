open Printf
open Toolbox
open Toolbox.Core
open Toolbox.Extensions
open Toolbox.Operators
open Toolbox.Pair

exception Invalid_input of string

module Point = struct
    type t = { x: int; y: int }
    let to_tup p = (p.x, p.y)
    let from_tup (x, y) = {x; y}

    let compare p1 p2 = match Int.compare p1.x p2.x with
        | 0 -> Int.compare p1.y p2.y
        | n -> n

    let equals p1 p2 = p1.x = p2.x && p1.y = p2.y
end

module type TileT = sig
    type t
    val init: t
    val parse: char -> t
end

module Tile(TT: TileT) = struct
    include Matrix.Make(TT)

    let parse = Seq.map String.to_chars
        >> Seq.map (List.map TT.parse)
        >> List.of_seq
        >> from_lists
end

module type TiledViewT = sig
    include TileT
    val remap: (int * int) -> t -> t
end

module TiledView(TWT: TiledViewT) = struct
    module Tile = Tile(TWT)
    type m = TWT.t Tile.t
    type t = (m * int * int)

    let create m tw th = (m, tw, th)

    let width (mm, tw, _) = (Tile.width mm) * tw
    let height (mm, _, th) = (Tile.height mm) * th

    let _mm (mm, _, _) = mm

    let _coords mm (virt_x, virt_y) =
        let w = Tile.width mm and h = Tile.height mm in
        let tile_x = virt_x / w and tile_y = virt_y / h in
        let real_x = virt_x mod w and real_y = virt_y mod h in
        ((tile_x, real_x), (tile_y, real_y))

    let get (mm, _, _) p =
        let ((tile_x, real_x), (tile_y, real_y)) = _coords mm p in
        TWT.remap (tile_x, tile_y) mm.(real_y).(real_x)

    let valid_point m (virt_x, virt_y) =
            0 <= virt_x && virt_x < (width m)
        &&  0 <= virt_y && virt_y < (height m)
end

type pt = Point.t

module PointSet = Set.Make(Point)
type ptset = PointSet.t


module RepeatingTiles = struct
    type t = int
    let init = 0
    let remap (tile_x, tile_y) v =
        ((v + tile_x + tile_y - 1) mod 9) + 1
    let parse = Char.code >> Fun.flip (-) (Char.code '0')
end

module CaveMap = TiledView(RepeatingTiles)

type map = CaveMap.t

module type PriorityQueueType = sig
    type k
    type v
    val compare: v -> v -> int
end

module PriorityQueue(MT: PriorityQueueType) = struct
    type k = MT.k
    type v = MT.v
    type p = k * v
    type t = p list

    let cmp' = MT.compare

    let empty: t = []

    let rec add (k: k) (v: v): t -> t = function
        | [] -> [(k, v)]
        | (p, _)::_ when p = k -> raise (Failure "Duplicates!")
        | (pr, vr)::rest when cmp' v vr < 0 -> (k, v)::(pr, vr)::rest
        | (pl, vl)::(pr, vr)::rest when cmp' vl v <= 0 && cmp' v vr < 0
                -> (pl, vl)::(k, v)::(pr, vr)::rest
        | r::rest -> r :: add k v rest

    let get (k: k): t -> v =
        List.assoc k

    let peek: t -> k * v =
        List.hd

    let get_opt (k: k): t -> v option =
        List.assoc_opt k

    let has (k: k): t -> bool =
        List.mem_assoc k

    let remove (k: k): t -> t =
        List.remove_assoc k

    let fetch: t -> ((k * v) * t) = function
        | h::l -> (h, l)
        | [] -> raise (Failure "No elements left in the queue")

    let of_list: (k * v) list -> t =
        List.fold_left (fun l (k, v) -> add k v l) empty

    let rec update (k: k) (f: v option -> v option): t -> t =
        let rec fix_order_fwd = function
            | (ka, va)::(kb, vb)::rest when va > vb -> (kb, vb) :: (fix_order_fwd ((ka, va)::rest))
            | rest -> rest in
        let fix_order_rev = function
            | (ka, va)::(kb, vb)::rest when va > vb -> (kb, vb) :: (ka, va)::rest
            | rest -> rest in

        let update_elt f k v rest = match f v with
            | Some v' -> fix_order_fwd ((k, v')::rest)
            | None -> rest in

        function
        | (k', v)::rest when k = k' -> update_elt f k (Some v) rest
        | p::rest -> fix_order_rev (p :: update k f rest)
        | [] -> update_elt f k None []

    let update_ex (k: k) (f: v -> v): t -> t =
        update k (Option.map f)
end


module VisitedPoints = Hashtbl.Make(struct
    type t = pt
    let equal = Point.equals
    let hash (pt: pt) = pt.y * 256 + pt.x
end)


module Search = struct
    type data_t = {
        dist: int;
        source: pt option
    }

    type visited = data_t VisitedPoints.t

    module Unvisited = struct
        type k = pt
        type v = data_t
        let compare (a: v) (b: v) =
            Int.compare a.dist b.dist
    end

    module PQ = struct
        include PriorityQueue(Unvisited)
    end

    type pqueue = PQ.t

    class dijkstra (m: map) = object (self)
        val sm = VisitedPoints.create 50

        method private select_next (u: pqueue): pt * pqueue =
            (PQ.peek u, u) |> first fst

        method private neighbours (p: pt) =
           let offsets = [(-1, 0); (1, 0); (0, -1); (0, 1)] in
           List.map (lift2 (+) (p.x, p.y)) offsets
               |> List.filter (CaveMap.valid_point m)
               |> List.map Point.from_tup

        method uset (start: pt): pqueue =
            PQ.empty |> PQ.add start {source = None; dist = 0}


        method private _dump (start: pt) (u: pqueue) =
            printf "# --- dump [%d %d] ---\n" start.x start.y;

            printf "# v:  {\n";
            VisitedPoints.iter (fun k v -> printf "#     [%d %d]=%d\n" k.x k.y v.dist) sm;
            printf "# }\n";

            printf "# pq: {\n";
            List.iter (fun ((k, v): pt * data_t) -> printf "#     [%d %d]=%d\n" k.x k.y v.dist) u;
            printf "# }\n";
            Stdlib.(flush stdout)


        method private search' (v: ptset) (target: pt) (start: pt) (u: pqueue) =
            let cur_v = PQ.get start u in
                VisitedPoints.add sm start cur_v;

            let cur_d = cur_v.dist in

            let update_distance (u: pqueue) (n: pt) =
                let cost = CaveMap.get m (n.x, n.y) in
                let new_entry: data_t = { source = Some start; dist = cur_d + cost } in
                let updater: data_t option -> data_t option = function
                    | None -> Some new_entry
                    | Some p when cur_d + cost < p.dist -> Some new_entry
                    | n -> n in
                PQ.update n updater u in

            if Point.equals start target
            then begin PQ.remove start u |> ignore; sm end
            else self#neighbours start
                    |> List.filter (Fun.flip PointSet.mem v >> not)
                    |> List.fold_left update_distance u
                    |> PQ.remove start
                    |> self#select_next
                    |> uncurry (self#search' (PointSet.add start v) target)

        method search (start: pt) (target: pt) =
            VisitedPoints.reset sm;
            self#search' (PointSet.empty) target start (self#uset start)
    end


    let run (m: map): (int * ptset) =
        let target = fork CaveMap.width CaveMap.height m
                |> both (Fun.flip (-) 1)
                |> Point.from_tup in

        let final_cost (v: visited) =
            (VisitedPoints.find v target).dist in

        let reconstruct_path (v: visited) =
            let unfolder: pt option -> (pt * pt option) option = function
                | Some p -> Some (p, (VisitedPoints.find v p).source)
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

    let indices_y = (0 &-- CaveMap.height map)
    and indices_x = (0 &-- CaveMap.width map) in

    let iter_cell y x =
        printf (fmt (x, y)) (CaveMap.get map (x, y)) in

    let iter_row y =
        printf "# ";
        List.iter (iter_cell y) indices_x;
        printf "\n" in
    List.iter (iter_row) indices_y


let () =
    let argc = Array.length Sys.argv - 1 in

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    Printexc.record_backtrace true;
    File.as_seq Sys.argv.(1)
        |> CaveMap.Tile.parse
        |> fun m -> CaveMap.create m 5 5
        |> fun map -> map
        |> Search.run
        (*|> Fun.peek (snd >> print_path map)*)
        |> fst |> printf "%d\n"
