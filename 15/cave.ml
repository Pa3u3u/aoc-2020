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


module Search = struct
    module Unvisited = struct
        type k = pt
        type v = SearchMap.v
        let compare (a: v) (b: v) =
            Int.compare a.dist b.dist
    end

    module PQ = struct
        include PriorityQueue(Unvisited)
    end

    module Unvisited' = struct
        type t = pt * SearchMap.v
        let compare ((p1, vt1): t) ((p2, vt2): t) =
            match Int.compare vt1.dist vt2.dist with
                | 0 -> Point.compare p1 p2
                | n -> n
    end

    type pqueue = PQ.t

    class dijkstra (m: map) = object (self)
        val mutable sm = SearchMap.create 0 0

        method private mark (p: pt) (u: pqueue) =
            let r = PQ.get p u in
            sm.(p.y).(p.x) <- r;
                PQ.remove p u

        method private select_next (u: pqueue): pt * pqueue =
            (PQ.peek u, u) |> first fst

        method private neighbours (p: pt) =
           let offsets = [(-1, 0); (1, 0); (0, -1); (0, 1)] in
           List.map (lift2 (+) (p.x, p.y)) offsets
               |> List.filter (SearchMap.valid_point sm)
               |> List.map Point.from_tup

        method uset (start: pt): pqueue =
            PQ.empty |> PQ.add start {source = None; dist = 0}


        method private search' (v: ptset) (target: pt) (start: pt) (u: pqueue) =
            sm.(start.y).(start.x) <- PQ.get start u;
            let cur_d = sm.(start.y).(start.x).dist in

            let update_distance (u: pqueue) (n: pt) =
                let cost = m.(n.y).(n.x) in
                let new_entry: SearchMap.v = { source = Some start; dist = cur_d + cost } in
                let updater: SearchMap.v option -> SearchMap.v option = function
                    | None -> Some new_entry
                    | Some p when cur_d + cost < p.dist -> Some new_entry
                    | n -> n in
                PQ.update n updater u in

            if Point.equals start target
            then begin self#mark start u |> ignore; sm end
            else self#neighbours start
                    |> List.filter (Fun.flip PointSet.mem v >> not)
                    |> List.fold_left update_distance u
                    |> self#mark start
                    |> self#select_next
                    |> uncurry (self#search' (PointSet.add start v) target)

        method search (start: pt) (target: pt) =
            sm <- SearchMap.lift m;
            self#search' (PointSet.empty) target start (self#uset start)
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
