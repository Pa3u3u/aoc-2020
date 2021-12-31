open Printf
open Toolbox.Core
open Toolbox.Extensions
open Toolbox.Operators
open Toolbox.Parser

module Graph(N: Set.OrderedType) = struct
    module Node = struct
        type t = N.t
        let compare = N.compare
        let equal n1 n2 = N.compare n1 n2 = 0
    end

    type tn = Node.t

    module Edge = struct
        type t = tn * tn

        let equal (s1, e1) (s2, e2) =
            Node.equal s1 s2 && Node.equal e1 e2

        let compare (s1, e1) (s2, e2) =
            List.zip_with Node.compare [s1; e1] [s2; e2]
                |> List.find_opt ((!=) 0)
                |> Option.value ~default:0

        let rev (s, e) = (e, s)
    end

    type te = Edge.t

    module NodeSet = Set.Make(N)
    module EdgeSet = Set.Make(Edge)

    type t = NodeSet.t * EdgeSet.t

    let empty: t = (NodeSet.empty, EdgeSet.empty)

    let add_node (n: tn) ((ns, es): t) =
        (NodeSet.add n ns, es)

    let add_nodes (n: tn list) ((ns, es): t) =
        (List.fold_left (Fun.flip NodeSet.add) ns n, es)

    let add_edge (e: te) ((ns, es): t) =
        let (s, t) = e in
        (NodeSet.add s ns |> NodeSet.add t, EdgeSet.add e es)

    let add_edge_bidi (e: te) (g: t) =
        add_edge e g |> add_edge (Edge.rev e)

    let add_edges_bidi (el: te list) (g: t) =
        List.fold_left (Fun.flip add_edge_bidi) g el

    let disconnect (n: tn) ((ns, es): t) =
        (ns, EdgeSet.filter (fun (s, t) -> not (Node.equal n s) && not (Node.equal n t)) es)

    let neighbours (n: tn) ((_, es): t): tn list =
        EdgeSet.elements es
            |> List.filter (fun (s, _) -> Node.equal s n)
            |> List.map (fun (_, e) -> e)
end

module Map = struct
    module Tile = struct
        type t =
            | Hall of int
            | Room of int * int

        let same a b = match a, b with
            | Hall _, Hall _ -> true
            | Room _, Room _ -> true
            | _ -> false

        let compare a b = match a, b with
            | Hall x, Hall y -> Int.compare x y
            | Room (p, x), Room (q, y) -> List.zip_with Int.compare [p; x] [q; y]
                |> List.find_opt ((!=) 0) |> Option.value ~default:0
            | Hall _, Room _ -> -1
            | Room _, Hall _ -> 1

        let to_string (t: t) = match t with
            | Hall n -> sprintf "‹Hall %d›" n
            | Room (k, n) -> sprintf "‹Room %d %d›" k n
    end

    module TileGraph = Graph(Tile)
    module TileMap = Map.Make(Tile)
    type m = char TileMap.t
    type g = TileGraph.t

    type t = m * (g * int)

    let room_size (_, _, rs) = rs
    let room_range rs = 0 &-- rs
    let hall_size = 11
    let hall_range = 0 &-- hall_size

    let map rs =
        let hall_parts = List.map (fun n -> Tile.Hall (n - 1), Tile.Hall n) (List.tl hall_range) in
        let room k = (Tile.Room (k, 0), Tile.Hall (2 * (k + 1)))
            :: List.map (fun i -> (Tile.Room (k, i - 1), Tile.Room (k, i))) (List.tl (0 &-- rs)) in
        let rooms = List.map room (0 &-- 4) |> List.concat in
        TileGraph.(empty
            |> add_edges_bidi hall_parts
            |> add_edges_bidi rooms
    )

    module CritterCost = Map.Make(Char)
    type cc = int CritterCost.t

    let critter_index c =
        let index = CritterCost.(empty
                |> add 'A' 0
                |> add 'B' 1
                |> add 'C' 2
                |> add 'D' 3) in
        CritterCost.find c index

    let move_cost c =
        Num.ipow 10 (critter_index c)

    let create (l: char list): t =
        let map_critter (n, m) c =
            (n + 1, TileMap.add (Room (n mod 4, n / 4)) c m) in
        let map_critters =
            List.fold_left map_critter (0, TileMap.empty) >> snd in
        let rs = List.length l / 4 in

        assert (List.length l mod 4 == 0);
        assert (List.length l > 0);
        (map_critters l, (map rs, rs))

    let print ((m, (_, rs)): t) =
        let get_hall k =
            TileMap.find_opt (Hall k) m |> Option.value ~default:'.' in
        let get_room k n =
            TileMap.find_opt (Room (k, n)) m |> Option.value ~default:'.' in
        let hall =
            hall_range |> List.map get_hall |> List.to_seq |> String.of_seq in
        printf "#############\n";
        printf "#%s#\n" hall;
        printf "###%c#%c#%c#%c###\n" (get_room 0 0) (get_room 1 0) (get_room 2 0) (get_room 3 0);
        (1 &-- rs) |> List.iter (fun k ->
            printf "  #%c#%c#%c#%c#  \n" (get_room 0 k) (get_room 1 k) (get_room 2 k) (get_room 3 k)
        );
        printf "  #########\n"

    module Solver = struct
        let pwcmp l r =
            List.zip_with Int.compare l r
                |> List.find_opt ((!=) 0)
                |> Option.value ~default: 0

        module Cost = struct
            type t = { cost: int; h: int }
            let compare (a: t) (b: t) = pwcmp [a.h; a.cost] [b.h; b.cost]
            let init = { cost = 0; h = 0 }
            let min (a: t) (b: t) = if compare a b < 0 then a else b
            let to_string (a: t) = sprintf "[%d, %d]" a.cost a.h
        end

        type cost = Cost.t

        let get_rooms m rs k =
            room_range rs |> List.map (fun n -> TileMap.find_opt (Room (k, n)) m)

        let same_critter x y =
             Option.is_some x && Option.is_some y && x = y

        let get_room_contents m rs =
            (0 &-- 4) |> List.map (fun k -> (k, get_rooms m rs k))

        let solved ((m, (_, rs)): t) =
            get_room_contents m rs
                |> List.map (fun (k, contents) ->
                        List.map (fun o -> Option.is_some o && k = critter_index (Option.get o)) contents)
                |> List.flatten
                |> List.all

        let finished_room_range m rs k n =
            (n &-- rs)
                    |> List.map (fun n' -> TileMap.find_opt (Room (k, n')) m)
                    |> List.map (function
                        | Some c -> k = critter_index c
                        | None -> false)
                    |> List.all

        let reachable_paths tile cr ((m, (g, rs)): t) =
            let rec reachable_cell found = function
                | [] -> TileMap.bindings found |> List.filter (fun (_, c) -> c > 0)
                | (from, c)::rest -> match TileMap.find_opt from found with
                    | Some oldc when oldc <= c -> reachable_cell found rest
                    | Some _ -> reachable_cell (TileMap.update from (fun _ -> Some c) found) rest
                    | None -> TileGraph.neighbours from g
                        |> List.filter (fun e -> not (TileMap.mem e m))
                        |> List.map (fun e -> (e, c + 1))
                        |> Fun.flip List.append rest |> reachable_cell (TileMap.add from c found) in

            let valid_room_move t = match t with
                | Tile.Room (k, n) -> k = critter_index cr && finished_room_range m rs k (n + 1)
                | _ -> true in

            let valid_hall_move t = match t with
                | Tile.Hall n -> n mod 2 != 0 || n < 2 || n > 8
                | Tile.Room _ -> true in

            let valid_move src dst = match src, dst with
                | Tile.Hall _, Tile.Hall _ -> false
                | _ -> true in

            reachable_cell TileMap.empty [(tile, 0)]
                |> List.filter (fun (t, _) -> valid_move tile t)
                |> List.filter (fun (t, _) -> valid_room_move t)
                |> List.filter (fun (t, _) -> valid_hall_move t)

        let valid_moves ((m, (g, rs)): t): (Tile.t * Tile.t * int) list =
            let finished_tile tile _ = match tile with
                | Tile.Hall _ -> false
                | Room (k, n) -> finished_room_range m rs k n in
            let find_neighbours (tile, crit) =
                if finished_tile tile crit then [] else
                reachable_paths tile crit (m, (g, rs))
                    |> List.map (fun (e, cm) -> (tile, e, cm * (move_cost crit))) in

            let [@warning "-26"] print_moves (t, e, cost) =
                printf "#  move %s → %s (%d)\n" (Tile.to_string t) (Tile.to_string e) cost in

            TileMap.bindings m
                |> List.map find_neighbours
                |> List.concat
              (*|> Fun.peek (List.iter print_moves)*)

        let turn (((m, (g, rs)), path, c): (t * m list * cost)): (t * m list * cost) list =
            let apply_move (src, dest, dc) =
                let critter = TileMap.find src m in
                ((TileMap.remove src m |> TileMap.add dest critter, (g, rs)), { c with cost = c.cost + dc }) in

            valid_moves (m, (g, rs))
                    |> List.map apply_move
                    |> List.map (fun (m', c') -> (m', (fst m')::path, c'))

        let recalculate_h ((m, g), vp, vc): (t * m list * cost) =
            let errors =
                let ecost = function
                    | (Tile.Hall _, _) -> 1
                    | (Tile.Room (k, _), c) -> if k = critter_index c then 0 else 20 in
                TileMap.bindings
                >> List.map ecost
                >> List.sum in
            ((m, g), vp, { vc with h = errors m })

        let rec insert_to (sorted: (t * m list * cost) list): (t * m list * cost) list -> (t * m list * cost) list =
            let rec insert (v, vp, vc) = function
                | [] -> [(v, vp, vc)]
                | (u, up, uc)::rest when Cost.compare vc uc < 0 -> (v, vp, vc)::(u, up, uc)::rest
                | (u, up, uc)::rest when u = v -> insert (u, up, Cost.min vc uc) rest
                | u::rest -> u :: insert (v, vp, vc) rest in
            function
            | x::xs -> insert_to (insert (recalculate_h x) sorted) xs
            | [] -> sorted

        let [@warning "-27"] peek cc ((x, c): t * cost) best prospect =
            let print_best =  function
                | None -> printf "#   best: none\n"
                | Some (_, _, c) -> printf "#   best: %s\n" (Cost.to_string c) in
            let print_prospect = function
                | None -> ()
                | Some p -> printf "#   pros: %d\n" (p + c.cost) in
            printf "# New solution:\n";
            printf "#   iter: %d\n" cc;
            printf "#   cost: %s\n" (Cost.to_string c);
            print_best best;
            print_prospect prospect;
            Stdlib.(flush stdout)

        let [@warning "-26"] prospect m =
            let extract (t, c) = match t with
                | Tile.Hall n -> (t, c, move_cost c, n)
                | Tile.Room (k, _) -> (t, c, move_cost c, k) in
            let cost_bound (t, c, mc, a) = match t with
                | Tile.Hall _ -> Int.abs((critter_index c) - a) * mc
                | Tile.Room _ -> let ci = critter_index c in
                    if ci = a then 0 else (2 * (Int.abs (ci - a)) + 2) * mc in

            TileMap.bindings m
                |> List.map extract
                |> List.map cost_bound
                |> List.sum

        let [@warning "-27"] rec run' cc (best: (t * m list * cost) option): (t * m list * cost) list -> (t * m list * cost) option =
            let choose_best xopt (y, yb, cy) = match xopt with
                | None -> Some (y, yb, cy)
                | Some (x, xb, cx) -> Some (if (cx < cy) then (x, xb, cx) else (y, yb, cy)) in
            let skip (x, c): (t * m list * cost) option -> bool = function
                | None -> false
                | Some (x', vp, c') -> let p = prospect (fst x) in
                    (*peek (x, c) (Some (x', vp, c')) (Some p);*)
                    c > c' || p + c.cost > c'.cost in

            function
            | (x, xp, c)::rest when solved x -> peek cc (x, c) best None;
                    run' (cc + 1) (choose_best best (x, xp, c)) rest
            | (x, xp, c)::xs ->
                    if skip (x, c) best
                    then run' (cc + 1) best xs
                    else turn (x, xp, c) |> insert_to xs |> run' (cc + 1) best
            | [] -> best

        let run (s: t) =
            let (m, _) = s in
            run' 0 None [(s, [m], Cost.init)]
                |> Option.get
                |> fun (a, b, c) -> (a, List.rev b, c)
    end

    module Parser = struct
        include CharParser

        let line: char list p =
            between (str "###" <|> str "  #") (str "###" <|> str "#")
                (sep_by (symbol '#') (any_of "ABCD"))

        let p: char list p =
            count 2 (skip_until any eol)
                @>> sep_by eol line |> map List.flatten

        let parse =
            Seq.resident >> run p () >> function
                | Ok v -> v
                | Error (s, _) -> raise (Failure s)
    end
end

let () =
    let argc = Array.length Sys.argv - 1 in

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    Printexc.record_backtrace true;

    File.as_char_seq Sys.argv.(1)
        |> Map.Parser.parse
        |> Map.create
        |> Fun.peek (fun m -> Map.print m; Stdlib.(flush stdout))
        |> Map.Solver.run
        |> fun ((m, (g, rs)), p, c) -> printf "# -- Path to solution:\n";
            List.iter (fun x -> Map.print (x, (g, rs))) p;
            Map.print (m, (g, rs)); printf "%d\n" c.cost
