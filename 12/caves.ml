open Printf
open Toolbox
open Toolbox.Operators


exception Invalid_input of string

module Graph = struct
    type node =
        | Start
        | End
        | Node of bool * string

    module Node = struct
        let to_string = function
            | Start -> "\x1b[92m•\x1b[0m"
            | End -> "\x1b[91m•\x1b[0m"
            | Node (t, label) -> sprintf "‹%s%s\x1b[0m›" (ifv t "\x1b[33m" "\x1b[37m") label
    end

    type edge = (node * node)
    type path = node list

    module Path = struct
        let to_string =
            List.map Node.to_string >> String.concat " → "
    end

    class graph = object (self)
        val mutable edges = ([]: edge list)

        method has_edge (e: edge): bool =
            match List.find_opt (fun v -> v = e) edges with
            | None -> false
            | Some _ -> true

        method add (e: edge) =
            if not (self#has_edge e) then
                edges <- e :: edges

        method get_edges = edges

        method neighbours (n: node): node Seq.t =
            let rec next_neighbour = function
                | (a, b)::rest when a = n -> Some (b, rest)
                | _::rest -> next_neighbour rest
                | [] -> None in

            Seq.unfold next_neighbour edges
    end

    class undir_graph = object
        inherit graph as super

        method! add ((a, b): edge) = begin
            super#add (a, b); super#add (b, a)
        end
    end
end


let parse_node: string -> Graph.node =
    let is_upper c =
        Char.uppercase_ascii c = c in
    function
    | "start" -> Start
    | "end" -> End
    | "" -> raise (Invalid_input "Empty node")
    | str -> Node (String.get str 0 |> is_upper, str)


let parse_input: string Seq.t -> Graph.edge Seq.t =
    let unpack = function
        | [x; y] -> (x, y)
        | _ -> raise (Invalid_input "Invalid edge specification") in
    Seq.map (String.split_on_char '-' >> List.map parse_node >> unpack)


let create_graph: Graph.edge Seq.t -> Graph.graph =
    Seq.fold_left (fun g e -> g#add e; g) (new Graph.undir_graph)


module NodeMapType = struct
    type t = Graph.node
    let compare (n1: t) (n2: t) = match (n1, n2) with
        | (Start, _) -> -1
        | (_, Start) ->  1
        | (_, End) -> 1
        | (End, _) -> -1
        | (Node (_, a), Node (_, b)) -> String.compare a b
end


module NodeMap = struct
    include Map.Make(NodeMapType)

    let add_inc n =
        let updater = function
            | None -> Some 1
            | Some v -> Some (v + 1) in
        update n updater

    let has_value value =
        exists (fun _ v -> value = v)
end


type nodemap = int NodeMap.t


let find_paths: Graph.graph -> Graph.path list =
    let rec traverse_fold (path: Graph.path) (s: nodemap) (g: Graph.graph)
            (paths: Graph.path list) (node: Graph.node): Graph.path list =
        List.append paths (traverse node (node::path) s g)

    and traverse (n: Graph.node) (path: Graph.path) (s: nodemap) (g: Graph.graph): Graph.path list =
        match n with
        | Start when (List.length path != 1) -> []
        | End -> [List.rev path]
        | Node (false, _) when NodeMap.mem n s -> []
        | _ -> g#neighbours n
                |> Seq.fold_left (traverse_fold path (NodeMap.add_inc n s)  g) [] in

    traverse Start [Start] NodeMap.empty


let () =
    let argc = Array.length Sys.argv - 1 in

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    File.as_seq Sys.argv.(1)
        |> parse_input
        |> create_graph
        |> peek (fun g -> List.iter (both Graph.Node.to_string >> uncurry (printf "# edge %s → %s\n")) g#get_edges)
        |> find_paths
        |> peek (List.iter (Graph.Path.to_string >> printf "# path %s\n"))
        |> List.length |> printf "%d\n"
