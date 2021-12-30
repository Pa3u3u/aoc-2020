open Printf
open Toolbox.Core
open Toolbox.Extensions
open Toolbox.Operators
open Toolbox.Parser


module Dim = struct
    type t = int * int

    let make a b = assert (a <= b); (a, b)
    let empty (a, b) = a >= b
    let equal (a, b) (p, q) = a = p && b = q

    let compare (a, b) (p, q) =
        List.zip_with Int.compare [a; b] [p; q]
            |> List.find_opt ((!=) 0)
            |> Option.value ~default:0

    let contains_value v (a, b) = a <= v && v < b

    let contains (a, b) (p, q) = a <= p && q <= b
    let contained_in d1 d2 = contains d2 d1

    let overlaps (a, b) (p, q) =
            contains_value p (a, b) || contains_value a (p, q)

    let size (a, b) = b - a

    let break (a, b) (p, q) =
        let rec break' = function
            | a::b::rest -> (a, b) :: break' (b::rest)
            | _ -> [] in
        if      overlaps (a, b) (p, q)
        then    List.sort Int.compare [a; b; p; q] |> break'
        else    []

    let join (a, b) (p, q) =
        if      b + 1 = p
        then    Some (a, q)
        else    None

    let to_string (a, b) =
        sprintf "⟨%d, %d)" a b
end

module Shape = struct
    type t = Dim.t list

    let dims = List.length
    let same_dim s1 s2 = dims s1 = dims s2

    let empty l = List.exists Dim.empty l
    let equal s1 s2 = List.zip_with Dim.equal s1 s2 |> List.all

    let compare s1 s2 =
        List.zip_with Dim.compare s1 s2
            |> List.find_opt ((!=) 0)
            |> Option.value ~default:0

    let contains_point p s =
        List.zip_with Dim.contains_value p s |> List.all

    let contains s1 s2 =
        List.zip_with Dim.contains s1 s2 |> List.all
    let contained_in s1 s2 = Fun.flip contains s1 s2

    let overlaps s1 s2 = List.zip_with Dim.overlaps s1 s2 |> List.all

    let size = List.map Dim.size >> List.prod

    let break s r =
        let valid shard = not (empty shard)
            && (overlaps s shard || overlaps r shard) in
        List.zip_with Dim.break s r
            |> List.nproduct
            |> List.filter valid

    let split s r =
        break s r |> List.partition (overlaps s)

    let to_string =
        List.map Dim.to_string >> String.concat " × "

    module D2 = struct
        let print (dim: t) (shapes: t list) =
            let (y1, y2) = List.nth dim 1
            and (x1, x2) = List.nth dim 0 in

            let print_cell x y = match List.index (contains_point [x; y]) shapes with
                | Some n -> Char.chr ((Char.code 'A') + n) |> printf "%c"
                | None -> printf "·" in

            let print_row y =
                printf "# ";
                x1 &-- x2 |> List.iter (fun x -> print_cell x y);
                printf "\n" in

            printf "# -- 2D shape --\n";
            y1 &-- y2 |> List.iter print_row
    end
end

type shape = Shape.t


module Shapes = struct
    type t = shape list

    let empty = []

    let merge al bl =
        let rec merge' xs ys rs = match xs, ys with
            | [], ys -> ys @ rs
            | xs, [] when rs = [] -> xs
            | x::xs, [] -> x :: merge' xs rs []
            | x::xs, y::ys when not (Shape.overlaps x y) -> merge' (x::xs) ys (y::rs)
            | x::xs, y::ys -> let (x', y') = Shape.split x y in
                merge' (x' @ xs) (y' @ ys) rs
        in

        merge' al bl []

    let add v s =
        merge s [v]

    let remove v s =
        merge s [v] |> List.reject (Shape.overlaps v)

    let crop d =
        let rec crack d = function
            | [] -> []
            | s::xs when not (Shape.overlaps d s) -> s :: crack d xs
            | s::xs -> let (s', _) = Shape.split s d in
                    s' @ crack d xs in
        crack d >> List.filter (Shape.overlaps d)

    let print l =
        printf "# -- Shapes dump --\n";
        printf "# {\n";
        List.iter (Shape.to_string >> printf "#     %s\n") l;
        printf "# }\n";
end

type shapes = Shapes.t


module Reactor = struct
    module Command = struct
        type mode = On | Off
        type t = mode * Shape.t

        let mode_to_string = function
            | On -> "on"
            | Off -> "off"

        let to_string (m, s) =
            sprintf "%s [%s]" (mode_to_string m) (Shape.to_string s)
    end

    type command = Command.t

    module Parser = struct
        include CharParser

        let parse_mode: Command.mode p =
            choose2 (str "on" @>> return Command.On)
                    (str "off" @>> return Command.Off)

        let parse_range c: Dim.t p =
            symbol c @>> symbol '=' @>> integer |> then_skip (str "..")
                |> Fun.flip combine integer
                |> map (fun (a, b) -> Dim.make a (b + 1))

        let parse_shape: Shape.t p =
            ['x'; 'y'; 'z']
                |> List.map (parse_range >> then_skip (optional (symbol ',')))
                |> sequence

        let parse_command: command p =
            combine (parse_mode <?> "mode") (symbol ' ' @>> parse_shape <?> "shape")

        let parse =
            Seq.resident >> run (sep_by eol parse_command) () >> function
                | Ok v -> v
                | Error (s, _) -> raise (Failure s)
    end

    let interpret: command list -> shapes =
        let exe (l: shapes): command -> shapes = function
            | (On, s) -> Shapes.add s l
            | (Off, s) -> Shapes.remove s l in
        List.fold_left exe Shapes.empty

    let crop d =
        Dim.make (-50) 51 |> List.replicate d |> Shapes.crop

    let size =
        List.map Shape.size >> List.sum
end

let () =
    let argc = Array.length Sys.argv - 1 in

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    Printexc.record_backtrace true;

    File.as_char_seq Sys.argv.(1)
        |> Reactor.Parser.parse
        |> Fun.peek (List.iter (Reactor.Command.to_string >> printf "# %s\n"))
        |> Reactor.interpret
        |> Reactor.crop 3
        |> Reactor.size
        |> printf "%d\n"
