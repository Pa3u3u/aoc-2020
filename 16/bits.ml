open Printf
open Toolbox
open Toolbox.Operators
open Toolbox.Pair

exception Invalid_input of string

module BITS = struct
    let (>>=) = Option.bind

    let (<|>) (f: 'a -> 'b option) (g: 'a -> 'b option) (s: 'a): 'b option =
        match f s with
            | None -> g s
            | v -> v

    let check (f: 'a -> bool) (v: 'a): 'a option =
        if f v then Some v else None

    module BitStream = struct
        include Seq

        type tt = (int Seq.t * int list)

        let from_bits: int Seq.t -> tt =
            Fun.flip Pair.make []

        let peek ((s, l): tt): int option = match s (), l with
            | _, 0::_ -> None
            | Cons (b, _), _ -> Some b
            | Nil, _ -> None

        let take (n: int): tt -> (int list * tt) option =
            let rec take' (n: int) (acc: int list) ((s, l): tt) = match n, s(), l with
                | 0, _, _ -> Some (List.rev acc, (s, l))
                | _, Nil, _ -> None
                | _, _, 0::_ -> None
                | n, Cons (b, rest), l -> take' (n - 1) (b::acc) (rest, List.map (Int.add Int.minus_one) l) in
            take' n []

        let take_int (n: int) (s: tt): (int * tt) option =
            let to_int: int list -> int =
                let rec to_int' acc = function
                    | b::rest -> to_int' ((acc lsl 1) + b) rest
                    | [] -> acc in
                to_int' 0 in
            take n s >>= (first to_int >> Option.some)

        let limit ((_, l): tt) = match l with
            | l::_ -> l
            | [] -> Int.max_int

        let set_limit (n: int) ((s, l): tt) = (s, n::l)
        let pop_limit ((s, l): tt) = match l with
            | _::r -> (s, r)
            | [] -> raise (Failure "Limit stack is empty")
    end

    type bitstream = BitStream.tt

    module RawPacket = struct
        type t = {
            version: int;
            type_id: int;
            value: int;
            subpackets: t list;
        }

        type pstate = t * bitstream
        let eat = BitStream.take_int
        let take = BitStream.take

        let empty = { version = 0; type_id = 0; value = 0; subpackets = [] }

        let decode_version (p, s): pstate option =
            eat 3 s >>= (fun (v, s) -> Some ({ p with version = v }, s))

        let decode_type (p, s): pstate option =
            eat 3 s >>= (fun (v, s) -> Some ({ p with type_id = v }, s))

        let try_literal (p, s): pstate option =
            let is_literal p = p.type_id = 4 in
            let add acc n = (acc lsl 4) + n in
            let rec decode_literal' (acc: int) ((p, s): pstate) = match eat 1 s with
                | Some (0, s') -> eat 4 s'
                        >>= (fun (v, s'') -> Some ({ p with value = add acc v }, s''))
                | Some (_, s') -> eat 4 s'
                        >>= (fun (v, s'') -> decode_literal' (add acc v) (p, s''))
                | None -> None in

            let decode_literal (p, s) =
                decode_literal' 0 (p, s) in
            check (fst >> is_literal) (p, s) >>= decode_literal

        let try_subpackets par ((p, s): pstate): pstate option =
            let packet_fin p = { p with subpackets = List.rev p.subpackets }
            and packet_add p c = { p with subpackets = c::p.subpackets } in

            let rec fill_packets limit p (s: bitstream): pstate option =
                if limit = 0
                then Some (packet_fin p, s)
                else match par s empty with
                    | Some (child, s') -> fill_packets (limit - 1) (packet_add p child) s'
                    | None when BitStream.limit s = 0 -> Some (packet_fin p, s)
                    | None -> None in

            let total_subpackets ((p, s): pstate) =
                eat 15 s >>= (fun (v, s) -> Some (v, s))
                    >>= (uncurry BitStream.set_limit >> Option.some)
                    >>= fill_packets Int.max_int p
                    >>= (second BitStream.pop_limit >> Option.some) in

            let count_subpackets ((p, s): pstate) =
                eat 11 s >>= (fun (count, s') ->
                    fill_packets count p s'
                ) in

            match eat 1 s with
                | Some (0, s') -> total_subpackets (p, s')
                | Some (_, s') -> count_subpackets (p, s')
                | None -> None

        let rec decode (x: int) (s: bitstream) (p: t): pstate option =
            Some (p, s) >>= decode_version >>= decode_type
                >>= (try_literal <|> try_subpackets (decode (x + 1)))

        let print =
            let rec print_d off p =
                printf "# %*s" (off * 2) "";
                printf "packet version=%d type=%d" p.version p.type_id;
                if p.type_id = 4 then
                    printf " value=%d\n" p.value
                else
                    printf " subpackets=%d\n" (List.length p.subpackets);
                    List.iter (print_d (off + 1)) p.subpackets; in
            print_d 0
    end

    let decode (s: bitstream): RawPacket.t option =
        Fun.flip (RawPacket.decode 0) RawPacket.empty s
            >>= (fst >> Option.some)


    module Packet = struct
        type t =
            | Aggregation of string * t list * (int list -> int)
            | Comparison of string * (t * t) * (int -> int -> bool)
            | Literal of int

        let minimum =
            List.fold_left (fun a v -> if a < v then a else v) Int.max_int
        let maximum =
            List.fold_left (fun a v -> if a > v then a else v) Int.min_int

        let rec from_raw (r: RawPacket.t): t =
            let unwrap = function
                | a::b::[] -> (a, b)
                | _ -> raise (Failure "Invalid number of packets in expression") in
            let children = List.map from_raw r.subpackets in

            match r.type_id with
            | 0 -> Aggregation ("Σ", children, List.sum)
            | 1 -> Aggregation ("Π", children, List.prod)
            | 2 -> Aggregation ("min", children, minimum)
            | 3 -> Aggregation ("max", children, maximum)
            | 4 -> Literal r.value
            | 5 -> Comparison (">", unwrap children, (>))
            | 6 -> Comparison ("<", unwrap children, (<))
            | 7 -> Comparison ("=", unwrap children, (=))
            | t -> raise (Failure (sprintf "Unknown packet type %d\n" t))

        let rec eval (p: t) = match p with
            | Literal v -> v
            | Aggregation (_, l, f) -> f (List.map eval l)
            | Comparison (_, (a, b), p) -> if p (eval a) (eval b) then 1 else 0

        let rec to_string (p: t) =
            match p with
                | Literal l -> sprintf "%d" l
                | Aggregation (s, l, _) ->
                        sprintf "%s(%s)" s (String.concat "; " (List.map to_string l))
                | Comparison (s, (a, b), _) ->
                        sprintf "%s %s %s" (to_string a) s (to_string b)

        let print (p: t) =
            printf "# %s\n" (to_string p)
    end
end


let to_bits: string Seq.t -> int Seq.t =
    let hex_to_byte = function
        | digit when '0' <= digit && digit <= '9' -> Char.code digit - Char.code '0'
        | hex when 'A' <= hex && hex <= 'F' -> 10 + Char.code hex - Char.code 'A'
        | c -> raise (Invalid_input (sprintf "Invalid character in hex input: %c" c)) in

    let byte_to_bits b =
        let bite = function
            | 0 -> None
            | n -> let k = n - 1 in
                Some ((b land (1 lsl k)) lsr k, k) in
        Seq.unfold bite 4 in

    let str_to_hex_chars: string Seq.t -> char Seq.t =
        Seq.flat_map (String.to_chars >> List.to_seq)  in

    let hex_chars_to_bits: char Seq.t -> int Seq.t =
        Seq.flat_map (hex_to_byte >> byte_to_bits) in

    str_to_hex_chars >> hex_chars_to_bits


let sum_versions (p: BITS.RawPacket.t): int =
    let rec fold_versions acc (p': BITS.RawPacket.t) =
        List.fold_left fold_versions (acc + p'.version) p'.subpackets in
    fold_versions 0 p


let dump_bits (s: int Seq.t): unit =
    let c = ref 0 in

    let print_bit b =
        if !c mod 16 = 0
        then printf "\n# | "
        else if !c mod 8 = 0
        then printf " ";
        c := !c + 1;
        printf "%d" b in
    printf "# | ";
    Seq.iter print_bit s;
    printf "\n";
    Stdlib.(flush stdout)


let () =
    let argc = Array.length Sys.argv - 1 in

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    Printexc.record_backtrace true;
    File.as_seq Sys.argv.(1)
        |> to_bits
        (*|> Fun.peek (dump_bits)*)
        |> BITS.BitStream.from_bits
        |> BITS.decode
        |> Option.map BITS.Packet.from_raw
        |> Fun.peek (Option.iter BITS.Packet.print)
        |> Option.iter (BITS.Packet.eval >> printf "%d\n")
