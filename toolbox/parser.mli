module type ParseType = sig
    type token
    type user
end

module Make(P: ParseType): sig
    type t = P.token
    type s = t Seq.t
    type u = P.user
    type h = (u * s * int)
    type pin = h
    type 'r pout = ('r * h, string * h) result
    type 'r p = pin -> 'r pout

    val (>>=): ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
    val (>=>): ('a -> ('b, 'e) result) -> ('b -> ('c, 'e) result) -> 'a -> ('c, 'e) result

    val perhaps: 'a -> 'a p -> 'a p
    val optional: 'a p -> unit p
    val sequence: 'a p list -> 'a list p
    val count: int -> 'a p -> 'a list p
    val choose: 'a p list -> 'a p
    val choose2: 'a p -> 'a p -> 'a p
    val accept: ('a -> bool) -> 'a p -> 'a p
    val return: 'a -> 'a p
    val bind: 'a p -> ('a -> 'b p) -> 'b p
    val (@>>=): 'a p -> ('a -> 'b p) -> 'b p
    val (@>>): 'a p -> 'b p -> 'b p

    val seq: 'a p -> 'b p -> 'b p
    val read: t p
    val eof: unit p
    val eofv: 'a p -> 'a p

    val (<|>): 'a p -> 'a p -> 'a p
    val (<?>): 'a p -> string -> 'a p
    val fold: ('a -> 'b -> 'a) -> 'a -> 'b p -> 'a p
    val map: ('a -> 'b) -> 'a p -> 'b p

    val ignore: 'a p -> unit p
    val many: 'a p -> 'a list p
    val many1: 'a p -> 'a list p
    val many_until_before: 'a p -> 'b p -> 'a list p
    val many_until: 'a p -> 'b p -> 'a list p
    val skip_until_before: 'a p -> 'b p -> unit p
    val skip_until: 'a p -> 'b p -> unit p
    val skip: 'b p -> 'a p -> 'a p
    val combine: 'a p -> 'b p -> ('a * 'b) p
    val (@>>&): 'a p -> 'b p -> ('a * 'b) p

    val sep_by: 's p -> 'r p -> 'r list p
    val between: 'l p -> 'r p -> 'a p -> 'a p
    val run: 'r p -> u -> s -> ('r, string * h) result
end

module CharParser: sig
    include module type of Make(struct
        type token = char
        type user = unit
    end)

    val any: char p
    val any_of: string -> char p
    val none_of:  string -> char p
    val digit: char p
    val integer: int p
    val symbol: t -> t p
    val str: string -> unit p
    val eol: t p
end
