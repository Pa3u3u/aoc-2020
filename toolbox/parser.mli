module type ParseType = sig
    type token
    type user
end

module Make(P: ParseType): sig
    type t = P.token
    type s = t Seq.t
    type u = P.user
    type pin = (u * s)
    type 'r pout = ('r * u * s) option
    type 'r p = pin -> 'r pout

    val (>>=): 'a option -> ('a -> 'b option) -> 'b option
    val (>=>): ('a -> 'b option) -> ('b -> 'c option) -> 'a -> 'c option

    val choose: 'a p list -> 'a p
    val choose2: 'a p -> 'a p -> 'a p
    val (<|>): 'a p -> 'a p -> 'a p
    val accept: ('a -> bool) -> 'a p -> 'a p
    val return: 'a -> 'a p
    val read: t p

    val fold: ('a -> 'b -> 'a) -> 'a -> 'b p -> 'a p
    val map: ('a -> 'b) -> 'a p -> 'b p
    val bind: 'a p -> ('a -> 'b p) -> 'b p
    val seq: 'a p -> 'b p -> 'b p

    val (@>>=): 'a p -> ('a -> 'b p) -> 'b p
    val (@>>): 'a p -> 'b p -> 'b p

    val many: 'a p -> 'a list p
    val many1: 'a p -> 'a list p
    val ignore: 'a p -> unit p

    val sep_by: 's p -> 'r p -> 'r list p
    val between: 'l p -> 'r p -> 'a p -> 'a p
    val run: 'r p -> u -> s -> 'r option
end

module CharParser: sig
    include module type of Make(struct
        type token = char
        type user = unit
    end)

    val any_of: string -> char p
    val digit: char p
    val integer: int p
    val symbol: t -> t p
end
