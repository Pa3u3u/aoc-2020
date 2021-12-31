module Fun: sig
    include module type of Fun

    val const: 'a -> 'b -> 'a
    val peek: ('a -> unit) -> 'a -> 'a
    val shift: ('a -> 'b) -> 'x -> 'a -> 'b
end

module Misc: sig
    val ifv: bool -> 'a -> 'a -> 'a
    val guard: ('a -> 'b) -> ('a -> 'b option) -> 'a -> 'b
end

module File: sig
    val as_seq: string -> string Seq.t
    val as_char_seq: string -> char Seq.t
    val as_list: string -> string list
end

module Num: sig
    val parse_int: string -> int option
    val parse_int_exn: string -> int
    val ipow: int -> int -> int
end

module Range: sig
    type 'a t = 'a * 'a * ('a -> bool)

    val sign: int -> int
    val make_step: int -> int -> int -> int t
    val make: int -> int -> int t
    val make_inc: int -> int -> int t
    val infinite: int -> int -> int t
    val as_seq: int t -> int Seq.t
    val as_list: int t -> int list
end
