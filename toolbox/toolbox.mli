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


module Pair: sig
    val make: 'a -> 'b -> ('a * 'b)
    val dup: 'a -> 'a * 'a
    val swap: ('a * 'b) -> ('b * 'a)

    val fork: ('a -> 'b) -> ('a -> 'c) -> 'a -> ('b * 'c)

    val first: ('a -> 'b) -> ('a * 'c) -> ('b * 'c)
    val second: ('b -> 'c) -> ('a * 'b) -> ('a * 'c)
    val both: ('a -> 'b) -> ('a * 'a) -> ('b * 'b)
    val bimap: ('a -> 'c) -> ('b -> 'd) -> ('a * 'b) -> ('c * 'd)

    val lift2: ('a -> 'b -> 'c) -> ('a * 'a) -> ('b * 'b) -> ('c * 'c)

    val curry: (('a * 'b) -> 'c) -> 'a -> 'b -> 'c
    val uncurry: ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
end


module Array: sig
    include module type of Array
    val foldi_left: (int -> 'a -> 'b -> 'a) -> 'a -> 'b array -> 'a
end


module File: sig
    val as_seq: string -> string Seq.t
    val as_list: string -> string list
end


module type MatrixType = sig
    type t
    val init: t
end


module Matrix: sig
    module Make (MT: MatrixType): sig
        type v = MT.t
        type 'a t = 'a array array
        type m = v t
        type pos = int * int

        val create: int -> int -> m
        val load: m -> v list list -> m
        val from_lists: v list list -> m
        val height: m -> int
        val width: m -> int
        val mapi: (pos -> 'a -> 'b) -> 'a t -> 'b t
        val map: ('a -> 'b) -> 'a t -> 'b t
        val foldi_left: (pos -> 'a -> 'b -> 'a) -> 'a -> 'b t -> 'a
        val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
        val iteri: (pos -> 'a -> unit) -> 'a t -> unit
        val remap: ('a -> 'a) -> 'a t -> 'a t
        val for_all: ('a -> bool) -> 'a t -> bool
        val valid_point: m -> pos -> bool
    end
end


module Num: sig
    val parse_int: string -> int option
    val parse_int_exn: string -> int
end


module List: sig
    include module type of List
    val reject: ('a -> bool) -> 'a list -> 'a list
    val equal: ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
    val map_pairs: ('a -> 'a -> 'b) -> 'a list -> 'b list
    val sum: int list -> int
    val flist: (('a -> 'b) list) -> 'a -> 'b list
    val take: int -> 'a list -> 'a list
    val product: 'a list -> 'b list -> ('a * 'b) list
end


module Operators: sig
    val (|<): ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
    val (>>): ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
    val (&--): int -> int -> int list
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


module Seq: sig
    include module type of Seq
    val repeat: 'a -> 'a Seq.t
    val zip_with: ('a -> 'b -> 'c) -> 'a Seq.t -> 'b Seq.t -> 'c Seq.t
    val zip: 'a Seq.t -> 'b Seq.t -> ('a * 'b) Seq.t
    val find_opt: ('a -> bool) -> 'a Seq.t -> 'a option
end


module String: sig
    include module type of String
    val to_chars: String.t -> char list
end
