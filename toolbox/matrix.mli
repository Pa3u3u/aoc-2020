module type MatrixType = sig
    type t
    val init: t
end

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
    val get_opt: m -> pos -> v option
end
