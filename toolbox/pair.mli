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
