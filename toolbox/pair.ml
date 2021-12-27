open Operators
let make a b = (a, b)
let dup x = (x, x)
let swap (a, b) = (b, a)

let fork f g x = (f x, g x)

let first f (a, b) = (f a, b)
let second f (a, b) = (a, f b)
let both f = first f >> second f
let bimap f g = first f >> second g

let lift2 f (a, b) (c, d) = (f a c, f b d)

let curry f a b = f (a, b)
let uncurry f (a, b) = f a b
