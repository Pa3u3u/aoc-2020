let (|<) g f x = g (f x)
let (>>) f g x = g (f x)

let (&--) a z = if z - a < 0 then [] else List.init (z - a) ((+) a)
