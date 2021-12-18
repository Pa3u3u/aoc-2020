let (|<) g f x = g (f x)
let (>>) f g x = g (f x)

let (&--) a z = List.init (z - a) ((+) a)
