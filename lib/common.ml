open Core

let id x = x

let compose f g x = f (g x)

let (<.>) = compose

let const x = fun _ -> x

let flip f a b = f b a

let (<||>) p q x = (p x) || (q x)

let (<&&>) p q x = (p x) && (q x)

let neg p x = not (p x)
