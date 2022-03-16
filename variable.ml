(* Type pour distinguer les variables décisionnelles fixées ou à déterminer *)
type t = Fixed of Q.t | Free of Q.t

let equal a b = match (a, b) with
  | (Fixed _, Free _) -> false
  | (Free _, Fixed _) -> false
  | (Fixed x, Fixed y) | (Free x, Free y) -> Q.(=) x y

(* Si l'élément est fractionnaire le résultat sera la division entière *)
let to_int = function
  | Fixed x | Free x -> Q.to_int x

let to_string = function
  | Fixed x -> "Fixed " ^ (Q.to_string x) 
  | Free x -> "Free " ^ (Q.to_string x)