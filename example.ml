let () = Printf.printf "EXAMPLE 1\n"

(* Données d'entrée *)
let u1 = [|8;18;20;11|]
let w1 = [|3;7;9;6|]
let c1 = 17

let s1 = Knapsack.solve u1 w1 c1
let () = Printf.printf "solution = "; Solution.print_int s1
let () =
  assert (s1.(0) == 0);
  assert (s1.(1) == 1);
  assert (s1.(2) == 1);
  assert (s1.(3) == 0)
  

let () = Printf.printf "EXAMPLE 2\n"

(* Données d'entrée *)
let u2 = [|22;33;47;12;1;5;9;8;10;11|]
let w2 = [|4;2;46;7;12;32;9;51;4;27|]
let c2 = 50

let s2 =  Knapsack.solve u2 w2 c2
let () = Printf.printf "solution = "; Solution.print_int s2
