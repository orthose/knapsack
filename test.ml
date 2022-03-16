open Variable

(* Données d'entrée *)
let utility = [|8;18;20;11|]
let weight = [|3;7;9;6|]
let capacity = 17

(* Test de Knapsack.glutton_heuristic *)

let s1 = Knapsack.glutton_heuristic utility weight capacity
let () = Solution.print_int s1
let () =
  assert (s1.(0) == 0);
  assert (s1.(1) == 1);
  assert (s1.(2) == 1);
  assert (s1.(3) == 0)
  
(* Cas limites *)
let s2 = Knapsack.glutton_heuristic utility [|18;18;18;18|] capacity
let () = Solution.print_int s2
let () =
  assert (s2.(0) == 0);
  assert (s2.(1) == 0);
  assert (s2.(2) == 0);
  assert (s2.(3) == 0)

(* Test de Knapsack.fayard_heuristic *)

(* Cas limites *)
let s3 = Array.make 4 (Free Q.zero)
let () = assert (Knapsack.fayard_heuristic utility [|18;18;18;18|] capacity s3)
let () = Solution.print_var s3
let () =
  assert (Variable.equal s3.(0) (Free Q.zero));
  assert (Variable.equal s3.(1) (Free Q.zero));
  assert (Variable.equal s3.(2) (Free (Q.make (Z.of_int 17) (Z.of_int 18))));
  assert (Variable.equal s3.(3) (Free Q.zero))

(* On consomme exactement toute la capacité *)
let s4 = Array.make 4 (Free Q.zero)
let () = assert (Knapsack.fayard_heuristic utility [|3;7;7;6|] capacity s4)
let () = Solution.print_var s4
let () =
  assert (Variable.equal s4.(0) (Free Q.one));
  assert (Variable.equal s4.(1) (Free Q.one));
  assert (Variable.equal s4.(2) (Free Q.one));
  assert (Variable.equal s4.(3) (Free Q.zero))

(* Déroulement à la main de l'algorithme Branch and Bound *)
(* Initialisation de la solution *)
let sol = Array.make 4 (Free Q.zero)
let () = Solution.print_var sol 
let () =
  assert (Variable.equal sol.(0) (Free Q.zero));
  assert (Variable.equal sol.(1) (Free Q.zero));
  assert (Variable.equal sol.(2) (Free Q.zero));
  assert (Variable.equal sol.(3) (Free Q.zero))

(* Toutes les variables sont libres *)
let () = assert (Knapsack.fayard_heuristic utility weight capacity sol)
let () = Solution.print_var sol
let () =
  assert (Variable.equal sol.(0) (Free Q.one));
  assert (Variable.equal sol.(1) (Free Q.one));
  assert (Variable.equal sol.(2) (Free (Q.make (Z.of_int 7) (Z.of_int 9))));
  assert (Variable.equal sol.(3) (Free Q.zero))

(* On fixe x2 <- 0 *)
let () = Solution.reset sol
let () = sol.(2) <- Fixed Q.zero
let () = assert (Knapsack.fayard_heuristic utility weight capacity sol)
let () = Solution.print_var sol
let () =
  assert (Variable.equal sol.(0) (Free Q.one));
  assert (Variable.equal sol.(1) (Free Q.one));
  assert (Variable.equal sol.(2) (Fixed Q.zero));
  assert (Variable.equal sol.(3) (Free Q.one))

(* On fixe x2 <- 1 *)
let () = Solution.reset sol
let () = sol.(2) <- Fixed Q.one
let () = assert (Knapsack.fayard_heuristic utility weight capacity sol)
let () = Solution.print_var sol
let () =
  assert (Variable.equal sol.(0) (Free Q.one));
  assert (Variable.equal sol.(1) (Free (Q.make (Z.of_int 5) (Z.of_int 7))));
  assert (Variable.equal sol.(2) (Fixed Q.one));
  assert (Variable.equal sol.(3) (Free Q.zero))

(* On fixe x2 <- 1; x1 <- 0 *)
let () = Solution.reset sol
let () = sol.(1) <- Fixed Q.zero
let () = assert (Knapsack.fayard_heuristic utility weight capacity sol)
let () = Solution.print_var sol
let () =
  assert (Variable.equal sol.(0) (Free Q.one));
  assert (Variable.equal sol.(1) (Fixed Q.zero));
  assert (Variable.equal sol.(2) (Fixed Q.one));
  assert (Variable.equal sol.(3) (Free (Q.make (Z.of_int 5) (Z.of_int 6))))

(* On fixe x2 <- 1; x1 <- 1 *)
let () = Solution.reset sol
let () = sol.(1) <- Fixed Q.one
let () = assert (Knapsack.fayard_heuristic utility weight capacity sol)
let () = Solution.print_var sol
let () =
  assert (Variable.equal sol.(0) (Free (Q.make (Z.of_int 1) (Z.of_int 3))));
  assert (Variable.equal sol.(1) (Fixed Q.one));
  assert (Variable.equal sol.(2) (Fixed Q.one));
  assert (Variable.equal sol.(3) (Free Q.zero))
  
(* Solution invalide *)
(* On fixe x2 <- 1; x1 <- 1; x0 <- 1 *)
let () = Solution.reset sol
let () = sol.(0) <- Fixed Q.one
let () = assert (not (Knapsack.fayard_heuristic utility weight capacity sol))
let () = Solution.print_var sol

(* On fixe x2 <- 1; x1 <- 1; x0 <- 0 *)
let () = Solution.reset sol
let () = sol.(0) <- Fixed Q.zero
let () = assert (Knapsack.fayard_heuristic utility weight capacity sol)
let () = Solution.print_var sol
let () =
  assert (Variable.equal sol.(0) (Fixed Q.zero));
  assert (Variable.equal sol.(1) (Fixed Q.one));
  assert (Variable.equal sol.(2) (Fixed Q.one));
  assert (Variable.equal sol.(3) (Free (Q.make (Z.of_int 1) (Z.of_int 6))))

(* On fixe x2 <- 1; x1 <- 1; x0 <- 0; x3 <- 0 *)
let () = Solution.reset sol
let () = sol.(3) <- Fixed Q.zero
let () = assert (Knapsack.fayard_heuristic utility weight capacity sol)
let () = Solution.print_var sol
let () =
  assert (Variable.equal sol.(0) (Fixed Q.zero));
  assert (Variable.equal sol.(1) (Fixed Q.one));
  assert (Variable.equal sol.(2) (Fixed Q.one));
  assert (Variable.equal sol.(3) (Fixed Q.zero))
  
(* Solution invalide *)
(* On fixe x2 <- 1; x1 <- 1; x0 <- 0; x3 <- 1 *)
let () = Solution.reset sol
let () = sol.(3) <- Fixed Q.one
let () = assert (not (Knapsack.fayard_heuristic utility weight capacity sol))
let () = Solution.print_var sol
