(**************************************************************)
(* Algorithme Branch and Bound pour la résolution du problème *)
(* d'optimisation linéaire du sac à dos en variables entières *)
(**************************************************************)

(***************************************)
(* Étudiant : Maxime Vincent           *)
(* Enseignante : Dominique Quadri      *)
(* UE Modèles Mathématiques            *)
(* M1 ISD Université Paris-Saclay 2022 *)
(***************************************)

open Variable
open Solution

(* Ajout de fonctions aux tableaux non-implémentées dans la bibliothèque standard *)
module Array = struct
  include Array

  (**
   * Tri des indices d'un tableau
   * @param cmp: Fonction de comparaison entre 2 éléments renvoyant un entier
   * @param a: Tableau à trier 
   * @return: Tableau d'entiers représentant les indices
   **)
  let argsort cmp a =
    let index_a = Array.mapi (fun i x -> (i, x)) a in
    let () = Array.sort (fun (_, x) (_, y) -> cmp x y) index_a in
    Array.map (fun (i, _) -> i) index_a

  (**
   * Recherche d'un élément pour renvoyer l'indice correspondant 
   * @param find: Fonction de recherche renvoyant un booléen
   * @param a: Tableau dans lequel effectuer la recherche
   * @return: Some i avec l'indice i de l'élément si trouvé sinon None
   **)
  let find_opt_index find a =
    let rec find_opt_index i =
      if not (i < Array.length a) then None
      else if find a.(i) then Some i
      else find_opt_index (i + 1)
    in find_opt_index 0
    
  (**
   * Produit scalaire entre deux vecteurs
   * @param sum: Opération de somme entre 2 éléments
   * @param prod: Opération de produit entre 2 éléments
   * @param zero: Valeur initiale de l'accumulateur de la somme
   * @param u, v: Tableaux de même taille
   * @return: Valeur du produit scalaire càd un nombre
   **)
  let dot sum prod zero u v =
    Array.fold_left sum zero (Array.map2 prod u v)

end

(**
 * Fonction objectif calculant la somme des produits 
 * entre variables décisionnelles et tableau d'utilité des objets
 *
 * @param solution: Tableau des variables de décision sous forme entière ou fractionnaire
 * @param utility: Tableau de l'utilité de chaque objet sous forme entière ou fractionnaire
 * @return: entier ou fraction
 **)
let fun_objective_int = Array.dot (+) ( * ) 0
let fun_objective_frac = Array.dot Q.(+) Q.( * ) Q.zero

(**
 * Heuristique gloutonne pour trouver la borne inférieure
 * du problème du sac à dos
 *
 * @param utility: Tableau de l'utilité de chaque objet
 * @param weight: Tableau du poids de chaque objet
 * @param capacity: Entier représentant la capacité du sac
 * @return: Tableau des variables de décision sous forme entière
 * des objets à prendre dans le sac
 **)
let glutton_heuristic utility weight capacity : int array =
  let sol = Array.make (Array.length utility) 0 in
  (* Tri par ordre décroissant d'utilité des indices *)
  let index = Array.argsort (fun x y ->
    if x > y then -1 else if y < x then 1 else 0) utility in
  (* Parcours des indices *)
  let _ = Array.fold_left (fun capa i ->
    (* Tant qu'il reste de la capacité on ajoute l'objet dans le sac *)
    let rest_capa = capa - weight.(i) in
    if rest_capa >= 0 then
      let () = sol.(i) <- 1 in rest_capa
    (* Sinon on n'ajoute pas l'objet et on converse la capacité actuelle *)
    else capa
    ) capacity index in sol

(**
 * Heuristique Fayard et Plateau (1978) pour résoudre le problème du sac à dos
 * en relation continue et obtenir la borne supérieure
 *
 * @param utility: Tableau de l'utilité de chaque objet
 * @param weight: Tableau du poids de chaque objet
 * @param capacity: Entier représentant la capacité du sac
 * @param inout sol: Tableau des variables de décision sous forme fractionnaire
 * des objets à prendre dans le sac. En entrée seulement des 0 et 1.
 * En sortie des 0, 1 et éventuellement un élement fractionnaire.
 * @return: true si une solution a été trouvée false sinon
 **)
let fayard_heuristic utility weight capacity (sol:Solution.t) =
  let n = Array.length utility in
  (* Ratios eniers (utility / weight) pour éviter de perdre en précision *)
  let ratios = Array.map2 (fun x y ->
    Q.make (Z.of_int x) (Z.of_int y)) utility weight in
  (* Tri par ordre décroissant des ratios *)
  let index = Array.argsort (fun x y -> Q.compare y x) ratios in
  
  (* Parcours des ratios pour remplir le sac *)
  let rec fayard_heuristic capa i =
    if i < n then
      (* indice j pour désigner l'objet dans utility et weight *)
      let j = index.(i) in
      match sol.(j) with
      (* On prend en compte les variables fixées sans les mofifier *)
      | Fixed _ ->
        (* Appel récursif en gardant la taille du sac car déjà pris en compte *)
        fayard_heuristic capa (i + 1)
      (* Les variables libres peuvent être modifiées *)
      | Free x ->
        (* S'il reste de la capacité on ajoute l'objet dans le sac *)
        let rest_capa = capa - weight.(j) in
        if rest_capa >= 0 then
          let () = sol.(j) <- Free Q.one in
          (* Appel récursif en diminuant la taille du sac et en augmentant le poids embarqué *)
          fayard_heuristic rest_capa (i + 1)
        else
          (* On fractionne la dernière variable décisionnelle avec un ratio entier *)
          let sum_weight = capacity - capa in
          sol.(j) <- Free (Q.make (Z.of_int (capacity - sum_weight)) (Z.of_int weight.(j)))
  in 
  
  (* Initialisation de la capacité en fonction des variables fixées *)
  let init_capa = fst (Array.fold_left (fun (capa, i) x ->
    match x with
    | Fixed x -> (capa - (weight.(i) * Q.to_int x), i + 1)
    | Free _ -> (capa, i + 1)
    ) (capacity, 0) sol) 
  in
  
  (* Si la capacité est négative c'est que la solution initiale est fausse *)
  if init_capa < 0 then false
  (* La solution donnée sera toujours bonne si la capacité initiale n'est pas négative *)
  else let () = fayard_heuristic init_capa 0 in true

(**
 * Algorithme principal Branch and Bound appliqué au problème du sac à dos.
 * Le but est de prendre des objets dans un sac à dos de manière à maximiser
 * l'utilité du sac tout en respectant la contrainte de la capacité du sac.
 *
 * Si on a plusieurs possibilités de fonctions d'initialisation de lb ajouter
 * un argument optionnel
 *
 * @param utility: Tableau de l'utilité de chaque objet
 * @param weight: Tableau du poids de chaque objet
 * @param capacity: Entier représentant la capacité du sac
 * @return: Liste des indices des objets à prendre dans le sac
 **)
let solve utility weight capacity : int array =
  (* Vérification des invariants nécessaires au bon déroulement de l'algorithme *)
  let () = assert (Array.length utility == Array.length weight) in
  let () = assert (capacity >= 0) in

  (* Parcours de l'arbre des solutions *)
  (* On peut ajouter depth pour arrêter si on dépasse profondeur *)
  let rec solve lower_bound (sol:Solution.t) : bool =
    (* Calcul de la borne supérieure à partir de la solution donnée *)
    let upper_bound = fun_objective_frac 
      (Array.map (function | Fixed x | Free x -> x) sol) 
      (Array.map (fun x -> Q.of_int x) utility) in
      
    (* Affichages pour débogage *)
    Printf.printf "solution = %s\n" (Solution.to_string sol);
    Printf.printf "lower_bound = %d\n" lower_bound;
    Printf.printf "upper_bound = %s\n" (Q.to_string upper_bound);
      
    (* On utilise sol par référence donc faire attention car modifiée partout *)
    (* Condition d'arrêt si la borne inférieure est identique à la borne supérieure *)
    if Q.(=) (Q.of_int lower_bound) upper_bound then true
    (* Si la borne supérieure est inférieure à la borne supérieure on tue ce noeud *)
    else if Q.(>) (Q.of_int lower_bound) upper_bound then false
    else
      (* Création de 2 nouveaux noeuds par appel récursif *)
      let cont i =
        (* L'intégrité de sol repose sur l'évaluation paresseuse *)
        (reset sol; 
          sol.(i) <- Fixed Q.zero; 
          (fayard_heuristic utility weight capacity sol)
          && (solve lower_bound sol)
        ) || (reset sol;
          sol.(i) <- Fixed Q.one; 
          (fayard_heuristic utility weight capacity sol) 
          && (solve lower_bound sol)
        )
      in
      (* Indice éventuel de la variable décisionnelle fractionnaire *)
      match (Array.find_opt_index (function
        (* On ne peut pas avoir de fraction si la variable est fixée *)
        | Fixed x -> false 
        | Free x -> Q.(<>) Q.zero x && Q.(<>) Q.one x) sol) with
      (* On divise l'arbre en deux branches en mettant à 0 puis à 1 la variable
         décisionnelle fractionnaire *)
      | Some i -> cont i
      (* Il n'y a pas de variable décisionnelle fractionnaire donc la borne supérieure est entière *)
      | None -> match (Array.find_opt_index (function
        (* On choisit la première variable décisionnelle libre *)
        | Fixed x -> false | Free x -> true) sol) with
        (* On a trouvé une variable libre on crée 2 nouveaux noeuds *)
        | Some i -> cont i
        (* Si toutes les variables sont fixées alors on a trouvé la solution *)
        | None -> true
  in 
  
  (* 1. Initialisation de la borne inférieure par heuristique gloutonne *)
  let lower_bound = fun_objective_int
    (glutton_heuristic utility weight capacity) utility in
  (* 2. Calcul de la solution initiale de la borne supérieure par heuristique Fayard et Plateau *)
  let sol = (Array.make (Array.length utility) (Free Q.zero)) in
  let _ =
    (* 3. Détermination de la racine de l'arbre des solutions *)
    (* Si aucune solution de départ n'est trouvée on renvoie la solution initiale
       avec toutes les variables à 0 c'est normalement impossible *)
    fayard_heuristic utility weight capacity sol 
    (* 4. Parcours de l'arbre des solutions par récursion *)
    && solve lower_bound sol
  in Solution.to_int sol
