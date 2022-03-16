open Variable

(* Tableau de solution des variables décisionnelles sous forme fractionnaire *)
type t = Variable.t array

let string_of_array to_str a =
  "[|" ^ String.concat ";" (List.map 
    to_str (List.of_seq (Array.to_seq a))) ^ "|]"

let print_array to_str a =
  Printf.printf "%s\n" (string_of_array to_str a)

let print_int = print_array string_of_int

let print_var = print_array Variable.to_string
    
let to_int = Array.map Variable.to_int

let to_string = string_of_array Variable.to_string

(* Remise à zéro des variables de décision libres *)
let reset sol =
  Array.iteri (fun i _ ->
    match sol.(i) with
    | Fixed _ -> ()
    | Free _ -> sol.(i) <- Free Q.zero  
    ) sol

