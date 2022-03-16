# Problème du sac à dos

Le problème du sac à dos est un problème dont l'énoncé est très simple.
On a sac d'une capacité de B kg. On a également n objets pour lesquels on connaît
le poids p = (p_1, p_2, ..., p_n) et l'utilité u = (u_1, u_2, ..., u_n).

Le but est d'ajouter des objets dans le sac sans dépasser sa capacité, 
et en maximisant l'utilité globale du sac.

On se propose d'implémenter en OCaml l'algorithme **Branch and Bound** pour résoudre 
ce problème d'optimisation linéaire en variables entières.

# Projet universitaire

* Étudiant : Maxime Vincent
* Enseignante : Dominique Quadri
* UE Modèles Mathématiques
* Master M1 Informatique et Science des Données
* Université Paris-Saclay 
* Année 2021-2022

# Installation de l'environnement

J'utilise Linux Ubuntu 20.04 et la [procédure recommandée](https://ocaml.org/docs/install.html) 
pour installer OCaml.

```shell
sudo apt install opam
opam init
# Version récente de OCaml
opam switch create 4.13.1
eval $(opam env)
# Bibliothèque pour les fractions
opam install zarith
eval $(opam env)
ocaml --version
```

J'utilise la bibliothèque externe [Zarith](https://github.com/ocaml/Zarith) 
pour réaliser des calculs fractionnaires. En effet, si j'utilise des flottants
dans l'algorithme Fayard et Plateau, je risque de faire des erreur dues à l'imprécision
des flottants. La documentation est disponible [ici](https://antoinemine.github.io/Zarith/doc/latest/index.html).

Pour utiliser cette bibliothèque dans le toplevel il faut invoquer ces commandes.
```ocaml
#use "topfind";;
#require "zarith";;
```

# Compilation et exécution du projet

Le fichier exécutable **test** effectue des tests sur les fonctions utilisées par le solveur
comme heuristiques pour déterminer les bornes inférieures et supérieures.

Le fichier exécutable **example** montre des exemples d'utilisation du solveur.

```shell
git clone https://github.com/orthose/knapsack
cd knapsack
make clean
make all
./test
./example
``` 

# Exemple d'utilisation

Il est possible de tester dans le toplevel le solveur qui est compilé sous forme
d'une librairie OCaml (fichier .cma).

Le solveur attend 3 arguments.
* Le tableau d'entiers représentant l'utilité de chaque objet
* Le tableau d'entiers représentant le poids de chaque objet
* L'entier représentant la capacité totale du sac

Finalement il renvoie un tableau d'entiers 0 ou 1 qui représentent les variables
booléennes de décision. Si sol.(i) == 1 alors on prend l'objet i dans le sac.

```shell
ocaml knapsack.cma
let utility = [|8;18;20;11|];;
let weight = [|3;7;9;6|];;
let capacity = 17;;
let sol = Knapsack.solve utility weight capacity;;
Solution.print_int sol;;
```
