all: example test knapsack.cma

example: variable.cmo solution.cmo knapsack.cmo example.cmo
				ocamlfind ocamlc -linkpkg -package zarith -o example variable.cmo solution.cmo knapsack.cmo example.cmo

test: variable.cmo solution.cmo knapsack.cmo test.cmo
				ocamlfind ocamlc -linkpkg -package zarith -o test variable.cmo solution.cmo knapsack.cmo test.cmo

example.cmo: example.ml
				ocamlfind ocamlc -linkpkg -package zarith -c example.ml
	
test.cmo: test.ml
				ocamlfind ocamlc -linkpkg -package zarith -c test.ml

knapsack.cma: variable.cmo solution.cmo knapsack.cmo
				ocamlfind ocamlc -linkpkg -package zarith -a -o knapsack.cma variable.cmo solution.cmo knapsack.cmo

knapsack.cmo: knapsack.ml
				ocamlfind ocamlc -linkpkg -package zarith -c knapsack.ml
				
solution.cmo: solution.ml
				ocamlfind ocamlc -linkpkg -package zarith -c solution.ml
				
variable.cmo: variable.ml
				ocamlfind ocamlc -linkpkg -package zarith -c variable.ml

clean:
				rm -f *.cmo *.cmi *.cma test example