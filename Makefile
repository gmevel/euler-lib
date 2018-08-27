default: all

all: lib doc test

lib:
	dune build

doc:
	dune build @doc
	@# We replace odoc theme with ocamldoc theme (hackish!):
	@cp ocamldoc-style.css _build/default/_doc/_html/odoc.css

test:
	@echo -e '\e[1;41mTODO\e[0m use a test tool!'
	@false

clean:
	dune clean

# example Makefile for dune:
#     https://github.com/c-cube/ocaml-containers/raw/master/Makefile
