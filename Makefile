default: all

all: lib doc test

lib:
	dune build
	@# We do not expose internal modules (hackish!!):
	@rm _build/install/default/lib/PE/PE__*
	@sed '/_build\/install\/default\/lib\/PE\/PE__.*/d' -i _build/default/PE.install
	@sed '/_build\/install\/default\/lib\/PE\/PE.cm[xt]/d' -i _build/default/PE.install
	@cp _build/default/PE.install PE.install

doc:
	dune build @doc
	@# We replace odoc theme with ocamldoc theme (hackish!):
	@cp ocamldoc-style.css _build/default/_doc/_html/odoc.css

test:
	@echo -e '\e[1;41mTODO\e[0m use a test tool!'
	@false

clean:
	dune clean

install: lib
	dune install

# example Makefile for dune:
#     https://github.com/c-cube/ocaml-containers/raw/master/Makefile
