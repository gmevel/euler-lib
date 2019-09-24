# This option is intended to be used from opam.
DUNE_BUILD_OPTIONS ?=

default: all

all: lib doc test

lib:
	dune build $(DUNE_BUILD_OPTIONS)

doc:
	dune build @doc

test:
	@echo -e '\e[1;41mTODO\e[0m use a test tool!'
	@false

clean:
	dune clean

install: lib
	ocamlfind remove pe
	dune install

# example Makefile for dune:
#     https://github.com/c-cube/ocaml-containers/raw/master/Makefile
