(lang dune 2.0)

(name euler)

(generate_opam_files)

(source (github gmevel/euler-lib))
(license WTFPL)
(authors "Glen Mével <glen.mevel@crans.org>")
(maintainers "Glen Mével <glen.mevel@crans.org>")
(documentation "https://gmevel.github.io/euler-lib/index.html")

(package
  (name euler)
  (synopsis "An arithmetic library for OCaml's native integers ")
  (description

"\| Euler is a library for doing arithmetic with OCaml’s native integers (31 or 63 bits). It provides:
"\|
"\| * overflow-detecting arithmetic, that can be susbstituted for Stdlib arithmetic;
"\| * advanced arithmetic functions;
"\| * solvers for some forms of integer equations;
"\| * modular arithmetic, with a nice functorial interface.
)
  (depends
    (ocaml (>= 4.07))
    (stdcompat (>= 18))
    (containers (>= 3.0))
  )
)
