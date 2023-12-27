dev:
	dune build --watch

run:
	dune exec jinja_html_parser

ast:
	ocamlc -dparsetree ./lib/tester.ml
