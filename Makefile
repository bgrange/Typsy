#FILES = TypedSyntax.ml Printing.mli Printing.ml Util.mli Eval.mli Eval.ml  Testing.ml Main.ml

main:
	ocamlbuild -use-menhir Main.d.byte

parser:
	ocamllex Lexer.mll; menhir --infer Parser.mly

clean:
	ocamlbuild -clean && rm -f Lexer.ml Parser.ml Parser.mli
