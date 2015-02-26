TESTFILES = TypedSyntax.ml Printing.mli Printing.ml Util.mli Eval.mli Eval.ml  Testing.ml Main.ml

testing: $(EVALTESTFILES)
	ocamlbuild Main.d.byte

clean:
	ocamlbuild -clean
