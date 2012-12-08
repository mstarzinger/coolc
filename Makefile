# Solution to Stanford Compilers Course.
# (c) Copyright 2012 Michael Starzinger. All Rights Reserved.

all:

run-pa1:
	perl ../pa1/pa1-grading.pl -skip -x
	gsed -i 's/\/bin\/sed/\/opt\/local\/bin\/gsed/' ./grading/PA2-filter
	gsed -i 's/\/bin\/sed/\/opt\/local\/bin\/gsed/' ./grading/PA2-simplefilter
	perl ../pa1/pa1-grading.pl -skip -r

run-pa2: lexer
	perl ../pa2/pa2-grading.pl -x
	gsed -i 's/\/bin\/sed/\/opt\/local\/bin\/gsed/' ./grading/PA3-filter
	perl ../pa2/pa2-grading.pl -r

run-pa3: lexer parser
	perl ../pa3/pa3-grading.pl -skip -x
	gsed -i 's/\/bin\/sed/\/opt\/local\/bin\/gsed/' ./grading/PA4-msgfilter
	perl ../pa3/pa3-grading.pl -skip -r

run-pa4: lexer parser semant
	perl ../pa4/pa4-grading.pl -skip -x
	gsed -i 's/\/usr\/class\/cs143\/bin\/spim/..\/..\/spim\/spim.sh/' ./grading/143gradesingle
	gsed -i 's/\/bin\/sed/\/opt\/local\/bin\/gsed/' ./grading/PA5-filter
	perl ../pa4/pa4-grading.pl -skip -r

lexer: cool-lexer.hs lexer.hs
	ghc -o $@ $^

parser: cool-ast.hs cool-lexer.hs cool-parser.hs cool-printer.hs parser.hs
	ghc -o $@ $^

semant: cool-ast.hs cool-basic.hs cool-lexer.hs cool-parser.hs cool-printer.hs cool-semant.hs semant.hs
	ghc -o $@ $^

cgen: asm-base.hs asm-base-mips.hs asm-printer.hs cool-ast.hs cool-basic.hs cool-codegen.hs cool-lexer.hs cool-parser.hs cool-semant.hs generator.hs
	ghc -o $@ $^

cool-lexer.hs: cool.x
	alex -o $@ $<

cool-parser.hs: cool.y
	happy -o $@ $<

clean:
	rm -rf grading
	rm -f *.hi *.o
	rm -f lexer
	rm -f parser
	rm -f semant
	rm -f cgen
	rm -f cool-lexer.hs
	rm -f cool-parser.hs
