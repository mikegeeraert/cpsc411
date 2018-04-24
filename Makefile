all:
	happy -gca ParMplus.y
	alex -g LexMplus.x
	ghc --make TestMplus.hs -o TestMplus
	ghc --make TestMPlus-IR.hs -o TestMplus-IR
	ghc --make TestMPlus-Stack.hs -o TestMplus-Stack

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocMplus.* LexMplus.* ParMplus.* LayoutMplus.* SkelMplus.* PrintMplus.* TestMplus.* AbsMplus.* TestMplus ErrM.* SharedString.* ComposOp.* mplus.dtd XMLMplus.* Makefile*
	
