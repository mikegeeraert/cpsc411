all:
	happy -gca ParMplus.y
	alex -g LexMplus.x
	ghc --make TestMplus.hs -o TestMplus

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocMplus.* LexMplus.* ParMplus.* LayoutMplus.* SkelMplus.* PrintMplus.* TestMplus.* AbsMplus.* TestMplus ErrM.* SharedString.* ComposOp.* mplus.dtd XMLMplus.* Makefile*
	
