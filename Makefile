year:
	runhaskell Main.hs 10 -6 7 1.0 >test.tex ; pdflatex test.tex ; open test.pdf
