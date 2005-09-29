all: star-cvs.pdf

star-cvs.pdf: star-cvs.tex star-cvs.bbl
	TEXINPUTS=speclass: pdflatex star-cvs.tex

star-cvs.bbl: star-cvs.bib
	test -f star-cvs.aux || TEXINPUTS=speclass: pdflatex star-cvs.tex
	bibtex star-cvs
