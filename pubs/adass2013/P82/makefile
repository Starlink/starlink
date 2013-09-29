all: P82.pdf

# Add figure requirements here:
P82.dvi: P82.tex

%.dvi: %.tex %.bib
	latex $<
	bibtex $(<:.tex=)
	latex $<
	latex $<

%.ps: %.dvi
	dvips -Ppdf $<

%.pdf: %.ps
	ps2pdf14 $<
