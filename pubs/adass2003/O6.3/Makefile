%.dvi: %.tex
	latex $<
	if grep -q 'Reference.*undefined' ${<:.tex=.log}; then latex $<; fi

%.ps: %.dvi
	dvips -o $@ $<

%.pdf: %.ps
	ps2pdf $<

all: O6-3-slides.pdf O6-3.ps

tidy:
	rm -f *.dvi *.log *.toc *.aux *.out

clean: tidy
	rm -f O6-3*.ps O6-3*.pdf
