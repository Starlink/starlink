# Metapost command -- needs to be mp on some TeX installations
MPOST=mpost

%.dvi: %.tex
	latex $<
	if grep -q 'Reference.*undefined' ${<:.tex=.log}; then latex $<; fi

%.ps: %.dvi
	dvips -o $@ $<

%.pdf: %.ps
	ps2pdf $<

%.1: %.mp
	$(MPOST) $<

all: O6-3-slides.pdf O6-3.ps

O6-3.dvi: O6-3.tex O6-3_1.eps

#O6-3_1.eps: models.eps
O6-3_1.eps: uls-model.1
	ln -sf $< $@

tidy:
	rm -f *.dvi *.log *.toc *.aux *.out

clean: tidy
	rm -f O6-3*.ps O6-3*.eps O6-3*.pdf
