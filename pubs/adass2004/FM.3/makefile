DOC = FM-3

%.dvi: %.tex
	latex $<
	if grep -iq 'label.*may have changed' ${<:.tex=.log}; then latex $<; fi

%.ps: %.dvi
	dvips -o $@ $<

all: $(DOC).dvi

clean:
	rm -f $(DOC).dvi $(DOC).aux $(DOC).log $(DOC).ps

