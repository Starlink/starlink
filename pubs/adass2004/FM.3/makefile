DOC = FM-3

%.dvi: %.tex
	latex $<
	if grep -q 'Reference.*undefined' ${<:.tex=.log}; then latex $<; fi

%.ps: %.dvi
	dvips -o $@ $<

all: $(DOC).dvi

clean:
	rm -f $(DOC).dvi $(DOC).aux $(DOC).log $(DOC).ps

