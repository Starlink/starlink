poster.pdf: poster.tex script-big.pdf script-little.pdf
	pdflatex poster
poster.ps: poster.tex script-big.eps script-little.eps
	latex poster
	dvips -o $@ poster.dvi

.SUFFIXES: .pdf .eps

.eps.pdf:
	epstopdf $<

# Alternative colours: ".3 setgray" is quite dark, for the small one,
# and ".9 setgray" is quite light for the large one.
script-little.eps: script.eps.in Makefile
	x=60; bb=`echo $$x | awk '{printf "0 0 %d %d", $$1*12, $$1*8}'`; \
	  sed "s/@MODULE@/$$x/;s/@BB@/$$bb/;s/@COLOUR@/0.8 0.0 0.2 setrgbcolor/" $< >$@
script-big.eps: script.eps.in Makefile
	x=200; bb=`echo $$x | awk '{printf "0 0 %d %d", $$1*12, $$1*8}'`; \
	  sed "s/@MODULE@/$$x/;s/@BB@/$$bb/;s/@COLOUR@/0.9 0.9 1 setrgbcolor/" $< >$@

clean:
	rm -f *.pdf *.eps
