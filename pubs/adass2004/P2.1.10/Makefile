poster.pdf: poster.tex script-big.pdf script-little.pdf
	pdflatex poster

.SUFFIXES: .pdf .eps

.eps.pdf:
	epstopdf $<

# Alternative colours: ".3 setgray" is quite dark, for the small one,
# and ".9 setgray" is quite light for the large one.
script-little.eps: script.eps.in
	sed 's/@MODULE@/72/;s/@BB@/0 0 1100 520/;s/@COLOUR@/0.8 0.0 0.2 setrgbcolor/' $< >$@
script-big.eps: script.eps.in
	sed 's/@MODULE@/200/;s/@BB@/0 0 3200 1400/;s/@COLOUR@/0.8 0.8 1 setrgbcolor/' $< >$@

clean:
	rm -f *.pdf *.eps
