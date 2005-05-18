## Process this file with automake to produce Makefile.in

stardocs_DATA = @STAR_XML_DOCUMENTATION@ $(EPSFILES)

METAPOST=@METAPOST@
# pstoimg is a bit creaky, but it should be available, since it's installed
# as part of star2html
PSTOIMG=@PSTOIMG@

# Though the following rule uses a configured pstoimg, it will fail if
# epstopdf doesn't happen to be in the path
SUFFIXES = .eps .pdf .gif
EPSFILES = ssn78-1.eps ssn78-2.eps ssn78-3.eps ssn78-4.eps ssn78-5.eps
PDFFILES = ssn78-1.pdf ssn78-2.pdf ssn78-3.pdf ssn78-4.pdf ssn78-5.pdf
PNGFILES = ssn78-1.png ssn78-2.png ssn78-3.png ssn78-4.png ssn78-5.png
GIFFILES = ssn78-1.gif ssn78-2.gif ssn78-3.gif ssn78-4.gif ssn78-5.gif
$(EPSFILES): ssn78.mp
	@METAPOST@ ssn78.mp
	for f in `ls ssn78.* | sed -n 's/.*\.\([0-9]*\)$$/\1/p'`; do \
	    mv ssn78.$$f ssn78-$$f.eps; \
	done
.eps.pdf:
	epstopdf --outfile=$@ $<
.eps.gif:
	L2HMODULE=`dirname $(PSTOIMG)` \
	    $(PSTOIMG) -gif -scale 1.5 -antialias -out $@ $<

ssn78.dvi: $(EPSFILES)
ssn78.pdf: $(PDFFILES)

MOSTLYCLEANFILES = $(EPSFILES) $(PDFFILES) $(GIFFILES)
