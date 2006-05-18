## Process this file with automake to produce Makefile.in

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@
starexamples_DATA = $(distdir)-examples.tar

$(distdir)-examples.tar: examples
	DN=$(distdir)-exampless; \
	    mkdir $$DN; \
	    for f in examples/*; do if test -f $$f; then cp $$f $$DN; fi; done;\
	    tar cf $@ $$DN; \
	    rm -Rf $$DN
