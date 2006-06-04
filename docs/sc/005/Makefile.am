## Process this file with automake to produce Makefile.in

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

sc5.htx_tar: sc5.htx_tar.extras

$(distdir)-examples.tar: examples
	DN=$(distdir)-examples; \
	mkdir $$DN; \
	for f in examples/*; do if test -e $$f; then cp -r $$f $$DN; fi; done;\
        togo=`find $$DN -name CVS`; if test -n "$$togo"; then rm -Rf $$togo; fi;\
	tar cf $@ $$DN; \
	rm -Rf $$DN

SUBDIRS = examples examples/data examples/data/bias examples/data/flats \
          examples/data/targets examples/scripts
          