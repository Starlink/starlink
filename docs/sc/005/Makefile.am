## Process this file with automake to produce Makefile.in

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@
starexamples_DATA = $(distdir)-examples.tar

sc5.htx_tar: sc5.htx_tar.extras

$(distdir)-examples.tar: examples
	DN=$(distdir)-examples; \
	mkdir $$DN; \
	for f in examples/*; do if test -e $$f; then cp -r $$f $$DN; fi; done;\
        togo=`find $$DN -name CVS`; if test -n "$$togo"; then rm -Rf $$togo; fi;\
	tar cf $@ $$DN; \
	rm -Rf $$DN

sc5.htx_tar.extras: examples/scripts
	for f in examples/scripts/*.csh; do lf=`echo $$f | sed -e 's+examples/scripts/++' -e 's/csh/lis/'`; \
	cp $$f ./$$lf; done; \
	ls *.lis > $@
