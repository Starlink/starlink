## Process this file with automake to produce Makefile.in

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

sc5.htx_tar: sc5.htx_tar.extras

sc5.htx_tar.extras: examples/scripts
	for f in examples/scripts/*.csh; do lf=`echo $$f | sed -e 's+examples/scripts/++' -e 's/csh/lis/'`; \
	cp $$f ./$$lf; done; \
	ls *.lis > $@

SUBDIRS = examples examples/data examples/data/bias examples/data/flats \
          examples/data/targets examples/scripts
