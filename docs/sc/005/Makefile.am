## Process this file with automake to produce Makefile.in

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ \
sc5_biasstrips.pdf sc5_ccdlup.pdf sc5_gaia.pdf sc5_gaiareduce.pdf \
sc5_histpeak.pdf sc5_m51_bias.pdf sc5_m51_flat.pdf sc5_m51_mosaic.pdf \
sc5_m51_raw.pdf sc5_organisendfs.pdf sc5_organisendftypes.pdf \
sc5_performreduction.pdf sc5_red.pdf sc5_reg.pdf sc5_sensitivity.pdf \
sc5_setdetector.pdf sc5_xreduce.pdf

sc5.htx_tar: sc5.htx_tar.extras

sc5.htx_tar.extras: examples/scripts
	for f in examples/scripts/*.csh; do lf=`echo $$f | sed -e 's+examples/scripts/++' -e 's/csh/lis/'`; \
	cp $$f ./$$lf; done; \
	ls *.lis > $@

SUBDIRS = examples examples/data examples/data/bias examples/data/flats \
          examples/data/targets examples/scripts

CLEANFILES = sc5.htx_tar sc5.ps sc5.pdf sc5.toc sc5.lot sc5.lof
