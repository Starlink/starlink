## Process this file with automake to produce Makefile.in

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ \
sc5_biasstrips.eps sc5_ccdlup.eps sc5_gaia.eps sc5_gaiareduce.eps \
sc5_histpeak.eps sc5_m51_bias.eps sc5_m51_flat.eps sc5_m51_mosaic.eps \
sc5_m51_raw.eps sc5_organisendfs.eps sc5_organisendftypes.eps \
sc5_performreduction.eps sc5_red.eps sc5_reg.eps sc5_sensitivity.eps \
sc5_setdetector.eps sc5_xreduce.eps

sc5.htx_tar: sc5.htx_tar.extras

sc5.htx_tar.extras: examples/scripts
	for f in examples/scripts/*.csh; do lf=`echo $$f | sed -e 's+examples/scripts/++' -e 's/csh/lis/'`; \
	cp $$f ./$$lf; done; \
	ls *.lis > $@

SUBDIRS = examples examples/data examples/data/bias examples/data/flats \
          examples/data/targets examples/scripts

CLEANFILES = sc5.htx_tar sc5.ps sc5.toc sc5.lot sc5.lof
