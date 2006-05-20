## Process this file with automake to produce Makefile.in

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@
starexamples_DATA = $(distdir)-examples.tar

sc6.htx_tar: sc6.htx_tar.extras sc6_airmass.ps sc6_ap1.ps sc6_ap2.ps \
sc6_gaia1.ps sc6_gaia2.ps sc6_gaia3.ps sc6_gaia4.ps sc6_intensity.ps \
sc6_irwindows.ps sc6_sensitivity.ps sc6_ubv.ps sc6_xcatview1.ps \
sc6_xcatview2.ps

$(distdir)-examples.tar: examples
	DN=$(distdir)-exampless; \
	    mkdir $$DN; \
	    for f in examples/*; do if test -f $$f; then cp $$f $$DN; fi; done;\
	    tar cf $@ $$DN; \
	    rm -Rf $$DN
