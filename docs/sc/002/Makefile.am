## Process this file with automake to produce Makefile.in

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@
starexamples_DATA = $(distdir)-examples.tar

sc2.htx_tar: sc2.htx_tar.extras sc2_autoslice.ps sc2_histogram.ps \
sc2_import.ps sc2_isosurface.ps sc2_jcmtslice.ps sc2_jcmtsurface.ps \
sc2_open.ps sc2_particle.ps sc2_rosatslice.ps sc2_simple.ps \
sc2_slice.ps sc2_vpe.ps

$(distdir)-examples.tar: examples
	DN=$(distdir)-exampless; \
	    mkdir $$DN; \
	    for f in examples/*; do if test -f $$f; then cp $$f $$DN; fi; done;\
	    tar cf $@ $$DN; \
	    rm -Rf $$DN
