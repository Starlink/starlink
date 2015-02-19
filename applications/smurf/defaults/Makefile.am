noinst_DATA = params.tex
dist_bin_SCRIPTS = $(DEFAULTS)

DEFAULTS = smurf_makemap.def smurf_extinction.def smurf_sc2clean.def \
           smurf_calcflat.def smurf_calcqu.def smurf_fit1d.def \
           smurf_fit1d_uval.def

# Generate LaTeX include file for the config parameters
edit=sed \
	-e 's,@PROLAT\@,$(PROLAT),g'
params.tex: $(DEFAULTS)
	rm -f make_pardocs
	$(edit) make_pardocs.in > make_pardocs
	chmod +x make_pardocs
	rm -f params.tex
	./make_pardocs

