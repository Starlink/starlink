noinst_DATA = params.tex

bindatadir = $(bindir)
dist_bindata_DATA = $(DEFAULTS)

DEFAULTS = smurf_makemap.def smurf_extinction.def smurf_sc2clean.def \
           smurf_calcflat.def smurf_calcqu.def smurf_fit1d.def \
           smurf_fit1d_uval.def

# Generate LaTeX include file for the config parameters
params.tex: $(DEFAULTS)
	-rm -f params.tex
	./make_pardocs

