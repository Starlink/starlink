##

## The makefile simply has to process the .fpp include files
## See prm for example
## These files should not be installed

FPP_INC_IN = gkmc.par.fpp
FPP_INC_OUT = $(FPP_INC_IN:.fpp=)

noinst_INCLUDES = $(FPP_INC_OUT)

# dependencies
gkmc.par:	gkmc.par.fpp


# These files should be run through the C pre processor
# so we have to explain how to do that
SUFFIXES = .fpp
.fpp:
	rm -f $@
	$(FPP) $(FPPFLAGS) $(DEFAULT_INCLUDES) $(CPPFLAGS) $(DEFS) $< $(FPP_OUTPUT)

# Need to generate the fpp include files
BUILT_SOURCES = $(FPP_INC_OUT)
