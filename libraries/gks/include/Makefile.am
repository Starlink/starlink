##

## The makefile simply has to process the .F include files
## See prm for example
## These files should not be installed

FPP_INC_IN = gkmc.par.F
FPP_INC_OUT = $(FPP_INC_IN:.F=)

noinst_INCLUDES = $(FPP_INC_OUT)

include_HEADERS = $(PUBLIC_INCLUDES)

#  List of public include files.  These are include files which form
#  part of the package and may be required by users of it

PUBLIC_INCLUDES = GKSE_PAR GKS_PAR


# dependencies
#gkmc.par.F:	gkmc.par


# These files should be run through the C preprocessor,
# so we have to explain how to do that.
# The following deals with case-insensitive filesystems, on which
# foo.f and foo.F would be the same file.  FPP_OUTPUT is
# either "" (in which case the preprocessor writes to foo.f, and
# the filesystem is presumably case-sensitive) or ">$@".
SUFFIXES = .F
.F:
	rm -f $@
	$(FPP) -I.. $(FPPFLAGS) $(DEFAULT_INCLUDES) $(CPPFLAGS) $(DEFS) $< $(FPP_OUTPUT)
	test -f $@ || mv $(<:.F=.f) $@

# Need to generate the fpp include files
BUILT_SOURCES = $(FPP_INC_OUT)
