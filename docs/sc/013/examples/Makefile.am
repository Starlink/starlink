## Process this file with automake to produce Makefile.in

## NOTE NOTE NOTE: this isn't right, yet.  It's not specifying the
## FFLAGs correctly, and I don't know why!

bin_PROGRAMS = fpdemo fpp dfpp mixed crash timewaster
noinst_PROGRAMS = islittleendian

EXAMPLES=crash.c fpp.c fpdemo.c \
	io-f.f io-c.c \
	p1.f p2.f p3.f \
	islittleendian.c mixed-f.f mixed-c.c \
	maple.ms idl-functions.pro

# Not all of these are _extra_ distributed files, since some of the
# $(EXAMPLES) would be distributed anyway, since their program files
EXTRA_DIST = $(EXAMPLES) gausssine.dat

fpdemo_SOURCES = fpdemo.c bytesex.h
fpdemo_CPPFLAGS = $(ENABLENAN)

fpp_SOURCES = fpp.c bytesex.h
fpp_CPPFLAGS = $(ENABLENAN) -DDOUBLEPRECISION=0

dfpp_SOURCES = fpp.c bytesex.h
dfpp_CPPFLAGS = $(ENABLENAN) -DDOUBLEPRECISION=1

timewaster_SOURCES = timewaster.f p1.f p2.f p3.f
timewaster_FFLAGS = $(PROFILINGOPTION) $(AM_FFLAGS)
timewaster_LDFLAGS = $(PROFILINGOPTION)
p1_FFLAGS = $(PROFILING) $(AM_FFLAGS)
p2_FFLAGS = $(PROFILING) $(AM_FFLAGS)
p3_FFLAGS = $(PROFILING) $(AM_FFLAGS)

mixed_SOURCES = mixed-f.f mixed-c.c
mixed_f_FFLAGS = $(NOUNDERSCORES) $(AM_FFLAGS)

bytesex.h: islittleendian
	echo '#define BIGENDIAN' `if ./islittleendian; then echo 0; else echo 1;fi` >$@

BUILT_SOURCES = bytesex.h



# The following are maintenance targets only.
# Create SGML-ised versions of the example programs (with < and & escaped)
%.sgml: %
	sed -e 's/&/\&amp;/g' -e 's/</\&lt;/g' $< > $@

sgmlexamples: $(EXAMPLES:=.sgml)

