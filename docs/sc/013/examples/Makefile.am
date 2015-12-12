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

timewaster_SOURCES = p1.f p2.f p3.f
timewaster_FCFLAGS = $(PROFILINGOPTION) $(AM_FCFLAGS)
timewaster_LDFLAGS = $(PROFILINGOPTION)
p1_FCFLAGS = $(PROFILING) $(AM_FCFLAGS)
p2_FCFLAGS = $(PROFILING) $(AM_FCFLAGS)
p3_FCFLAGS = $(PROFILING) $(AM_FCFLAGS)

mixed_SOURCES = mixed-f.f mixed-c.c
mixed_f_FCFLAGS = $(NOUNDERSCORES) $(AM_FCFLAGS)

bytesex.h: islittleendian
	echo '#define BIGENDIAN' `if ./islittleendian; then echo 0; else echo 1;fi` >$@

BUILT_SOURCES = bytesex.h
