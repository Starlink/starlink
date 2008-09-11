## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libjpl.la

# Starlink link scripts
dist_bin_SCRIPTS = jpl_link jpl_link_adam

# JPL helper programs
bin_PROGRAMS = jplpleph jplstate jplconst jplstate shorteph

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

staretc_DATA = jpleph.dat

# Build tools
noinst_PROGRAMS = ephdsk

# Add comdat.o to work around gfortran linker problem on OS X.
ephdsk_SOURCES = ephdsk.f
ephdsk_LDADD = libjpl.la comdat.o

shorteph_SOURCES = shorteph.f
shorteph_LDADD = libjpl.la comdat.o

jplpleph_SOURCES = jplpleph.f
jplpleph_LDADD = libjpl.la comdat.o

jplstate_SOURCES = jplstate.f
jplstate_LDADD = libjpl.la comdat.o

jplconst_SOURCES = jplconst.f
jplconst_LDADD = libjpl.la comdat.o


libjpl_la_SOURCES = $(BLOCK_DATA_ROUTINES) $(F_ROUTINES) $(FPP_ROUTINES)

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately).
F_ROUTINES = const.f ctoj.f interp.f jtoc.f pleph.f \
             rci.f split.f state.f

FPP_ROUTINES = ephopn.F

#  List of Fortran BLOCK DATA routines
BLOCK_DATA_ROUTINES = comdat.f

#  Creating the ephemeris
jpleph.dat: ephdsk 
	-rm -f EPHTAP
	$(LN_S) ephtap.dat EPHTAP
	echo "A" >/tmp/ephdskinput
	echo " " >>/tmp/ephdskinput
	./ephdsk </tmp/ephdskinput
	rm /tmp/ephdskinput
	rm EPHTAP
	mv JPLEPH jpleph.dat



# Testing

TESTS = runtesteph
check_PROGRAMS = testeph
check_SCRIPTS = runtesteph

testeph_SOURCES = testeph.f
testeph_LDADD = libjpl.la

dist_starnews_DATA = jpl.news

EXTRA_DIST = ephtap.dat testephinput.dat

dist_pkgdata_DATA = read.me

CLEANFILES = jpleph.dat


