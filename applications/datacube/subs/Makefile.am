## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libdatacube_subs.la

libdatacube_subs_la_SOURCES = \
   $(F_ROUTINES) $(PUBLIC_INCLUDES) $(GENSRC:.gen=.f) 

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately). Keep this list in alphabetical
#  order so that the subroutine description html comes out in the right 
#  order.

F_ROUTINES = \
  copyaxis.f  cub1_ainit.f  getbound.f  putaxis.f 

GEN_ROUTINES = \
  cug1_cpya.gen   cug1_doast.gen  cug1_filla.gen

PUBLIC_INCLUDES = \
	CUB1_PAR
	
#  Define the known suffixes in this subsystem.  Automake will infer
#  this list from the suffix rules without us having to write it
#  explicitly, but it gets it in the wrong order, which can cause
#  some problems.
SUFFIXES = .o .c .gen .grd .glrdi .glrdic .gln .f .h

#  Suffix rules.
.gen.f:
	$(GENERIC) -t n $< 

.grd.f:
	cp $< $(<:.grd=.gen)
	$(GENERIC) -t rd $(<:.grd=.gen)
	$(RM) $(<:.grd=.gen)

.glrdic.f:
	cp $< $(<:.glrdic=.gen)
	$(GENERIC) -t lrdic $(<:.glrdic=.gen)
	$(RM) $(<:.glrdic=.gen)

.glrdi.f:
	cp $< $(<:.glrdi=.gen)
	$(GENERIC) -t lrdi $(<:.glrdi=.gen)
	$(RM) $(<:.glrdi=.gen)

.gln.f:
	cp $< $(<:.gln=.gen)
	$(GENERIC) -t ln $(<:.gln=.gen)
	$(RM) $(<:.gln=.gen)
