## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

noinst_LTLIBRARIES = libdatacube_subs.la

libdatacube_subs_la_SOURCES = \
   $(F_ROUTINES) $(GEN_ROUTINES:.gen=.f) 

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately). Keep this list in alphabetical
#  order so that the subroutine description html comes out in the right 
#  order.

F_ROUTINES = \
  cub1_ainit.f

GEN_ROUTINES = \
  cug1_cpya.gen   cug1_doast.gen  cug1_filla.gen

#  Define the known suffixes in this subsystem.  Automake will infer
#  this list from the suffix rules without us having to write it
#  explicitly, but it gets it in the wrong order, which can cause
#  some problems.
SUFFIXES = .o .c .gen .grd .glrdi .glrdic .gln .f .h

#  Suffix rules.
.gen.f:
	$(FGENERIC) -t n $< 

.grd.f:
	cp $< $(<:.grd=.gen)
	$(FGENERIC) -t rd $(<:.grd=.gen)
	$(RM) $(<:.grd=.gen)

.glrdic.f:
	cp $< $(<:.glrdic=.gen)
	$(FGENERIC) -t lrdic $(<:.glrdic=.gen)
	$(RM) $(<:.glrdic=.gen)

.glrdi.f:
	cp $< $(<:.glrdi=.gen)
	$(FGENERIC) -t lrdi $(<:.glrdi=.gen)
	$(RM) $(<:.glrdi=.gen)

.gln.f:
	cp $< $(<:.gln=.gen)
	$(FGENERIC) -t ln $(<:.gln=.gen)
	$(RM) $(<:.gln=.gen)
