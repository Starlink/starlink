#-----------------------------------------------------------------------------
# GNU makefile for building WCSLIB.
#
# Summary of the main targets
# ---------------------------
#    all:       Build the library
#    clean:     Delete intermediate object files.
#    realclean: clean, and also delete executables and the object library.
#    test:      Compile and run the test programs.
#    show_all:  Print the values of all variables used.
#
# Notes:
#    1) The makefile sets certain variables to default values and may then
#       change some of them depending on the value of the FLAVOUR variable.
#       The following FLAVOURs are available:
#
#          SUN/GNU    GNU  C compiler running under some version of SunOS.
#          SUN/ANSI   ANSI C compiler running under some version of SunOS.
#          SUN/K&R    K&R  C compiler running under some version of SunOS.
#          DEC/Alpha  DEC/Alpha running Digital Unix (OSF/1).
#          DEC/Ultrix DEC/Ultrix.
#          Convex     Convex/ConvexOS.
#          SGI/32     SGI/IRIX using the 32-bit C and FORTRAN compilers.
#          SGI/64     SGI/IRIX using the 64-bit C and FORTRAN compilers.
#          SGI/P64    SGI/IRIX using the 64-bit Power C and FORTRAN compilers.
#
#       The FLAVOUR may conveniently be set as an environment variable or on
#       the GNU make command line, e.g.
#
#          gmake FLAVOUR='SUN/K&R'
#
#       If your requirements don't match any predefined FLAVOUR then you might
#       be able to modify an existing one, add a new one, or simply modify the
#       defaults.
#
#    2) The native Convex C compiler does not have the IEEE copysign()
#       function, and some versions of Ultrix have a version which returns
#       garbage.  In such cases a preprocessor macro replacement for
#       copysign() is available by defining the COPYSIGN preprocessor macro.
#
#       Likewise, the SGI compiler doesn't have the IEEE signbit() function.
#
# $Id: Makefile,v 2.5 1996/09/10 06:30:24 mcalabre Exp $
#-----------------------------------------------------------------------------
# C compiler and options.
  CC := cc
  CFLAGS := -O

# Reset this to ":" if ranlib is not needed.
  RANLIB := ranlib

# Linker options.
  LDFLAGS := -s

# Extra required libraries.
  LIBS := -lm

# If your mathematics library has cosd(), sind(), tand(), acosd(), asind(),
# atand(), and atan2d() and you want to use them then declare the required
# header file here.
  TRIGD :=

# The FC, FFLAGS, and PGPLOTLIB options are only required for compiling test
# programs (tproj2, tcel, and twcs2) which use PGPLOT to plot test grids.
# You can circumvent this by setting PGPLOTLIB to blank.  PGPLOT is a FORTRAN
# plotting library available from astro.caltech.edu.

# FORTRAN compiler (used only for linking test programs to PGPLOT).
  FC     := f77
  FFLAGS := -O

# PGPLOT link list; unset this to defeat compilation of test programs which
# plot test grids.
  PGPLOTLIB = -lpgplot -lX11

# Extra files to clean up.
  override EXTRA_CLEAN :=

# Overrides for various combinations of
# architecture, operating system and compiler.
#---------------------------------------------

ifeq "$(FLAVOUR)" "SUN/GNU"
   CC := gcc

   # Although written in C the plotting programs are linked to PGPLOT using
   # the FORTRAN compiler since it passes the correct FORTRAN libraries to
   # 'ld'.  However, the GNU C library must then be added explicitly to the
   # link list to resolve certain symbols specific to gcc.  The following
   # definition is very specific to OS/compiler version numbers.
   PGPLOTLIB += -L/usr/local/gnu/lib/gcc-lib/sparc-sun-solaris2.5/2.7.2 -lgcc
endif

ifeq "$(FLAVOUR)" "SUN/ANSI"
   CC      := cc
   LIBS    := -lsunmath -lm
   TRIGD   := sunmath.h
   FFLAGS  += -u
endif

ifeq "$(FLAVOUR)" "SUN/K&R"
   CC      := /usr/ucb/cc
   CFLAGS  += -DSIGNBIT
endif

ifeq "$(FLAVOUR)" "DEC/Alpha"
   CFLAGS  += -DSIGNBIT -D"atan2d=atand2"
   TRIGD   := math.h
   FFLAGS  += -nofor_main
endif

ifeq "$(FLAVOUR)" "DEC/Ultrix"
   CFLAGS  += -DCOPYSIGN -DSIGNBIT
endif

ifeq "$(FLAVOUR)" "Convex"
   CFLAGS  := -O3 -DCOPYSIGN -DSIGNBIT
   FFLAGS  := -O3
endif

ifeq "$(FLAVOUR)" "SGI/32"
   CFLAGS  += -32 -DSIGNBIT
   RANLIB  := @ :
   FFLAGS  += -32
   LDFLAGS += -32
   PGPLOTLIB := -L/usr/local/lib -lpgplot -lgl_s -lX11
endif

ifeq "$(FLAVOUR)" "SGI/64"
   CFLAGS  += -64 -DSIGNBIT
   RANLIB  := @ :
   FFLAGS  += -64
   PGPLOTLIB := -L/usr/local/lib64 -lpgplot -lX11
endif

ifeq "$(FLAVOUR)" "SGI/P64"
   CFLAGS  += -64 -pca keep -DSIGNBIT
   RANLIB  := @ :
   FFLAGS  += -64 -pfa keep
   LIBS    += -lmp
   PGPLOTLIB := -L/usr/local/lib64 -lpgplot -lX11
   override EXTRA_CLEAN := *.L *.M *.P *.anl *.m
endif

#-----------------------------------------------------------------------------
# You shouldn't need to change anything below here.

ifeq "$(TRIGD)" ""
   WCSTRIG := wcstrig.o
endif

LIBRARY := libwcs_c.a
MODULES := $(WCSTRIG) \
           lin.o  \
           proj.o \
           sph.o  \
           cel.o  \
           wcs.o

ifneq "$(PGPLOTLIB)" ""
   TPROJ2 := tproj2
   TCEL   := tcel
   TWCS2  := twcs2
endif

%.o : %.c
	-@ echo ""
	$(CC) $(CFLAGS) -c $<

%.o : %.f
	-@ echo ""
	$(FC) $(FFLAGS) -c $<

.PHONY : all clean lib realclean test

all : show_all lib

lib : $(LIBRARY)

$(LIBRARY) : $(MODULES:%=$(LIBRARY)(%))
	$(RANLIB) $(LIBRARY)

clean :
	- $(RM) *.o *.i a.out core $(EXTRA_CLEAN)

realclean : clean
	- $(RM) tlin tproj1 tproj2 tsph tcel twcs1 twcs2 $(LIBRARY)

test : show_all tlin tproj1 $(TPROJ2) tsph $(TCEL) twcs1 $(TWCS2)
	-@ echo ""
	-@ echo "Running WCSLIB test programs:"
	-@ echo ""
	-@ tlin
	-@ echo ""
	-@ tproj1
        ifdef TPROJ2
	   -@ echo ""
	   -@ tproj2 < /dev/null
        endif
	-@ echo ""
	-@ tsph
        ifdef TCEL
	   -@ echo ""
	   -@ tcel < /dev/null
        endif
	-@ echo ""
	-@ twcs1
        ifdef TWCS2
	   -@ echo ""
	   -@ twcs2 < /dev/null
        endif

tlin tproj1 tsph twcs1 : % : %.c $(LIBRARY)
	-@ echo ""
	   $(CC) $(CFLAGS) $(LDFLAGS) -o $@ $< $(LIBRARY) $(LIBS)
	   $(RM) $@.o

tproj2 tcel twcs2 : % : %.o tpgc.o tpgf.o $(LIBRARY)
	-@ echo ""
	   $(FC) $(FFLAGS) $(LDFLAGS) -o $@ $< tpgc.o tpgf.o $(LIBRARY) $(LIBS) $(PGPLOTLIB)

show_all :
	-@ echo ""
	-@ echo "FLAVOUR   := $(FLAVOUR)"
	-@ echo "CC        := $(CC)"
	-@ echo "CFLAGS    := $(CFLAGS)"
	-@ echo "RANLIB    := $(RANLIB)"
	-@ echo "LDFLAGS   := $(LDFLAGS)"
	-@ echo "LIBS      := $(LIBS)"
	-@ echo "TRIGD     := $(TRIGD)"
	-@ echo "WCSTRIG   := $(WCSTRIG)"
	-@ echo "FC        := $(FC)"
	-@ echo "FFLAGS    := $(FFLAGS)"
	-@ echo "PGPLOTLIB := $(PGPLOTLIB)"
	-@ echo "EXTRA_CLEAN := $(EXTRA_CLEAN)"

wcstrig.o : wcstrig.h
lin.o  : lin.h
proj.o : proj.h
sph.o  : wcstrig.h
cel.o  : cel.h
wcs.o  : wcs.h

tlin   : lin.h
tproj1 : proj.h
tproj2 : proj.h
tsph   : wcstrig.h
tcel   : cel.h
twcs1  : wcs.h
twcs2  : wcs.h

proj.h : wcstrig.h
cel.h  : proj.h
wcs.h  : cel.h lin.h
