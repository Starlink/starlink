## Process this file with automake to produce Makefile.in

## In automake terminology, this is the `bin' prefix to the `SCRIPTS' primary.
bin_SCRIPTS = pgplot_link pgplot_link_adam
dist_bin_SCRIPTS = rgb.txt

# Made up from sublibs
lib_LTLIBRARIES = libpgplot.la libpgplot_star.la

# We are building libpgplot.la from convenience libraries.  However, all
# the libraries contain Fortran source, and so must be assembled into
# a library using the F77 or FC languages, rather than the C linker
# (this matters on OSX, since the OSX linker has a big downer on
# common blocks, and needs the Fortran voodoo to make it happy).  In
# order to make this happen, we list a dummy Fortran file as a
# source.  This is enough to tell automake to use Fortran in the link.
libpgplot_la_SOURCES = dummy_fortran.f
libpgplot_la_LIBADD = src/libpgplot_base.la sys/libpgplot_sys.la drivers/libpgplot_drivers.la

libpgplot_star_la_SOURCES = dummy_fortran.f
libpgplot_star_la_LIBADD = src/libpgplot_star_base.la sys/libpgplot_star_sys.la drivers/libpgplot_star_drivers.la

