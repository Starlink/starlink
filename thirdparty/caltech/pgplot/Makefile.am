## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@

## In automake terminology, this is the `bin' prefix to the `SCRIPTS' primary.
bin_SCRIPTS = pgplot_link

# Made up from sublibs
lib_LTLIBRARIES = libpgplot.la

libpgplot_la_SOURCES = 
libpgplot_la_LIBADD = src/libpgplot_base.a drivers/libpgplot_drivers.a

