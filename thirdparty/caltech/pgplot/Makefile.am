## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@

## In automake terminology, this is the `bin' prefix to the `SCRIPTS' primary.
bin_SCRIPTS = drivers/pgplot_link drivers/pgplot_link_adam rgb.txt

# Made up from sublibs
lib_LTLIBRARIES = libpgplot.la libpgplot_star.la

libpgplot_la_SOURCES = 
libpgplot_la_LIBADD = src/libpgplot_base.la sys/libpgplot_sys.la drivers/libpgplot_drivers.la

libpgplot_star_la_SOURCES = 
libpgplot_star_la_LIBADD = src/libpgplot_star_base.la sys/libpgplot_star_sys.la drivers/libpgplot_star_drivers.la

