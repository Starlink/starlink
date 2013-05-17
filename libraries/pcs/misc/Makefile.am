## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = misc_link_adam

lib_LTLIBRARIES = libmisc_adam.la

libmisc_adam_la_SOURCES = \
	$(C_ROUTINES) \
	$(F_ROUTINES)

libmisc_adam_la_LIBADD = @CURSES_LIB@

C_ROUTINES = icl_reada.c putenv.c

F_ROUTINES = icl_writea.f

