## Process this file with automake to produce Makefile.in

PUBLIC_C_INCLUDES = hdspar.h
cincludedir = $(includedir)/star
cinclude_HEADERS = $(PUBLIC_C_INCLUDES)

dist_bin_SCRIPTS = hdspar_link_adam

lib_LTLIBRARIES = libhdspar_adam.la

libhdspar_adam_la_SOURCES = \
	$(F_ROUTINES) \
	$(C_ROUTINES) \
	$(PUBLIC_C_INCLUDES)

F_ROUTINES = dat_assoc.f \
               dat_cancl.f \
               dat_creat.f \
               dat_def.f \
               dat_delet.f \
               dat_exist.f \
               dat_updat.f

C_ROUTINES = hdspar.c

