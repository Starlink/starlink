## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libscl.la
AM_FCFLAGS = -I../inc

libscl_la_SOURCES = $(F_ROUTINES) $(PRIVATE_INCLUDES)

F_ROUTINES = commands.f do_syscomm.f init_symbols.f scl_commsyms.f \
scl_continue.f scl_do.f scl_help.f scl_if.f \
scl_matchcomm.f scl_parse.f scl_symbols.f scl_wild.f


PRIVATE_INCLUDES = SCL_BUFFER.INC SCL_IFDO.INC SCL_DO.INC

