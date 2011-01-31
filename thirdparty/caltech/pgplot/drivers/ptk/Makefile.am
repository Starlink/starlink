## Process this file with automake to produce Makefile.

if HAVE_TK

  lib_LTLIBRARIES = libptkpgplot.la
  libptkpgplot_la_SOURCES = $(C_ROUTINES)

  include_HEADERS = $(PUBLIC_INCLUDES)

  C_ROUTINES = ptkpgplot.c

  PUBLIC_INCLUDES = ptkpgplot.h

  PRIVATE_INCLUDES = \
Lang.h \
Lang_f.h \
Xlib_f.h \
default.h \
ptkpgplot.h \
tcl.h \
tclDecls.h \
tclDecls_f.h \
tk.h \
tkConfig.h \
tkDecls_f.h \
tkEvent.h \
tkEvent_f.h \
tkInt.h \
tkIntDecls.h \
tkIntDecls_f.h \
tkInt_f.h \
tkPort.h \
tkUnixDefault.h \
tkUnixPort.h \
tkVMacro.h \
tk_f.h \
Lang.m \
Xlib.m \
tclDecls.m \
tk.m \
tkDecls.m \
tkEvent.m \
tkInt.m \
tkIntDecls.m \
Lang.t \
Xlib.t \
tclDecls.t \
tk.t \
tkDecls.t \
tkEvent.t \
tkInt.t \
tkIntDecls.t

  AM_CFLAGS = -DPG_PPU  -I../

  dist_noinst_HEADERS = $(PRIVATE_INCLUDES)

endif
