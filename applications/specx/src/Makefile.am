## Process this file with automake to produce Makefile.in

bin_PROGRAMS = specx specx_help

AM_FCFLAGS = -I../inc

specx_SOURCES = run_specx.c
specx_help_SOURCES = helpm.c

specx_help_LDADD = `shl_link` @FCLIBS@

noinst_LTLIBRARIES = libspecx.la
libspecx_la_SOURCES = scl_main.f TITLE

FIG_LIBS = \
-lfigfit -lfigtio -lfigich -lfiggen

specx_LDADD = \
libspecx.la \
../command/libcommand.la \
../exprt/libexprt.la \
../external/libexternal.la \
../fitting/libfitting.la \
../fv4/libfv4.la \
../reduce/libreduce.la \
../scl/libscl.la \
../term/libterm.la \
../graph/libgraph.la \
../mapdis4/libmapdis4.la \
../tek/libtek.la \
../mv4/libmv4.la \
../plots/libplots.la \
../util/libutil.la \
../vms/libvms.la \
../astro/libastro.la \
../gen/libgen.la \
../getaline/libgetaline.la \
$(FIG_LIBS) \
`$(srcdir)/specx_link` @FCLIBS@


