## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libtra.la

libtra_la_SOURCES = $(F_ROUTINES)  $(G_ROUTINES:.gen=.f)


F_ROUTINES = tra1_arstr.f tra1_puts.f tra1_trcon.f tra_locin.f \
tra_putc.f tra_putl.f tra1_cminf.f tra1_thier.f tra_arelm.f    \
tra_primi.f tra_putdm.f tra_putvl.f

G_ROUTINES = tra_putx.gen

#  Run generic to get numeric types.
GFLAGS = n
.gen.f:
	$(FGENERIC)  -t $(GFLAGS) $<

EXTRA_DIST = tra_putx.gen
