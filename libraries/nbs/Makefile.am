## Process this file with automake to produce Makefile.in

bin_SCRIPTS = nbs_link nbs_link_adam

bin_PROGRAMS = nbtrace

nbtrace_SOURCES = nbtrace.c
nbtrace_LDADD = `$(srcdir)/nbs_link`

lib_LTLIBRARIES = libnbs.la

libnbs_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(PRIVATE_INCLUDES) \
	$(C_ROUTINES)

include_HEADERS = $(PUBLIC_INCLUDES)

PUBLIC_INCLUDES = \
	NBS_PAR NBS_ERR \
	nbs_err.h nbs_par.h

PRIVATE_INCLUDES = \
	nbs_mac.h  nbs_module.h  nbs_typ.h

C_ROUTINES = \
	nbs.c  nbs_gbl.c  nbslow.c  

BUILT_SOURCES = NBS_ERR nbs_err.h


# Testing

TESTS = exercise time words
check_PROGRAMS = exercise time words
exercise_SOURCES = exercise.c
time_SOURCES = time.c timer.c
words_SOURCES = words.c

# Because we invoke nbs_link
exercise_LDADD = `$(srcdir)/nbs_link`
time_LDADD = `$(srcdir)/nbs_link`
words_LDADD = `$(srcdir)/nbs_link`
