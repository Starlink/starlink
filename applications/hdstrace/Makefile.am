## Process this file with automake to produce Makefile.in

SUBDIRS = tra

bin_MONOLITHS = hdstrace
hdstrace_SOURCES = hdstrace.f

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

# no variable hdstrace_TASKS -- hdstrace is an ATASK
hdstrace_LDADD = $(LDADD) tra/libtra.la `prm_link_adam` `hds_link_adam` `fio_link_adam` `par_link_adam`

TESTS = hdtest
check_SCRIPTS = hdtest
