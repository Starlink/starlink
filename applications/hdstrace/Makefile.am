## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@

bin_MONOLITHS = hdstrace
hdstrace_SOURCES = hdstrace.f

hdstrace_TASKS = 
hdstrace_LDADD = $(LDADD) tra/libtra.a `prm_link_adam` `hds_link_adam` `fio_link_adam` `par_link_adam`

TESTS = hdtest
check_SCRIPTS = hdtest
