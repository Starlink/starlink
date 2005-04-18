## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = lex_link_adam

lib_LTLIBRARIES = liblex_adam.la

liblex_adam_la_SOURCES = \
	$(include_HEADERS) \
	$(include_MESSAGES) \
	$(F_ROUTINES)

include_HEADERS = LEX_PAR
include_MESSAGES = LEX_ERR

F_ROUTINES = \
	lex_cmdline.f \
	lex_cmdset.f \
	lex_init.f \
	lex_parse.f \
	lex_set.f

BUILT_SOURCES = $(include_MESSAGES)

EXTRA_DIST = lex_err.msg
