## Process this file with automake to produce Makefile.in

bin_SCRIPTS = lex_link_adam

lib_LTLIBRARIES = liblex_adam.la

liblex_adam_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(F_ROUTINES)

include_HEADERS = $(PUBLIC_INCLUDES)

PUBLIC_INCLUDES = LEX_ERR LEX_PAR

F_ROUTINES = \
	lex_cmdline.f \
	lex_cmdset.f \
	lex_init.f \
	lex_parse.f \
	lex_set.f

BUILT_SOURCES = LEX_ERR
