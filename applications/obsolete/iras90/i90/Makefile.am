
noinst_LTLIBRARIES = libi90.la

libi90_la_SOURCES = $(F_ROUTINES)
libi90_la_DEPENDENCIES = $(INCLUDE_FILES)

F_ROUTINES = i90_init.f

INCLUDE_FILES = I90_DAT I90_PAR I90_COM

TESTS = i90_test
check_PROGRAMS = i90_test
i90_test_SOURCES = i90_test.f
i90_test_LDADD = libi90.la `err_link`

