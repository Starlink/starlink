## Process this file with automake to produce Makefile.in

# Build the pgbind and cpgdemo programs

noinst_PROGRAMS = pgbind
pgbind_SOURCES = $(PGBIND_ROUTINES)
PGBIND_ROUTINES = pgbind.c

# Use the pgbind program to generate the source files cpgplot.c and cpgplot.h

C_WRAPPERS = cpgplot.c
PUBLIC_INCLUDES = cpgplot.h

cpgplot.c: pgbind
	grep -h '^C%' ../src/pg*.f > ./pgbind_prototypes
	./pgbind bsd -h -w pgbind_prototypes

cpgplot.h: cpgplot.c

# Build the actual library

lib_LTLIBRARIES = libcpgplot.la
libcpgplot_la_SOURCES = $(C_WRAPPERS)
include_HEADERS = $(PUBLIC_INCLUIDES)

# Build the test

TESTS = cpgdemo
check_PROGRAMS = cpgdemo
cpgdemo_SOURCES = $(DEMO_ROUTINES)
DEMO_ROUTINES = cpgdemo.c
cpgdemo_LDADD = libcpgplot.la ../src/libpgplot_base.la

