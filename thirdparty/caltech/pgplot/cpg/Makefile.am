## Process this file with automake to produce Makefile.in

# Build the pgbind and cpgdemo programs

noinst_PROGRAMS = pgbind
pgbind_SOURCES = $(PGBIND_ROUTINES)
PGBIND_ROUTINES = pgbind.c

# Use the pgbind program to generate the source files cpgplot.c and cpgplot.h
# we don't mention the .h file at all as a target because it should appear as
# well as the .c file when pgbind is run, if it didn't already exist.

C_WRAPPERS = cpgplot.c

cpgplot.c: pgbind
	grep -h '^C%' ../src/pg*.f > ./pgbind_prototypes
	./pgbind bsd -h -w pgbind_prototypes

# Build the actual library

lib_LTLIBRARIES = libcpgplot.la
libcpgplot_la_SOURCES = $(C_WRAPPERS)


# Build the test

TESTS = cpgdemo
check_PROGRAMS = cpgdemo
cpgdemo_SOURCES = $(DEMO_ROUTINES)
DEMO_ROUTINES = cpgdemo.c
cpgdemo_LDADD = libcpgplot.la

