## Process this file with automake to produce Makefile.in

bin_PROGRAMS = icl icl_io

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@
stariclhelpdir = $(starhelpdir)/icl
stariclhelp_DATA = $(HELP_SOURCES:.hlp=.shl)

icl_SOURCES = parse.y lex.l $(F_ROUTINES) $(C_ROUTINES)
icl_LDADD = `ams_link_adam` `hds_link_adam` `hlp_link_adam` `cnf_link_adam` `nbs_link_adam` `cnfrtl_link_adam` @FCLIBS@

#  Make sure we use the C compiler for linking. Using Fortran causes
#  problems with multiple mains (Tru64 UNIX). Either of the next three 
#  lines should do this.
#icl_LINK = $(CCLD) -o $@
#icl_LINK = $(LINK) -o $@
FCLD = $(CC)

# May not be portable. We need the y.tab.h file to be created
# By default YACC does not create one. Note that we expect parse.h
# to be created from parse.y
AM_YFLAGS = -d

AM_LFLAGS = @icl_AM_LFLAGS@

# Curses library is found using configure.ac
icl_io_SOURCES = icl_io.c
icl_io_LDADD = `ams_link_adam`

include_MESSAGES = UFACE_ERR

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately).

F_ROUTINES = uface_pwhlp.f

#  C routines required for building the package.  This is just a list of
#  all the C source files.

C_ROUTINES = main.c\
             node.c \
             interp.c \
             symtab.c \
             functions.c \
             procs.c \
             value.c \
             utils.c \
             fileio.c \
             expr.c \
             control.c \
             input.c \
             unix.c \
             adam.c \
             uface.c \
             carith.c \
             output.c \
	     hds.c

#  List of private include files.  These are additional include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

ICL_INCLUDES =     icl.h\
                   utils.h \
                   value.h \
                   symtab.h \
                   node.h \
                   control.h \
                   expr.h \
                   interp.h \
                   carith.h \
                   input.h \
                   procs.h

PRIVATE_INCLUDES = $(ICL_INCLUDES) \
                   iclhds.h \
                   output.h \
                   icl_io.h \
                   adam.h \
                   adam_functions.h \
                   uface_functions.h \
                   UFACE_CMN

BUILT_SOURCES = $(include_MESSAGES)
HELP_SOURCES = iclhelp.hlp

#  List yacc source
YACC_SOURCE = parse.y

#  List lex source
LEX_SOURCE = lex.l

#  List program source
PROGRAM_SOURCE = icl_io.c

EXTRA_DIST = $(PRIVATE_INCLUDES) $(stariclhelp_DATA)

dist_starnews_DATA = icl.news

dist_pkgdata_DATA = ICL_CONDITIONS

#  Rule for building the help library from the help source.
.hlp.shl:
	@HLIB@ $<
