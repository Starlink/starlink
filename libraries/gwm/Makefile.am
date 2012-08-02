## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = gwm_link gwm_link_adam

bin_PROGRAMS = xmake xdestroy gwmXrefresh

# Do not use starX_link. Do it ourselves directly via configure

xmake_SOURCES = xmake.c
xmake_LDADD = libgwm.la $(X_LIBS) $(X_PRE_LIBS) -lX11 `ems_link` `cnf_link`

xdestroy_SOURCES = xdestroy.c
xdestroy_LDADD = libgwm.la $(X_LIBS) $(X_PRE_LIBS) -lX11 -lXt `ems_link` `cnf_link`

gwmXrefresh_SOURCES = gwmXrefresh.c
gwmXrefresh_LDADD = libgwm.la $(X_LIBS) $(X_PRE_LIBS) -lX11 -lXt `ems_link` `cnf_link`

AM_CFLAGS = $(X_CFLAGS)

lib_LTLIBRARIES = libgwm.la

libgwm_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(C_ROUTINES) \
	$(PRIVATE_INCLUDES)

include_MESSAGES = gwm_err.h GWM_ERR
include_HEADERS = $(PUBLIC_INCLUDES)

BUILT_SOURCES = GWM_ERR gwm_err.h

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

C_ROUTINES =							  \
	findwindow.c getwininfo.c processevent.c error.c	  \
	makewindow.c createwindow.c destroywindow.c getpixmap.c	  \
	makeintowindow.c errormessage.c				  \
	setpixmap.c getscroll.c setscroll.c getcoltable.c	  \
	setcoltable.c setcolour.c getovmask.c setovscroll.c	  \
	getovscroll.c getfgcol.c getbgcol.c setbgcol.c setfgcol.c \
	close.c crwin.c dswin.c exist.c getci.c open.c wsetc.c	  \
	wseti.c wsetl.c xtrap.c gwm_globals.c

EXAMPLE_FILES = background.c clear.c colour.c foreground.c input.c ovclear.c \
ovcolour.c ovscroll.c scroll.c

PUBLIC_INCLUDES = gwm.h
## PUBLIC_INCLUDES = gwm.h gwm_err.h GWM_ERR

PRIVATE_INCLUDES = gwm_for.h  gwm_global.h  gwm_sys.h cshead.icon starlink.icon

# Testing. This requires that gwmXrefresh is in your path. It may well not
# be if you do not have "." in your path (until you install it)

TESTS = gwm_test
check_PROGRAMS = gwm_test

gwm_test_SOURCES = gwm_test.f
gwm_test_LDADD = libgwm.la $(X_LIBS) $(X_PRE_LIBS) -lX11 `ems_link` `cnf_link`

dist_starnews_DATA = gwm.news
dist_pkgdata_DATA = LICENCE $(EXAMPLE_FILES)

EXTRA_DIST = gwm_err.msg

CLEANFILES = GWM_ERR



