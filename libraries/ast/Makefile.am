## Process this file with automake to produce Makefile.in

# First declare various groups of files.  These were initially extracted
# from the grp.make file, as constructed by the SDT newdev command
GRP_C_ROUTINES = \
    axis.c \
    c2f77.c \
    channel.c \
    cmpframe.c \
    cmpmap.c \
    dssmap.c \
    error.c \
    fchannel.c \
    fcmpframe.c \
    fcmpmap.c \
    fdssmap.c \
    ferror.c \
    ffitschan.c \
    fframe.c \
    fframeset.c \
    fgrismmap.c \
    fpolymap.c \
    fintramap.c \
    fitschan.c \
    flutmap.c \
    fmapping.c \
    fmathmap.c \
    fmatrixmap.c \
    fobject.c \
    fpcdmap.c \
    fpermmap.c \
    fplot.c \
    frame.c \
    frameset.c \
    fskyframe.c \
    fslamap.c \
    fspecframe.c \
    fspecmap.c \
    fsphmap.c \
    ftranmap.c \
    funitmap.c \
    fwcsmap.c \
    fshiftmap.c \
    fwinmap.c \
    fxmlchan.c \
    fzoommap.c \
    grismmap.c \
    polymap.c \
    intramap.c \
    loader.c \
    lutmap.c \
    mapping.c \
    mathmap.c \
    matrixmap.c \
    memory.c \
    object.c \
    pcdmap.c \
    permmap.c \
    plot.c \
    pointset.c \
    skyaxis.c \
    skyframe.c \
    slamap.c \
    specframe.c \
    specmap.c \
    sphmap.c \
    tranmap.c \
    unit.c \
    unitmap.c \
    wcsmap.c \
    shiftmap.c \
    winmap.c \
    xml.c \
    xmlchan.c \
    zoommap.c

# Header files which contribute to the "ast.h" file, organised to correspond
# with the class hierarchy.
AST_H_FILES = \
    xml.h \
    wcstrig.h \
    proj.h \
    memory.h \
    error.h \
    unit.h \
    ast_err.h \
    version.h \
    object.h \
       pointset.h \
       axis.h \
          skyaxis.h \
       mapping.h \
          cmpmap.h \
          dssmap.h \
          grismmap.h \
          intramap.h \
          lutmap.h \
          mathmap.h \
          matrixmap.h \
          pcdmap.h \
          permmap.h \
          polymap.h \
          shiftmap.h \
          slamap.h \
          specmap.h \
          sphmap.h \
          tranmap.h \
          unitmap.h \
          wcsmap.h \
          winmap.h \
          zoommap.h \
          frame.h \
             cmpframe.h \
             frameset.h \
                plot.h \
             skyframe.h \
             specframe.h \
       channel.h \
          fitschan.h \
          xmlchan.h 

# All the (C) include files required to build the library.
GRP_C_INCLUDE_FILES = \
    $(AST_H_FILES) \
    c2f77.h \
    ems.h \
    err.h \
    f77.h \
    grf.h \
    loader.h \
    skyaxis.h \
    slalib.h \
    wcsmath.h \
    wcstrig.h \
    xmlchan.h

# The following list should include AST_PAR, but that must not be
# distributed, and so it is listed separately in
# nodist_libast_la_SOURCES below.
GRP_F_INCLUDE_FILES = \
    GRF_PAR \
    AST_ERR

LATEX_DOCUMENTATION_FILES = \
    sun210.tex \
    sun211.tex

DOCUMENTATION_PRODUCTS = $(PAPER_DOCUMENTATION) $(HYPER_DOCUMENTATION)
PAPER_DOCUMENTATION = sun210.tex sun211.tex sun210.ps sun211.ps
HYPER_DOCUMENTATION = sun210.htx_tar sun211.htx_tar

POSTSCRIPT_FIGURES = \
    cmpframe.eps \
    complex.eps \
    frames.eps \
    frameset.eps \
    fronta.eps \
    fronta_bw.eps \
    frontb.eps \
    frontb_bw.eps \
    frontc.eps \
    frontc_bw.eps \
    fsalign.eps \
    fsconvert.eps \
    fsexample.eps \
    fsmerge.eps \
    fsremap.eps \
    gridplot.eps \
    gridplot_bw.eps \
    mapping.eps \
    overgrid.eps \
    overgrid_bw.eps \
    parallel.eps \
    series.eps \
    simpexamp.eps

WCSLIB_FILES = \
    COPYING.LIB \
    proj.c \
    tpn.c \
    proj.h \
    wcstrig.c \
    wcsmath.h \
    wcstrig.h

## Some of the following groups are probably redundant, yes?
## From here...
#UNIX_LINK_FILES = \
#    ast_link \
#    ast_link_adam
#
#UNIX_STARTUP_FILES = \
#    ast_dev
#
#VERSION_FILES = \
#    makefile \
#    sun_master.tex
#
#MESSAGE_SYSTEM_FILES = \
#    ast_err.msg
#
#UTILITY_PROGS = \
#    astbad.c
#
#TEST_PROGS = \
#    ast_test.c
#
#DEVELOPMENT_TOOLS = \
#    makeh
## ... to here

bin_SCRIPTS = ast_link
dist_bin_SCRIPTS = ast_link_adam
noinst_SCRIPTS = ast_cpp
dist_noinst_SCRIPTS = makeh
# Scripts are not distributed by default (since they might be derived objects)
# Add these to the distribution below.  In fact, it would be useful
# and straightforward  to make ast_link{,_adam} derived, since they
# could then have installation directories painlessly edited in to
# them.  This might be a requirement for scripts which supported
# linking against shared libraries.

# Headers required by library users.  Both of the following lines
# indicate headers which are installed.
include_HEADERS = AST_ERR GRF_PAR
# Following are generated, so should not be distributed.
nodist_include_HEADERS = ast.h AST_PAR

lib_LTLIBRARIES = \
    libast.la \
    libast_err.la \
    libast_ems.la \
    libast_grf_2.0.la \
    libast_grf_3.2.la \
    libast_pgplot.la \
    libast_slalib.la \
    libast_wcslib.la


libast_la_SOURCES = \
    $(GRP_C_ROUTINES) \
    $(GRP_C_INCLUDE_FILES) \
    $(GRP_F_INCLUDE_FILES) \
    ast_err.h
# AST_PAR is really part of GRP_F_INCLUDE_FILES, but it must not be
# distributed, so list it separately.
nodist_libast_la_SOURCES = \
    ast.h \
    AST_PAR

#  The default error reporting module.
libast_err_la_SOURCES = err_null.c

#  The error reporting module that uses EMS to deliver errors.
libast_ems_la_SOURCES = err_ems.c

#  The module containing null implementations of the graphics routines 
#  required by AST V2.0
libast_grf_2_0_la_SOURCES = grf_2.0.c 

#  The module containing null implementations of the graphics routines 
#  added by AST V3.2 and not present in V2.0
libast_grf_3_2_la_SOURCES = grf_3.2.c 

#  The graphics module that uses PGPLOT for graphical output.
libast_pgplot_la_SOURCES = grf_pgplot.c

#  The interface between C code and the FORTRAN version of SLALIB.
libast_slalib_la_SOURCES = sla.c

#  Modules adapted from the wcslib library.
libast_wcslib_la_SOURCES = proj.c wcstrig.c tpn.c

# The following files are built by the targets in this makefile.
MAINTAINERCLEANFILES = error.h version.h builddocs addversion \
	ast.h $(DOCUMENTATION_PRODUCTS)

# Special cases start here

# The AST_PAR include file is produced by compiling the astbad.c
# program and editing its output into the ast_par.source file (while
# also changing the "E" exponent character to a "D").  The astbad.c
# and ast_par.source must be distributed (the generation of the
# AST__BAD value must be done on the build host) but not installed.
noinst_PROGRAMS = astbad
astbad_SOURCES = astbad.c pointset.h
AST_PAR: ast_par.source astbad
	sed -e 's/<AST__BAD>/'`./astbad | tr 'E' 'D'`'/' ast_par.source >$@

# ast_link is generated from ast_link.in; ast_link_adam does not
# need configuration, and so is not mentioned in AC_CONFIG_FILES within
# configure.ac, and so is not distributed automatically.
#
# makeh is required in order to build ast.h after distribution (see below).
EXTRA_DIST = ast_par.source

# ast.h depends on the error-facility files.  ast.h _could_ be made
# before distribution, but since it's generated from other distributed
# files, it's more robust to distribute makeh and make ast.h on the
# build host.
ast.h: $(AST_H_FILES) ast_err.h makeh
	@PERL@ ./makeh -s $(srcdir) $(AST_H_FILES) >$@

# AST_ERR and ast_err.h are `generated source files', and so must be
# generated before any other compilations are done.  Note that these
# files are generated on the distribution host, and so this
# declaration has no effect post-distribution.
#
# AST_PAR is also a generated source file, but it should _not_ be
# included in the list of BUILT_SOURCES, otherwise `make' tries to make
# it before it makes the `astbad' program it depends on.  Instead,
# just rely on the dependencies expressed in the main body above to
# have AST_PAR built before it is needed.
#
# error.h doesn't _have_ to be built this early, but almost all of the 
# modules depend on it, so it's more robust to list it here rather than
# in each of the _SOURCES declarations above.
BUILT_SOURCES = AST_ERR ast_err.h error.h

object.c: version.h

#  Form a second link to the main object library. This is used when a second
#  pass through the library is needed during linking.
install-exec-local: libast.la
	cd $(DESTDIR)$(libdir) && \
		rm -f libast_pass2.a && \
		$(LN_S) $(DESTDIR)$(libdir)/libast.a libast_pass2.a
	if test -n "$$MANIFEST_FD"; then \
	    echo "$(DESTDIR)$(libdir)/libast_pass2.a" >&$${MANIFEST_FD-0}; \
	fi

# Make pre-distribution files.  These are files which are required for
# building the distribution, but are not themselves distributed.
# The source files here should be mentioned in STAR_PREDIST_SOURCES in
# configure.ac
@PREDIST@predist_subs = sed \
@PREDIST@    -e 's,@star_facilitycode\@,$(star_facilitycode),' \
@PREDIST@    -e 's,@PACKAGE_VERSION\@,$(PACKAGE_VERSION),' \
@PREDIST@    -e 's,@PACKAGE_VERSION_MAJOR\@,$(PACKAGE_VERSION_MAJOR),' \
@PREDIST@    -e 's,@PACKAGE_VERSION_MINOR\@,$(PACKAGE_VERSION_MINOR),' \
@PREDIST@    -e 's,@PACKAGE_VERSION_RELEASE\@,$(PACKAGE_VERSION_RELEASE),' \
@PREDIST@    -e 's,@PERL\@,$(PERL),'
@PREDIST@error.h: error.h.in
@PREDIST@	rm -f $@; $(predist_subs) $< >$@
@PREDIST@version.h: version.h.in
@PREDIST@	rm -f $@; $(predist_subs) $< >$@
@PREDIST@builddocs: builddocs.in
@PREDIST@	rm -f $@; $(predist_subs) $< >$@; chmod +x $@
@PREDIST@addversion: addversion.in
@PREDIST@	rm -f $@; $(predist_subs) $< >$@; chmod +x $@

# Documentation
@PREDIST@$(PAPER_DOCUMENTATION): sun211_figures builddocs addversion
@PREDIST@	./builddocs

# The contents of the sun211_figures directory is identical to that
# sun210_figures
sun211_figures: sun210_figures
	$(LN_S) sun210_figures sun211_figures

# Installation check

TESTS = ast_test
check_PROGRAMS = ast_test
ast_test_SOURCES = ast_test.c
ast_test_LDADD = `ast_link`
