#  File to define groups for use by the SDT grp command.
#  ====================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Wed Jan  3 16:40:22 GMT 1996


#  Define new groups here...
C_ROUTINES = \
axis.c \
c2f77.c \
channel.c \
cmpframe.c \
cmpmap.c \
dssmap.c \
err_ems.c \
err_null.c \
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
fpolymap.c \
frame.c \
frameset.c \
fshiftmap.c \
fskyframe.c \
fslamap.c \
fspecframe.c \
fspecmap.c \
fsphmap.c \
ftranmap.c \
funitmap.c \
fwcsmap.c \
fwinmap.c \
fxmlchan.c \
fzoommap.c \
grf_2.0.c \
grf_3.2.c \
grf_null.c \
grf_pgplot.c \
grismmap.c \
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
polymap.c \
proj.c \
shiftmap.c \
skyaxis.c \
skyframe.c \
sla.c \
slamap.c \
specframe.c \
specmap.c \
sphmap.c \
tpn.c \
tranmap.c \
unit.c \
unitmap.c \
wcsmap.c \
wcstrig.c \
winmap.c \
xml.c \
xmlchan.c \
zoommap.c

F_ROUTINES = \
object.f \
frame.f

DERIVED_FILES = \
ast.h \
ast_err.h \
ast_err \
fac_1521_err

C_INCLUDE_FILES = \
axis.h \
c2f77.h \
channel.h \
cmpframe.h \
cmpmap.h \
dssmap.h \
ems.h \
err.h \
error.h \
f77.h \
fitschan.h \
frame.h \
frameset.h \
grf.h \
grismmap.h \
intramap.h \
loader.h \
lutmap.h \
mapping.h \
mathmap.h \
matrixmap.h \
memory.h \
object.h \
pcdmap.h \
permmap.h \
plot.h \
pointset.h \
polymap.h \
proj.h \
shiftmap.h \
skyaxis.h \
skyframe.h \
slalib.h \
slamap.h \
specframe.h \
specmap.h \
sphmap.h \
tranmap.h \
unit.h \
unitmap.h \
wcsmap.h \
wcsmath.h \
wcstrig.h \
winmap.h \
xml.h \
xmlchan.h \
zoommap.h

F_INCLUDE_FILES = \
ast_par.source \
grf_par

LATEX_DOCUMENTATION_FILES = \
sun210.tex \
sun211.tex

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

UNIX_LINK_FILES = \
ast_link \
ast_link_adam

UNIX_STARTUP_FILES = \
ast_dev

VERSION_FILES = \
makefile \
sun_master.tex

MESSAGE_SYSTEM_FILES = \
ast_err.msg

UTILITY_PROGRAMS = \
astbad.c

TEST_PROGRAMS = \
ast_test.c

DEVELOPMENT_TOOLS = \
makeh

#  Target for use by the grp command.
$(action)

#  Keywords for use by SCCS.
#%Z%%M%   %I%   %E% %U%   %D% %T%
#  Reference directory makefile for the AST library.
#  ================================================

#  Defaults.
#  --------
EXPORT=$(HOME)
VMS_EXPORT=$(EXPORT)

#  Pass unknown targets on to be handled by the release makefile.
.DEFAULT: ; @ $(MAKE) -e -f $(REL_MAKE) $@

#  Files built by the error message system.
MESSFILES=$(PKG)_err fac_$(FAC)_err $(PKG)_err.h

#  Header files which contribute to the "ast.h" file, organised to correspond
#  with the class hierarchy.
HEADFILES = \
xml.h \
wcstrig.h \
proj.h \
memory.h \
error.h \
unit.h \
ast_err.h \
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
      xmlchan.h \
      zoommap.h \
      frame.h \
         cmpframe.h \
         frameset.h \
            plot.h \
         skyframe.h \
         specframe.h \
   channel.h \
      fitschan.h

#  Rules for building the development system (libraries).
#  -----------------------------------------------------
build: $(MESSFILES) $(PKG).h lib$(PKG)_pass2.a
	@ refup; chmod +x $(PKG)_link
	@ $(MAKE) -e -f $(REL_MAKE) lib$(PKG).a \
                                    $(PKG)_par $(PKG)_err \
                                    lib$(PKG)_err.a lib$(PKG)_ems.a \
                                    lib$(PKG)_grf.a lib$(PKG)_pgplot.a \
                                    lib$(PKG)_slalib.a \
                                    lib$(PKG)_wcslib.a

#  This is just a link to the main library and is used if a second pass
#  through the library is needed when linking.
lib$(PKG)_pass2.a:
	$(LINK) lib$(PKG).a $@

#  Create the ast.h file by processing the required internal include files.
$(PKG).h: $(HEADFILES) $(MESSFILES)
	chmod +x $${AST_REF}/makeh $${AST_REF}/ast_cpp
	$${AST_REF}/makeh >ast.h $(HEADFILES)

#  Create a reference copy of the release makefile.
$(REL_MAKE):
	@ refup $(REL_MAKE)

#  Rule to produce a UNIX release.
#  ------------------------------
#  Create a scratch directory in the reference copy directory and go to it.
#  Assemble the release, then remove the scratch directory.
release: addcopyright addversion getnewversion
	cd $${AST_REF}; chmod +x addcopyright addversion getnewversion
	$${AST_REF}/getnewversion
	rm -r -f $${AST_REF}/release.tmp*
	mkdir release.tmp$$$$; \
        cd release.tmp$$$$; \
        $(MAKE) -e -f $(MAKEFILE) do_unix_release; \
        cd $${AST_REF}; \
        rm -r -f release.tmp$$$$

#  Rule to assemble the files for a UNIX release.
#  ---------------------------------------------
do_unix_release:
#
#  Fetch all the source files required.
	fetch $(C_ROUTINES) $(F_ROUTINES) $(C_INCLUDE_FILES) \
              $(F_INCLUDE_FILES) $(UNIX_STARTUP_FILES) \
              $(UNIX_LINK_FILES) $(MESSAGE_SYSTEM_FILES) \
              $(UTILITY_PROGRAMS) $(TEST_PROGRAMS) COPYING.LIB
#
#  Create files generated from other source files, and delete the
#  original source where necessary.
	$(MAKE) -e -f $(MAKEFILE) $(DERIVED_FILES)
	rm -f $(MESSAGE_SYSTEM_FILES)
#
#  Make the remaining files accessible to others.
	for f in *; do \
           cat "$${f}" | ${AST_REF}/addcopyright \
                       | ${AST_REF}/addversion >tmp.tmp;\
           rm -f "$${f}";\
           mv tmp.tmp "$${f}";\
        done
	chmod 644 *
#
#  Pack the files into the source archive, omitting any which must be kept
#  separate for licensing reasons. Then remove the original copies of the
#  files that were packed.
	tar_files="`{  echo $(WCSLIB_FILES);\
                      awk 'BEGIN{printf(\" > \")}' </dev/null;\
                      echo $(C_ROUTINES) $(F_ROUTINES) $(C_INCLUDE_FILES) \
                           $(DERIVED_FILES) $(F_INCLUDE_FILES) \
                           $(UNIX_STARTUP_FILES) $(UNIX_LINK_FILES) \
                           $(UTILITY_PROGRAMS) $(TEST_PROGRAMS);\
                    } | awk 'BEGIN{RS=\" \"}{ \
                       if ( $$1 == \">\" ) { \
                          x++ \
                       } else if ( !x ) { \
                          a[ $$1 ]++ \
                       } else { \
                          if ( !a[ $$1 ] ) print \
                       } }'`";\
        $(TAR_IN) $(PKG)_source.tar $${tar_files};\
        rm -f $${tar_files}
#
#  Pack the wcslib files into their own library and then remove the originals.
	tar_files="$(WCSLIB_FILES)";\
        $(TAR_IN) wcslib.tar $${tar_files};\
        rm -f $${tar_files}
#
#  Get the hypertext documentation and pack it into a tar file.
	(cd $$AST_DEV; $(TAR_IN) - sun210.htx sun211.htx) | $(TAR_OUT) -
	chmod 755 sun210.htx sun211.htx
	chmod 644 sun210.htx/* sun211.htx/*
	$(TAR_IN) sun210.htx_tar sun210.htx/*
	$(TAR_IN) sun211.htx_tar sun211.htx/*
	rm -r sun210.htx sun211.htx
#
#  Get the postscript figures and store in suitable directories.
	mkdir sun210_figures sun211_figures
	(cd sun210_figures; fetch $(POSTSCRIPT_FIGURES))
	(cd sun211_figures; fetch $(POSTSCRIPT_FIGURES))
	chmod 755 sun210_figures sun211_figures
	chmod 644 sun210_figures/* sun211_figures/*
#
#  Fetch the news file and licence conditions and any additional files
#  required in the release. Make all files accessible.
	files="$(PKG).news AST_CONDITIONS makefile mk \
               $(LATEX_DOCUMENTATION_FILES)";\
        fetch $${files}; \
        for f in $${files}; do \
           cat "$${f}" | ${AST_REF}/addcopyright \
                       | ${AST_REF}/addversion >tmp.tmp;\
           rm -f "$${f}";\
           mv tmp.tmp "$${f}";\
           chmod 644 "$${f}";\
        done
	chmod 755 mk
#
#  Export the package source to the export directory, ensuring suitable file
#  permissions.
	umask 022; $(MAKE) -e -f makefile export_source

#  Rules for generating files built by the error message system.
#  ------------------------------------------------------------
$(PKG)_err:     $(PKG)_err.msg; messgen -f $?
$(PKG)_err.h:   $(PKG)_err.msg; messgen -c $?
fac_$(FAC)_err: $(PKG)_err.msg; messgen -e $?

#  Keywords for use by SCCS.
#%Z%%M%   %I%   %E% %U%   %D% %T%
