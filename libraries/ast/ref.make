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
wcstrig.h \
proj.h \
memory.h \
error.h \
ast_err.h \
object.h \
   pointset.h \
   axis.h \
      skyaxis.h \
   mapping.h \
      cmpmap.h \
      dssmap.h \
      intramap.h \
      lutmap.h \
      matrixmap.h \
      permmap.h \
      slamap.h \
      sphmap.h \
      unitmap.h \
      wcsmap.h \
      winmap.h \
      zoommap.h \
      frame.h \
         cmpframe.h \
         frameset.h \
            plot.h \
         skyframe.h \
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
	@ cd $${AST_REF}; chmod +x addcopyright addversion getnewversion
	@ $${AST_REF}/getnewversion
	@ rm -r -f $${AST_REF}/release.tmp*
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
	fetch `grp c_routines f_routines c_include_files f_include_files \
                   unix_startup_files unix_link_files message_system_files \
                   utility_programs test_programs` COPYING.LIB
	echo `grp c_routines f_routines c_include_files f_include_files \
                   unix_startup_files unix_link_files message_system_files \
                   utility_programs test_programs` COPYING.LIB
#
#  Create files generated from other source files, and delete the
#  original source where necessary.
	$(MAKE) -e -f $(MAKEFILE) `grp derived_files`
	rm -f `grp message_system_files`
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
	tar_files=`{  grp wcslib_files;\
                      awk 'BEGIN{printf(" > ")}' </dev/null;\
                      grp c_routines f_routines c_include_files derived_files \
                      f_include_files unix_startup_files unix_link_files \
                      utility_programs test_programs;\
                    } | awk 'BEGIN{RS=" \n"}{ \
                       if ( $$1 == ">" ) { \
                          x++ \
                       } else if ( !x ) { \
                          a[ $$1 ]++ \
                       } else { \
                          if ( !a[ $$1 ] ) print \
                       } }'`;\
        $(TAR_IN) $(PKG)_source.tar $${tar_files};\
        rm -f $${tar_files}
#
#  Pack the wcslib files into their own library and then remove the originals.
	tar_files=`grp wcslib_files`;\
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
	(cd sun210_figures; fetch `grp postscript_figures`)
	(cd sun211_figures; fetch `grp postscript_figures`)
	chmod 755 sun210_figures sun211_figures
	chmod 644 sun210_figures/* sun211_figures/*
#
#  Fetch the news file and licence conditions and any additional files
#  required in the release. Make all files accessible.
	files="$(PKG).news LICENCE makefile mk \
               `grp latex_documentation_files`";\
        fetch $${files};\
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
