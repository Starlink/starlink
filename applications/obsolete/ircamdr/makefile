########################################################################
#                                                                      #
#     This is the Starlink template makefile for adam applications.    #
#                                                                      #
#     Replace all occurrences of the string "{PACKAGE}" with the       #
#     long form of the package name (as used in documentation),        #
#     and all occurrences of the strings "{PKG}" and "{pkg}" with      #
#     the short form of the package name (as used within a library     #
#     name), maintaining the case of the original.                     #
#                                                                      #
#     Also, replace the comments in the prologue under                 #
#     "External Dependencies" with comments appropriate to your        #
#     particular package.                                              #
#                                                                      #
#     Similarly edit "{PACKAGE}" in the mk script.                     #
#                                                                      #
#     These changes are all that are necessary for a simple library.   #
#     More complicated packages may require additional alterations.    #
#                                                                      #
#     Finally delete this box (22 lines).                              #
#                                                                      #
########################################################################
#+
#  Name: 
#     makefile
#
#  Version:
#     Application makefile Mk V
#
#  Purpose:
#     Build and install the {PACKAGE} package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     {PACKAGE} package from the distributed source files, to install
#     the resulting system for use, and to perform other housekeeping
#     tasks.
#
#  Invocation:
#     This makefile is not intended to be used by make when invoked
#     directly (although this is possible), but instead to be used via
#     the accompanying mk script.  This script sets up a number of
#     environment variables which are used as macros within the
#     makefile and which accommodate differences between machines and
#     operating systems (it invokes make with the -e option).  Please
#     consult the mk script prologue for full details.
#
#  Targets:
#     The following make targets are defined in this script for
#     external use:
#
#        [help]
#           This is the default target.  It outputs a message describing
#           the mk script and lists the targets provided.
#
#        check
#           Performs a simple check that all necessary source files are
#           present, and displays the version number and current state
#           of the package (built/installed/tested, etc.).
#
#        build
#           Compiles the source files and creates all files needed
#           prior to installing the package for use.
#
#        install
#           Installs the package for use by putting the necessary files
#           into sub-directories of the $INSTALL directory (the $HOME
#           directory is used if the environment variable INSTALL is
#           not defined).  Links to the installed files are left in the
#           source directory.
#
#        deinstall
#           Reverses the action of the install target, removing files
#           from sub-directories of the $INSTALL directory and
#           restoring them to the source directory (the $HOME directory
#           is used by default if the environment variable INSTALL is
#           not defined).
#
#        test
#           Builds and runs a simple test program to check for correct
#           installation of the package.
#
#        export
#           Produces an export copy of the built package suitable for
#           passing to another user.  A compressed tar file is created
#           in the $EXPORT directory containing copies of the source
#           files and built files for the package (the current
#           directory is used by default if the environment variable
#           EXPORT is not defined).  The package should normally be
#           built, installed and tested (see above) before using this
#           target.  After unpacking the exported file on a similar
#           machine, the recipient may simply install it for use.
#
#        export_run
#           Produces an export copy of the built package suitable for
#           passing to another user.  A compressed tar file is created
#           in the $EXPORT directory containing copies of the built 
#           files for the package (the current directory is used by 
#           default if the environment variable EXPORT is not defined).
#           The package should normally be built, installed and tested 
#           (see above) before using this target.  After unpacking the
#           exported file on a similar machine, the recipient may simply
#           install it for use.
#
#        export_source
#           Produces an export copy of the source for the package
#           suitable for passing to another user to build (possibly on
#           a different type of machine).  A compressed tar file is
#           created in the $EXPORT directory containing copies of just
#           the source files for the package (the current directory is
#           used by default if the environment variable EXPORT is not
#           defined).  After unpacking the exported file, the recipient
#           must build the package before installing it for use.
#
#        clean
#           Cleans up after building the package, removing all
#           intermediate files created during the building process, but
#           leaving the built files themselves.
#
#        unbuild
#           Reverses the building process, removing all intermediate
#           files along with all the built files.
#
#        strip
#           This target `strips' the executable files.  This can lead
#           to significant reduction in the sizes of binaries.
#
#  External Dependencies:
#     The {PACKAGE} package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#        ems                                                {
#           Error message service                           {
#        hds                                                {  as
#           Hierarchical data system                        {  appropriate
#        sae_par                                            {
#           Global include files                            {
#        etc                                                {
#
#  Notes:
#     This makefile uses the presence/absence of the hidden files
#     .BUILT, .INSTALLED_$(SYSTEM) and .TESTED_$(SYSTEM) to record the
#     current state of the system during housekeeping operations.
#
#  Implementation Deficiencies:
#
#  Copyright:
#     Copyright (C) 1995 Rutherford Appleton Laboratory
#
#  Authors:
#     RFWS: R.F.Warren-Smith (Starlink, RAL)
#     PMA: P.M.Allan (Starlink, RAL)
#     PTW: P.T.Wallace (Starlink, RAL)
#     BLY: M.J.Bly (Starlink, RAL)
#     AJC: A.J.Chipperfield (Starlink, RAL)
#     {enter_new_authors_here}
#
#  History:
#     4-JAN-1993 (RFWS):
#        Original version.
#     15-MAR-1993 (RFWS):
#        Adapted for use on sun4_Solaris.
#     11-JUN-1993 (PMA):
#        Added parts for handling platform specific files.
#     27-SEP-1993 (PMA):
#        Added the target "filler1" to prevent the list of platform
#        specific files being empty, thereby causing a make syntax error.
#     3-NOV-1993 (PMA):
#        Change the check target to print out for which system the
#        software has been built.
#     3-NOV-1993 (PMA):
#        Add a test to check that files really are extracted from the
#        tar file.
#     18-NOV-1993 (PTW):
#        Cosmetics.
#     26-NOV-1993 (PMA):
#        Remove the dummy file "filler1" and add a dummy target called
#        dummy_target1.
#        Add a file called " " to all of the for loops that install and
#        deinstall files to prevent syntax errors if any of the relevent
#        macros are null.
#        Add a macro STARTUP_SCRIPT that is the name of the startup
#        script.
#     4-JAN-1994 (PMA):
#        Add some missing semicolons to for loops. This seems not to
#        cause errors on our systems, but it is a syntax error.
#     7-FEB-1994 (PMA):
#        Move the block data files to the beginning of the definition of
#        OBJECT_FILES. This makes it possible to extract the block data
#        modules with the -u loader flag. This is needed when using f2c.
#     8-AUG-1994 (BLY):
#        Add dependency on source archive to extraction targets.
#     30-NOV-1994 (AJC):
#        Add facility error files
#        Add export_run target
#        Add where installed
#        Prevent installation if not built for this SYSTEM
#        Ensure deinstall is from where it is installed
#        Revise use of package name variables, adding PACK_NAME for
#         use in messages and PKG_LINK for use in INCLUDE FILE links.
#         Use variables in all references.
#        Use installed $(PKG_NAME)_dev to handle INCLUDE files for test
#     24-APR-1995 (AJC):
#        -p on install mkdir
#        Add else clause to catch failue of "elif $(MAKE)"
#        Add exit 1 to detected illegal install and deinstall cases.
#        Remove dependency of files on source archive.
#     2-MAY-1995 (BLY):
#        Shared libraries now in INSTALL_SHARE, also chmod 755.
#        INSTALL_SHARE added to INSTALL_DIRS.
#        RANLIB default changed to : (colon).
#        BLD_SHR default changed to : (colon).
#     5-JUN-1995 (RFWS):
#        Added -f flag to rm commands in clean and unbuild targets.
#     15-JUN-1995 (AJC):
#        Revised check target to cope with export_run versions.
#     16-JUN-1995 (BLY):
#        Designated Mk IVb.
#     26-JUN-1995 (BLY):
#        Modified export, export_run and export_source targets.
#        Cosmetics.
#        Designated Mk IVc.
#     29-JUN-1995 (BLY):
#        Added sections for manipulating Latex and Hypertest documentation.
#        Revised install and deinstall sequences.
#        Changed export* targets to make compressed tar files in one step.
#        Replacing of `rm' with `rm -f' in most cases.
#        Designated Mk V.
#     03-JUL-1995 (BLY):
#        Template makefile now generated automatically.
#     17-JUL-1995 (AJC):
#        Modified hypertext installation/deinstallation to avoid linking
#        if no hypertext documents defined.
#     10-FEB-1996 (frossie)
#        IRCAMDR specific makefile. Sed terminator switched from # to % to 
#        avoid hosing the xemacs makefile mode.
#
#  Bugs:
#     {note_any_bugs_here}
#
#------------------------------------------------------------------------------

#  Help target.
#  ===========
#
#  This is the default target, so it appears first.

#  Display information about the mk script and the make targets.

help:
	@ echo \
   '   The makefile provided is intended to be used by the make utility when';\
        echo \
   '   invoked via the associated mk script.This script defines environment';\
        echo \
   '   variables which are used by make to accommodate differing machine and';\
        echo \
   '   operating system characteristics.  Please see the mk script prologue';\
        echo \
   '   for full details.';\
        echo;\
        echo \
   '   The following targets are provided:';\
        echo;\
        echo \
   '      help          - Display this message';\
        echo \
   '      check         - Check source file presence and show current state';\
        echo \
   '      build         - Build the package from source';\
        echo \
   '      install       - Install the built package for use';\
        echo \
   '      deinstall     - Deinstall the package';\
        echo \
   '      test          - Perform a quick test of the installation';\
        echo \
   '      export        - Make a compressed tar file for exporting the'\
   'built package';\
        echo \
   '                      complete with source and documentation';\
        echo \
   '      export_run    - Make a compressed tar file for exporting the'\
   'built package';\
        echo \
   '                      with documentation but no source';\
        echo \
   '      export_source - Make a compressed tar file for exporting the'\
   'source files';\
        echo \
   '      clean         - Tidy up after building the package';\
        echo \
   '      unbuild       - Remove all the built files';\
        echo \
   '      strip         - Strip the binary files (executables)';\
        echo;\
        echo \
   '   To build and install the $(PACK_NAME) package on a supported system:';\
        echo;\
        echo \
   '      mk build; mk install; mk test; mk clean';\
        echo

#------------------------------------------------------------------------------

#  Defaults.
#  ========
#
#  This section defines default macros and should rarely need changing.
#  The values given here should be overridden externally to adapt to
#  the local system setup (either use the mk script or use environment
#  variables and invoke "make" with the "-e" option).

#  Name of computer hardware/OS combination.

SYSTEM = unknown

#  Name used to distinguish platform-specific source files.

SOURCE_VARIANT = $(SYSTEM)

#  Pathname of the root directory beneath which other Starlink software
#  is currently installed.

STARLINK = /star

#  Pathnames of Starlink sub-directories that may be referenced when
#  building this package.

STAR_BIN = $(STARLINK)/bin
STAR_DATES = $(STARLINK)/dates
STAR_DOCS = $(STARLINK)/docs
STAR_ETC = $(STARLINK)/etc
STAR_HELP = $(STARLINK)/help
STAR_INC = $(STARLINK)/include
STAR_LIB = $(STARLINK)/lib

#  Pathname of the root directory beneath which the built files for
#  this package should be installed for use.  This defaults to the
#  user's home directory.

INSTALL = $(HOME)

#  Pathname of the directory into which exported tar files will be
#  placed.  This defaults to the current working directory.

EXPORT = .

#  Default macros for compiling C and Fortran source code.

CC = c89
CFLAGS = -O
FC = fort77
FFLAGS = -O

#  Command for forming a link to a file.

LINK = ln

#  Command for "randomizing" an object library.  The default acts as a
#  null command.

RANLIB = :

#  Commands for adding to and extracting from an archive file (.tar).

TAR_IN = pax -w -v -x ustar -f
TAR_OUT = pax -r -f

#  Command for adding a file to an object archive (.a).

AR_IN = ar -r

#  ADAM monolith linker command.
# Added ilink - Frossie

ALINK = alink
ILINK = ilink
#------------------------------------------------------------------------------
###############################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.
# PACK_NAME = {PACKAGE}

PACK_NAME = IRCAMDR

#  Prefix for the package in lower-case as used in filenames etc.
# PKG_NAME = {pkg}

PKG_NAME = ircamdr


#  Prefix for the package in upper-case as used in include file links.
# PKG_LINK = {PKG}

PKG_LINK = IRCAMDR


#  Version number (as in the documentation - i.e. not the same thing
#  as the shared library version number).
#
#  The major component of the version number (before the dot) should
#  normally only be incremented following major changes to the package.
#  The minor version number (after the dot) is the number normally
#  incremented following development which introduces new documented
#  functionality.  Any revision number (appended after a dash) should
#  be incremented for other minor changes (bug fixes, etc.) which do
#  not merit documentation changes.
# PKG_VERS = 1.0

PKG_VERS = 1.1-0

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).  It is advisable to use several
#  smaller source archives in place of one large one to speed up build
#  times.
# SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION) \
#    $(PKG_NAME)_sub1.tar $(PKG_NAME)_sub2.tar

SOURCE_FILES = ${PKG_NAME}_aif.tar ${PKG_NAME}_aspker.tar \
${PKG_NAME}_atlib.tar ${PKG_NAME}_dat.tar ${PKG_NAME}_goodies.tar \
${PKG_NAME}_icl.tar ${PKG_NAME}_ifl.tar ${PKG_NAME}_io2ro.tar \
${PKG_NAME}_kergen.tar ${PKG_NAME}_kersub.tar ${PKG_NAME}_lists.tar \
${PKG_NAME}_mapgl.tar ${PKG_NAME}_misc.tar ${PKG_NAME}_obsrap.tar \
${PKG_NAME}_plt2d.tar ${PKG_NAME}_polrap.tar ${PKG_NAME}_rapgen.tar \
${PKG_NAME}_rapi2d.tar ${PKG_NAME}_scripts.tar ${PKG_NAME}_sdf.tar \
${PKG_NAME}_trace.tar makefile mk $(DOCUMENTATION)


#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.
# PUBLIC_SCRIPTS =

#  Startup script.  This is the file that must be executed (sourced)
#  by a user of this package in order to define appropriate aliases 
#  and environment variables.  It is listed separately from the public 
#  scripts as it is edited by the installation procedure.
# STARTUP_SCRIPT = $(PKG_NAME).csh 

STARTUP_SCRIPT = $(PKG_NAME)


#  Link script.  This script does not form part of the package but is 
#  required for building it.  It will not be installed.
# LINK_SCRIPT = $(PKG_NAME)_link_adam

#  Startup ICL script.  This is the file that must be loaded by ICL
#  for a user of this package,  in order to define the package command
#  set within ICL.  It is listed separately from the public scripts as 
#  it is edited by the installation procedure.
# ICL_SCRIPTS = $(PKG_NAME).icl 

ICL_SCRIPTS = ablock.icl accoff.icl again.icl amcorrlot.icl anncol.icl \
applymasklot.icl array_tests.icl border.icl box.icl cablock.icl \
calexp.icl calmag.icl calzer.icl cbox.icl ccircle.icl ccross.icl \
ccut.icl cellipse.icl cent1.icl cent2.icl chdisp.icl chred.icl \
circle.icl clear.icl clearit.icl cline.icl cnsigma.icl coff.icl \
colinv.icl coll.icl coltab.icl cont_title.icl contoff.icl contour.icl \
cplot.icl cranplot.icl cross.icl crosscut.icl crosscut_peak.icl \
cstats.icl curhot.icl cursor.icl cut.icl cut2ff.icl cut_title.icl \
cvargrey.icl cxwin.icl daocen.icl daofind.icl daogid.icl daogid2.icl \
darklot.icl define_atasks.icl defprocs.icl deglot.icl disp.icl \
dispick.icl ellipse.icl exit.icl fclose_a.icl fclose_b.icl \
fclose_c.icl fclose_d.icl fclose_e.icl flat2.icl flatlot.icl \
focrun.icl focrun2.icl formname.icl formname2.icl fpstriplot.icl \
get_imagename.icl get_imagename_old.icl getcm.icl getoff.icl \
getrast.icl glitchmark.icl glkill.icl global_vars.icl \
god_ircamdr_starlink.icl god_ircamdr_ukirt.icl grid.icl hardannot.icl \
hardcom.icl hardcopy.icl hardcopy_cv.icl hardcopy_il.icl \
hardcopy_ilv.icl hardcopy_image.icl hardcopy_iv.icl hardcopy_line.icl \
hardlot.icl hardprint.icl hardprint_unix.icl hardsym.icl io2ro.icl \
jitreg.icl lagain.icl limag.icl line.icl line_width.icl linecol.icl \
logo.icl lplt.icl mcursor.icl medlot.icl mofflot.icl moonraw.icl \
morensigma.icl moreplot.icl moreranplot.icl morevargrey.icl mos2.icl \
nomag.icl nsigma.icl oadd.icl objproc.icl objred.icl ocadd.icl \
ocdiv.icl ocmult.icl ocstats.icl ocsub.icl odist.icl odiv.icl \
ohisto.icl olook.icl omult.icl ostats.icl osub.icl pclose.icl \
pencol.icl penint.icl pho.icl pho2.icl pho3.icl picklot.icl plot.icl \
plotglitch.icl plotlot.icl pol2.icl polflat.icl polpick2.icl \
polreg.icl polseplot.icl polshift2.icl polsky2.icl popen.icl \
popen_new.icl popen_unix.icl psf_test.icl ranplot.icl \
reload_obsrap.icl remos.icl rflot.icl rocent.icl rodarklot.icl \
roindex.icl romed.icl ropar.icl rotlot.icl rout.icl satlot.icl \
satpho.icl satraw.icl satsub.icl scaledark.icl scalelot.icl see.icl \
setarea.icl setcen.icl setcomori.icl setcont.icl setcontic.icl \
setcur.icl setcurmark.icl setcut.icl setcutaxrat.icl setfile.icl \
setfile_unix.icl setfont.icl sethard.icl setmag.icl setmax.icl \
setmin.icl setmm.icl setnum.icl setnumori.icl setnumsca.icl \
setpolcol.icl setpre.icl setps.icl setquad.icl setradec.icl \
setstd.icl setvar.icl setvargrey.icl setvec.icl shift2.icl shift3.icl \
short_display.icl skyext.icl skysub.icl skysub4.icl splitlot.icl \
stdpho.icl stdproc.icl stdred.icl stlot.icl stred.icl strehl.icl \
striplot.icl surround.icl testval.icl tickio.icl ticklen.icl \
tomag.icl tslot.icl utils.icl utils_old.icl utils_unix.icl valstr.icl \
valstr_old.icl vans.icl vargrey.icl vcen.icl vcor.icl vdel.icl \
vec.icl vec_title.icl vsub.icl wraplot.icl wrccom.icl wrcom.icl \
writelut.icl x2mag.icl x5mag.icl zaplot.icl 


#  List of private include files.  These are include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.
# PRIVATE_INCLUDES = $(I_SUB1) $(I_SUB2)
# I_SUB1 =
# I_SUB2 =

PRIVATE_INCLUDES = $(I_SUBA)

I_SUBA = PLT2DCOM


#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.
# EXTERNAL_INCLUDES = SAE_PAR

EXTERNAL_INCLUDES = ACT_ERR ADAMDEFNS ADAMERRS CHR_ERR DAT_ERR \
DAT_PAR DTDEFNS DTERRS FIO_ERR FIO_PAR GKS_PAR MAG_ERR NDF_ERR \
NDF_PAR PAR_ERR PAR_PAR PRM_PAR SAE_PAR

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines and the main routine, which are treated separately).  
# F_ROUTINES = $(F_SUB1) $(F_SUB2) $(MONO_SOURCE)
# F_SUB1 = lib1_sub1.f lib1_sub2.f
# F_SUB2 = lib2_sub1.f lib2_sub2.f

F_ROUTINES = $(F_SUB0) $(F_SUB1) $(F_SUB2) $(F_SUB3) $(F_SUB4) \
$(F_SUB5) $(F_SUB6) $(F_SUB7) $(F_SUB8) $(F_SUB9) $(F_SUBA) \
$(F_SUBB) $(F_SUBC) $(F_SUBD) $(F_SUBE) $(F_SUBF)

# aif.tar

F_SUB0 = aif_choic.f aif_exacc.f aif_exacd.f aif_exaci.f aif_exacl.f \
aif_exacr.f aif_fillvb.f aif_fillvc.f aif_fillvd.f \
aif_fillvi.f aif_fillvl.f aif_fillvr.f aif_fillvub.f \
aif_fillvuw.f aif_fillvw.f aif_get0c.f aif_get0d.f \
aif_get0i.f aif_get0l.f aif_get0r.f aif_get1c.f aif_get1d.f \
aif_get1i.f aif_get1l.f aif_get1r.f aif_geven.f aif_goddn.f \
aif_mmvd.f aif_mmvi.f aif_mmvr.f aif_mmvuw.f aif_mmvw.f \
aif_scalvb.f aif_scalvd.f aif_scalvi.f aif_scalvl.f \
aif_scalvr.f aif_scalvub.f aif_scalvuw.f aif_scalvw.f \
aif_sort1b.f aif_sort1c.f aif_sort1d.f aif_sort1i.f \
aif_sort1l.f aif_sort1r.f aif_sort1ub.f aif_sort1uw.f \
aif_sort1w.f 



# aspker.tar

F_SUB1 = kfh_adarar.f kfh_adarsc.f kfh_aplot.f kfh_arrcre.f \
kfh_bttns.f kfh_butvl.f kfh_copy.f kfh_cross.f kfh_dfaint.f \
kfh_dflash.f kfh_diarar.f kfh_diarsc.f kfh_dimscl.f \
kfh_discar.f kfh_drcur.f kfh_expar.f kfh_faint.f kfh_fill.f \
kfh_flash.f kfh_gtcol.f kfh_help.f kfh_hist.f \
kfh_histogram.f kfh_histst.f kfh_hstrep.f kfh_imcopy.f \
kfh_imscl.f kfh_interpolate.f kfh_linset.f kfh_logar.f \
kfh_lutwk.f kfh_move.f kfh_muarar.f kfh_muarsc.f \
kfh_nhplot.f kfh_nlist.f kfh_nlplot.f kfh_nwlinplot.f \
kfh_nwlist.f kfh_nwpeep.f kfh_nwslice.f kfh_outrn.f \
kfh_power.f kfh_pscale.f kfh_rdbox.f kfh_replc.f \
kfh_repval.f kfh_rgcopy.f kfh_scale.f kfh_setcu.f \
kfh_stats.f kfh_suarar.f kfh_suarsc.f kfh_trans.f \
kfh_transfer.f kfh_transfer2.f kfh_trnfr.f kfh_write.f

# atlib.tar

F_SUB2 = addlinesub.f amcorrsub.f annstatssub.f anntestsub.f \
aperphotsub.f applymasksub.f asciilistsub.f astrom.f \
automos_apply.f binup2d.f calcolsub.f check_comment.f \
colcyclesub.f colmedsub.f coltab_fill.f column_label.f \
convert_local2ut.f copyab.f copyab_comb.f csfitsub.f \
csgensub.f defgradsub.f devfcssub.f figndfsub.f find_median.f \
findpeaksub.f fitswarray.f fitswcalb.f fitswget.f \
fitswget2.f fitswhead.f fitswsub.f gaudrive.f \
get_intensities.f hdscreate_setprim.f hist_maxmin.f \
histogram_save.f hotshotsub.f imcombsub.f insetbsub.f \
integer_picture.f interpolate_intensities.f invers.f \
lincont_lin.f lincont_ndr_lin.f lincont_ndr_poly.f \
lincont_poly.f linimag_ndr_lin.f locate.f makebadsub.f \
makeglitchsub.f makemasksub.f manycol_putcol.f manycolsub.f \
margin.f med3d_calmedsub.f med3d_medfil.f med3d_norm.f \
med3d_scaling.f med3d_to3d.f med3d_tozero.f mfclose.f \
mfnext.f mfopen.f moscorsub.f moscorsub2.f moscorsub3.f \
mosstatssub.f mosstatssub2.f mtread.f oefixsub.f outdata.f \
outsetbsub.f phaseshiftsub.f photom.f pol2_calpt.f \
pol2_errcal.f pol2_intcal.f pol2_pical.f pol2_polcal.f \
pol2_stokescal.f pol2_thetacal.f pol2_upical.f pol_calpt.f \
pol_calpt2.f pol_errcal.f pol_intcal.f pol_pical.f \
pol_polcal.f pol_stokescal.f pol_thetacal.f pol_upical.f \
polshot_remove.f polthreshsub.f polthresh2sub.f \
radim_data.f radimsub.f rapgauth.f redcont_checkdata.f \
redcont_whatset.f rings.f rowmedsub.f search.f setvalsub.f \
spec3dsub.f statssub2.f stcoadd_aver.f stcoadd_image.f \
stcoadd_zero.f stepimsub.f thetafixsub.f toeqsub.f vmul.f \
wmfnext.f wmfopen.f wrapcorsub.f xgrowsub.f yaddsub.f \
ygrowsub.f

# goodies.tar

F_SUB3 = get_word.f

# io2ro.tar

F_SUB4 = reduce.f roname.f

# kergen.tar

F_SUB5 = appg0c.f appg0i.f appg0r.f appgir.f copy1d.f copy2d.f \
copy3d.f creout.f getinp.f inset.f oddget.f zero1d.f \
zero2d.f

# kersub.tar

F_SUB6 = flips.f ma1to2.f ma1to3.f ma2to1.f ma2to3.f ma3to1.f \
ma3to2.f maless.f mamore.f masame.f medref.f medrep.f \
medset.f medwts.f newcoords.f oldcoords.f rapblo.f rapgau.f \
rotas1.f rotas2.f rotas3.f rotas4.f rotnrs.f rotsize.f \
shifts.f shiftx.f shifty.f sqshs.f sqshx.f sqshy.f strx.f \
stry.f

# mapgl.tar

F_SUB7 = mapgl_fillsdf2.f mapgl_readsdf.f mapgl_readsdf2.f

# misc.tar

F_SUB8 = amcal.f anstd.f creobj.f crestd.f daogid.f daogid2.f \
daosymb.f ext3d.f fpp.f fprun.f predict.f predict3.f \
rotoff.f spiral.f


# obsrap.tar 

F_SUB9 = ab_translate_env.f abcom.f absep.f addline.f amcorr.f \
annstats.f apermag.f aperphot.f applymask.f asciifile.f \
asciilist.f automos.f binup.f business.f calcol.f clean.f \
colcycle.f colmed.f contplot.f copfile.f crecolt.f \
crequilt.f defgrad.f delfile.f dist.f extract.f fcoadd.f figndf.f\
findpeak.f fitswm.f gaussth.f histgen.f hotshot.f imcomb.f \
index.f inqfile.f insetb.f insetc.f intlk.f lfig_gplot.f \
lincont.f linimag_ndr.f looker.f lowcase.f makebad.f \
makeglitch.f makemask.f manycol.f med3d.f mosaic2.f \
moscor.f obsext.f obslist.f oefix.f outsetb.f \
plotslice.f psgrey.f radim.f root.f root2d.f rowmed.f \
setval.f skypara.f starclean.f stcoadd.f stepim.f typfile.f \
welcome_obsrap.f wmosaic.f wmosaic_add.f wmosaic_addbm.f \
wmosaic_addbv.f wquilt.f wrapcor.f xgrow.f yadd.f ygrow.f

# plt2d.tar

F_SUBA = access_pixel.f block_numbers.f block_ticks.f \
calculate_maxmin.f clear_area.f clear_screen.f \
colour_block.f colour_blocksub.f colour_table.f \
contour_annot.f contour_draw.f contour_map.f \
contour_numbers.f contour_plot.f contour_ticks.f \
contour_ticksec.f convert_values.f copy_image.f copy_sub.f \
cursor_display.f cursor_mark.f cursor_position.f \
cursor_values.f cut_display.f cut_interpolate.f \
cut_interpolatec.f cut_interpolater.f cut_lineplot.f \
cut_linset.f cut_linsetc.f cut_linsetr.f cut_maxmin.f \
cut_numbers.f cut_ticks.f decode_label.f \
default_annotation.f device_close.f device_code.f \
device_open.f fits_val.f flash_scale.f glitch_mark.f \
glitch_values.f image_clear.f image_display.f image_scale.f \
image_setval.f int_to_str.f ndfx__get_fitsc.f \
ndfx__get_fitsd.f ndfx__get_fitsi.f ndfx__get_fitsl.f \
ndfx__get_fitsr.f ndfx__seek_item.f ndfx_get_fitsc.f \
ndfx_get_fitsd.f ndfx_get_fitsi.f ndfx_get_fitsl.f \
ndfx_get_fitsr.f nsigma_maxmin.f nsigma_scale.f oldpars.f \
plot_border.f plot_box.f plot_circle.f plot_comment.f \
plot_cross.f plot_curbox.f plot_curcircle.f \
plot_curcomment.f plot_curcross.f plot_curellipse.f \
plot_curline.f plot_ellipse.f plot_grid.f plot_line.f \
plt2d_readfile.f plt2d_readfilef.f polax.f \
polax_annot.f polax_map.f polax_plot.f radec_concat1.f \
radec_concat2.f radec_convert.f radec_new.f radec_string.f \
range_maxmin.f range_scale.f reset_device.f ronext.f \
ropars.f ropars_new.f ropars_old.f set_colour.f \
set_colour2.f set_linewidth.f set_pencol.f set_pencol2.f \
sgs_ellips.f surround_annotate.f vargrey_scale.f write_ct.f

# polrap.tar

F_SUBB = aperpol.f csfit.f csgen.f devfcs.f polcal.f polcal2.f \
polly.f polly2.f polsep.f polshot.f polthresh.f \
polthresh2.f thetafix.f toeq.f welcome_polrap.f

# rapgen.tar

F_SUBC = addarr2d.f addsca2d.f divarr2d.f divsca2d.f exparr2d.f \
genhis.f histprop.f logarr2d.f maxmin.f  \
multarr2d.f multsca2d.f next_name.f normal.f \
nthmin.f poisson.f powarr2d.f remap.f subarr2d.f subsca2d.f \
upcase.f

# rapi2d.tar

F_SUBD = add.f aperadd.f block.f cadd.f cdiv.f centroid.f chpix.f \
cmult.f compadd.f compave.f compick.f compress.f creframe.f \
csub.f div2d.f expon.f flip.f gauss.f glitch.f histeq.f \
histo.f laplace.f logar.f look.f manic.f median.f moff.f \
mosaic.f mult.f numb.f outset.f pickim.f pixdupe.f pow2d.f \
quilt.f rotate.f shadow.f shift.f shsize.f \
sqorst.f stats.f sub.f thresh.f trandat.f trig.f \
welcome_rapi2d.f zaplin.f

# rapsub.tar

F_SUBE = aperaddsub.f centroid_locate.f chpixsub.f compaddsub.f \
compavesub.f compicksub.f compresssub.f creframesub.f \
glitchauto.f glitchlist.f glitchsub.f histeqsub.f \
insetsub.f laplacesub.f listsub.f lsdir.f lsputc.f lsputd.f \
lsputi.f lsputl.f lsputr.f lsputx.f manyg.f moffdcbad.f \
moffdcsub.f moffxybad.f moffxysub.f mosaic_add.f \
mosaic_addb.f mosaic_addbm.f mosaic_addbv.f mosaic_div.f \
numbs.f outsetsub.f peepsub.f pickimsub.f pixdupesub.f \
shadowsub.f statssub.f threshsub.f trandatsub.f trigscal.f \
trigsub.f utl_ls.f zaplinsub.f

# trace.tar

F_SUBF = arelem.f arrstr.f cominf.f locinf.f primit.f putdim.f \
putval.f trace.f tracon.f trarr1.f trarr2.f trarr3.f \
trarr4.f trarr5.f trarr6.f trstr1.f trstr2.f trstr3.f \
trstr4.f trstr5.f


#  List of Fortran BLOCK DATA routines.
# BLOCK_DATA = $(B_SUB1) $(B_SUB2)
# B_SUB1 = lib1_bd1.f lib1_bd2.f
# B_SUB2 = lib2_bd1.f lib2_bd2.f

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.
# PLATFORM_F = $(PF_SUB1) $(PF_SUB2)
# PF_SUB1 =
# PF_SUB2 =

 
#  C routines required for building the package.  This is just a list of
#  all the C source files.
# C_ROUTINES = $(C_SUB1) $(C_SUB2)
# C_SUB1 = lib1_sub1.c lib1_sub2.c
# C_SUB2 = lib2_sub1.c lib2_sub2.c

#  List of applications/tasks in monolith.
# TASKS =


#  Monolith routine source name.
# MONO_SOURCE = $(PKG_NAME)_mon.f

MONO_SOURCE = $(F_MON4) $(F_MON9) $(F_MONA) $(F_MONB) $(F_MOND)

F_MON4 = io2ro.f

F_MON9 = obsrap.f

F_MONA = plt2d.f

F_MONB = polrap.f

F_MOND = rapi2d.f

#  Monolith binary name.
# MONOLITH = $(PKG_NAME)_mon

MONOLITHS = io2ro obsrap plt2d polrap rapi2d
#  Monolith interface file name.
# MONO_IFL = $(PKG_NAME)_mon.ifl

#  List of task interface files.
# IFL_FILES = task1.ifl task2.ifl

IFL_FILES = io2ro.ifl obsrap.ifl plt2d.ifl polrap.ifl rapi2d.ifl

#  List of help library sources.
# HELP_SOURCES = $(PKG_NAME).hlp

#  List of ancilliary data files.
# DATA_FILES =

DATA_FILES = $(D_SUBX) $(D_SUBY) $(D_SUBZ)


D_SUBX = bpm_fpa42_128x128.sdf bpm_fpa42_256x256.sdf \
bpm_fpa42_64x64.sdf col1.sdf col10.sdf col11.sdf col12.sdf \
col13.sdf col14.sdf col15.sdf col16.sdf col17.sdf col18.sdf \
col19.sdf col2.sdf col20.sdf col21.sdf col22.sdf col23.sdf \
col24.sdf col25.sdf col26.sdf col27.sdf col28.sdf col29.sdf \
col3.sdf col30.sdf col31.sdf col32.sdf col33.sdf col34.sdf \
col35.sdf col36.sdf col37.sdf col38.sdf col39.sdf col4.sdf \
col40.sdf col41.sdf col42.sdf col43.sdf col44.sdf col45.sdf \
col46.sdf col47.sdf col48.sdf col49.sdf col5.sdf col6.sdf \
col7.sdf col8.sdf col9.sdf colb.sdf colc.sdf colg.sdf \
colo.sdf colp.sdf colr.sdf cols.sdf coly.sdf grey.sdf \
heat.sdf logo.sdf pony.sdf

D_SUBY = fp.dat fs.dat ukirt.dat spiral.off

D_SUBZ = bad_objects.list extinct.list \
lincoeff_fpa118_postjul88.list \
lincoeff_fpa118_postjul88_ndr.list \
lincoeff_fpa118_prejul88.list \
lincoeff_fpa118_prejul88_ndr.list lincoeff_fpa175.list \
lincoeff_fpa175_ndr.list


#  Lists of Latex and hypertext documents.

LATEX_DOCS = sun41.tex
#HYPERTEXT_DOCS = sun41.htx

#  List of documentation files.
#DOCUMENTATION = $(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar) $(PKG_NAME).news

DOCUMENTATION = $(LATEX_DOCS)

###############################################################################
#------------------------------------------------------------------------------

#  Define files required for building the package.
#  ==============================================
#
#  This section defines the set of files produced from the source files
#  when the package is built and installed.

#  Use only .o, .c, .f, .ifc, .ifl, .shl and .hlp suffix rules.

.SUFFIXES:
.SUFFIXES: .o .c .f .ifc .ifl .shl .hlp

#  List of files which must be built from the source files before the
#  package can be installed for use.  This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).
# BUILT_FILES = $(PUBLIC_SCRIPTS) $(OBJECT_LIBRARIES) $(MONOLITHS) \
# $(IFC_FILES) $(HELP_FILES) $(ICL_SCRIPTS) $(DATA_FILES) $(STARTUP_SCRIPT)

#BUILT_FILES = $(OBJECT_LIBRARIES) $(TASKS) \
#$(IFC_FILES) $(ICL_SCRIPTS) $(DATA_FILES) $(STARTUP_SCRIPT)

BUILT_FILES = $(OBJECT_LIBRARIES) $(ICL_SCRIPTS) $(MONOLITHS)\
$(DATA_FILES) $(STARTUP_SCRIPT) $(IFC_FILES)

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.
# INCLUDE_LINKS = $(EXTERNAL_INCLUDES) $(L_SUB1) $(L_SUB2) \
#   $(PKG_LINK)_ERR $(PKG_LINK)_PAR
# L_SUB1 =
# L_SUN2 =

INCLUDE_LINKS = $(EXTERNAL_INCLUDES)

#  Rules to set up links to locate each of the above include files.
# $(PKG_LINK)_ERR:   $(PKG_NAME)_err;               $(LINK) $? $@
# $(PKG_LINK)_PAR:   $(PKG_NAME)_par;               $(LINK) $? $@
# SAE_PAR:   $(STAR_INC)/sae_par;   $(LINK) $? $@

ACT_ERR:        $(STAR_INC)/act_err;    $(LINK) $? $@
ADAMDEFNS:      $(STAR_INC)/adam_defns; $(LINK) $? $@
ADAMERRS:       $(STAR_INC)/adam_err;  $(LINK) $? $@
CHR_ERR:        $(STAR_INC)/chr_err;    $(LINK) $? $@
DAT_ERR:        $(STAR_INC)/dat_err;    $(LINK) $? $@
DAT_PAR:        $(STAR_INC)/dat_par;    $(LINK) $? $@
DTDEFNS:        $(STAR_INC)/act_err;    $(LINK) $? $@
DTERRS:         $(STAR_INC)/dtask_err;  $(LINK) $? $@
FIO_ERR:        $(STAR_INC)/fio_err;    $(LINK) $? $@
FIO_PAR:        $(STAR_INC)/fio_par;    $(LINK) $? $@
GKS_PAR:        $(STAR_INC)/gks_par;    $(LINK) $? $@
MAG_ERR:        $(STAR_INC)/mag_err;    $(LINK) $? $@
NDF_ERR:        $(STAR_INC)/ndf_err;    $(LINK) $? $@
NDF_PAR:        $(STAR_INC)/ndf_par;    $(LINK) $? $@
PAR_ERR:        $(STAR_INC)/par_err;    $(LINK) $? $@
PAR_PAR:        $(STAR_INC)/par_par;    $(LINK) $? $@
PRM_PAR:        $(STAR_INC)/prm_par;    $(LINK) $? $@
SAE_PAR:        $(STAR_INC)/sae_par;    $(LINK) $? $@

 
#  Rules for extracting source files from the source archive.
# 
# $(PUBLIC_SCRIPTS) $(PRIVATE_INCLUDES) $(LINK_SCRIPT) $(MONO_SOURCE) \
# $(HELP_SOURCES) $(MONO_IFL) $(STARTUP_SCRIPT) $(IFL_FILES) $(ICL_SCRIPTS):
# 	$(TAR_OUT) $(PKG_NAME)_source.tar $@
# 	@ if test -f $@; then :;\
#            else echo $@ is not in the tar file; exit 1; fi
# 
# $(F_SUB1) $(I_SUB1) $(B_SUB1) $(C_SUB1):
# 	$(TAR_OUT) $(PKG_NAME)_sub1.tar $@
# 	@ if test -f $@; then :;\
#            else echo $@ is not in the tar file; exit 1; fi
# 
# $(F_SUB2) $(I_SUB2) $(B_SUB2) $(C_SUB2):
# 	$(TAR_OUT) $(PKG_NAME)_sub2.tar $@
# 	@ if test -f $@; then :;\
#            else echo $@ is not in the tar file; exit 1; fi

$(F_SUB0):
	$(TAR_OUT) $(PKG_NAME)_aif.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

$(F_SUB1):
	$(TAR_OUT) $(PKG_NAME)_aspker.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

$(F_SUB2):
	$(TAR_OUT) $(PKG_NAME)_atlib.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

$(F_SUB3):
	$(TAR_OUT) $(PKG_NAME)_goodies.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

$(F_SUB4) $(F_MON4):
	$(TAR_OUT) $(PKG_NAME)_io2ro.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

$(F_SUB5):
	$(TAR_OUT) $(PKG_NAME)_kergen.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

$(F_SUB6):
	$(TAR_OUT) $(PKG_NAME)_kersub.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

$(F_SUB7):
	$(TAR_OUT) $(PKG_NAME)_mapgl.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

$(F_SUB8):
	$(TAR_OUT) $(PKG_NAME)_misc.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

$(F_SUB9) $(F_MON9):
	$(TAR_OUT) $(PKG_NAME)_obsrap.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

$(F_SUBA) $(I_SUBA) $(F_MONA):
	$(TAR_OUT) $(PKG_NAME)_plt2d.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

$(F_SUBB) $(F_MONB):
	$(TAR_OUT) $(PKG_NAME)_polrap.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

$(F_SUBC):
	$(TAR_OUT) $(PKG_NAME)_rapgen.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

$(F_SUBD) $(F_MOND):
	$(TAR_OUT) $(PKG_NAME)_rapi2d.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

$(F_SUBE):
	$(TAR_OUT) $(PKG_NAME)_rapsub.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

$(F_SUBF):
	$(TAR_OUT) $(PKG_NAME)_trace.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi




# data files

$(D_SUBX):
	$(TAR_OUT) $(PKG_NAME)_sdf.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

$(D_SUBY):
	$(TAR_OUT) $(PKG_NAME)_dat.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

$(D_SUBZ): 
	$(TAR_OUT) $(PKG_NAME)_lists.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

# interface files

$(IFL_FILES):
	$(TAR_OUT) $(PKG_NAME)_ifl.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

# ICL files

$(ICL_SCRIPTS):
	$(TAR_OUT) $(PKG_NAME)_icl.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

# scripts

$(STARTUP_SCRIPT):
	$(TAR_OUT) $(PKG_NAME)_scripts.tar $@
	@ if test -f $@; then :;\
              else echo $@ is not in the tar file; exit 1; fi

#  Rules for extracting platform specific source files from the source
#  archives.

#  It is quite likely that there are no platform-specific source files
#  and that the macro PF_SUBn is empty. To prevent a syntax error in
#  make, a dummy target is present.

#$(PF_SUB1) dummy_target1:
#	$(TAR_OUT) $(PKG_NAME)_sub1.tar $@_$(SOURCE_VARIANT)
#	@ if test -f $@_$(SOURCE_VARIANT); then :;\
#           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
#	mv $@_$(SOURCE_VARIANT) $@

#$(PF_SUB2) dummy_target2:
#	$(TAR_OUT) $(PKG_NAME)_sub2.tar $@_$(SOURCE_VARIANT)
#	@ if test -f $@_$(SOURCE_VARIANT); then :;\
#           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
#	mv $@_$(SOURCE_VARIANT) $@

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

#OBJECT_FILES = $(O_SUB1) $(O_SUB2) $(MONO_OBJECT)
#O_SUB1 = $(B_SUB1:.f=.o) $(F_SUB1:.f=.o) $(PF_SUB1:.f=.o) $(C_SUB1:.c=.o)
#O_SUB2 = $(B_SUB2:.f=.o) $(F_SUB2:.f=.o) $(PF_SUB2:.f=.o) $(C_SUB2:.c=.o)
#MONO_OBJECT = $(MONO_SOURCE:.f=.o)

OBJECT_FILES = $(O_SUB0) $(O_SUB1) $(O_SUB2) $(O_SUB3) $(O_SUB4) $(O_SUB5)\
$(O_SUB6) $(O_SUB7) $(O_SUB8) $(O_SUB9) $(O_SUBA) $(O_SUBB)\
$(O_SUBC) $(O_SUBD) $(O_SUBE) $(O_SUBF) $(MONO_OBJECT)


O_SUB0 = $(F_SUB0:.f=.o)
O_SUB1 = $(F_SUB1:.f=.o)
O_SUB2 = $(F_SUB2:.f=.o)
O_SUB3 = $(F_SUB3:.f=.o)
O_SUB4 = $(F_SUB4:.f=.o)
O_SUB5 = $(F_SUB5:.f=.o)
O_SUB6 = $(F_SUB6:.f=.o)
O_SUB7 = $(F_SUB7:.f=.o)
O_SUB8 = $(F_SUB8:.f=.o)
O_SUB9 = $(F_SUB9:.f=.o)
O_SUBA = $(F_SUBA:.f=.o)
O_SUBB = $(F_SUBB:.f=.o)
O_SUBC = $(F_SUBC:.f=.o)
O_SUBD = $(F_SUBD:.f=.o)
O_SUBE = $(F_SUBE:.f=.o)
O_SUBF = $(F_SUBF:.f=.o)



MONO_OBJECT = $(MONO_SOURCE:.f=.o)

#.c.o:
#	$(CC) $(CFLAGS) -c $<

.f.o:
	$(FC) $(FFLAGS) -c $<

#  List of object library files to be built and rules for building
#  them.
#OBJECT_LIBRARIES = libsub1.a libsub2.a
# libsub1.a: $(O_SUB1)
#	$(AR_IN) $@ $?
#	$(RANLIB) $@
# libsub2.a: $(O_SUB2)
#	$(AR_IN) $@ $?
#	$(RANLIB) $@

OBJECT_LIBRARIES = libaif.a libaspker.a libatlib.a libgoodies.a \
libio2ro.a libkergen.a libkersub.a libmapgl.a libmisc.a libobsrap.a \
libplt2d.a libpolrap.a librapgen.a librapi2d.a librapsub.a \
libtrace.a


libaif.a: $(O_SUB0)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libaspker.a: $(O_SUB1)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libatlib.a: $(O_SUB2)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libgoodies.a: $(O_SUB3)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libio2ro.a: $(O_SUB4)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libkergen.a: $(O_SUB5)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libkersub.a: $(O_SUB6)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libmapgl.a: $(O_SUB7)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libmisc.a: $(O_SUB8)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libobsrap.a: $(O_SUB9)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libplt2d.a: $(O_SUBA)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libpolrap.a: $(O_SUBB)
	$(AR_IN) $@ $?
	$(RANLIB) $@

librapgen.a: $(O_SUBC)
	$(AR_IN) $@ $?
	$(RANLIB) $@

librapi2d.a: $(O_SUBD)
	$(AR_IN) $@ $?
	$(RANLIB) $@

librapsub.a: $(O_SUBE)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libtrace.a: $(O_SUBF)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  Rule and dependencies for building the monolith.
# $(MONOLITHS): $(MONO_OBJECT) $(OBJECT_LIBRARIES)
#	$(ALINK) $(MONO_OBJECT) -o $(MONOLITHS) $(OBJECT_LIBRARIES) \
#            -L$(STAR_LIB) `./$(LINK_SCRIPT)`

plt2d: plt2d.o libplt2d.a
	$(ILINK) plt2d.o -o $@ libplt2d.a libgoodies.a libatlib.a \
        -L$(STAR_LIB) `ndf_link_adam` `sgs_link_adam` \
        `fio_link_adam` `gks_link_adam` $(LDFLAGS)      
 
rapi2d: rapi2d.o librapi2d.a
	$(ALINK) rapi2d.o -o $@ librapi2d.a libaspker.a libatlib.a \
        libaif.a libtrace.a librapsub.a librapgen.a libkersub.a \
        libkergen.a `ndf_link_adam` `fio_link_adam` -L$(STAR_LIB) \
        -lnag $(LDFLAGS)
 
obsrap: obsrap.o libobsrap.a
	$(ALINK) obsrap.o -o $@ libobsrap.a librapi2d.a libplt2d.a \
        libaspker.a libatlib.a libaif.a librapsub.a librapgen.a \
        libkersub.a libkergen.a `ndf_link_adam` `fio_link_adam` \
        `mag_link_adam` -L$(STAR_LIB) -lnag $(LDFLAGS)
 
polrap: polrap.o libpolrap.a
	$(ALINK) polrap.o -o $@ libpolrap.a libobsrap.a librapi2d.a \
        libplt2d.a libaspker.a libatlib.a libaif.a libtrace.a \
        librapsub.a librapgen.a libkersub.a libkergen.a \
        `ndf_link_adam` `fio_link_adam` -L$(STAR_LIB) -lnag $(LDFLAGS)
 
io2ro: io2ro.o libio2ro.a 
	$(ALINK) io2ro.o -o $@ libio2ro.a $(LDFLAGS)


#  Rules for building the help libraries.
#HELP_FILES = $(HELP_SOURCES:.hlp=.shl)
#.hlp.shl:
#	$(STAR_BIN)/hlib $<


##  Rules for compiling the interface files.
# IFC_FILES = $(IFL_FILES:.ifl=.ifc) $(MONO_IFL:.ifl=.ifc)

IFC_FILES = $(IFL_FILES:.ifl=.ifc)


.ifl.ifc:
	$(STAR_BIN)/compifl $<

#  Name of the date stamp file.  This is used to record the time of the
#  most recent build for use in subsequent operations that require it.
#  There must be no rule for generating this file; it is updated only
#  as a side effect of building the package.

DATE_STAMP = $(PKG_NAME)_datestamp

#  Pathnames of directories into which files may be placed when the
#  package is installed.
#INSTALL_BIN = $(INSTALL)/bin/$(PKG_NAME)
#INSTALL_DATES = $(INSTALL)/dates
#INSTALL_DOCS = $(INSTALL)/docs
#INSTALL_HELP = $(INSTALL)/help/$(PKG_NAME)

INSTALL_BIN = $(INSTALL)/bin/$(PKG_NAME)
INSTALL_DATES = $(INSTALL)/dates
INSTALL_DOCS = $(INSTALL)/docs


#  List of directories actually used for installation (selected from
#  those above) and rules to create them.
#INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_DOCS) $(INSTALL_HELP)

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_DOCS)

$(INSTALL_DIRS):
	mkdir -p $@

#------------------------------------------------------------------------------

#  Primary targets.
#  ===============
#
#  These are the targets intended for normal external use (apart from
#  help, which appears at the start of the file).

#  check: Check source file presence and show current state.
#  --------------------------------------------------------

check:
	@ echo
	@ echo \
   '*** This is $(PACK_NAME) version V$(PKG_VERS) on system $(SYSTEM)'
	@ echo
	@ nosource='';\
          for f in $(SOURCE_FILES); do \
             if test ! -f $$f; then \
                nosource='1';\
                break;\
             else :; fi;\
          done;\
          if test -n "$$nosource"; then \
             echo '    Source files are NOT present';\
          else \
             echo '    All essential source files are present';\
          fi
	@ echo
#
#  Display the current state.
	@ if test -f .BUILT;\
          then echo '    The package is currently:  built for system'\
             `cat .BUILT`;\
          else echo '    The package is currently:  not built';fi
	@ if test -f .INSTALLED_$(SYSTEM);\
          then echo '                               installed in'\
             `cat .INSTALLED_$(SYSTEM)`;\
          else echo '                               not installed';fi
	@ if test -f .TESTED_$(SYSTEM);\
          then echo '                               tested';\
          else echo '                               not tested';fi
	@ echo
	@ if test -f .BUILT;\
          then if test "$(SYSTEM)" != "`cat .BUILT`";\
             then echo '***  WARNING  ***';\
                echo \
'    The package is built for a system other than the current one';\
                echo ;\
             else :;fi;\
          else :;fi

#  build: Build the system.
#  -----------------------
#
#  Compile the source and build the required files in the source
#  directory.

#  The build target first checks that the package is not installed.  If
#  not, it then causes the .BUILT target to be made which ensures that
#  the package has been built.

build:
	@ if test -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo \
   '*** The $(PACK_NAME) package is currently installed -- please use the';\
           echo '    "deinstall" target before re-building it';\
           echo;\
        elif $(MAKE) .BUILT; then \
           echo;\
           echo '*** The $(PACK_NAME) package has been built';\
           echo;\
        else \
           echo;\
           echo '*** "make" failed building the $(PACK_NAME) package';\
           echo;\
           exit 1;\
        fi

#  The .BUILT target records the time of the most recent build which
#  modified any of the built files.  It depends on all the built files
#  being up to date (which causes them to be built).

.BUILT: $(BUILT_FILES)
#
#  Enter information about the current machine and build environment
#  into the date stamp file.
	@ echo 'Package : $(PACK_NAME)'        >$(DATE_STAMP)
	@ echo 'Version : V$(PKG_VERS)'       >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo "Built by: $(USER) on node `uname -n`" \
                                              >>$(DATE_STAMP)
	@ echo "On      : `date`"             >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo \
  "Machine : `uname -m` running `uname -s` `uname -v` (release `uname -r`)" \
                                              >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo 'make macros:'                 >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo '   SYSTEM  : $(SYSTEM)'       >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo '   EXPORT  : $(EXPORT)'       >>$(DATE_STAMP)
	@ echo '   INSTALL : $(INSTALL)'      >>$(DATE_STAMP)
	@ echo '   STARLINK: $(STARLINK)'     >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
	@ echo '   AR_IN   : $(AR_IN)'        >>$(DATE_STAMP)
	@ echo '   CC      : $(CC)'           >>$(DATE_STAMP)
	@ echo '   CFLAGS  : $(CFLAGS)'       >>$(DATE_STAMP)
	@ echo '   FC      : $(FC)'           >>$(DATE_STAMP)
	@ echo '   FFLAGS  : $(FFLAGS)'       >>$(DATE_STAMP)
	@ echo '   LINK    : $(LINK)'         >>$(DATE_STAMP)
	@ echo '   RANLIB  : $(RANLIB)'       >>$(DATE_STAMP)
	@ echo '   SOURCE_VARIANT: $(SOURCE_VARIANT)' \
                                              >>$(DATE_STAMP)
	@ echo '   TAR_IN  : $(TAR_IN)'       >>$(DATE_STAMP)
	@ echo '   TAR_OUT : $(TAR_OUT)'      >>$(DATE_STAMP)
	@ echo ''                             >>$(DATE_STAMP)
#
#  Record completion of the build.
	@ echo '$(SYSTEM)' > .BUILT

#  install: Install the package for use.
#  ------------------------------------
#
#  Copy the built files to their installation directories, from where
#  they may be accessed.

#  The install target first checks if any part of the package is
#  already installed.  If not, it checks that the system is built for this
#  SYSTEM and, if it is, causes the .INSTALLED_$(SYSTEM) target to be made
#  which performs the installation.

install:
	@ if test -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo \
  '*** The $(PACK_NAME) package has already been installed -- please use the';\
           echo \
   '    "deinstall" target first if you wish to reinstall it';\
           echo;\
        elif test -f .BUILT; then \
           if test "`cat .BUILT`" = "$(SYSTEM)"; then \
              if $(MAKE) .INSTALLED_$(SYSTEM); then \
                 echo;\
                 echo \
    '*** The $(PACK_NAME) package has been installed in directory $(INSTALL)';\
                 echo;\
              else \
                 echo;\
                 echo \
'*** "make" failed installing the $(PACK_NAME) package in dir $(INSTALL)';\
                 echo;\
                 exit 1;\
              fi;\
           else \
              echo;\
              echo \
   "*** The $(PACK_NAME) package is built for system `cat .BUILT` -"\
   'so cannot be installed on system $(SYSTEM)';\
              echo;\
              exit 1;\
           fi;\
        else \
           echo;\
           echo \
   '*** The $(PACK_NAME) package is not built, so cannot be installed';\
           echo;\
           exit 1;\
        fi

#  The .INSTALLED_$(SYSTEM) target copies each file from the source
#  directory using "cp -p" to preserve its date, and replaces each
#  original file by a link to the installed copy.

.INSTALLED_$(SYSTEM): $(INSTALL_DIRS)

#
#  Create .INSTALLED_$(SYSTEM), containing $INSTALL, to record that the 
#  package is installed (at least in part).
	@ echo $(INSTALL) > .INSTALLED_$(SYSTEM)
#
#  Install the public scripts, giving them world execute permission.

	for f in $(PUBLIC_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the monolith, giving it world execute permission.
	for f in $(MONOLITHS) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the interface files, giving them world read permission.
	for f in $(IFC_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the data files, giving them world read permission.
	for f in $(DATA_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the help files, giving them world read permission.

	for f in $(HELP_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_HELP);\
              chmod 644 $(INSTALL_HELP)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_HELP)/$$f $$f;\
           else :; fi;\
       done
##
#  Create the application/task name soft links.

	for f in $(TASKS) ""; do \
           if test -n "$$f"; then \
              $(LINK) ./$(MONOLITHS) $(INSTALL_BIN)/$$f ;\
           else :; fi;\
        done

#
#  Install the package startup script.  The package version number,
#  the name of the directory containing the installed binaries and 
#  interface files, and the name of the directory containing the help
#  libraries must be edited into this, and execute permission given.
#  Leave the original file in place.

	if test -n "$(STARTUP_SCRIPT)"; then \
           sed -e s%INSTALL_BIN%$(INSTALL_BIN)% \
               -e s%INSTALL_HELP%$(INSTALL_HELP)% \
               -e s%PKG_VERS%$(PKG_VERS)% \
              $(STARTUP_SCRIPT) >$(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
           chmod 755 $(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
        else :; fi

#
#  Install the package ICL script.  The package version number must be 
#  edited into this, and read permission given.  Leave the original file
#  in place.
#	if test -n "$(ICL_SCRIPTS)"; then \
#           sed -e s%PKG_VERS%$(PKG_VERS)% \
#              $(ICL_SCRIPTS) >$(INSTALL_BIN)/$(ICL_SCRIPTS) ;\
#           chmod 755 $(INSTALL_BIN)/$(ICL_SCRIPTS) ;\
#        else :; fi

# more than one ICL script in IRCAMDR

	for f in $(ICL_SCRIPTS) ""; do \
         if test -n "$$f"; then \
           sed -e s%INSTALL_BIN%$(INSTALL_BIN)% \
               -e s%INSTALL_HELP%$(INSTALL_HELP)% \
               -e s%PKG_VERS%$(PKG_VERS)% \
              $$f >$(INSTALL_BIN)/$$f ;\
           chmod 755 $(INSTALL_BIN)/$$f ;\
         else :; fi;\
        done


#
#  Install the Latex documentation, giving it world read permission,
#  leaving the source copy in place.
	for f in $(LATEX_DOCS) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_DOCS);\
              chmod 644 $(INSTALL_DOCS)/$$f;\
           else :; fi;\
        done
#
#  Install any hypertext documents, giving world read access to all the files
#  they contain and linking with other documents.
	if test -n "$(HYPERTEXT_DOCS)"; then \
           pwd=`pwd`;\
           (cd $(INSTALL_DOCS);\
           for f in $(HYPERTEXT_DOCS) ""; do \
              if test -n "$$f"; then\
                 $(TAR_OUT) $$pwd/$${f}_tar;\
                 chmod 755 `find $$f -type d -print`;\
                 chmod 644 `find $$f ! -type d -print`;\
                 touch $$f;\
              else :; fi;\
           done);\
           HTX_PATH='$(STAR_DOCS):$(STAR_HELP)';\
           export HTX_PATH;\
           $(STAR_BIN)/hlink $(INSTALL_DOCS) $(INSTALL_HELP);\
        fi;
#
#  Install the date stamp file and make it read-only to prevent its
#  date being changed.
	cp -p $(DATE_STAMP) $(INSTALL_DATES)
	chmod 444 $(INSTALL_DATES)/$(DATE_STAMP)
	chmod 644 $(DATE_STAMP)
	rm $(DATE_STAMP)
	$(LINK) $(INSTALL_DATES)/$(DATE_STAMP) $(DATE_STAMP)

#  deinstall: Deinstall the package.
#  --------------------------------
#
#  Reverse the action of the install target, removing the installed
#  files and returning them to the source directory.

#  The deinstall target checks that the package is installed in the INSTALL
#  directory.  If so, it causes the do_deinstall target to be made which 
#  performs the deinstallation.

deinstall:
	@ if test ! -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo '*** The $(PACK_NAME) package is not currently installed';\
           echo;\
        else \
           if test "`cat .INSTALLED_$(SYSTEM)`" = "$(INSTALL)"; then \
              if $(MAKE) do_deinstall; then \
                 echo;\
                 echo \
'*** The $(PACK_NAME) package has been deinstalled from directory $(INSTALL)';\
                 echo;\
              else \
                 echo;\
                 echo \
'*** "make" failed deinstalling the $(PACK_NAME) package from dir $(INSTALL)';\
                 echo;\
                 exit 1;\
              fi;\
           else \
              echo;\
              echo \
"*** The $(PACK_NAME) package is installed in `cat .INSTALLED_$(SYSTEM)`";\
              echo \
"*** and not in your INSTALL directory ($(INSTALL))";\
              echo '*** Not deinstalled';\
              exit 1;\
           fi;\
        fi

#  The do_deinstall target (which should never exist) checks that an
#  installed version of each file exists (in case an install failed
#  part of the way through) and returns it to the source directory,
#  using "cp -p" to preserve file dates.  Links are removed from the
#  source directory before copying.

do_deinstall:
#
#  Note the package will need to be tested again.
	@- if test -f .TESTED_$(SYSTEM); then rm -f .TESTED_$(SYSTEM); else :; fi
#
#  Deinstall the public script files, if installed versions exist.
	- for f in $(PUBLIC_SCRIPTS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the monolith, if an installed version exists.
	- for f in $(MONOLITHS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the interface files, if installed versions exist.
	- for f in $(IFC_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the data files, if installed versions exist.
	- for f in $(DATA_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the help files, if installed versions exist.
	- for f in $(HELP_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_HELP)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_HELP)/$$f .;\
              rm -f $(INSTALL_HELP)/$$f;\
           else :; fi;\
        done
#
#  Remove the application/task soft links.
	- for f in $(TASKS) ""; do \
             if test -n "$$f"; then \
                rm -f $(INSTALL_BIN)/$$f ;\
           else :; fi;\
        done


# Martin, read this:
#
# The make lines have been ommited in the following two sections. As far
# as I can see after deinstall there should be nothing left in /star/bin/ircamdr
# rather than a bunch of icl scripts. I can maybe see the point of re-extracting
# them back into /star/ircamdr but that's not what ends up happening !
#
# - frossie
#
#  Deinstall the package startup file.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
#	- if test -n "$(STARTUP_SCRIPT)"; then \
#           if test -f $(INSTALL_BIN)/$(STARTUP_SCRIPT); then\
#              rm -f $(INSTALL_BIN)/$(STARTUP_SCRIPT);\
#           else :; fi ;\
#           $(MAKE) $(STARTUP_SCRIPT);\
#        else :; fi

	- if test -n "$(STARTUP_SCRIPT)"; then \
           if test -f $(INSTALL_BIN)/$(STARTUP_SCRIPT); then\
              rm -f $(INSTALL_BIN)/$(STARTUP_SCRIPT);\
           else :; fi ;\
        else :; fi
#
#  Deinstall the package ICL file.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
#	- if test -n "$(ICL_SCRIPTS)"; then \
#           if test -f $(INSTALL_BIN)/$(ICL_SCRIPTS); then\
#              rm -f $(INSTALL_BIN)/$(ICL_SCRIPTS);\
#           else :; fi ;\
#          $(MAKE) $(ICL_SCRIPTS);\
#        else :; fi

	- for f in $(ICL_SCRIPTS) "" do \
        if test -n "$(ICL_SCRIPTS)"; then \
           if test -f $(INSTALL_BIN)/$(ICL_SCRIPTS); then\
              rm -f $(INSTALL_BIN)/$(ICL_SCRIPTS);\
           else :; fi ;\
        else :; fi;\
       done

#
#  Deinstall the Latex documentation, if installed versions exist.
	- for f in $(LATEX_DOCS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_DOCS)/$$f; then \
              rm -f $(INSTALL_DOCS)/$$f;\
           else :; fi;\
        done
#
#  Deinstall any hypertext documents,  and relink the hypertext if required.
	- if test -n "$(HYPERTEXT_DOCS)"; then \
           for f in $(HYPERTEXT_DOCS) ""; do \
              if test -n "$$f" -a -d $(INSTALL_DOCS)/$$f; then \
                 rm -f -r $(INSTALL_DOCS)/$$f;\
              else :; fi;\
           done;\
           HTX_PATH='$(STAR_DOCS):$(STAR_HELP)';\
           export HTX_PATH;\
           $(STAR_BIN)/hlink $(INSTALL_DOCS) $(INSTALL_HELP);\
        fi
#
#  Deinstall the date stamp file after setting its protection so it may
#  be removed.
	- if test -f $(INSTALL_DATES)/$(DATE_STAMP); then \
           chmod 644 $(DATE_STAMP); rm $(DATE_STAMP);\
           chmod 644 $(INSTALL_DATES)/$(DATE_STAMP);\
           cp -p $(INSTALL_DATES)/$(DATE_STAMP) .;\
           rm $(INSTALL_DATES)/$(DATE_STAMP);\
        else :; fi
#
#  Note the system is no longer installed.  Re-create .BUILT, since we have
#  returned the built files to the source directory but an unbuild may have
#  been done.
	@- rm -f .INSTALLD_$(SYSTEM) 1>/dev/null 2>/dev/null
	@ echo '$(SYSTEM)' > .BUILT

#  test: Perform an installation test.
#  ----------------------------------
#
#  Check that installed files are in their correct places and that a
#  simple test program will run correctly.

#  The test target checks that the package is currently installed.  If
#  so, it causes the do_test target to be made, which performs the
#  installation test.

test:
	@ if test ! -f .INSTALLED_$(SYSTEM); then \
           echo;\
           echo '*** The $(PACK_NAME) package is not currently installed';\
           echo;\
        elif $(MAKE) do_test; then\
           echo;\
           echo \
           '*** Installation test for the $(PACK_NAME) package has been run';\
           echo;\
        else \
           echo;\
           echo \
           '*** Installation test for the $(PACK_NAME) package failed';\
           echo;\
           exit 1;\
        fi

#  The do_test target performs the installation test. A file named do_test
#  should never exist.

do_test:
#
#  Note the test has not yet succeeded.
	@- if test -f .TESTED_$(SYSTEM); then rm -f .TESTED_$(SYSTEM); else :; fi
#
	@ echo
	@ echo '$(PACK_NAME) does not have a make driven installation test'
	@ echo
	@ echo 'To test $(PACK_NAME), you should run the demonstation script,'
	@ echo 'thus (% is the shell prompt):'
	@ echo
	@ echo ' % cd /empty_directory'
	@ echo ' % source /star/etc/cshrc'
	@ echo ' % xdisplay'
	@ echo ' % $(PKG_NAME)setup'
	@ echo
#
#  Note the test has been run.
	@ touch .TESTED_$(SYSTEM)

#  export: Export the installed system.
#  -----------------------------------
#
#  Export the source plus all the built files to a new user.

#  The export target depends on the resulting compressed tar file being
#  up to date.

export: $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z
	@ echo
	@ echo \
'*** Export copy of the built $(PACK_NAME) package is in the compressed'
	@ echo \
'    tar file $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z'
	@ echo

#  The compressed tar file is up to date if it exists and is more
#  recent than all the source files and the date stamp file (which
#  records the time of the last build which modified any files).

 $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z: $(SOURCE_FILES) $(DATE_STAMP)
#
#  Issue a warning if the package has not been tested.
	@ if test ! -f .TESTED_$(SYSTEM); then \
           echo;\
           echo '*** Warning: the $(PACK_NAME) package has not been tested';\
           echo;\
        else :; fi
#
#  Remove any pre-existing tar files before creating new ones.
	if test -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z; then \
           rm -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z; else :; fi
	$(TAR_IN) - $(SOURCE_FILES) $(BUILT_FILES) $(DATE_STAMP) .BUILT \
           | compress -v > $(EXPORT)/$(PKG_NAME)_$(SYSTEM).tar.Z

#  export_run: Export the built system (without source).
#  -----------------------------------------------------
#
#  Export all the built files to a new user.

#  The export_run target depends on the resulting compressed tar file being
#  up to date.

export_run: $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z
	@ echo
	@ echo \
'*** Export copy of the "runtime" $(PACK_NAME) package is in the compressed'
	@ echo \
'    tar file $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z'
	@ echo

#  The compressed tar file is up to date if it exists and is more
#  recent than all the source files and the date stamp file (which
#  records the time of the last build which modified any files).

$(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z: $(SOURCE_FILES) $(DATE_STAMP)
#
#  Issue a warning if the package has not been tested.
	@ if test ! -f .TESTED_$(SYSTEM); then \
           echo;\
           echo '*** Warning: the $(PACK_NAME) package has not been tested';\
           echo;\
        else :; fi
#
#  Remove any pre-existing tar files before creating new ones.
	if test -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z; then \
           rm -f $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z; else :; fi
	$(TAR_IN) - mk makefile $(DOCUMENTATION) $(BUILT_FILES) \
           $(DATE_STAMP) .BUILT \
           | compress -v > $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_run.tar.Z

#  export_source: Export the source.
#  --------------------------------
#
#  Export the source files only to a new user.

#  This target depends on the resulting compressed tar file being up to
#  date.

export_source: $(EXPORT)/$(PKG_NAME).tar.Z
	@ echo
	@ echo \
'*** Export copy of the $(PACK_NAME) package source is in the compressed'
	@ echo \
'    tar file $(EXPORT)/$(PKG_NAME).tar.Z'
	@ echo

#  The compressed tar file is up to date if it exists and is more
#  recent than all the source files.

$(EXPORT)/$(PKG_NAME).tar.Z: $(SOURCE_FILES)
#
#  Remove any pre-existing tar files before creating new ones.
	if test -f $(EXPORT)/$(PKG_NAME).tar.Z; then \
           rm -f $(EXPORT)/$(PKG_NAME).tar.Z; else :; fi
	$(TAR_IN) - $(SOURCE_FILES) \
           | compress -v > $(EXPORT)/$(PKG_NAME).tar.Z

#  clean: Clean up the source directory.
#  ------------------------------------
#
#  Remove all intermediate files.  Do not remove built files.

clean:
	@- rm -f $(INCLUDE_LINKS) $(PRIVATE_INCLUDES) $(IFL_FILES) \
              $(MONO_IFL) $(HELP_SOURCES) $(PLATFORM_F) $(MONO_SOURCE) \
              $(C_ROUTINES) $(LINK_SCRIPT) 1>/dev/null 2>/dev/null
	@- rm -f $(F_ROUTINES) 1>/dev/null 2>/dev/null
	@- rm -f $(OBJECT_FILES) 1>/dev/null 2>/dev/null
	@ echo
	@ echo '*** Intermediate files removed'
	@ echo

#  unbuild: Reverse the build process.
#  ----------------------------------

#  Remove all intermediate files and all built files, and note that the
#  package is no longer built or tested.

unbuild: clean
	@- rm -f $(BUILT_FILES) $(DATE_STAMP) .BUILT 1>/dev/null 2>/dev/null
	@ echo '*** Built files removed'
	@ echo

#  strip: strip the monoliths of debug information.
#  -----------------------------------------------

strip:
	strip $(MONOLITHS)
	@ echo '*** Monoliths stripped'

#------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files).  These are normally generated automatically from the source
#  files.

# $(PKG_NAME)_subroutine.o:       SAE_PAR

ab_translate_env.o: SAE_PAR
abcom.o: SAE_PAR NDF_PAR NDF_ERR
absep.o: SAE_PAR NDF_PAR NDF_ERR
access_pixel.o: PLT2DCOM
add.o: SAE_PAR NDF_PAR NDF_ERR
addarr2d.o: SAE_PAR
addline.o: SAE_PAR NDF_PAR NDF_ERR
addlinesub.o: SAE_PAR
addsca2d.o: SAE_PAR
aif_choic.o: SAE_PAR PAR_ERR
aif_exacc.o: SAE_PAR
aif_exacd.o: SAE_PAR
aif_exaci.o: SAE_PAR
aif_exacl.o: SAE_PAR
aif_exacr.o: SAE_PAR
aif_fillvb.o: SAE_PAR
aif_fillvc.o: SAE_PAR
aif_fillvd.o: SAE_PAR
aif_fillvi.o: SAE_PAR
aif_fillvl.o: SAE_PAR
aif_fillvr.o: SAE_PAR
aif_fillvub.o: SAE_PAR
aif_fillvuw.o: SAE_PAR
aif_fillvw.o: SAE_PAR
aif_get0c.o: SAE_PAR PAR_ERR
aif_get0d.o: SAE_PAR PAR_ERR
aif_get0i.o: SAE_PAR PAR_ERR
aif_get0l.o: SAE_PAR PAR_ERR
aif_get0r.o: SAE_PAR PAR_ERR
aif_get1c.o: SAE_PAR
aif_get1d.o: SAE_PAR
aif_get1i.o: SAE_PAR
aif_get1l.o: SAE_PAR
aif_get1r.o: SAE_PAR
aif_geven.o: SAE_PAR PAR_ERR
aif_goddn.o: SAE_PAR PAR_ERR
aif_mmvd.o: SAE_PAR
aif_mmvi.o: SAE_PAR
aif_mmvr.o: SAE_PAR
aif_mmvuw.o: SAE_PAR
aif_mmvw.o: SAE_PAR
aif_scalvb.o: SAE_PAR
aif_scalvd.o: SAE_PAR
aif_scalvi.o: SAE_PAR
aif_scalvl.o: SAE_PAR
aif_scalvr.o: SAE_PAR
aif_scalvub.o: SAE_PAR
aif_scalvuw.o: SAE_PAR
aif_scalvw.o: SAE_PAR
aif_sort1b.o: SAE_PAR
aif_sort1c.o: SAE_PAR
aif_sort1d.o: SAE_PAR
aif_sort1i.o: SAE_PAR
aif_sort1l.o: SAE_PAR
aif_sort1r.o: SAE_PAR
aif_sort1ub.o: SAE_PAR
aif_sort1uw.o: SAE_PAR
aif_sort1w.o: SAE_PAR
amcorr.o: SAE_PAR NDF_PAR NDF_ERR CHR_ERR FIO_PAR
amcorrsub.o: SAE_PAR
annstats.o: SAE_PAR NDF_PAR NDF_ERR
annstatssub.o: SAE_PAR CHR_ERR FIO_PAR
anntestsub.o: SAE_PAR FIO_PAR
aperadd.o: SAE_PAR NDF_PAR NDF_ERR
aperaddsub.o: SAE_PAR
aperphot.o: SAE_PAR NDF_PAR NDF_ERR FIO_PAR CHR_ERR
aperphotsub.o: SAE_PAR
aperpol.o: SAE_PAR NDF_PAR NDF_ERR CHR_ERR FIO_PAR
appg0c.o: SAE_PAR PAR_ERR
appg0i.o: SAE_PAR PAR_ERR
appg0r.o: SAE_PAR PAR_ERR
appgir.o: SAE_PAR PAR_ERR
applymask.o: SAE_PAR NDF_PAR NDF_ERR
applymasksub.o: SAE_PAR
arelem.o: SAE_PAR DAT_PAR
arrstr.o: SAE_PAR DAT_PAR
asciifile.o: ADAMDEFNS ADAMERRS FIO_PAR CHR_ERR
asciilist.o: SAE_PAR NDF_PAR NDF_ERR FIO_PAR
asciilistsub.o: SAE_PAR
automos.o: SAE_PAR NDF_PAR NDF_ERR CHR_ERR FIO_PAR
automos_apply.o: SAE_PAR
binup.o: SAE_PAR NDF_PAR NDF_ERR
block.o: SAE_PAR
block_numbers.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS SAE_PAR CHR_ERR PLT2DCOM
block_ticks.o: PLT2DCOM
cadd.o: SAE_PAR NDF_PAR NDF_ERR
calcol.o: SAE_PAR NDF_PAR NDF_ERR
calcolsub.o: SAE_PAR
calculate_maxmin.o: PLT2DCOM
cdiv.o: SAE_PAR NDF_PAR NDF_ERR
centroid.o: SAE_PAR NDF_PAR NDF_ERR
centroid_locate.o: SAE_PAR
chpix.o: SAE_PAR NDF_PAR NDF_ERR
chpixsub.o: SAE_PAR
clean.o: SAE_PAR NDF_PAR NDF_ERR
clear_area.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS SAE_PAR NDF_PAR NDF_ERR PLT2DCOM
clear_screen.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS PLT2DCOM
cmult.o: SAE_PAR NDF_PAR NDF_ERR
colcycle.o: SAE_PAR NDF_PAR NDF_ERR
colcyclesub.o: SAE_PAR
colmed.o: SAE_PAR NDF_PAR NDF_ERR
colmedsub.o: SAE_PAR
colour_block.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS NDF_PAR NDF_ERR SAE_PAR CHR_ERR PLT2DCOM
colour_blocksub.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS PLT2DCOM
colour_table.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS NDF_PAR NDF_ERR SAE_PAR
column_label.o: CHR_ERR
cominf.o: SAE_PAR DAT_PAR
compadd.o: SAE_PAR NDF_PAR NDF_ERR
compaddsub.o: SAE_PAR
compave.o: SAE_PAR NDF_PAR NDF_ERR
compavesub.o: SAE_PAR
compick.o: SAE_PAR NDF_PAR NDF_ERR
compicksub.o: SAE_PAR
compress.o: SAE_PAR NDF_PAR NDF_ERR
compresssub.o: SAE_PAR
contour_annot.o: PLT2DCOM SAE_PAR CHR_ERR
contour_draw.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS CHR_ERR PLT2DCOM
contour_map.o: PLT2DCOM
contour_numbers.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS CHR_ERR PLT2DCOM
contour_plot.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS NDF_PAR NDF_ERR SAE_PAR PLT2DCOM
contour_ticks.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS PLT2DCOM
contour_ticksec.o: PLT2DCOM
convert_local2ut.o: CHR_ERR SAE_PAR
convert_values.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS SAE_PAR PLT2DCOM
copfile.o: SAE_PAR
copy1d.o: SAE_PAR
copy2d.o: SAE_PAR
copy3d.o: SAE_PAR
copy_image.o: PLT2DCOM
copy_sub.o: PLT2DCOM
copyab.o: SAE_PAR
copyab_comb.o: SAE_PAR
crecolt.o: SAE_PAR NDF_PAR NDF_ERR
creframe.o: SAE_PAR NDF_PAR NDF_ERR
creframesub.o: SAE_PAR
creout.o: SAE_PAR PAR_ERR NDF_PAR NDF_ERR
crequilt.o: SAE_PAR FIO_PAR
csfit.o: SAE_PAR NDF_PAR NDF_ERR
csfitsub.o: FIO_PAR SAE_PAR
csgen.o: SAE_PAR NDF_PAR NDF_ERR
csgensub.o: SAE_PAR
csub.o: SAE_PAR NDF_PAR NDF_ERR
cursor_display.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS SAE_PAR PLT2DCOM
cursor_mark.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
cursor_position.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS SAE_PAR PLT2DCOM
cursor_values.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS SAE_PAR NDF_PAR NDF_ERR PLT2DCOM
cut_display.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS NDF_PAR NDF_ERR SAE_PAR CHR_ERR PLT2DCOM
cut_lineplot.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS CHR_ERR FIO_ERR PLT2DCOM
cut_maxmin.o: PLT2DCOM
cut_numbers.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS CHR_ERR PLT2DCOM
cut_ticks.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS PLT2DCOM
decode_label.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS SAE_PAR CHR_ERR
default_annotation.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
defgrad.o: SAE_PAR NDF_PAR NDF_ERR CHR_ERR
defgradsub.o: SAE_PAR
delfile.o: SAE_PAR
devfcs.o: SAE_PAR NDF_PAR NDF_ERR
devfcssub.o: SAE_PAR
device_close.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
device_code.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
device_open.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS GKS_PAR PLT2DCOM
dist.o: SAE_PAR
div2d.o: SAE_PAR NDF_PAR NDF_ERR
divarr2d.o: SAE_PAR
divsca2d.o: SAE_PAR
exparr2d.o: SAE_PAR PRM_PAR
expon.o: SAE_PAR NDF_PAR NDF_ERR
fcoadd.o: SAE_PAR NDF_PAR NDF_ERR CHR_ERR
figndf.o: SAE_PAR NDF_PAR NDF_ERR PRM_PAR
find_median.o: SAE_PAR
findpeak.o: SAE_PAR NDF_PAR NDF_ERR
findpeaksub.o: SAE_PAR
fits_val.o: SAE_PAR CHR_ERR
fitswget.o: SAE_PAR DAT_PAR
fitswget2.o: SAE_PAR
fitswhead.o: CHR_ERR
fitswm.o: SAE_PAR DAT_PAR FIO_PAR CHR_ERR
flash_scale.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS PLT2DCOM
flip.o: SAE_PAR NDF_PAR NDF_ERR
flips.o: SAE_PAR
gauss.o: SAE_PAR NDF_PAR NDF_ERR
gaussth.o: SAE_PAR NDF_PAR NDF_ERR
genhis.o: SAE_PAR
get_word.o: ADAMERRS CHR_ERR SAE_PAR
getinp.o: SAE_PAR
glitch.o: SAE_PAR NDF_PAR NDF_ERR
glitch_mark.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS SAE_PAR FIO_ERR PLT2DCOM
glitch_values.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS SAE_PAR NDF_PAR NDF_ERR PLT2DCOM
glitchauto.o: SAE_PAR
glitchlist.o: SAE_PAR FIO_PAR
glitchsub.o: SAE_PAR
hdscreate_setprim.o: SAE_PAR
histeq.o: SAE_PAR NDF_PAR NDF_ERR
histeqsub.o: SAE_PAR
histgen.o: SAE_PAR NDF_PAR NDF_ERR
histo.o: SAE_PAR NDF_PAR NDF_ERR
histprop.o: SAE_PAR
hotshot.o: SAE_PAR NDF_PAR NDF_ERR CHR_ERR
image_clear.o: ADAMDEFNS ADAMERRS NDF_PAR NDF_ERR DTDEFNS DTERRS SAE_PAR PLT2DCOM
image_display.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS NDF_PAR NDF_ERR SAE_PAR PLT2DCOM
image_scale.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS PLT2DCOM
image_setval.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS PLT2DCOM
imcomb.o: SAE_PAR NDF_PAR NDF_ERR
imcombsub.o: SAE_PAR
index.o: SAE_PAR DAT_PAR FIO_PAR CHR_ERR
inqfile.o: SAE_PAR
inset.o: SAE_PAR
insetb.o: SAE_PAR NDF_PAR NDF_ERR PRM_PAR
insetbsub.o: SAE_PAR
insetc.o: SAE_PAR NDF_PAR NDF_ERR PRM_PAR
insetsub.o: SAE_PAR
int_to_str.o: SAE_PAR CHR_ERR
integer_picture.o: CHR_ERR
intlk.o: SAE_PAR NDF_PAR NDF_ERR
io2ro.o: SAE_PAR PAR_PAR CHR_ERR
kfh_adarar.o: SAE_PAR
kfh_adarsc.o: SAE_PAR
kfh_aplot.o: SAE_PAR
kfh_arrcre.o: SAE_PAR
kfh_bttns.o: SAE_PAR
kfh_butvl.o: SAE_PAR
kfh_cross.o: SAE_PAR
kfh_dfaint.o: SAE_PAR
kfh_dflash.o: SAE_PAR
kfh_diarar.o: SAE_PAR
kfh_diarsc.o: SAE_PAR
kfh_dimscl.o: SAE_PAR
kfh_discar.o: SAE_PAR
kfh_drcur.o: SAE_PAR
kfh_expar.o: SAE_PAR PRM_PAR
kfh_faint.o: SAE_PAR
kfh_fill.o: SAE_PAR NDF_PAR NDF_ERR
kfh_flash.o: SAE_PAR
kfh_gtcol.o: SAE_PAR
kfh_hist.o: SAE_PAR
kfh_histogram.o: SAE_PAR
kfh_histst.o: SAE_PAR NDF_PAR NDF_ERR
kfh_hstrep.o: SAE_PAR
kfh_imscl.o: SAE_PAR
kfh_logar.o: SAE_PAR
kfh_lutwk.o: SAE_PAR
kfh_move.o: SAE_PAR
kfh_muarar.o: SAE_PAR
kfh_muarsc.o: SAE_PAR
kfh_nhplot.o: SAE_PAR
kfh_nlplot.o: SAE_PAR
kfh_nwlinplot.o: SAE_PAR
kfh_nwlist.o: SAE_PAR CHR_ERR
kfh_nwpeep.o: SAE_PAR
kfh_nwslice.o: SAE_PAR
kfh_outrn.o: SAE_PAR
kfh_power.o: SAE_PAR
kfh_repval.o: SAE_PAR
kfh_rgcopy.o: SAE_PAR
kfh_stats.o: SAE_PAR
kfh_suarar.o: SAE_PAR
kfh_suarsc.o: SAE_PAR
kfh_trans.o: SAE_PAR
kfh_transfer.o: SAE_PAR
kfh_transfer2.o: SAE_PAR
kfh_trnfr.o: SAE_PAR
kfh_write.o: SAE_PAR
laplace.o: SAE_PAR NDF_PAR NDF_ERR
laplacesub.o: SAE_PAR
lincont.o: SAE_PAR DAT_PAR DAT_ERR FIO_PAR CHR_ERR
lincont_lin.o: SAE_PAR
lincont_ndr_lin.o: SAE_PAR
linimag_ndr.o: SAE_PAR DAT_PAR DAT_ERR CHR_ERR FIO_PAR
linimag_ndr_lin.o: SAE_PAR
listsub.o: SAE_PAR FIO_PAR
locinf.o: SAE_PAR DAT_PAR
logar.o: SAE_PAR NDF_PAR NDF_ERR
logarr2d.o: SAE_PAR
look.o: SAE_PAR NDF_PAR NDF_ERR
looker.o: SAE_PAR
lowcase.o: SAE_PAR
ma1to2.o: SAE_PAR
ma1to3.o: SAE_PAR
ma2to1.o: SAE_PAR
ma2to3.o: SAE_PAR
ma3to1.o: SAE_PAR
ma3to2.o: SAE_PAR
makebad.o: SAE_PAR NDF_PAR NDF_ERR
makebadsub.o: FIO_PAR SAE_PAR
makeglitch.o: SAE_PAR NDF_PAR NDF_ERR
makeglitchsub.o: FIO_PAR SAE_PAR
makemask.o: SAE_PAR NDF_PAR NDF_ERR
makemasksub.o: SAE_PAR
maless.o: SAE_PAR NDF_PAR NDF_ERR
mamore.o: SAE_PAR NDF_PAR NDF_ERR
manic.o: SAE_PAR NDF_PAR NDF_ERR
manycol.o: SAE_PAR NDF_PAR NDF_ERR
manycol_putcol.o: SAE_PAR
manycolsub.o: SAE_PAR
manyg.o: SAE_PAR
masame.o: SAE_PAR NDF_PAR NDF_ERR
maxmin.o: SAE_PAR
med3d.o: SAE_PAR NDF_PAR NDF_ERR FIO_PAR CHR_ERR
med3d_calmedsub.o: SAE_PAR
med3d_medfil.o: SAE_PAR
med3d_norm.o: SAE_PAR
med3d_scaling.o: SAE_PAR
median.o: SAE_PAR NDF_PAR NDF_ERR
medref.o: SAE_PAR
medrep.o: SAE_PAR
medset.o: SAE_PAR
medwts.o: SAE_PAR
mfclose.o: SAE_PAR FIO_PAR
mfnext.o: SAE_PAR NDF_PAR NDF_ERR FIO_PAR
mfopen.o: SAE_PAR FIO_PAR NDF_PAR NDF_ERR
moff.o: SAE_PAR NDF_PAR NDF_ERR
moffdcbad.o: SAE_PAR
moffdcsub.o: SAE_PAR
moffxybad.o: SAE_PAR
moffxysub.o: SAE_PAR
mosaic.o: SAE_PAR NDF_PAR NDF_ERR
mosaic2.o: SAE_PAR NDF_PAR NDF_ERR
mosaic_add.o: SAE_PAR
mosaic_addb.o: SAE_PAR
mosaic_addbm.o: SAE_PAR
mosaic_addbv.o: SAE_PAR
mosaic_div.o: SAE_PAR
moscor.o: SAE_PAR NDF_PAR NDF_ERR CHR_ERR
moscorsub.o: SAE_PAR
moscorsub2.o: SAE_PAR
moscorsub3.o: SAE_PAR
mosstatssub.o: SAE_PAR
mosstatssub2.o: SAE_PAR
mtread.o: MAG_ERR
mult.o: SAE_PAR NDF_PAR NDF_ERR
multarr2d.o: SAE_PAR
multsca2d.o: SAE_PAR
ndfx__get_fitsc.o: SAE_PAR DAT_PAR PAR_ERR
ndfx__get_fitsd.o: SAE_PAR DAT_PAR PAR_ERR
ndfx__get_fitsi.o: SAE_PAR DAT_PAR PAR_ERR
ndfx__get_fitsl.o: SAE_PAR DAT_PAR PAR_ERR
ndfx__get_fitsr.o: SAE_PAR DAT_PAR PAR_ERR
ndfx__seek_item.o: SAE_PAR DAT_PAR PAR_ERR
ndfx_get_fitsc.o: SAE_PAR DAT_PAR PAR_ERR
ndfx_get_fitsd.o: SAE_PAR DAT_PAR PAR_ERR
ndfx_get_fitsi.o: SAE_PAR DAT_PAR PAR_ERR
ndfx_get_fitsl.o: SAE_PAR DAT_PAR PAR_ERR
ndfx_get_fitsr.o: SAE_PAR DAT_PAR PAR_ERR
newcoords.o: SAE_PAR
next_name.o: SAE_PAR
normal.o: SAE_PAR
nsigma_maxmin.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS SAE_PAR CHR_ERR PLT2DCOM
nsigma_scale.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
nthmin.o: SAE_PAR
numb.o: SAE_PAR NDF_PAR NDF_ERR PRM_PAR
numbs.o: SAE_PAR
obsext.o: SAE_PAR DAT_PAR CHR_ERR
obslist.o: SAE_PAR DAT_PAR CHR_ERR FIO_PAR
obsrap.o: SAE_PAR PAR_PAR NDF_PAR NDF_ERR
oddget.o: SAE_PAR PAR_ERR
oefix.o: SAE_PAR DAT_PAR
oefixsub.o: SAE_PAR DAT_PAR FIO_PAR
oldcoords.o: SAE_PAR
oldpars.o: SAE_PAR DAT_PAR DAT_ERR
outset.o: SAE_PAR PRM_PAR
outsetb.o: SAE_PAR NDF_PAR NDF_ERR PRM_PAR
outsetbsub.o: SAE_PAR
outsetsub.o: SAE_PAR
peepsub.o: SAE_PAR
pickim.o: SAE_PAR NDF_PAR NDF_ERR
pickimsub.o: SAE_PAR
pixdupe.o: SAE_PAR NDF_PAR NDF_ERR
pixdupesub.o: SAE_PAR
plot_border.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
plot_box.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
plot_circle.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
plot_comment.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
plot_cross.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
plot_curbox.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
plot_curcircle.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
plot_curcomment.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
plot_curcross.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
plot_curellipse.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
plot_curline.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
plot_ellipse.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
plot_grid.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
plot_line.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS PLT2DCOM
plt2d.o: PAR_PAR ADAMDEFNS ADAMERRS ACT_ERR NDF_PAR NDF_ERR PLT2DCOM
plt2d_readfile.o: SAE_PAR FIO_ERR
plt2d_readfilef.o: SAE_PAR FIO_ERR
poisson.o: SAE_PAR
polax.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS SAE_PAR NDF_PAR NDF_ERR
polax_annot.o: SAE_PAR CHR_ERR PLT2DCOM
polax_map.o: SAE_PAR CHR_ERR PLT2DCOM
polax_plot.o: DTDEFNS DTERRS PLT2DCOM
polcal.o: SAE_PAR NDF_PAR NDF_ERR
polcal2.o: SAE_PAR NDF_PAR NDF_ERR
polly.o: SAE_PAR
polly2.o: SAE_PAR
polrap.o: SAE_PAR PAR_PAR NDF_PAR NDF_ERR
polsep.o: SAE_PAR NDF_PAR NDF_ERR
polshot.o: SAE_PAR NDF_PAR NDF_ERR
polthresh.o: SAE_PAR NDF_PAR NDF_ERR PRM_PAR
polthresh2.o: SAE_PAR NDF_PAR NDF_ERR PRM_PAR
polthresh2sub.o: SAE_PAR
polthreshsub.o: SAE_PAR
pow2d.o: SAE_PAR NDF_PAR NDF_ERR
powarr2d.o: SAE_PAR PRM_PAR
primit.o: SAE_PAR DAT_PAR
psgrey.o: SAE_PAR NDF_PAR NDF_ERR
putdim.o: SAE_PAR DAT_PAR
putval.o: SAE_PAR DAT_PAR
quilt.o: SAE_PAR NDF_PAR NDF_ERR
radec_concat1.o: SAE_PAR CHR_ERR
radec_concat2.o: SAE_PAR CHR_ERR
radec_string.o: SAE_PAR CHR_ERR PLT2DCOM
radim.o: SAE_PAR NDF_PAR NDF_ERR
radimsub.o: SAE_PAR
range_maxmin.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS PLT2DCOM
range_scale.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS PLT2DCOM
rapblo.o: SAE_PAR
rapgau.o: SAE_PAR
rapgauth.o: SAE_PAR
rapi2d.o: SAE_PAR PAR_PAR NDF_PAR NDF_ERR
redcont_checkdata.o: SAE_PAR DAT_PAR
redcont_whatset.o: SAE_PAR DAT_PAR
reduce.o: SAE_PAR DAT_PAR CHR_ERR
remap.o: SAE_PAR
reset_device.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS PLT2DCOM
roname.o: SAE_PAR CHR_ERR
ronext.o: SAE_PAR NDF_PAR NDF_ERR
root.o: SAE_PAR NDF_PAR NDF_ERR
root2d.o: SAE_PAR
ropars.o: SAE_PAR NDF_PAR NDF_ERR DAT_PAR DAT_ERR CHR_ERR
ropars_new.o: SAE_PAR NDF_PAR NDF_ERR DAT_PAR DAT_ERR CHR_ERR
ropars_old.o: SAE_PAR NDF_PAR NDF_ERR DAT_PAR DAT_ERR CHR_ERR
rotas1.o: SAE_PAR
rotas2.o: SAE_PAR
rotas3.o: SAE_PAR
rotas4.o: SAE_PAR
rotate.o: SAE_PAR NDF_PAR NDF_ERR
rotnrs.o: SAE_PAR
rotsize.o: SAE_PAR
rowmed.o: SAE_PAR NDF_PAR NDF_ERR
rowmedsub.o: SAE_PAR
set_colour.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS PLT2DCOM
set_colour2.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS PLT2DCOM
set_linewidth.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS GKS_PAR PLT2DCOM
set_pencol.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS CHR_ERR PLT2DCOM
set_pencol2.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS CHR_ERR PLT2DCOM
setval.o: SAE_PAR NDF_PAR NDF_ERR PRM_PAR
setvalsub.o: SAE_PAR
shadow.o: SAE_PAR NDF_PAR NDF_ERR
shadowsub.o: SAE_PAR
shift.o: SAE_PAR NDF_PAR NDF_ERR
shifts.o: SAE_PAR
shiftx.o: SAE_PAR
shifty.o: SAE_PAR
shsize.o: SAE_PAR NDF_PAR NDF_ERR
sqorst.o: SAE_PAR
sqshs.o: SAE_PAR
sqshx.o: SAE_PAR
sqshy.o: SAE_PAR
stats.o: SAE_PAR NDF_PAR NDF_ERR
statssub.o: SAE_PAR
statssub2.o: SAE_PAR
stcoadd.o: SAE_PAR NDF_PAR NDF_ERR
stepim.o: SAE_PAR NDF_PAR NDF_ERR
stepimsub.o: SAE_PAR
strx.o: SAE_PAR
stry.o: SAE_PAR
sub.o: SAE_PAR NDF_PAR NDF_ERR
subarr2d.o: SAE_PAR
subsca2d.o: SAE_PAR
surround_annotate.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS PLT2DCOM
thetafix.o: SAE_PAR NDF_PAR NDF_ERR
thetafixsub.o: SAE_PAR
thresh.o: SAE_PAR NDF_PAR NDF_ERR PRM_PAR
threshsub.o: SAE_PAR
toeq.o: SAE_PAR NDF_PAR
toeqsub.o: SAE_PAR
trace.o: SAE_PAR DAT_PAR
tracon.o: SAE_PAR DAT_PAR
trandat.o: SAE_PAR NDF_PAR NDF_ERR
trandatsub.o: SAE_PAR
trarr1.o: SAE_PAR DAT_PAR
trarr2.o: SAE_PAR DAT_PAR
trarr3.o: SAE_PAR DAT_PAR
trarr4.o: SAE_PAR DAT_PAR
trarr5.o: SAE_PAR DAT_PAR
trarr6.o: SAE_PAR DAT_PAR
trig.o: SAE_PAR PAR_ERR NDF_PAR NDF_ERR
trigscal.o: SAE_PAR
trigsub.o: SAE_PAR
trstr1.o: SAE_PAR DAT_PAR
trstr2.o: SAE_PAR DAT_PAR
trstr3.o: SAE_PAR DAT_PAR
trstr4.o: SAE_PAR DAT_PAR
trstr5.o: SAE_PAR DAT_PAR
typfile.o: SAE_PAR
upcase.o: SAE_PAR CHR_ERR
vargrey_scale.o: ADAMDEFNS ADAMERRS DTDEFNS DTERRS PLT2DCOM
welcome_obsrap.o: SAE_PAR
welcome_polrap.o: SAE_PAR
welcome_rapi2d.o: SAE_PAR
wmfnext.o: SAE_PAR FIO_PAR NDF_PAR NDF_ERR
wmfopen.o: SAE_PAR NDF_PAR NDF_ERR FIO_PAR
wmosaic.o: SAE_PAR NDF_PAR NDF_ERR
wmosaic_add.o: SAE_PAR
wmosaic_addbm.o: SAE_PAR
wmosaic_addbv.o: SAE_PAR
wquilt.o: SAE_PAR NDF_PAR NDF_ERR
wrapcor.o: SAE_PAR NDF_PAR NDF_ERR
wrapcorsub.o: SAE_PAR
write_ct.o: ADAMDEFNS ADAMERRS SAE_PAR DTDEFNS DTERRS CHR_ERR PLT2DCOM
xgrow.o: SAE_PAR NDF_PAR NDF_ERR
xgrowsub.o: SAE_PAR
yadd.o: SAE_PAR NDF_PAR NDF_ERR
yaddsub.o: SAE_PAR
ygrow.o: SAE_PAR NDF_PAR NDF_ERR
ygrowsub.o: SAE_PAR
zaplin.o: SAE_PAR NDF_PAR NDF_ERR
zaplinsub.o: SAE_PAR
zero1d.o: SAE_PAR
zero2d.o: SAE_PAR

#-------------------------------------------------------------------------------
#
#  End of makefile.
#.
