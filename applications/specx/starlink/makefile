#+
#  Name:
#     makefile
#
#  Version:
#     Package makefile Mk V
#
#  Purpose:
#     Build and install the SPECX package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     SPECX package from the distributed source files, to install
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
#     The SPECX package depends on the following other Starlink
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
#     22-AUG-1997 (BLY):
#        Modified build and link of main task to tidy it up using CFLINK
#        and CFLIBS macros (see mk script).
#     25-NOV-1997 (BLY):
#        Added SC/8 to documentation.
#     12-FEB-1998 (BLY): Version 6.7-3.  
#        Correction to graphcap.txt to use Starlink PGPLOT device names.
#     23-JUL-1998 (BLY): Version 6.7-4. 
#        Removed unsupported devices from GKS graphcap.txt.
#     09-DEC-1998 (BLY): Version 6.7-5.
#        Modified specx_link to use pgp_link for Starlink PGPLOT rather 
#           than pgplot_link for Native PGPLOT.  
#        Changed link libs order to make it work under Digital Unix.
#     10-JAN-2000 (BLY): Version 6.7-6.
#        Added Y2K fixes from Tim Jenness.
#        Added new SUN/17 + hypertext.
#        Added SPECX manual (MUD/70).
#      8-MAY-2000 (AJC):
#        Version 6.7-8
#        Unix port etc.
#        Add SPECX_FITS for initsp
#        Add MAPS for init_specx
#        Use separate GSD
#        Add NDF_ERR for mapopen
#        Use copy and link installation for documentation
#     28-NOV-2000 (AJC):
#        Avoid tar file dependence on source files.
#      3-MAY-2001 (AJC)
#        Version 6.7-9
#        Remove hardwired /star/lib
#        Correct test instructions
#     13-SEP-2001 (AJC):
#        Add ' to CLOSE-MAP line of test: target
#      4-FEB-2002 (AJC):
#        Version 6.7-10
#      8-MAR-2002 (AJC):
#        Version 6.7-11
#     {enter_further_changes_here}
#
#  Bugs:
#     {note_any_bugs_here}
#
#-------------------------------------------------------------------------------

#  Help target.
#  ===========
#
#  This is the default target, so it appears first.

#  Display information about the mk script and the make targets.

help:
	@ echo \
   '   The makefile provided is intended to be used by the make utility when';\
        echo \
   '   invoked via the associated mk script.  This script defines environment';\
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

#-------------------------------------------------------------------------------

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
STAR_SHARE = $(STARLINK)/share

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

TAR_IN = tar cvfh
TAR_OUT = tar xf

#  Command for adding a file to an object archive (.a).

AR_IN = ar -r

#  Linker command.  Used to compile/link the final executables.

ELINK = $(FC) -O

#  Linker flags.  These are placed either side of the library list
#  and are used adding system-dependent linker flags to link commands.

LDFLAG1 =
LDFLAG2 =

#  Linker command to link C main programs with Fortran subroutines and
#  libraries (and C functions).
 
CFLINK = $(FC) -O
 
#  Extra link libraries for CFLINK.
 
CFLIBS =

#  Location of FIGARO libraries.

FIGARO_SOURCE = $(STARLINK)/sources/figaro

#-------------------------------------------------------------------------------
################################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.

PACK_NAME = SPECX

#  Prefix for the package in lower-case as used in filenames etc.
PKG_NAME = specx

#  Prefix for the package in upper-case as used in include file links.
PKG_LINK = SPECX

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

PKG_VERS = 6.7-12

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).  It is advisable to use several
#  smaller source archives in place of one large one to speed up build
#  times.

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION) \
   $(PKG_NAME)_astro.tar $(PKG_NAME)_exprt.tar $(PKG_NAME)_external.tar \
   $(PKG_NAME)_fitting.tar $(PKG_NAME)_fv4.tar $(PKG_NAME)_gen.tar \
   $(PKG_NAME)_getaline.tar $(PKG_NAME)_graph.tar \
   $(PKG_NAME)_mapdis4.tar $(PKG_NAME)_mv4.tar $(PKG_NAME)_plots.tar \
   $(PKG_NAME)_readline.tar $(PKG_NAME)_reduce.tar $(PKG_NAME)_scl.tar \
   $(PKG_NAME)_tek.tar $(PKG_NAME)_term.tar $(PKG_NAME)_util.tar \
   $(PKG_NAME)_vms.tar $(PKG_NAME)_command.tar \
   $(PKG_NAME)_inc.tar $(PKG_NAME)_comfiles.tar

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLIC_SCRIPTS = baseline.spx convert.spx fetch.spx fiddle.spx \
frqfix.spx fsw.spx gsd2fits.spx helpfits.spx image.spx map_av.spx \
newton_root.spx rddualgsd.spx rdwidesp.spx readall.spx showastro.spx \
source.spx specx_to_dbmem.spx st.spx stack.spx tofits.spx tsky.spx \
unaverage.spx vertical.spx

#  Startup script.  This is the file that must be executed (sourced)
#  by a user of this package in order to define appropriate aliases 
#  and environment variables.  It is listed separately from the public 
#  scripts as it is edited by the installation procedure.

STARTUP_SCRIPT = specxstart

#  Link script.  This script does not form part of the package but is 
#  required for building it.  It will not be installed.
 
LINK_SCRIPT = $(PKG_NAME)_link

#  List of private include files.  These are include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = $(I_SUBA) $(I_SUBB) \
                   $(I_SUB1) $(I_SUB2) $(I_SUB3) $(I_SUB4) \
                   $(I_SUB5) $(I_SUB6) $(I_SUB7) $(I_SUB8) $(I_SUB9) \
                   $(I_SUB10) $(I_SUB11) $(I_SUB12) $(I_SUB13) \
                   $(I_SUB14) $(I_SUB15) $(I_SUB16) $(I_SUB17) $(I_SUB18) 

I_SUBA = \
   commands.inc cube.inc errors.inc files.inc filhd.inc filhdi2.inc \
   flagcomm.inc header.inc iodata.inc jpi.inc maphd.inc maps.inc \
   maptitles.inc newxy.inc nokeep.inc \
   plot2d.inc plotpar1.inc prototype.inc specx_pars.inc stackcomm.inc \
   stakpar.inc symbols.inc weights.inc \
   specx_fits.inc doppler.inc mapv4.inc

I_SUBB = title.inc

I_SUB1 = 

I_SUB2 = gsd_file.inc gsd_index.inc gsd_var.inc

I_SUB3 = 

I_SUB4 = 

I_SUB5 = 

I_SUB6 = cli_stack.inc eval_ae4.inc gen_symbols.inc logical_units.inc symbols.h

I_SUB7 = 

I_SUB8 = sxg_graphcap.inc sxgpgplot.inc sxg_graphcap.h

#I_SUB9 = gsd_pars.inc \
#         gsd.h gsd2_alpha_OSF1.h gsd2_sun4_Solaris.h gsd1.h gsd2_sun4.h

I_SUB10 = anm_col.inc anm_logcol.inc

I_SUB11 =

I_SUB12 = 

I_SUB13 = ansi_stdlib.h history.h memalloc.h readline.h rldefs.h chardefs.h \
          keymaps.h posixstat.h rlconf.h tilde.h emacs_keymap.c vi_keymap.c

I_SUB14 =

I_SUB15 = scl_buffer.inc scl_do.inc scl_ifdo.inc

I_SUB16 = 

I_SUB17 = 

I_SUB18 = luntab.inc vmtab.inc

#  List of platform dependent private include files.  These are include 
#  files  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PLATFORM_PRIVATE_INCLUDES = $(PI_SUB1) $(PI_SUB2) $(PI_SUB3) $(PI_SUB4) \
          $(PI_SUB5) $(PI_SUB6) $(PI_SUB7) $(PI_SUB8) $(PI_SUB9) \
          $(PI_SUB10) $(PI_SUB11) $(PI_SUB12) $(PI_SUB13) \
          $(PI_SUB14) $(PI_SUB15) $(PI_SUB16) $(PI_SUB17) \
          $(PI_SUB18)

PI_SUB1 =

PI_SUB2 =

PI_SUB3 =

PI_SUB4 =

PI_SUB5 =

PI_SUB6 =

PI_SUB7 =

PI_SUB8 =

PI_SUB9 =

PI_SUB10 =

PI_SUB11 =

PI_SUB12 =

PI_SUB13 =

PI_SUB14 =

PI_SUB15 =

PI_SUB16 =

PI_SUB17 =

PI_SUB18 =

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = DAT_PAR DAT_ERR SAE_PAR PRM_PAR NDF_PAR NDF_ERR f77.h

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines and the main routine, which are treated separately).  

F_ROUTINES = $(F_SUB1) $(F_SUB2) $(F_SUB3) $(F_SUB4) $(F_SUB5) \
             $(F_SUB6) $(F_SUB7) $(F_SUB8) $(F_SUB9) \
             $(F_SUB10) $(F_SUB11) $(F_SUB12) $(F_SUB13) \
             $(F_SUB14) $(F_SUB15) $(F_SUB16) $(F_SUB17) $(F_SUB18) \
             $(F_SUB19) $(F_SUB20) \
             $(EXE_SOURCES_F)

F_SUB1 = a13.f astro_times.f astro_vels.f cuv.f  cvt_to_date_obs.f \
         date_cvt.f dattim_from_mjd.f decdat.f dectim.f deg_to_dms.f deg_to_string.f \
         dms_to_rad.f dms_to_string.f hadec_to_azel.f hours_to_str.f julda.f \
         lostim2.f pre.f radec_to_l2b2.f red.f specxjd_to_mjd.f \
         string_to_dms.f utcalc2.f

F_SUB2 = \
   calc_image.f close_fits.f fitsmap.f \
   open_fits.f wrfitsmap.f wrfitsspec.f wrfitscube.f\
   rdfitsspec.f bad2blank.f \
   index_gsd.f read_gsd.f rd_gsd_ras.f sgsdreopen.f\
   sgsdclose.f sgsdlist.f sgsdlocate.f sgsdopen.f sgsdv4data.f \
   sgsdv4header.f sgsdv51data.f sgsdv52data.f sgsdv5data.f sgsdv5header.f \
   write_gildas.f

F_SUB3 = externals.f getintspec.f ut_cvt.f

F_SUB4 = basfit.f display_gauss.f gaussf.f sinfit.f baslin.f enter_gauss.f \
         linfit.f calc_gauss.f funval.f lsqfit.f

F_SUB5 = chkacc.f fv4_filinf.f fv4_nativi.f fv4_specix.f rdump.f \
         fv4_cnv321.f fv4_filini.f fv4_nativr.f fv4_specrd.f wdump.f \
         fv4_filacc.f fv4_filmerg.f fv4_nativw.f fv4_specwa.f write_ascii.f \
         fv4_filcls.f fv4_filopn.f fv4_opf321.f fv4_specwi.f fv4_filcmp.f \
         fv4_filrcv.f fv4_spcrdtj.f getfil.f fv4_filhde.f fv4_filrop.f \
         fv4_spec2c.f lstfil.f fv4_filidx.f fv4_nativd.f fv4_specdl.f nfile.f

F_SUB6 = cli_empty.f gen_alphanum.f gen_at.f gen_comach.f gen_cvt_type.f \
         gen_decode.f gen_delim.f gen_dformat.f gen_digits.f gen_do_op.f \
         gen_doarry.f gen_dofunc.f gen_eformat.f gen_encode.f gen_ermsg.f \
         gen_eval_ae.f gen_eval_all.f gen_eval_str.f gen_exop.f gen_exopa.f \
         gen_exopl.f gen_exopr.f gen_exops.f gen_factor.f gen_floating.f \
         gen_get_luns.f gen_geti4.f gen_geti4a.f gen_geti4a2.f gen_getit3.f \
         gen_getit4.f gen_getline.f gen_getmore.f gen_getr4.f gen_getr4a.f \
         gen_getr4a2.f gen_getr8.f gen_getr8a.f gen_getr8a2.f gen_getstr.f \
         gen_getstr2.f gen_getsym1.f gen_getwrd.f gen_hash.f \
         gen_hdnorm.f gen_ichtot.f gen_iendch.f \
         gen_ilen.f gen_input.f gen_inqfunc.f gen_inqsymb.f gen_inqsymb1.f \
         gen_integer.f gen_jline.f gen_jnloff.f gen_jnlon.f gen_kntwrd.f \
         gen_lineno.f gen_make_var.f gen_makesym1.f gen_makesymb.f \
         gen_match.f gen_negate.f gen_operator.f gen_parseexp.f \
         gen_parseflt.f gen_parseint.f gen_parsename.f gen_parseop.f \
         gen_pause.f gen_put_symb.f gen_putpmt.f gen_rank.f gen_readnum.f \
         gen_rewind.f gen_sequal.f gen_set_cli.f gen_set_luns.f \
         gen_set_symt.f gen_setsym1.f gen_sprint.f gen_unwind.f gen_write.f \
         gen_yesno.f get_subexpr.f ins_cli_item.f refresh_cli.f \
         reset_stk_pt.f stack_pointer.f string_test.f strip_string.f xcopy.f

F_SUB7 =

F_SUB8 = getdev.f sxgpgplot.f ttsub.f

F_SUB9 = 

F_SUB10 = goodpt.f map_integ.f plot_chan4.f \
          anm_col.f grid_map.f map_locate.f plot_line4.f \
          contour_map4.f interp_map.f map_maxmin.f plot_map.f \
          convarr.f interparr.f map_window.f scale_bar.f \
          copy_mappos.f label_map.f mapimage.f set_disp_siz.f \
          draw_mapconb.f listmp.f mark_point.f setccol.f \
          export_map4.f make_chan4.f mark_samples.f setclevs.f \
          get_cube_data.f make_line4.f mwindo.f setgscal.f \
          getmap2.f make_map4.f new_scales.f setpix.f \
          getpars.f map_coords.f plot2d_range.f zlabel.f 

F_SUB11 = addmap.f cube_subs.f lname.f mv4_mapcls.f open_map.f \
          addmap2.f del_from_map.f mapopen.f mv4_mapnew.f read_dms.f \
          ask_map.f extmap.f mv4_41to42.f mv4_mapopn.f read_interp.f \
          calc_posoff.f extr_header.f mv4_cnv321.f mv4_protrd.f rotate_cube.f \
          calc_xycoff.f fromap.f mv4_headrd.f mv4_protwr.f set_maptitle.f \
          calc_xysoff.f get_spectrum.f mv4_headwr.f mv4_specdl.f set_weights.f \
          close_map.f index_subs.f mv4_indxrd.f mv4_specrd.f setmax.f \
          create_map.f interp_cube.f mv4_indxwr.f mv4_specwr.f 

F_SUB12 = alloc_dev.f draw_axes.f new_plot.f pltdat.f setrange.f \
          autorange.f draw_plot.f overlay.f qplot.f setrange2d.f \
          close_plot.f endplot.f plot_file.f rangexy.f \
          del_last.f make_xtitle.f plotxy.f rangexy2d.f \
          del_plot.f mkhis.f pltbuf.f see_plot.f

F_SUB13 = 

F_SUB14 = \
   amom1.f avgxy.f bindat.f centrd.f change_sb.f clipsp.f cnvlve.f \
   concatsp.f delchn.f diffsp.f findazel.f findmx.f fmcall.f fndwid.f \
   foldsp.f foursp.f fpspec.f grid_data.f grid_data8.f \
   hannsp.f intsty.f invert.f \
   invspc.f listsp.f lsrcor.f merge.f moment.f offsp.f quotsp.f regrid.f \
   remspk.f setchn.f shift.f skew.f slide.f smooth_data.f specx_jsort.f \
   spike.f stats.f trunc.f

F_SUB15 = \
   do_syscomm.f scl_commsyms.f scl_help.f scl_continue.f \
   scl_do.f scl_if.f \
   scl_matchcomm.f scl_wild.f scl_parse.f \
   scl_symbols.f

F_SUB16 = \
   chkbox.f confirm.f credat.f \
   draw_2dbox.f draw_box.f draw_defbox.f \
   draw_spectrum.f i2dopt.f ivtopt.f list_2dhelp.f list_vthelp.f \
   new_xy.f tpair.f

F_SUB17 = \
   ask_grey.f ask_interp.f ask_plot_dev.f ask_vel.f displx.f dispst.f \
   dstack.f getpts.f inithd.f ppair.f prscan.f \
   set_mapscale.f set_plot_dev.f show_store.f specx_set.f stackstat.f \
   veldecode.f velencode.f

F_SUB18 = \
   add.f compressq.f convol.f copybf.f doquad.f \
   dasmerge.f exchnge.f four2.f frqfix.f get_subarr.f icheck.f ichkq2.f \
   init_array.f initnpnew.f inquad.f interp.f \
   islct1q.f islctq.f lmm2.f lun_hndl.f \
   mult.f npts2.f ntot.f ntot2.f ntrans.f parse_date_obs.f pop.f push.f \
   qlim.f qmult.f range.f recall.f roll.f setx.f \
   solve4.f specx_reorder.f spliceq.f storex.f su.f \
   swap2.f swap8.f swap_arr.f tran_date.f tran_time.f \
   uread.f uwrite.f vm_hndl.f \
   xsnart.f xsnart8.f xtrans.f xtrans8.f xtrctq.f xy.f \
   pda_dsort.f pda_xermsg.f

F_SUB19 = \
   qualifiers_unix.f sysdep.f \
   parseat_unix.f \
   vms_help.f spack.f spacl.f spaeg.f \
   putscreen.f vms_date.f

F_SUB20 = do_command.f init_specx.f initsp.f specx_initab.f

#  List of Fortran BLOCK DATA routines.

BLOCK_DATA = $(B_SUB1) $(B_SUB2) $(B_SUB3)  $(B_SUB4)  $(B_SUB5) \
             $(B_SUB6) $(B_SUB7) $(B_SUB8) $(B_SUB9) \
             $(B_SUB10) $(B_SUB11) $(B_SUB12) $(B_SUB13) \
             $(B_SUB14) $(B_SUB15) $(B_SUB16) $(B_SUB17) \
             $(B_SUB18) $(B_SUB19) $(B_SUB20)

B_SUB1 = 
 
B_SUB2 = 

B_SUB3 = 

B_SUB4 = 

B_SUB5 = 

B_SUB6 = symtab_init.f usubs.f

B_SUB7 =

B_SUB8 =

B_SUB9 =

B_SUB10 = anm_block.f

B_SUB11 =

B_SUB12 =

B_SUB13 =

B_SUB14 =

B_SUB15 = commands.f init_symbols.f

B_SUB16 =

B_SUB17 =

B_SUB18 = initvm.f initlun.f

B_SUB19 =

B_SUB20 =

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F = $(PF_SUB1) $(PF_SUB2) $(PF_SUB3) $(PF_SUB4) $(PF_SUB5) \
             $(PF_SUB6) $(PF_SUB7) $(PF_SUB8) $(PF_SUB9) \
             $(PF_SUB10) $(PF_SUB11) $(PF_SUB12) $(PF_SUB13) \
             $(PF_SUB14) $(PF_SUB15) $(PF_SUB16) $(PF_SUB17) $(PF_SUB18) \
             $(PF_SUB19) $(PF_SUB20)

PF_SUB1 =

PF_SUB2 =

PF_SUB3 =

PF_SUB4 =

PF_SUB5 =

PF_SUB6 =

PF_SUB7 =

PF_SUB8 =

PF_SUB9 =

PF_SUB10 =

PF_SUB11 =

PF_SUB12 =

PF_SUB13 =

PF_SUB14 =

PF_SUB15 =

PF_SUB16 =

PF_SUB17 =

PF_SUB18 = ndlib.f

PF_SUB19 = openuf.f welcome_unix.f

PF_SUB20 = 

#  C routines required for building the package.  This is just a list of
#  all the C source files.

C_ROUTINES = $(C_SUB1) $(C_SUB2) $(C_SUB3)  $(C_SUB4)  $(C_SUB5) \
             $(C_SUB6) $(C_SUB7) $(C_SUB8) $(C_SUB9) \
             $(C_SUB10) $(C_SUB11) $(C_SUB12) $(C_SUB13) \
             $(C_SUB14) $(C_SUB15) $(C_SUB16) $(C_SUB17) $(C_SUB18) \
             $(C_SUB19) $(C_SUB20) $(EXE_SOURCES_C)

C_SUB1 =

C_SUB2 = bswap.c wbswap.c

C_SUB3 =

C_SUB4 =

C_SUB5 =

C_SUB6 = hash.c gen_makesym2.c 

C_SUB7 = empty.c empty2.c get_aline.c

C_SUB8 = sxg1pgplot.c

#C_SUB9 = gsd1_getval.c gsd2_copya.c gsdGet0x.c gsdOpenRead.c gsd1_rddata.c \
#         gsd2_nativx.c gsdGet1x.c gsd_f77.c gsd1_rdfildsc.c gsdClose.c \
#         gsdInqSize.c gsd1_rdhead.c gsdFind.c gsdItem.c

#C_SUB9 = gsd1_getval.c gsdGet0x.c gsdOpenRead.c gsd1_rddata.c \
#         gsdGet1x.c gsd_f77.c gsd1_rdfildsc.c gsdClose.c \
#         gsdInqSize.c gsd1_rdhead.c gsdFind.c gsdItem.c

C_SUB10 =

C_SUB11 =

C_SUB12 =

C_SUB13 = bind.c funmap.c parens.c signals.c xmalloc.c complete.c history.c \
          readline.c tilde.c display.c isearch.c rltty.c keymaps.c search.c \
          vi_mode.c

C_SUB14 =

C_SUB15 =

C_SUB16 =

C_SUB17 =

C_SUB18 =

C_SUB19 = spaef.c spaeh.c spaej.c spaek.c

C_SUB20 =

#  Main routine source name.

EXE_SOURCES_F = $(EXE_1_SOURCE_F) $(EXE_2_SOURCE_F)

EXE_1_SOURCE_F = scl_main.f 

EXE_2_SOURCE_F =

EXE_SOURCES_C = $(EXE_1_SOURCE_C) $(EXE_2_SOURCE_C)

EXE_1_SOURCE_C = run_specx.c

EXE_2_SOURCE_C = helpm.c helpc.c

#  Main binary names.

EXECUTABLES = $(EXE_1) $(EXE_2)

EXE_1 = specx

EXE_2 = specx_help

#  List of help library sources.

HELP_SOURCES = $(PKG_NAME).hlp

#  List of ancilliary data files.

DATA_FILES = specx_welcome.txt specx_init.dat init.spx test.spx graphcap.txt \
             l483core.dat l483cmap.map scan_0043.dat obs_das_0011.dat

#  Lists of Latex and hypertext documents.

LATEX_DOCS = sun17.tex sc8.tex mud70.tex \
sc8_badbaseline.ps sc8_cgm.ps sc8_chann.ps sc8_concat.ps sc8_cont.ps \
sc8_dasmerge_yn.ps sc8_despiked.ps sc8_dm_dasmerge.ps sc8_dm_dirty.ps \
sc8_dm_drop.ps sc8_dm_orig.ps sc8_duffy.ps sc8_fgm.ps sc8_fpb-result.ps \
sc8_fpb.ps sc8_fsw-fig.ps sc8_fsw_reduction.ps sc8_gr_sp.ps sc8_gray.ps \
sc8_hdo-lsb.ps sc8_hdo-orig.ps sc8_hdo-usb.ps sc8_hdo_freq.ps \
sc8_merge_diffs.ps sc8_rlb.ps sc8_s-l-r-f.ps sc8_smooth.ps \
sc8_spectrum.ps sc8_spiked.ps

FIGURES = mud70_figures

HYPERTEXT_DOCS = sun17.htx sc8.htx

#  List of documentation files.

DOCUMENTATION = $(LATEX_DOCS) $(FIGURES) $(HYPERTEXT_DOCS:.htx=.htx_tar) \
$(PKG_NAME).news

################################################################################
#-------------------------------------------------------------------------------

#  Define files required for building the package.
#  ==============================================
#
#  This section defines the set of files produced from the source files
#  when the package is built and installed.

#  Use only .o, .c, .f, .shl and .hlp suffix rules.

.SUFFIXES:
.SUFFIXES: .o .c .f .shl .hlp

#  List of files which must be built from the source files before the
#  package can be installed for use.  This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).

BUILT_FILES = $(PUBLIC_SCRIPTS) $(OBJECT_LIBRARIES) $(EXECUTABLES) \
$(HELP_FILES) $(DATA_FILES) $(STARTUP_SCRIPT)

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES) \
  $(L_SUBA) $(L_SUB1) $(L_SUB2) \
  $(L_SUB3) $(L_SUB4) $(L_SUB5) $(L_SUB6) $(L_SUB7) $(L_SUB8) \
  $(L_SUB9) $(L_SUB10) $(L_SUB11) $(L_SUB12) $(L_SUB13) $(L_SUB14) \
  $(L_SUB15) $(L_SUB16) $(L_SUB17) $(L_SUB18) $(L_SUB19) $(L_SUB20) \
  $(PKG_LINK)_ERR $(PKG_LINK)_PAR 

L_SUBA = COMMAND_TABLE ERRORS SYMBOLS SPECX_PARS \
         CUBE DOPPLER FILES FILHD FILHDI2 FLAGCOMM HEADER IODATA JPI \
         MAPHD MAPS MAPTITLES MAPV4 \
         NEWXY NOKEEP PLOT2D PLOTPAR1 PROTOTYPE SPECX_FITS STACKCOMM \
         STAKPAR TITLE WEIGHTS

L_SUB1 = 

L_SUB2 = GSD_FILE.INC GSD_INDEX.INC GSD_VAR.INC

L_SUB3 =

L_SUB4 = 

L_SUB5 =

L_SUB6 = CLI_STACK.INC EVAL_AE4.INC GEN_SYMBOLS.INC LOGICAL_UNITS.INC

L_SUB7 =

L_SUB8 = SXGPGPLOT.INC SXG_GRAPHCAP.INC

#L_SUB9 = gsd2.h GSDPARS

L_SUB10 = ANM_COL.INC ANM_LOGCOL.INC

L_SUB11 =

L_SUB12 =

L_SUB13 =

L_SUB14 =

L_SUB15 = SCL_BUFFER.INC SCL_DO.INC SCL_IFDO.INC

L_SUB16 =

L_SUB17 =

L_SUB18 = VMTAB.INC LUNTAB.INC

L_SUB19 =

L_SUB20 = GSD_VAR

#  Rules to set up links to locate each of the above include files.

$(PKG_LINK)_ERR:   $(PKG_NAME)_err;               $(LINK) $? $@
$(PKG_LINK)_PAR:   $(PKG_NAME)_par;               $(LINK) $? $@
DAT_PAR:   $(STAR_INC)/dat_par;   $(LINK) $? $@
NDF_PAR:   $(STAR_INC)/ndf_par;   $(LINK) $? $@
NDF_ERR:   $(STAR_INC)/ndf_err;   $(LINK) $? $@
f77.h:     $(STAR_INC)/f77.h;     $(LINK) $? $@

#  Links for include files in specx_inc.tar

COMMAND_TABLE: commands.inc;   $(LINK) $? $@
ERRORS:        errors.inc;     $(LINK) $? $@
SYMBOLS:       symbols.inc;    $(LINK) $? $@
SPECX_PARS:    specx_pars.inc; $(LINK) $? $@
CUBE:          cube.inc;       $(LINK) $? $@
DOPPLER:       doppler.inc;    $(LINK) $? $@
FILES:         files.inc;      $(LINK) $? $@
FILHD:         filhd.inc;      $(LINK) $? $@
FILHDI2:       filhdi2.inc;    $(LINK) $? $@
FLAGCOMM:      flagcomm.inc;   $(LINK) $? $@
HEADER:        header.inc;     $(LINK) $? $@
IODATA:        iodata.inc;     $(LINK) $? $@
JPI:           jpi.inc;        $(LINK) $? $@
MAPHD:         maphd.inc;      $(LINK) $? $@
MAPS:          maps.inc;       $(LINK) $? $@
MAPTITLES:     maptitles.inc;  $(LINK) $? $@
MAPV4:         mapv4.inc;      $(LINK) $? $@
NEWXY:         newxy.inc;      $(LINK) $? $@
NOKEEP:        nokeep.inc;     $(LINK) $? $@
PLOT2D:        plot2d.inc;     $(LINK) $? $@
PLOTPAR1:      plotpar1.inc;   $(LINK) $? $@
PROTOTYPE:     prototype.inc;  $(LINK) $? $@
SPECX_FITS:    specx_fits.inc; $(LINK) $? $@
STACKCOMM:     stackcomm.inc;  $(LINK) $? $@
STAKPAR:       stakpar.inc;    $(LINK) $? $@
WEIGHTS:       weights.inc;    $(LINK) $? $@
TITLE:         title.inc;      $(LINK) $? $@

#  Links for include files in specx_exprt.tar

GSD_FILE.INC:      gsd_file.inc;          $(LINK) $? $@
GSD_INDEX.INC:     gsd_index.inc;         $(LINK) $? $@
GSD_VAR.INC:       gsd_var.inc;           $(LINK) $? $@

#  Links for include files in specx_fv4.tar

SAE_PAR:           $(STAR_INC)/sae_par;   $(LINK) $? $@
DAT_ERR:           $(STAR_INC)/dat_err;   $(LINK) $? $@
PRM_PAR:           $(STAR_INC)/prm_par;   $(LINK) $? $@

#  Links for include files in specx_gen.tar

CLI_STACK.INC:     cli_stack.inc;     $(LINK) $? $@
EVAL_AE4.INC:      eval_ae4.inc;      $(LINK) $? $@
GEN_SYMBOLS.INC:   gen_symbols.inc;   $(LINK) $? $@
LOGICAL_UNITS.INC: logical_units.inc; $(LINK) $? $@

#  Links for include files in specx_graph.tar

SXGPGPLOT.INC:     sxgpgplot.inc;     $(LINK) $? $@
SXG_GRAPHCAP.INC:  sxg_graphcap.inc;  $(LINK) $? $@

#  Links for include files in specx_gsd.tar

#gsd2.h:            gsd2_$(SYSTEM).h;  $(LINK) $? $@

#  Links for include files in specx_mapdis4.tar

ANM_COL.INC:       anm_col.inc;       $(LINK) $? $@
ANM_LOGCOL.INC:    anm_logcol.inc;    $(LINK) $? $@

#  Links for include files in specx_scl.tar

SCL_BUFFER.INC:    scl_buffer.inc;    $(LINK) $? $@
SCL_IFDO.INC:      scl_ifdo.inc;      $(LINK) $? $@
SCL_DO.INC:        scl_do.inc;        $(LINK) $? $@

#  Links for include files in specx_util.tar

VMTAB.INC:         vmtab.inc;         $(LINK) $? $@
LUNTAB.INC:        luntab.inc;        $(LINK) $? $@

#  Links for include files in specx_command.tar

GSD_VAR:           gsd_var.inc;       $(LINK) $? $@

#  Rules for extracting source files from the source archive.

$(LINK_SCRIPT) $(EXE_SOURCES_F) $(EXE_SOURCES_C) \
$(HELP_SOURCES) $(STARTUP_SCRIPT) $(DATA_FILES):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(PUBLIC_SCRIPTS):
	$(TAR_OUT) $(PKG_NAME)_comfiles.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(I_SUBA):
	$(TAR_OUT) $(PKG_NAME)_inc.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(I_SUBB):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB1) $(I_SUB1) $(B_SUB1) $(C_SUB1):
	$(TAR_OUT) $(PKG_NAME)_astro.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB2) $(I_SUB2) $(B_SUB2) $(C_SUB2):
	$(TAR_OUT) $(PKG_NAME)_exprt.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB3) $(I_SUB3) $(B_SUB3) $(C_SUB3):
	$(TAR_OUT) $(PKG_NAME)_external.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB4) $(I_SUB4) $(B_SUB4) $(C_SUB4):
	$(TAR_OUT) $(PKG_NAME)_fitting.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB5) $(I_SUB5) $(B_SUB5) $(C_SUB5):
	$(TAR_OUT) $(PKG_NAME)_fv4.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB6) $(I_SUB6) $(B_SUB6) $(C_SUB6):
	$(TAR_OUT) $(PKG_NAME)_gen.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB7) $(I_SUB7) $(B_SUB7) $(C_SUB7):
	$(TAR_OUT) $(PKG_NAME)_getaline.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB8) $(I_SUB8) $(B_SUB8) $(C_SUB8):
	$(TAR_OUT) $(PKG_NAME)_graph.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#$(F_SUB9) $(I_SUB9) $(B_SUB9) $(C_SUB9):
#	$(TAR_OUT) $(PKG_NAME)_gsd.tar $@
#	@ if test -f $@; then :;\
#           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB10) $(I_SUB10) $(B_SUB10) $(C_SUB10):
	$(TAR_OUT) $(PKG_NAME)_mapdis4.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB11) $(I_SUB11) $(B_SUB11) $(C_SUB11):
	$(TAR_OUT) $(PKG_NAME)_mv4.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB12) $(I_SUB12) $(B_SUB12) $(C_SUB12):
	$(TAR_OUT) $(PKG_NAME)_plots.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB13) $(I_SUB13) $(B_SUB13) $(C_SUB13):
	$(TAR_OUT) $(PKG_NAME)_readline.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB14) $(I_SUB14) $(B_SUB14) $(C_SUB14):
	$(TAR_OUT) $(PKG_NAME)_reduce.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB15) $(I_SUB15) $(B_SUB15) $(C_SUB15):
	$(TAR_OUT) $(PKG_NAME)_scl.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB16) $(I_SUB16) $(B_SUB16) $(C_SUB16):
	$(TAR_OUT) $(PKG_NAME)_tek.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB17) $(I_SUB17) $(B_SUB17) $(C_SUB17):
	$(TAR_OUT) $(PKG_NAME)_term.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB18) $(I_SUB18) $(B_SUB18) $(C_SUB18):
	$(TAR_OUT) $(PKG_NAME)_util.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB19) $(I_SUB19) $(B_SUB19) $(C_SUB19):
	$(TAR_OUT) $(PKG_NAME)_vms.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(F_SUB20) $(I_SUB20) $(B_SUB20) $(C_SUB20):
	$(TAR_OUT) $(PKG_NAME)_command.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  Rules for extracting platform specific source files from the source
#  archives.

#  It is quite likely that there are no platform-specific source files
#  and that the macro PF_SUBn is empty. To prevent a syntax error in
#  make, a dummy target is present.

$(PF_SUB1) dummy_target1:
	$(TAR_OUT) $(PKG_NAME)_astro.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB2) dummy_target2:
	$(TAR_OUT) $(PKG_NAME)_exprt.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB3) dummy_target3:
	$(TAR_OUT) $(PKG_NAME)_external.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB4) dummy_target4:
	$(TAR_OUT) $(PKG_NAME)_fitting.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB5) dummy_target5:
	$(TAR_OUT) $(PKG_NAME)_fv4.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB6) dummy_target6:
	$(TAR_OUT) $(PKG_NAME)_gen.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB7) dummy_target7:
	$(TAR_OUT) $(PKG_NAME)_getaline.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB8) dummy_target8:
	$(TAR_OUT) $(PKG_NAME)_graph.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

#$(PF_SUB9) dummy_target9:
#	$(TAR_OUT) $(PKG_NAME)_gsd.tar $@_$(SOURCE_VARIANT)
#	@ if test -f $@_$(SOURCE_VARIANT); then :;\
#           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
#	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB10) dummy_target10:
	$(TAR_OUT) $(PKG_NAME)_mapdis4.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB11) dummy_target11:
	$(TAR_OUT) $(PKG_NAME)_mv4.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB12) dummy_target12:
	$(TAR_OUT) $(PKG_NAME)_plots.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB13) dummy_target13:
	$(TAR_OUT) $(PKG_NAME)_readline.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB14) dummy_target14:
	$(TAR_OUT) $(PKG_NAME)_reduce.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB15) dummy_target15:
	$(TAR_OUT) $(PKG_NAME)_scl.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB16) dummy_target16:
	$(TAR_OUT) $(PKG_NAME)_tek.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB17) dummy_target17:
	$(TAR_OUT) $(PKG_NAME)_term.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB18) dummy_target18:
	$(TAR_OUT) $(PKG_NAME)_util.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB19) dummy_target19:
	$(TAR_OUT) $(PKG_NAME)_vms.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB20) dummy_target20:
	$(TAR_OUT) $(PKG_NAME)_command.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(O_SUB1) $(O_SUB2) $(O_SUB3) $(O_SUB4) $(O_SUB5) \
               $(O_SUB6) $(O_SUB7) $(O_SUB8) $(O_SUB9) \
               $(O_SUB10) $(O_SUB11) $(O_SUB12) $(O_SUB13) \
               $(O_SUB14) $(O_SUB15) $(O_SUB16) $(O_SUB17) $(O_SUB18) \
               $(O_SUB19) $(O_SUB20) \
               $(EXE_1_OBJECT) $(EXE_2_OBJECT)

O_SUB1 = $(B_SUB1:.f=.o) $(F_SUB1:.f=.o) $(C_SUB1:.c=.o) $(PF_SUB1:.f=.o) 

O_SUB2 = $(B_SUB2:.f=.o) $(F_SUB2:.f=.o) $(C_SUB2:.c=.o) $(PF_SUB2:.f=.o)

O_SUB3 = $(B_SUB3:.f=.o) $(F_SUB3:.f=.o) $(C_SUB3:.c=.o) $(PF_SUB3:.f=.o)

O_SUB4 = $(B_SUB4:.f=.o) $(F_SUB4:.f=.o) $(C_SUB4:.c=.o) $(PF_SUB4:.f=.o)

O_SUB5 = $(B_SUB5:.f=.o) $(F_SUB5:.f=.o) $(C_SUB5:.c=.o) $(PF_SUB5:.f=.o)

O_SUB6 = $(B_SUB6:.f=.o) $(F_SUB6:.f=.o) $(C_SUB6:.c=.o) $(PF_SUB6:.f=.o)

O_SUB7 = $(B_SUB7:.f=.o) $(F_SUB7:.f=.o) $(C_SUB7:.c=.o) $(PF_SUB7:.f=.o)

O_SUB8 = $(B_SUB8:.f=.o) $(F_SUB8:.f=.o) $(C_SUB8:.c=.o) $(PF_SUB8:.f=.o)

O_SUB9 = $(B_SUB9:.f=.o) $(F_SUB9:.f=.o) $(C_SUB9:.c=.o) $(PF_SUB9:.f=.o)

O_SUB10 = $(B_SUB10:.f=.o) $(F_SUB10:.f=.o) $(C_SUB10:.c=.o) $(PF_SUB10:.f=.o)

O_SUB11 = $(B_SUB11:.f=.o) $(F_SUB11:.f=.o) $(C_SUB11:.c=.o) $(PF_SUB11:.f=.o)

O_SUB12 = $(B_SUB12:.f=.o) $(F_SUB12:.f=.o) $(C_SUB12:.c=.o) $(PF_SUB12:.f=.o)

O_SUB13 = $(B_SUB13:.f=.o) $(F_SUB13:.f=.o) $(C_SUB13:.c=.o) $(PF_SUB13:.f=.o)

O_SUB14 = $(B_SUB14:.f=.o) $(F_SUB14:.f=.o) $(C_SUB14:.c=.o) $(PF_SUB14:.f=.o)

O_SUB15 = $(B_SUB15:.f=.o) $(F_SUB15:.f=.o) $(C_SUB15:.c=.o) $(PF_SUB15:.f=.o)

O_SUB16 = $(B_SUB16:.f=.o) $(F_SUB16:.f=.o) $(C_SUB16:.c=.o) $(PF_SUB16:.f=.o)

O_SUB17 = $(B_SUB17:.f=.o) $(F_SUB17:.f=.o) $(C_SUB17:.c=.o) $(PF_SUB17:.f=.o)

O_SUB18 = $(B_SUB18:.f=.o) $(F_SUB18:.f=.o) $(C_SUB18:.c=.o) $(PF_SUB18:.f=.o)

O_SUB19 = $(B_SUB19:.f=.o) $(F_SUB19:.f=.o) $(C_SUB19:.c=.o) $(PF_SUB19:.f=.o)

O_SUB20 = $(B_SUB20:.f=.o) $(F_SUB20:.f=.o) $(C_SUB20:.c=.o) $(PF_SUB20:.f=.o)

EXE_1_OBJECT = $(EXE_1_SOURCE_F:.f=.o) $(EXE_1_SOURCE_C:.c=.o)

EXE_2_OBJECT = $(EXE_2_SOURCE_F:.f=.o) $(EXE_2_SOURCE_C:.c=.o)

.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = \
   libcommand.a libexprt.a libexternal.a libreduce.a libscl.a libfitting.a \
   libplots.a libterm.a libmapdis4.a libtek.a libplots.a libmapdis4.a \
   libmv4.a libfv4.a libutil.a libgraph.a libastro.a libgen.a libvms.a \
   libgetaline.a libreadline.a

libastro.a: $(O_SUB1)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libexprt.a: $(O_SUB2)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libexternal.a: $(O_SUB3)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libfitting.a: $(O_SUB4)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libfv4.a: $(O_SUB5)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libgen.a: $(O_SUB6)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libgetaline.a: $(O_SUB7)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libgraph.a: $(O_SUB8)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#libgsd.a: $(O_SUB9)
#	$(AR_IN) $@ $?
#	$(RANLIB) $@

libmapdis4.a: $(O_SUB10)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libmv4.a: $(O_SUB11)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libplots.a: $(O_SUB12)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libreadline.a: $(O_SUB13)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libreduce.a: $(O_SUB14)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libscl.a: $(O_SUB15)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libtek.a: $(O_SUB16)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libterm.a: $(O_SUB17)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libutil.a: $(O_SUB18)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libvms.a: $(O_SUB19)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libcommand.a: $(O_SUB20)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  List of executables and rules for building them.

$(EXE_1): $(EXE_1_OBJECT) $(OBJECT_LIBRARIES) $(LINK_SCRIPT)
	$(CFLINK) $(EXE_1_OBJECT) -o $(EXE_1) $(OBJECT_LIBRARIES) \
           -L$(FIGARO_SOURCE) -lfit -ltio -lgen -lich \
           $(LDFLAG1) $(ADDLIBS) -L$(STAR_LIB) \
           `./$(LINK_SCRIPT)` $(CFLIBS) $(LDFLAG2)

$(EXE_2): $(EXE_2_OBJECT) $(OBJECT_LIBRARIES) $(LINK_SCRIPT)
	$(CFLINK) $(EXE_2_OBJECT) -o $(EXE_2) $(OBJECT_LIBRARIES) \
           $(LDFLAG1) $(ADDLIBS) -L$(STAR_LIB) \
           `./$(LINK_SCRIPT)` $(CFLIBS) $(LDFLAG2)

#  Rules for building the help libraries.

HELP_FILES = $(HELP_SOURCES:.hlp=.shl)

.hlp.shl:
	$(STAR_BIN)/hlib $<

#  Name of the date stamp file.  This is used to record the time of the
#  most recent build for use in subsequent operations that require it.
#  There must be no rule for generating this file; it is updated only
#  as a side effect of building the package.

DATE_STAMP = $(PKG_NAME)_datestamp

#  Pathnames of directories into which files may be placed when the
#  package is installed.

INSTALL_BIN = $(INSTALL)/bin/$(PKG_NAME)
INSTALL_DATES = $(INSTALL)/dates
INSTALL_DOCS = $(INSTALL)/docs
# INSTALL_HELP = $(INSTALL)/help/$(PKG_NAME)

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_DOCS)

$(INSTALL_DIRS):
	mkdir -p $@

#-------------------------------------------------------------------------------

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
             if test ! -r $$f; then \
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
   '*** "make" failed installing the $(PACK_NAME) package in directory $(INSTALL)';\
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
#  Install the executables, giving them world execute permission.
	for f in $(EXECUTABLES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
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
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the package startup script.  The package version number,
#  the name of the directory containing the installed binaries and 
#  interface files, and the name of the directory containing the help
#  libraries must be edited into this, and execute permission given.
#  Leave the original file in place.
	if test -n "$(STARTUP_SCRIPT)"; then \
           sed -e s#INSTALL_BIN#$(INSTALL_BIN)# \
               -e s#INSTALL_HELP#$(INSTALL_HELP)# \
               -e s#PKG_VERS#$(PKG_VERS)# \
              $(STARTUP_SCRIPT) >$(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
           chmod 755 $(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
        else :; fi
#
#  Install the Latex documentation, giving it world read permission,
#  leaving the source copy in place.
	for f in $(LATEX_DOCS) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_DOCS);\
              chmod 644 $(INSTALL_DOCS)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_DOCS)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the documentation figures.
	for f in $(FIGURES) ""; do \
           if test -n "$$f"; then \
              $(TAR_IN) - $$f | ( cd $(INSTALL_DOCS) ; $(TAR_OUT) - ) ;\
              chmod 644 $(INSTALL_DOCS)/$$f/* ;\
              rm -rf $$f;\
              $(LINK) $(INSTALL_DOCS)/$$f $$f;\
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
'*** "make" failed deinstalling the $(PACK_NAME) package from directory $(INSTALL)';\
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
#  Deinstall the executables, if installed versions exist.
	- for f in $(EXECUTABLES) ""; do \
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
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the package startup file.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
	- if test -n "$(STARTUP_SCRIPT)"; then \
           if test -f $(INSTALL_BIN)/$(STARTUP_SCRIPT); then\
              rm -f $(INSTALL_BIN)/$(STARTUP_SCRIPT);\
           else :; fi ;\
           $(MAKE) $(STARTUP_SCRIPT);\
        else :; fi
#
#  Deinstall the Latex documentation, if installed versions exist.
	- for f in $(LATEX_DOCS) ""; do \
           if test -n "$$f" -a -f $(INSTALL_DOCS)/$$f; then \
              chmod 644 $$f; rm -f $$f;\
              chmod 644 $(INSTALL_DOCS)/$$f;\
              cp -p $(INSTALL_DOCS)/$$f .;\
              rm -f $(INSTALL_DOCS)/$$f;\
           else :; fi;\
        done

#  Deinstall the documentation figures.
	- for f in $(FIGURES) ""; do \
           if test -n "$$f" -a -d $(INSTALL_DOCS)/$$f; then \
              chmod 644 $$f; rm -f $$f;\
              chmod 744 $(INSTALL_DOCS)/$$f;\
              ( cd $(INSTALL_DOCS) ; $(TAR_IN) - $$f ) | $(TAR_OUT) -  ;\
              rm -rf $(INSTALL_DOCS)/$$f;\
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
	@- rm -f .INSTALLED_$(SYSTEM) 1>/dev/null 2>/dev/null
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
	@ echo '$(PACK_NAME) does not have a make-driven installation test'
	@ echo
	@ echo \
           'To test $(PACK_NAME), you should run the demonstration script thus'
	@ echo '(% is the shell prompt):'
	@ echo
	@ echo ' % setenv SYS_SPECX $(INSTALL)/bin/specx'
	@ echo ' % cd /empty_directory'
	@ echo ' % cp $$SYS_SPECX/l483core.dat .'
	@ echo ' % cp $$SYS_SPECX/l483cmap.map .'
	@ echo ' % cp $$SYS_SPECX/scan_0043.dat .'
	@ echo ' % cp $$SYS_SPECX/obs_das_0011.dat .'
	@ echo ' % source $$SYS_SPECX/specxstart'
	@ echo ' % specx'
	@ echo ' >> @ $$SYS_SPECX/test'
	@ echo ' ...'
	@ echo ' ** EXTRACT-QUADRANT'
	@ echo \
          ' -- SPECX#008 -E- Not enough spectra in stack for this command --'
	@ echo ' >> @ $$SYS_SPECX/test'
	@ echo ' ...'
	@ echo ' ** CLOSE-MAP'
	@ echo ' >> exit'
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
	@- rm -f $(INCLUDE_LINKS) $(PRIVATE_INCLUDES) \
              $(HELP_SOURCES) $(PLATFORM_F) $(BLOCK_DATA) \
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

#  strip: strip the executables of debug information.
#  -------------------------------------------------

strip:
	strip $(EXECUTABLES)
	@ echo '*** Executables stripped'

#  archive: produce the source tar files
#  -------------------------------------

#  First, some rules for extracting the platform specific files

$(PF_SUB18:.f=.f_alpha_OSF1) $(PF_SUB18:.f=.f_sun4_Solaris) \
$(PF_SUB18:.f=.f_ix86_Linux):
	$(TAR_OUT) $(PKG_NAME)_util.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(PF_SUB19:.f=.f_alpha_OSF1) $(PF_SUB19:.f=.f_sun4_Solaris) \
$(PF_SUB19:.f=.f_ix86_Linux):
	$(TAR_OUT) $(PKG_NAME)_vms.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  Now the main archive target.
#  To avoid specifying that the tar files depend on their content files,
#  and thus forcing all their contents to be extracted whenever
#  anything (e.g. an export) depends on the tar files, we make use of some
#  flag files (suffix .tarf) which are set to have the same modification time
#  as the associated tar file. We then make target archive1, which depends on
#  these flag files, which in turn depend upon the associated tar content
#  files. If flag file and (by inference) the tar file are out of date, we
#  re-create the tar file.
#  The effect of this is that 'make archive', ensures that all the tar
#  content files for all the tar files are present but only re-creates those
#  tar files whose content files have been updated.
 
archive: 
	touch -r $(PKG_NAME)_source.tar $(PKG_NAME)_source.tarf
	touch -r $(PKG_NAME)_astro.tar $(PKG_NAME)_astro.tarf
	touch -r $(PKG_NAME)_exprt.tar $(PKG_NAME)_exprt.tarf
	touch -r $(PKG_NAME)_external.tar $(PKG_NAME)_external.tarf
	touch -r $(PKG_NAME)_fitting.tar $(PKG_NAME)_fitting.tarf
	touch -r $(PKG_NAME)_fv4.tar $(PKG_NAME)_fv4.tarf
	touch -r $(PKG_NAME)_gen.tar $(PKG_NAME)_gen.tarf
	touch -r $(PKG_NAME)_getaline.tar $(PKG_NAME)_getaline.tarf
	touch -r $(PKG_NAME)_graph.tar $(PKG_NAME)_graph.tarf
	touch -r $(PKG_NAME)_mapdis4.tar $(PKG_NAME)_mapdis4.tarf
	touch -r $(PKG_NAME)_mv4.tar $(PKG_NAME)_mv4.tarf
	touch -r $(PKG_NAME)_plots.tar $(PKG_NAME)_plots.tarf
	touch -r $(PKG_NAME)_readline.tar $(PKG_NAME)_readline.tarf
	touch -r $(PKG_NAME)_reduce.tar $(PKG_NAME)_reduce.tarf
	touch -r $(PKG_NAME)_scl.tar $(PKG_NAME)_scl.tarf
	touch -r $(PKG_NAME)_tek.tar $(PKG_NAME)_tek.tarf
	touch -r $(PKG_NAME)_term.tar $(PKG_NAME)_term.tarf
	touch -r $(PKG_NAME)_util.tar $(PKG_NAME)_util.tarf
	touch -r $(PKG_NAME)_vms.tar $(PKG_NAME)_vms.tarf
	touch -r $(PKG_NAME)_command.tar $(PKG_NAME)_command.tarf
	touch -r $(PKG_NAME)_inc.tar $(PKG_NAME)_inc.tarf
	touch -r $(PKG_NAME)_comfiles.tar $(PKG_NAME)_comfiles.tarf
	@ if $(MAKE) archive1; then\
           echo;\
           echo \
           '*** Archive for the $(PACK_NAME) package complete';\
           echo;\
        else \
           echo;\
           echo \
           '*** Archive for the $(PACK_NAME) package failed';\
           echo;\
           exit 1;\
        fi

archive1: $(PKG_NAME)_source.tarf \
   $(PKG_NAME)_astro.tarf $(PKG_NAME)_exprt.tarf $(PKG_NAME)_external.tarf \
   $(PKG_NAME)_fitting.tarf $(PKG_NAME)_fv4.tarf $(PKG_NAME)_gen.tarf \
   $(PKG_NAME)_getaline.tarf $(PKG_NAME)_graph.tarf \
   $(PKG_NAME)_mapdis4.tarf $(PKG_NAME)_mv4.tarf $(PKG_NAME)_plots.tarf \
   $(PKG_NAME)_readline.tarf $(PKG_NAME)_reduce.tarf $(PKG_NAME)_scl.tarf \
   $(PKG_NAME)_tek.tarf $(PKG_NAME)_term.tarf $(PKG_NAME)_util.tarf \
   $(PKG_NAME)_vms.tarf $(PKG_NAME)_command.tarf \
   $(PKG_NAME)_inc.tarf $(PKG_NAME)_comfiles.tarf


$(PKG_NAME)_source.tarf: $(LINK_SCRIPT) $(EXE_SOURCES_F) $(EXE_SOURCES_C) \
                        $(I_SUBB) \
                        $(HELP_SOURCES) $(STARTUP_SCRIPT) $(DATA_FILES)
	$(TAR_IN) $(PKG_NAME)_source.tar $(LINK_SCRIPT) $(EXE_SOURCES_F) \
                        $(EXE_SOURCES_C) $(I_SUBB) \
                        $(HELP_SOURCES) $(STARTUP_SCRIPT) $(DATA_FILES)

$(PKG_NAME)_comfiles.tarf: $(PUBLIC_SCRIPTS)
	$(TAR_IN) $(PKG_NAME)_comfiles.tar $(PUBLIC_SCRIPTS)

$(PKG_NAME)_inc.tarf: $(I_SUBA)
	$(TAR_IN) $(PKG_NAME)_inc.tar $(I_SUBA)

$(PKG_NAME)_astro.tarf: $(F_SUB1) $(I_SUB1) $(B_SUB1) $(C_SUB1)
	$(TAR_IN) $(PKG_NAME)_astro.tar $(F_SUB1) $(I_SUB1) \
           $(B_SUB1) $(C_SUB1)

$(PKG_NAME)_exprt.tarf: $(F_SUB2) $(I_SUB2) $(B_SUB2) $(C_SUB2)
	$(TAR_IN) $(PKG_NAME)_exprt.tar $(F_SUB2) $(I_SUB2) \
                        $(B_SUB2) $(C_SUB2)

$(PKG_NAME)_external.tarf: $(F_SUB3) $(I_SUB3) $(B_SUB3) $(C_SUB3)
	$(TAR_IN) $(PKG_NAME)_external.tar $(F_SUB3) $(I_SUB3) \
                        $(B_SUB3) $(C_SUB3)

$(PKG_NAME)_fitting.tarf: $(F_SUB4) $(I_SUB4) $(B_SUB4) $(C_SUB4)
	$(TAR_IN) $(PKG_NAME)_fitting.tar $(F_SUB4) $(I_SUB4) \
                        $(B_SUB4) $(C_SUB4)

$(PKG_NAME)_fv4.tarf: $(F_SUB5) $(I_SUB5) $(B_SUB5) $(C_SUB5)
	$(TAR_IN) $(PKG_NAME)_fv4.tar $(F_SUB5) $(I_SUB5) \
                        $(B_SUB5) $(C_SUB5)

$(PKG_NAME)_gen.tarf: $(F_SUB6) $(I_SUB6) $(B_SUB6) $(C_SUB6)
	$(TAR_IN) $(PKG_NAME)_gen.tar $(F_SUB6) $(I_SUB6) \
                        $(B_SUB6) $(C_SUB6)

$(PKG_NAME)_getaline.tarf: $(F_SUB7) $(I_SUB7) $(B_SUB7) $(C_SUB7)
	$(TAR_IN) $(PKG_NAME)_getaline.tar $(F_SUB7) $(I_SUB7) \
                        $(B_SUB7) $(C_SUB7)

$(PKG_NAME)_graph.tarf: $(F_SUB8) $(I_SUB8) $(B_SUB8) $(C_SUB8)
	$(TAR_IN) $(PKG_NAME)_graph.tar $(F_SUB8) $(I_SUB8) \
                        $(B_SUB8) $(C_SUB8)

#$(PKG_NAME)_gsd.tarf: $(F_SUB9) $(I_SUB9) $(B_SUB9) $(C_SUB9)
#	$(TAR_IN) $(PKG_NAME)_gsd.tar $(F_SUB9) $(I_SUB9) \
#                        $(B_SUB9) $(C_SUB9)

$(PKG_NAME)_mapdis4.tarf: $(F_SUB10) $(I_SUB10) $(B_SUB10) $(C_SUB10)
	$(TAR_IN) $(PKG_NAME)_mapdis4.tar $(F_SUB10) \
                        $(I_SUB10) $(B_SUB10) $(C_SUB10)

$(PKG_NAME)_mv4.tarf: $(F_SUB11) $(I_SUB11) $(B_SUB11) $(C_SUB11)
	$(TAR_IN) $(PKG_NAME)_mv4.tar $(F_SUB11) $(I_SUB11) \
                        $(B_SUB11) $(C_SUB11)

$(PKG_NAME)_plots.tarf: $(F_SUB12) $(I_SUB12) $(B_SUB12) $(C_SUB12)
	$(TAR_IN) $(PKG_NAME)_plots.tar $(F_SUB12) $(I_SUB12) \
                        $(B_SUB12) $(C_SUB12)

$(PKG_NAME)_readline.tarf: $(F_SUB13) $(I_SUB13) $(B_SUB13) \
                          $(C_SUB13)
	$(TAR_IN) $(PKG_NAME)_readline.tar $(F_SUB13) $(I_SUB13) \
                        $(B_SUB13) $(C_SUB13)

$(PKG_NAME)_reduce.tarf: $(F_SUB14) $(I_SUB14) $(B_SUB14) $(C_SUB14)
	$(TAR_IN) $(PKG_NAME)_reduce.tar $(F_SUB14) $(I_SUB14) \
                        $(B_SUB14) $(C_SUB14)

$(PKG_NAME)_scl.tarf: $(F_SUB15) $(I_SUB15) $(B_SUB15) $(C_SUB15)
	$(TAR_IN) $(PKG_NAME)_scl.tar $(F_SUB15) $(I_SUB15) \
                        $(B_SUB15) $(C_SUB15)

$(PKG_NAME)_tek.tarf: $(F_SUB16) $(I_SUB16) $(B_SUB16) $(C_SUB16)
	$(TAR_IN) $(PKG_NAME)_tek.tar $(F_SUB16) $(I_SUB16) \
                        $(B_SUB16) $(C_SUB16)

$(PKG_NAME)_term.tarf: $(F_SUB17) $(I_SUB17) $(B_SUB17) $(C_SUB17)
	$(TAR_IN) $(PKG_NAME)_term.tar $(F_SUB17) $(I_SUB17) \
                        $(B_SUB17) $(C_SUB17)

$(PKG_NAME)_util.tarf: $(F_SUB18) $(I_SUB18) $(B_SUB18) $(C_SUB18) \
           $(PF_SUB18:.f=.f_alpha_OSF1) $(PF_SUB18:.f=.f_sun4_Solaris) \
           $(PF_SUB18:.f=.f_ix86_Linux)
	$(TAR_IN) $(PKG_NAME)_util.tar $(F_SUB18) $(I_SUB18) \
           $(B_SUB18) $(C_SUB18) \
           $(PF_SUB18:.f=.f_alpha_OSF1) $(PF_SUB18:.f=.f_sun4_Solaris \
           $(PF_SUB18:.f=.f_ix86_Linux

$(PKG_NAME)_vms.tarf: $(F_SUB19) $(I_SUB19) $(B_SUB19) $(C_SUB19) \
           $(PF_SUB19:.f=.f_alpha_OSF1) $(PF_SUB19:.f=.f_sun4_Solaris) \
           $(PF_SUB19:.f=.f_ix86_Linux)
	$(TAR_IN) $(PKG_NAME)_vms.tar $(F_SUB19) $(I_SUB19) \
           $(B_SUB19) $(C_SUB19) \
           $(PF_SUB19:.f=.f_alpha_OSF1) $(PF_SUB19:.f=.f_sun4_Solaris) \
           $(PF_SUB19:.f=.f_ix86_Linux)

$(PKG_NAME)_command.tarf: $(F_SUB20) $(I_SUB20) $(B_SUB20) $(C_SUB20)
	$(TAR_IN) $(PKG_NAME)_command.tar $(F_SUB20) $(I_SUB20) \
           $(B_SUB20) $(C_SUB20)

#-------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files).  These are normally generated automatically from the source
#  files.

#  Dependencies for source

scl_main.o: COMMAND_TABLE 
scl_main.o: CUBE 
scl_main.o: DAT_PAR 
scl_main.o: DOPPLER 
scl_main.o: ERRORS 
scl_main.o: FILES 
scl_main.o: FILHD 
scl_main.o: FILHDI2 
scl_main.o: FLAGCOMM 
scl_main.o: HEADER 
scl_main.o: MAPHD 
scl_main.o: MAPS 
scl_main.o: MAPV4 
scl_main.o: NEWXY 
scl_main.o: PLOT2D 
scl_main.o: PLOTPAR1 
scl_main.o: PROTOTYPE 
scl_main.o: SPECX_FITS 
scl_main.o: SPECX_PARS 
scl_main.o: STAKPAR 
scl_main.o: SYMBOLS 
scl_main.o: TITLE 
scl_main.o: WEIGHTS 
run_specx.o: f77.h

#  Dependencies for astro -- none

#  Dependencies for exprt

bad2blank.o: FLAGCOMM
calc_image.o: FLAGCOMM
close_fits.o: SPECX_FITS
fitsmap.o: FLAGCOMM SPECX_FITS
index_gsd.o: GSD_FILE.INC
open_fits.o: SPECX_FITS
rd_gsd_ras.o: FLAGCOMM GSD_FILE.INC GSD_VAR.INC STACKCOMM STAKPAR
rdfitsspec.o: FLAGCOMM SPECX_FITS STACKCOMM STAKPAR
read_gsd.o: FLAGCOMM GSD_FILE.INC GSD_VAR.INC STACKCOMM STAKPAR
sgsdclose.o: GSD_FILE.INC
sgsdlist.o: FLAGCOMM GSD_VAR.INC
sgsdlocate.o: GSD_VAR.INC SPECX_PARS STACKCOMM STAKPAR
sgsdopen.o: FLAGCOMM GSD_FILE.INC
sgsdreopen.o: FLAGCOMM GSD_FILE.INC
sgsdv4data.o: FLAGCOMM GSD_FILE.INC GSD_INDEX.INC GSD_VAR.INC SPECX_PARS STACKCOMM STAKPAR
sgsdv4header.o: FLAGCOMM GSD_FILE.INC GSD_INDEX.INC GSD_VAR.INC STACKCOMM
sgsdv51data.o: FLAGCOMM GSD_FILE.INC GSD_INDEX.INC GSD_VAR.INC SPECX_PARS STACKCOMM STAKPAR
sgsdv52data.o: FLAGCOMM GSD_FILE.INC GSD_INDEX.INC GSD_VAR.INC SPECX_PARS STACKCOMM STAKPAR
sgsdv5data.o: FLAGCOMM GSD_FILE.INC GSD_INDEX.INC GSD_VAR.INC SPECX_PARS STACKCOMM STAKPAR
sgsdv5header.o: FLAGCOMM GSD_FILE.INC GSD_INDEX.INC GSD_VAR.INC STACKCOMM
wrfitscube.o: CUBE DOPPLER FLAGCOMM MAPHD MAPS SPECX_FITS STACKCOMM STAKPAR
wrfitsmap.o: DOPPLER FLAGCOMM MAPHD MAPS PLOT2D PROTOTYPE SPECX_FITS
wrfitsspec.o: DOPPLER FLAGCOMM SPECX_FITS STACKCOMM
write_gildas.o: CUBE MAPHD MAPS PROTOTYPE

#  Dependencies for external

getintspec.o: MAPHD CUBE FLAGCOMM MAPHD MAPS PLOT2D STACKCOMM STAKPAR
ut_cvt.o: FLAGCOMM STACKCOMM

#  Dependencies for fitting

basfit.o: FLAGCOMM STACKCOMM
calc_gauss.o: FLAGCOMM SPECX_PARS STACKCOMM
display_gauss.o: FLAGCOMM
enter_gauss.o: FLAGCOMM
funval.o: SPECX_PARS
gaussf.o: SPECX_PARS
linfit.o: FLAGCOMM SPECX_PARS STACKCOMM STAKPAR
lsqfit.o: FLAGCOMM SPECX_PARS STACKCOMM
sinfit.o: FLAGCOMM SPECX_PARS STACKCOMM STAKPAR

#  Dependencies for fv4

chkacc.o: DAT_PAR FILES
fv4_cnv321.o: DAT_ERR DAT_PAR SAE_PAR STACKCOMM STAKPAR
fv4_filacc.o: DAT_PAR FILES SAE_PAR
fv4_filcls.o: DAT_PAR FILES SAE_PAR
fv4_filcmp.o: DAT_PAR FILES FILHD FLAGCOMM SAE_PAR
fv4_filhde.o: DAT_PAR FILES SAE_PAR
fv4_filidx.o: DAT_PAR FILES FILHD FLAGCOMM SAE_PAR
fv4_filinf.o: DAT_PAR FILES FILHD FLAGCOMM SAE_PAR
fv4_filini.o: DAT_PAR FILHD SAE_PAR
fv4_filmerg.o: DAT_PAR FILES FLAGCOMM SAE_PAR SPECX_PARS
fv4_filopn.o: DAT_PAR FILES SAE_PAR
fv4_filrcv.o: DAT_PAR FILES FILHD SAE_PAR
fv4_filrop.o: DAT_PAR FILES SAE_PAR
fv4_spcrdtj.o: DAT_PAR FILES FILHD SAE_PAR SPECX_PARS STACKCOMM STAKPAR
fv4_spec2c.o: DAT_PAR FLAGCOMM PRM_PAR SAE_PAR STACKCOMM
fv4_specdl.o: DAT_PAR FILES FILHD SAE_PAR
fv4_specix.o: DAT_PAR SAE_PAR SPECX_PARS
fv4_specrd.o: DAT_PAR FILES FILHD FLAGCOMM PRM_PAR SAE_PAR SPECX_PARS STACKCOMM STAKPAR
fv4_specwa.o: DAT_PAR FILES FILHD SAE_PAR
fv4_specwi.o: DAT_PAR FILES FILHD SAE_PAR STACKCOMM
getfil.o: DAT_PAR FILES
lstfil.o: DAT_PAR FILES
nfile.o: DAT_PAR FILES
rdump.o: DAT_PAR FILES
write_ascii.o: FLAGCOMM STACKCOMM

#  Dependencies for gen

cli_empty.o: CLI_STACK.INC
gen_at.o: CLI_STACK.INC
gen_do_op.o: eval_ae4.inc
gen_encode.o: LOGICAL_UNITS.INC
gen_eval_ae.o: eval_ae4.inc
gen_eval_all.o: eval_ae4.inc
gen_eval_str.o: gen_symbols.inc
gen_factor.o: eval_ae4.inc
gen_get_luns.o: LOGICAL_UNITS.INC
gen_getline.o: CLI_STACK.INC LOGICAL_UNITS.INC
gen_getmore.o: CLI_STACK.INC
gen_ichtot.o: CLI_STACK.INC
gen_input.o: CLI_STACK.INC
gen_inqsymb.o: GEN_SYMBOLS.INC
gen_lineno.o: CLI_STACK.INC
gen_operator.o: eval_ae4.inc
gen_putpmt.o: LOGICAL_UNITS.INC
gen_rewind.o: CLI_STACK.INC
gen_set_cli.o: CLI_STACK.INC
gen_set_luns.o: LOGICAL_UNITS.INC
gen_set_symt.o: GEN_SYMBOLS.INC
gen_sprint.o: LOGICAL_UNITS.INC
gen_unwind.o: CLI_STACK.INC
ins_cli_item.o: CLI_STACK.INC
refresh_cli.o: CLI_STACK.INC
reset_stk_pt.o: CLI_STACK.INC
stack_pointer.o: CLI_STACK.INC
hash.o: f77.h symbols.h
gen_makesym2.o: f77.h symbols.h

#  Dependencies for getaline -- none
   
#  Dependencies for graph

getdev.o: SXG_GRAPHCAP.INC
sxgpgplot.o: SXGPGPLOT.INC SXG_GRAPHCAP.INC SXGPGPLOT.INC SXG_GRAPHCAP.INC SXGPGPLOT.INC SXG_GRAPHCAP.INC
sxg1pgplot.o: f77.h sxg_graphcap.h
ttsub.o: SXGPGPLOT.INC
   
#  Dependencies for gsd

#gsd1_getval.o: gsd1.h
#gsd1_rddata.o: gsd1.h
#gsd1_rdfildsc.o: gsd1.h
#gsd1_rdhead.o: gsd1.h
#gsd2_copya.o: gsd1.h
#gsd2_copya.o: gsd2.h
#gsd2_nativx.o: gsd2.h
#gsdClose.o: gsd1.h
#gsdClose.o: gsd.h
#gsdFind.o: gsd1.h
#gsdFind.o: gsd.h
#gsdGet0x.o: gsd1.h
#gsdGet0x.o: gsd.h
#gsdGet1x.o: gsd1.h
#gsdGet1x.o: gsd.h
#gsdInqSize.o: gsd1.h
#gsdInqSize.o: gsd.h
#gsdItem.o: gsd1.h
#gsdItem.o: gsd.h
#gsdOpenRead.o: gsd1.h
#gsdOpenRead.o: gsd.h
#gsd_f77.o: gsd.h
   
#  Dependencies for mapdis4

anm_block.o: ANM_COL.INC
anm_col.o: ANM_COL.INC ANM_LOGCOL.INC FLAGCOMM
contour_map4.o: FLAGCOMM MAPHD MAPS PLOT2D STACKCOMM
export_map4.o: PLOT2D FLAGCOMM PLOT2D
getmap2.o: CUBE FLAGCOMM MAPHD MAPS PLOT2D
getpars.o: CUBE FLAGCOMM MAPHD MAPS PLOT2D PROTOTYPE
goodpt.o: FLAGCOMM MAPHD PLOT2D
grid_map.o: CUBE FLAGCOMM MAPHD MAPS MAPTITLES PLOT2D SPECX_PARS STACKCOMM
label_map.o: FLAGCOMM MAPHD MAPTITLES PLOT2D
listmp.o: CUBE FLAGCOMM MAPHD MAPS STACKCOMM
make_chan4.o: CUBE FLAGCOMM MAPHD MAPS MAPTITLES PLOT2D STACKCOMM
make_line4.o: CUBE FLAGCOMM MAPHD MAPS MAPTITLES PLOT2D STACKCOMM
make_map4.o: CUBE FLAGCOMM MAPHD MAPS PLOT2D STACKCOMM
map_coords.o: MAPTITLES
map_integ.o: FLAGCOMM PLOT2D SPECX_PARS
map_locate.o: FLAGCOMM PLOT2D
map_maxmin.o: PLOT2D
map_window.o: FLAGCOMM MAPHD PLOT2D
mark_samples.o: FLAGCOMM PLOT2D
mwindo.o: FLAGCOMM MAPHD PLOT2D
new_scales.o: FLAGCOMM PLOT2D
plot2d_range.o: MAPHD MAPTITLES PLOT2D
plot_chan4.o: CUBE FLAGCOMM MAPHD MAPS MAPTITLES PLOT2D STACKCOMM
plot_line4.o: FLAGCOMM MAPHD MAPS MAPTITLES PLOT2D STACKCOMM
plot_map.o: FLAGCOMM MAPHD MAPS MAPTITLES PLOT2D
setclevs.o: FLAGCOMM
setgscal.o: FLAGCOMM
   
#  Dependencies for mv4

addmap.o: CUBE FLAGCOMM MAPHD MAPS STACKCOMM
addmap2.o: CUBE FLAGCOMM MAPHD PROTOTYPE
close_map.o: CUBE FLAGCOMM MAPHD MAPS
create_map.o: CUBE FLAGCOMM MAPHD MAPS
cube_subs.o: CUBE MAPHD MAPS
del_from_map.o: CUBE MAPHD MAPS
extmap.o: MAPHD
extr_header.o: MAPHD PROTOTYPE
fromap.o: FLAGCOMM MAPHD
get_spectrum.o: CUBE FLAGCOMM MAPHD MAPS PLOT2D STACKCOMM STAKPAR
index_subs.o: MAPHD
interp_cube.o: CUBE FLAGCOMM MAPHD MAPS PLOT2D PROTOTYPE WEIGHTS
mapopen.o: DAT_ERR NDF_ERR FLAGCOMM MAPHD MAPS
mv4_41to42.o: DAT_ERR DAT_PAR NDF_PAR SAE_PAR
mv4_cnv321.o: DAT_ERR DAT_PAR FLAGCOMM MAPHD MAPV4 PRM_PAR SAE_PAR STACKCOMM
mv4_headrd.o: DAT_PAR MAPHD MAPV4 SAE_PAR
mv4_headwr.o: DAT_PAR MAPHD MAPV4 SAE_PAR
mv4_indxrd.o: DAT_PAR MAPHD MAPV4 PRM_PAR
mv4_indxwr.o: DAT_PAR MAPHD MAPV4 PRM_PAR
mv4_mapcls.o: DAT_PAR MAPHD MAPV4 SAE_PAR
mv4_mapnew.o: DAT_PAR FLAGCOMM MAPHD MAPV4 SAE_PAR
mv4_mapopn.o: DAT_PAR FLAGCOMM MAPHD MAPV4 SAE_PAR
mv4_protrd.o: DAT_PAR MAPHD MAPV4 PROTOTYPE SAE_PAR SPECX_PARS
mv4_protwr.o: DAT_PAR MAPHD MAPV4 SAE_PAR STACKCOMM
mv4_specdl.o: DAT_PAR MAPHD MAPV4 SAE_PAR
mv4_specrd.o: DAT_PAR FLAGCOMM MAPHD MAPV4 PRM_PAR SAE_PAR
mv4_specwr.o: DAT_PAR FLAGCOMM MAPHD MAPV4 PRM_PAR SAE_PAR
open_map.o: CUBE FLAGCOMM MAPHD MAPS PROTOTYPE
read_interp.o: CUBE MAPHD MAPS WEIGHTS
rotate_cube.o: CUBE FLAGCOMM MAPHD MAPS STACKCOMM
set_weights.o: MAPHD WEIGHTS
   
#  Dependencies for plots

alloc_dev.o: FLAGCOMM
close_plot.o: SPECX_PARS
draw_plot.o: PLOTPAR1 SPECX_PARS
make_xtitle.o: FLAGCOMM STACKCOMM
new_plot.o: FLAGCOMM NOKEEP
overlay.o: FLAGCOMM NOKEEP
plot_file.o: FLAGCOMM NEWXY PLOTPAR1 SPECX_PARS
plotxy.o: DOPPLER FLAGCOMM NEWXY PLOTPAR1
pltbuf.o: FLAGCOMM NEWXY NOKEEP STACKCOMM FLAGCOMM STACKCOMM
pltdat.o: DOPPLER FLAGCOMM SPECX_PARS
qplot.o: FLAGCOMM NOKEEP STACKCOMM
see_plot.o: FLAGCOMM NOKEEP
   
#  Dependencies for readline

readline.o: readline.c readline.h rldefs.h rlconf.h chardefs.h
readline.o: keymaps.h history.h
vi_mode.o:  rldefs.h rlconf.h readline.h history.h
funmap.o:   funmap.c readline.h rlconf.h
keymaps.o:  keymaps.c emacs_keymap.c vi_keymap.c keymaps.h chardefs.h rlconf.h
history.o: history.h memalloc.h
isearch.o: memalloc.h readline.h history.h
search.o: memalloc.h readline.h history.h
display.o: readline.h history.h rldefs.h rlconf.h
complete.o: readline.h rldefs.h rlconf.h
rltty.o: rldefs.h rlconf.h readline.h
bind.o: ansi_stdlib.h posixstat.h rldefs.h rlconf.h readline.h history.h
signals.o: rldefs.h rlconf.h readline.h history.h
parens.o: readline.h
rldefs.h: memalloc.h
readline.h: keymaps.h tilde.h
keymaps.h: chardefs.h
   
#  Dependencies for reduce

avgxy.o: FLAGCOMM SPECX_PARS STACKCOMM STAKPAR
bindat.o: FLAGCOMM STACKCOMM
centrd.o: FLAGCOMM STACKCOMM
change_sb.o: FLAGCOMM NEWXY STACKCOMM
clipsp.o: FLAGCOMM STACKCOMM
cnvlve.o: FLAGCOMM STACKCOMM
concatsp.o: SPECX_PARS STACKCOMM STAKPAR
delchn.o: FLAGCOMM STACKCOMM
diffsp.o: FLAGCOMM STACKCOMM
findazel.o: FLAGCOMM STACKCOMM
fmcall.o: FLAGCOMM STACKCOMM
fndwid.o: FLAGCOMM STACKCOMM
foldsp.o: STACKCOMM
hannsp.o: STACKCOMM
intsty.o: FLAGCOMM
invert.o: STACKCOMM
invspc.o: STACKCOMM
listsp.o: FLAGCOMM STACKCOMM
lsrcor.o: FLAGCOMM
merge.o: FLAGCOMM SPECX_PARS STACKCOMM
moment.o: FLAGCOMM STACKCOMM
offsp.o: FLAGCOMM STACKCOMM
quotsp.o: FLAGCOMM STACKCOMM STAKPAR
regrid.o: FLAGCOMM SPECX_PARS STACKCOMM STAKPAR
remspk.o: FLAGCOMM STACKCOMM
setchn.o: FLAGCOMM STACKCOMM
shift.o: FLAGCOMM STACKCOMM
skew.o: FLAGCOMM STACKCOMM
slide.o: FLAGCOMM STACKCOMM
smooth_data.o: FLAGCOMM STACKCOMM
stats.o: FLAGCOMM STACKCOMM
trunc.o: STACKCOMM STAKPAR
   
#  Dependencies for scl

commands.o: COMMAND_TABLE
do_syscomm.o: IODATA JPI SCL_BUFFER.INC SCL_IFDO.INC
init_symbols.o: COMMAND_TABLE SYMBOLS
scl_commsyms.o: COMMAND_TABLE SYMBOLS IODATA
scl_continue.o: ERRORS
scl_do.o: SCL_BUFFER.INC SCL_DO.INC SCL_DO.INC SCL_IFDO.INC
scl_help.o: COMMAND_TABLE
scl_matchcomm.o: COMMAND_TABLE
scl_symbols.o: IODATA
   
#  Dependencies for tek

draw_spectrum.o: FLAGCOMM STACKCOMM
i2dopt.o: FLAGCOMM PLOT2D
ivtopt.o: PLOTPAR1
new_xy.o: FLAGCOMM NEWXY PLOTPAR1
tpair.o: FLAGCOMM PLOTPAR1
   
#  Dependencies for term

ask_interp.o: FLAGCOMM MAPS
ask_plot_dev.o: FLAGCOMM
ask_vel.o: FLAGCOMM
displx.o: FLAGCOMM STACKCOMM
dispst.o: SPECX_PARS STACKCOMM STAKPAR
dstack.o: SPECX_PARS STAKPAR
getpts.o: FLAGCOMM
inithd.o: STACKCOMM
prscan.o: FLAGCOMM STACKCOMM
set_mapscale.o: FLAGCOMM MAPHD MAPS PLOT2D
set_plot_dev.o: FLAGCOMM
show_store.o: SPECX_PARS STACKCOMM STAKPAR
specx_set.o: CUBE FLAGCOMM MAPHD NEWXY PLOT2D SPECX_PARS STAKPAR
stackstat.o: FLAGCOMM
   
#  Dependencies for util

add.o: FLAGCOMM SPECX_PARS STACKCOMM STAKPAR
compressq.o: STACKCOMM
convol.o: FLAGCOMM STACKCOMM
copybf.o: STACKCOMM
dasmerge.o: FLAGCOMM SPECX_PARS STACKCOMM
doquad.o: FLAGCOMM SPECX_PARS STACKCOMM
icheck.o: STACKCOMM STAKPAR
ichkq2.o: FLAGCOMM STACKCOMM
initlun.o: LUNTAB.INC
initnpnew.o: STACKCOMM
initvm.o: VMTAB.INC
inquad.o: STACKCOMM
islct1q.o: FLAGCOMM STACKCOMM
islctq.o: FLAGCOMM STACKCOMM
lun_hndl.o: LUNTAB.INC
mult.o: FLAGCOMM STACKCOMM
npts2.o: SPECX_PARS STACKCOMM STAKPAR
ntot.o: SPECX_PARS STACKCOMM
ntot2.o: STACKCOMM STAKPAR
pop.o: STACKCOMM STAKPAR
push.o: STACKCOMM STAKPAR
qlim.o: STACKCOMM
qmult.o: STACKCOMM
recall.o: SPECX_PARS STACKCOMM STAKPAR
roll.o: STACKCOMM STAKPAR
setx.o: DOPPLER SPECX_PARS DOPPLER FLAGCOMM STACKCOMM
specx_reorder.o: FLAGCOMM STACKCOMM
storex.o: SPECX_PARS STACKCOMM STAKPAR
su.o: FLAGCOMM SPECX_PARS STACKCOMM STAKPAR
vm_hndl.o: VMTAB.INC
xtrctq.o: FLAGCOMM STACKCOMM
xy.o: STACKCOMM STAKPAR

#  Dependencies for vms

putscreen.o: FLAGCOMM
spack.o: SAE_PAR
spacl.o: SAE_PAR
spaeg.o: SAE_PAR
vms_help.o: SAE_PAR
   
#  Dependencies for command

do_command.o: FLAGCOMM MAPS SPECX_PARS STACKCOMM STAKPAR
init_specx.o: COMMAND_TABLE FLAGCOMM MAPS MAPTITLES
initsp.o: COMMAND_TABLE CUBE DAT_PAR FILES FLAGCOMM IODATA MAPS MAPTITLES \
          NEWXY NOKEEP PLOT2D SPECX_FITS SPECX_PARS STACKCOMM STAKPAR
specx_initab.o: CUBE ERRORS FILHD FLAGCOMM GSD_VAR MAPHD MAPS STACKCOMM


#-------------------------------------------------------------------------------
#
#  End of makefile.
#.
