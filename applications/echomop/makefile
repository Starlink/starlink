#+
#  Name:
#     makefile
#
#  Version:
#     Application makefile Mk V
#
#  Purpose:
#     Build and install the ECHOMOP package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     ECHOMOP package from the distributed source files, to install
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
#     The ECHOMOP package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#        ems
#           Error message service
#        hds
#           Hierarchical data system
#        gen
#           figaro subroutines
#        ndf
#           N-d data system
#        pda
#           Public Domain Algorithms' Library
#        hlp
#           Help system
#        fio
#           File I/O system
#        sla
#           Astrometry package
#        pgplot
#           Graph plotting
#        sae_par
#           Global include files
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
#        Added sections for manipulating Latex and Hypertext documentation.
#        Revised install and deinstall sequences.
#        Changed export* targets to make compressed tar files in one step.
#        Replacing of `rm' with `rm -f' in most cases.
#        Designated Mk V.
#     03-JUL-1995 (BLY):
#        Template makefile now generated automatically.
#     17-JUL-1995 (AJC):
#        Modified hypertext installation/deinstallation to avoid linking
#        if no hypertext documents defined.
#     28-JAN-1997 (BLY):
#        Updated to latest style.
#        Reinstated new archive handling techniques.
#     08-AUG-1997 (BLY/MJC):
#        Fix bug in ech_plot_graph.f.  Version 3.3-1.
#     26-AUG-1997 (BLY):
#        Added FIGARO_SOURCE macro to locate FIGARO libraries.
#        Modified build and link of help task to tidy it up using CFLINK
#        and CFLIBS macros (see mk script).  Version 3.3-2
#     06-MAR-1998 (BLY):
#        Several fixes to mismatched data types and precisions in
#           various routines.  Fix to uninitialised variable and illegal
#           format specifiers (Linux).   Version 3.3-3
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

TAR_IN = pax -w -v -x ustar -f
TAR_OUT = pax -r -f

#  Command for adding a file to an object archive (.a).

AR_IN = ar -r

#  ADAM monolith linker command.

ALINK = $(STAR_BIN)/alink

#  Linker flags.  These are placed either side of the library list
#  and are used adding system-dependent linker flags to link commands.

LDFLAG1 =
LDFLAG2 =

#  Linker command to link C main programs with Fortran subroutins and
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

PACK_NAME = ECHOMOP

#  Prefix for the package in lower-case as used in filenames etc.

PKG_NAME = echomop

#  Prefix for the package in upper-case as used in include file links.

PKG_LINK = ECHOMOP

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

PKG_VERS = 3.3-3

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).  It is advisable to use several
#  smaller source archives in place of one large one to speed up build
#  times.

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION) \
   $(TEST_FILES_ARCHIVE) $(DEMO_FILES_ARCHIVE) $(PKG_NAME)_data.tar \
   $(ARC_FILES_ARCHIVE) $(PKG_NAME)_inc.tar $(PKG_NAME)_ifl.tar \
   $(HYPERTEXT_ARCHIVE) $(PKG_NAME)_def.tar

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

PUBLIC_SCRIPTS = decos_many ech_toarchive

#  Startup script.  This is the file that must be executed (sourced)
#  by a user of this package in order to define appropriate aliases
#  and environment variables.  It is listed separately from the public
#  scripts as it is edited by the installation procedure.

STARTUP_SCRIPT = $(PKG_NAME).csh

#  List of public include files.  These are include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.
#  They are nevertheless commonly used in user-written applications and
#  should therefore remain in the built system and safe against
#  cleaning.

PUBLIC_INCLUDES =

#  List of private include files.  These are additional include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = AAA_COMMON.INC ECH_ADAM_SPECIFICS.INC   \
   ECH_ARC_DB.INC ECH_CONTEXT.INC ECH_DATA_CONSTRAINTS.INC \
   ECH_DTATCON.INC ECH_ECHOMOP.INC ECH_ENVIRONMENT.INC \
   ECH_ENVIR_CONSTANTS.INC ECH_FEATURE.INC ECH_FEATURE_DB.INC \
   ECH_FOREIGN_PAR.INC ECH_FTR_DB.INC ECH_GRAPHICS.INC ECH_IMG_NAMES.INC \
   ECH_INIT_RDCTN.INC ECH_MAPPING.INC ECH_MODULES.INC ECH_MODLUT.INC \
   ECH_ORDER_WAVES.INC ECH_QUALITIES.INC ECH_REPORT.INC \
   ECH_SERVER.INC ECH_USE_DIMEN.INC \
   ECH_USE_RDCTN.INC ECH_WORKSPACE.INC ECH_PSX.INC ECH_PLOT_PARS.INC \
   ECH_POLYSPLINE.INC

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = SAE_PAR PAR_ERR PRM_PAR DAT_PAR NDF_PAR MSG_PAR f77.h

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines and the main routine, which are treated separately).

F_ROUTINES = aaa_indexing.f ech_2dext_opt.f ech_access_data_file.f \
   ech_access_object.f ech_access_reduction_file.f ech_access_workspace.f \
   ech_active_tune.f ech_arc_arfind.f ech_arfit.f \
   ech_calc_meta_ftrs.f \
   ech_calc_order_idnums.f ech_calc_profsamp.f ech_calc_trace.f \
   ech_chebyshev.f ech_check_blends.f ech_check_frame.f ech_check_id_ftrs.f \
   ech_clip_2d_fit.f ech_clip_trace.f ech_closedown.f ech_copy_bytes.f \
   ech_count_orders.f ech_create_reduction_file.f \
   ech_datafile_tidyup.f ech_decos_estimate_sky.f ech_decos_fit_profinc.f \
   ech_decos_predict.f ech_decos_prob.f ech_decos_skyline.f \
   ech_decos_theoretical.f ech_decosmic_1.f ech_decosmic_2.f \
   ech_define_modules.f ech_dekker_limits.f ech_determine_slope.f \
   ech_dfeval.f ech_disable_order.f ech_dmp_mem_use.f \
   ech_echomop_option.f \
   ech_edit_id_waves.f ech_edit_parameters.f ech_edit_profile.f \
   ech_estimate_waves.f ech_eval_meta_dist.f ech_export_echarc.f \
   ech_ext_opt.f ech_extr_2decosopt.f ech_extr_2dincprof.f \
   ech_extr_2doptimal.f ech_extr_2dsimple.f ech_extr_covariance.f \
   ech_extr_decosopt.f ech_extr_incprof.f ech_extr_optimal.f \
   ech_extr_profile.f ech_extr_quick.f ech_extr_simple.f \
   ech_fatal_error.f ech_feval.f ech_figsubs.f ech_find_centre.f \
   ech_find_order_peaks.f ech_fit_2d_distortion.f ech_fit_gaussian.f \
   ech_fit_order_blaze.f ech_fit_order_trace.f ech_fit_ref_fwhms.f \
   ech_fit_ref_lines.f ech_fit_ref_waves.f ech_fitter.f \
   ech_flatten_orders.f ech_flatten_yblaze.f ech_ftrdb.f \
   ech_gen_flat.f ech_get_dims.f \
   ech_gen_rebin_scale.f ech_generate_fdb.f ech_get_arc_fwhm.f \
   ech_get_data_path.f ech_get_object_path.f \
   ech_get_parameter.f ech_get_ref_fwhm.f \
   ech_get_profile.f ech_get_wave_window.f ech_help.f \
   ech_ichsubs.f ech_id_interact.f ech_id_ref_features.f \
   ech_image_cosmic.f ech_image_trace.f ech_import_echarc.f \
   ech_init_obj_paths.f ech_initialise.f ech_kdhsubs.f \
   ech_locate_orders.f ech_locate_ref_lines.f \
   ech_meta_candidate.f ech_merge_orders.f \
   ech_model_back.f ech_model_fitvar.f ech_model_flat.f \
   ech_model_profile.f ech_model_scatter.f ech_model_sky.f \
   ech_model_ssky.f ech_module_init.f ech_module_name_index.f \
   ech_module_props.f ech_module_tidyup.f ech_multi_merge.f \
   ech_normal_scrunched.f ech_normal_yblaze.f ech_obj_ind.f \
   ech_object_limits.f ech_objinfo.f ech_open_file.f \
   ech_pack_results.f \
   ech_par_object.f ech_parameter_init.f ech_parse_env.f ech_plot_graph.f \
   ech_plot_profile.f ech_plot_traces.f ech_plotter.f ech_plotter_smooth.f \
   ech_plotter_rebin.f ech_plotter_rembad.f ech_plotter_style.f \
   ech_plotter_browse_menu.f ech_plotter_main_menu.f ech_plotter_unmap.f \
   ech_poly.f ech_poly_profile.f ech_profile_attrib.f \
   ech_read_ftr_src.f ech_read_grph_cursor.f ech_read_type.f \
   ech_read_keyboard.f ech_rebin_arcorder.f ech_rebin_order.f \
   ech_report.f ech_save_ascii.f ech_save_orders.f ech_save_dipstk.f \
   ech_scrn2d.f ech_scrunch_2d_order.f ech_scrunch_order.f \
   ech_scrorders.f ech_set_context.f ech_set_parameter.f \
   ech_setup_graphics.f ech_setup_object_ref.f ech_skyfit_weights.f \
   ech_spatial_avgprof.f ech_spatial_profile.f ech_spline.f \
   ech_test_meta_ftrs.f ech_top_meta_cands.f ech_trace_consistency.f \
   ech_trace_order.f ech_tune_batch.f ech_tune_quick.f ech_typeinfo.f \
   ech_update_object_ref.f \
   ech_wave_consistency.f ech_wave_polyfit.f ech_wavelength_calib.f \
   ech_word_len.f ech_write_results.f \
   ech_xchng_echarc.f \
   ech_zero_init.f \
   echomop.f

#  List of Fortran BLOCK DATA routines.

BLOCK_DATA =

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F =

#  C routines required for building the package.  This is just a list of
#  all the C source files.

C_ROUTINES = ech_system.c ech_zero_init2.c ech_mean_median.c

#  List of applications/tasks in monolith.

TASKS = ech_2dfit ech_decimg ech_decos1 ech_decos2 \
   ech_ext2d ech_extrct ech_ffield ech_trplt \
   ech_fitblz ech_idwave ech_linloc ech_genflat \
   ech_locate ech_mdlbck ech_plot ech_profile \
   ech_qextr ech_result ech_scrunch \
   ech_sky ech_spatial ech_trace ech_trcsis \
   ech_fcheck ech_slope ech_find ech_dekker \
   ech_object ech_scrobj ech_scrarc ech_wscale \
   ech_fitfwhm ech_fwhm ech_blaze ech_doblz \
   ech_tuner echmenu ech_wvcsis ech_fitord \
   ech_mulmrg

#  Monolith routine source name.

MONO_SOURCE = echmono.f
HELPTASK_SOURCE = helpm.c helpc.c

#  Monolith binary name.

MONOLITH = echmono
HELPTASK = echhelp

#  Monolith interface file name.

MONO_IFL = echmenu.ifl

#  List of task interface files.

IFL_FILES = ech_2dfit.ifl ech_decimg.ifl ech_decos1.ifl ech_decos2.ifl \
   ech_ext2d.ifl  ech_extrct.ifl ech_ffield.ifl ech_trplt.ifl \
   ech_fitblz.ifl ech_ftrdb.ifl ech_genflat.ifl ech_idwave.ifl \
   ech_linloc.ifl ech_locate.ifl ech_mdlbck.ifl ech_plot.ifl \
   ech_profile.ifl ech_qextr.ifl ech_result.ifl ech_scrn2d.ifl \
   ech_scrunch.ifl ech_sky.ifl ech_spatial.ifl ech_trace.ifl \
   ech_trcsis.ifl ech_fcheck.ifl ech_slope.ifl ech_find.ifl \
   ech_dekker.ifl ech_object.ifl ech_scrobj.ifl ech_scrarc.ifl \
   ech_wscale.ifl ech_fitfwhm.ifl ech_fwhm.ifl ech_blaze.ifl \
   ech_doblz.ifl ech_tuner.ifl ech_wvcsis.ifl ech_fitord.ifl \
   ech_mulmrg.ifl

#  List of help library sources.

HELP_SOURCES = $(PKG_NAME).hlp

#  Lists of ancilliary data files.

DEF_FILES = ECH_RDCTN.DEF ECH_RDUCD.DEF ECH_FTRDB.DEF ECH_ECHAR.DEF \
           ECH_COMPACT.DEF

ARC_FILES = THNE.ARC THAR.ARC CUAR.ARC THNE.sdf THAR.sdf CUAR.sdf

DATA_FILES = aaa_cat.txt aaa_key.txt \
   generic.tab hr1544.tab hr3454.tab hr4468.tab \
   hr4963.tab hr5501.tab hr718.tab hr7596.tab hr7950.tab \
   hr8634.tab hr9087.tab

DEMO_FILES = ech_demo.csh ech_extrct.txt \
   ech_ffield.txt ech_fitord.txt ech_idwave.txt ech_linloc.txt \
   ech_locate.txt ech_profile.txt ech_result.txt ech_scrunch.txt \
   ech_sky.txt ech_spatial.txt ech_trace.txt \
   intro.txt next.txt summary.txt test_arc.sdf test_flat.sdf  \
   test_obj.sdf ech_test.sdf

TEST_FILES = echtests.icl echtests.csh test1.icl test10.icl \
   test10a.ref test10b.ref test10c.ref test10d.ref test10e.ref  \
   test11.icl test11a.ref test11b.ref test11c.ref test11d.ref \
   test11e.ref test11f.ref test12.icl test12a.ref test12b.ref \
   test12c.ref test12d.ref test12e.ref test12f.ref test12g.ref \
   test12h.ref \
   test12i.ref test12j.ref test1a.ref test1b.ref test2.icl \
   test2a.ref test2b.ref test2c.ref test2d.ref test2e.ref test2f.ref \
   test2g.ref test2h.ref test2i.ref test2j.ref test2k.ref test2l.ref \
   test2m.ref test2n.ref test2o.ref test2p.ref test3.icl \
   test3a.ref test3b.ref test3c.ref test3d.ref test3e.ref test3f.ref \
   test3g.ref test3h.ref test4.icl test4a.ref test4b.ref \
   test4c.ref test4d.ref test4e.ref test4f.ref test4g.ref test4h.ref \
   test4i.ref test5.icl test5a.ref test5b.ref test5c.ref \
   test5d.ref test5e.ref test5f.ref test5g.ref test5h.ref test5i.ref \
   test5j.ref test5k.ref test5l.ref test5m.ref test5n.ref test5o.ref \
   test5p.ref test5q.ref test6.icl test6a.ref test6b.ref  \
   test6c.ref test6d.ref test6e.ref test6f.ref test6g.ref test7.icl \
   test7a.ref test7b.ref test7c.ref test7d.ref test7e.ref  \
   test8.icl test8a.ref test8b.ref test8c.ref test8d.ref \
   test9.icl test9a.ref test9b.ref test9c.ref test9d.ref \
   test9e.ref test9f.ref test9g.ref test9h.ref test9i.ref

#  Lists of archive files.

ARCHIVE_FILES = $(ARC_FILES_ARCHIVE) $(DEMO_FILES_ARCHIVE) \
   $(HYPERTEXT_ARCHIVE) $(TEST_FILES_ARCHIVE)

ARC_FILES_ARCHIVE = $(PKG_NAME)_arc.tar

DEMO_FILES_ARCHIVE = $(PKG_NAME)_demo.tar

HYPERTEXT_ARCHIVE = $(PKG_NAME)_hyper.tar

TEST_FILES_ARCHIVE = $(PKG_NAME)_test.tar

#  Lists of LaTeX and hypertext documents.

LATEX_DOCS = sun152.tex sun152_cover.eps \
   sun152_01.eps sun152_02.eps sun152_03.eps sun152_04.eps \
   sun152_05.eps sun152_06.eps sun152_07.eps sun152_08.eps \
   sun152_glossy1.eps sun152_glossy2.eps

HYPERTEXT_DOCS = sun152.htx

#  List of documentation files.

DOCUMENTATION = $(LATEX_DOCS) $(HYPERTEXT_DOCS:.htx=.htx_tar) $(PKG_NAME).news

################################################################################
#-------------------------------------------------------------------------------

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

BUILT_FILES = $(PUBLIC_SCRIPTS) $(PUBLIC_INCLUDES) $(OBJECT_LIBRARIES) \
   $(MONOLITH) ech_ftrdb ech_scrn2d $(HELPTASK) $(DATA_FILES) \
   $(IFC_FILES) $(HELP_FILES) $(DEF_FILES) $(STARTUP_SCRIPT)

#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES)

#  Rules to set up links to locate each of the above include files.

SAE_PAR:       $(STAR_INC)/sae_par; $(LINK) $? $@
PAR_ERR:       $(STAR_INC)/par_err; $(LINK) $? $@
PRM_PAR:       $(STAR_INC)/prm_par; $(LINK) $? $@
DAT_PAR:       $(STAR_INC)/dat_par; $(LINK) $? $@
NDF_PAR:       $(STAR_INC)/ndf_par; $(LINK) $? $@
MSG_PAR:       $(STAR_INC)/msg_par; $(LINK) $? $@
f77.h:         $(STAR_INC)/f77.h;   $(LINK) $? $@

#  Rules for extracting source files from the source archives.

$(PUBLIC_SCRIPTS) $(PUBLIC_INCLUDES) $(F_ROUTINES) $(MONO_SOURCE) \
   $(HELPTASK_SOURCE) $(BLOCK_DATA) $(C_ROUTINES) $(HELP_SOURCES) \
   $(STARTUP_SCRIPT):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(IFL_FILES) $(MONO_IFL):
	$(TAR_OUT) $(PKG_NAME)_ifl.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(DEF_FILES):
	$(TAR_OUT) $(PKG_NAME)_def.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(DATA_FILES):
	$(TAR_OUT) $(PKG_NAME)_data.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

$(PRIVATE_INCLUDES):
	$(TAR_OUT) $(PKG_NAME)_inc.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

#  Rules for extracting platform specific source files from the source
#  archive.
#  It is quite likely that there are no platform-specific source files
#  and that the macro PLATFORM_F is empty. To prevent a syntax error in
#  make, a dummy target is present.

$(PLATFORM_F) dummy_target1:
	$(TAR_OUT) $(PKG_NAME)_source.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(F_ROUTINES:.f=.o) $(BLOCK_DATA:.f=.o) \
   $(PLATFORM_F:.f=.o) $(C_ROUTINES:.c=.o)

MONO_OBJECT = $(MONO_SOURCE:.f=.o)

.c.o:
	$(CC) $(CFLAGS) -c $< -o $(<:.c=.o)
.f.o:
	$(FC) $(FFLAGS) -c $< -o $(<:.f=.o)

#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = lib$(PKG_NAME).a

lib$(PKG_NAME).a: $(OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  Rule and dependencies for building the monolith.

$(MONOLITH): $(MONO_OBJECT) $(OBJECT_LIBRARIES)
	$(ALINK) $@.o -o $@ $(OBJECT_LIBRARIES) \
	 -L$(FIGARO_SOURCE) -lgen $(ADDLIBS) \
	 `pda_link` `hlp_link` `pgplot_link` `sla_link_adam` \
	 `psx_link_adam` `fio_link_adam` `ndf_link_adam`

#  Rules for building the help browser.

$(HELPTASK): $(HELPTASK_SOURCE)
	$(CC) $(CFLAGS) -c helpc.c
	$(CC) $(CFLAGS) -c helpm.c
	$(CFLINK) helpm.o helpc.o -o $(HELPTASK) \
           $(LDFLAG1) $(ADDLIBS) -L$(STAR_LIB) \
           `hlp_link` `fio_link` `cnf_link` $(CFLIBS) $(LDFLAG2)
	- rm -f help[mc].o

#  Rules for building the help libraries.

HELP_FILES = $(HELP_SOURCES:.hlp=.shl)

.hlp.shl:
	$(STAR_BIN)/hlib $<

#  Rules for compiling the interface files.

IFC_FILES = $(IFL_FILES:.ifl=.ifc) $(MONO_IFL:.ifl=.ifc)

$(MONOLITH).ifl: $(IFL_FILES)
	echo 'monolith $(MONOLITH)' > $@
	cat $(IFL_FILES)            >> $@
	echo 'endmonolith'          >> $@

.ifl.ifc:
	$(STAR_BIN)/compifl $<

#  Rule and dependencies for building the database task.
ech_ftrdb: ech_ftrdb.o $(OBJECT_LIBRARIES)
	$(ALINK) ech_ftrdb $(OBJECT_LIBRARIES) -L$(FIGARO_SOURCE) -lgen \
	 $(ADDLIBS) `hlp_link` `pgplot_link` `sla_link_adam` `pda_link` \
	 `psx_link_adam` `fio_link_adam` `ndf_link_adam`
	chmod 755 ech_ftrdb

#  Rule and dependencies for building the 2d fitter.
ech_scrn2d: ech_scrn2d.o $(OBJECT_LIBRARIES)
	$(ALINK) ech_scrn2d $(OBJECT_LIBRARIES) -L$(FIGARO_SOURCE) -lgen \
	 $(ADDLIBS) `hlp_link` `pgplot_link` `sla_link_adam` `pda_link` \
	 `psx_link_adam` `fio_link_adam` `ndf_link_adam`
	chmod 755 ech_scrn2d

#  Developers targets.

#  Library.
lib: $(OBJECT_LIBRARIES)

#  "Manual" link of the monolith.

byhand:
	$(ALINK) echmono.o -o echmono $(OBJECT_LIBRARIES) \
	 -L$(FIGARO_SOURCE) -lgen $(ADDLIBS) `pda_link` \
	 `hlp_link` `pgplot_link` `sla_link_adam` \
	 `psx_link_adam` `fio_link_adam` `ndf_link_adam`

withnag:
	$(ALINK) echmono.o -o echmono $(OBJECT_LIBRARIES) \
	 -L$(FIGARO_SOURCE) -lgen -lnag $(ADDLIBS) `pda_link` \
	 `hlp_link` `pgplot_link` `sla_link_adam` \
	 `psx_link_adam` `fio_link_adam` `ndf_link_adam`

#
#  Build compressed archive of ECHOMOP alpha release.
#  This alpha release uses a prior installation of ECHOMOP for data files.

alpha: $(MONOLITH) $(IFC_FILES) $(DEF_FILES) $(HELPTASK) $(HELP_FILES) \
   echomopalpha.csh makefile $(DEF_FILES) echomop.news ech_demo.csh README \
   $(ARC_FILES)
	$(TAR_IN) $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_alpha.tar \
	echmono $(IFC_FILES)  $(HELPTASK) $(HELP_FILES) echomopalpha.csh \
	   makefile $(DEF_FILES) echomop.news ech_demo.csh README \
	   $(ARC_FILES)
	compress -v $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_alpha.tar
	@ echo
	@ echo \
'*** Alpha release copy of the $(PKG_NAME) package is in the compressed'
	@ echo \
'    tar file $(EXPORT)/$(PKG_NAME)_$(SYSTEM)_alpha.tar.Z'
	@ echo

alphainstall:
	if test -n "echomopalpha.csh"; then \
	   sed -e s#INSTALL_BIN#`pwd`# \
               -e s#INSTALL_HELP#`pwd`# \
               -e s#PKG_VERS#$(PKG_VERS)# \
              echomopalpha.csh > echalpha.csh;\
           chmod 755 echalpha.csh;\
	else :; fi
	strip $(MONOLITH)
	for f in $(TASKS); do \
	   $(LINK) -s $(MONOLITH) $$f;\
	done
	@ echo
	@ echo \
'*** Alpha release copy of the $(PKG_NAME) package is installed.'
	@ echo
	@ echo "    source `pwd`/echalpha.csh to use the alpha release."
	@ echo '    The command "echomop" will re-enable the previous version.'
	@ echo

#
#  "Manual" remove links for all the tasks in the monolith.
rmlinks:
	for f in $(TASKS); do \
           rm -f $$f;\
        done
#
#  "Manual" set links for all the tasks in the monolith.
links:
	for f in $(TASKS); do \
           $(LINK) -fs $(MONOLITH) $$f;\
        done

#  Name of the date stamp file.  This is used to record the time of the
#  most recent build for use in subsequent operations that require it.
#  There must be no rule for generating this file; it is updated only
#  as a side effect of building the package.

DATE_STAMP = $(PKG_NAME)_datestamp

#  Pathnames of directories into which files may be placed when the
#  package is installed.

INSTALL_ARC = $(INSTALL)/bin/$(PKG_NAME)/arc
INSTALL_BIN = $(INSTALL)/bin/$(PKG_NAME)
INSTALL_DATA = $(INSTALL)/bin/$(PKG_NAME)/data
INSTALL_DATES = $(INSTALL)/dates
INSTALL_DEMO = $(INSTALL)/bin/$(PKG_NAME)/demo
INSTALL_DOCS = $(INSTALL)/docs
INSTALL_HELP = $(INSTALL)/help/$(PKG_NAME)
INSTALL_HYPER = $(INSTALL)/bin/$(PKG_NAME)/hyper
INSTALL_TEST = $(INSTALL)/bin/$(PKG_NAME)/test

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_ARC) $(INSTALL_DATA) \
   $(INSTALL_DATES) $(INSTALL_DEMO) $(INSTALL_DOCS) $(INSTALL_HELP) \
   $(INSTALL_HYPER) $(INSTALL_TEST)

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
             if test ! -f $$f; then \
                echo "   File $$f is missing.";\
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
#  Install the monolith, giving it world execute permission.
	for f in $(MONOLITH) ech_ftrdb ech_scrn2d \
	         $(HELPTASK) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 755 $(INSTALL_BIN)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_BIN)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the definition files, giving them world read permission.
	for f in $(DEF_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_BIN);\
              chmod 644 $(INSTALL_BIN)/$$f;\
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
              cp -p $$f $(INSTALL_DATA);\
              chmod 644 $(INSTALL_DATA)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_DATA)/$$f $$f;\
           else :; fi;\
        done
#
#  Install the arc database files, giving them world read permission.
	if test -n "$(ARC_FILES_ARCHIVE)"; then \
           pwd=`pwd`;\
           (cd $(INSTALL_ARC);\
           for f in $(ARC_FILES_ARCHIVE) ""; do \
              if test -n "$$f"; then\
                 $(TAR_OUT) $$pwd/$${f};\
                 chmod 644 *;\
              else :; fi;\
           done);\
        fi;
#
#  Install the demo files, giving them world read permission.
	if test -n "$(DEMO_FILES_ARCHIVE)"; then \
           pwd=`pwd`;\
           (cd $(INSTALL_DEMO);\
           for f in $(DEMO_FILES_ARCHIVE) ""; do \
              if test -n "$$f"; then\
                 $(TAR_OUT) $$pwd/$${f};\
                 chmod 644 *;\
              else :; fi;\
           done);\
        fi;
#
#  Install the test files, giving them world read permission.
	if test -n "$(TEST_FILES_ARCHIVE)"; then \
           pwd=`pwd`;\
           (cd $(INSTALL_TEST);\
           for f in $(TEST_FILES_ARCHIVE) ""; do \
              if test -n "$$f"; then\
                 $(TAR_OUT) $$pwd/$${f};\
                 chmod 644 *;\
              else :; fi;\
           done);\
        fi;
#
#  Install the hypertext help files, giving them world read permission.
	if test -n "$(HYPERTEXT_ARCHIVE)"; then \
           pwd=`pwd`;\
           (cd $(INSTALL_HYPER);\
           for f in $(HYPERTEXT_ARCHIVE) ""; do \
              if test -n "$$f"; then\
                 $(TAR_OUT) $$pwd/$${f};\
                 chmod 644 *;\
              else :; fi;\
           done);\
        fi;
#
# Install the help files, giving them world read permission.
	for f in $(HELP_FILES) ""; do \
           if test -n "$$f"; then \
              cp -p $$f $(INSTALL_HELP);\
              chmod 644 $(INSTALL_HELP)/$$f;\
              rm -f $$f;\
              $(LINK) $(INSTALL_HELP)/$$f $(INSTALL_BIN)/$$f;\
              $(LINK) $(INSTALL_HELP)/$$f $$f;\
           else :; fi;\
        done
#
#  Create the application/task soft links.
	for f in $(TASKS) ""; do \
	   if test -n "$$f"; then \
	      $(LINK) ./$(MONOLITH) $(INSTALL_BIN)/$$f;\
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
        fi
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
#  Deinstall the monolith, if an installed version exists.
	- for f in $(MONOLITH) ech_ftrdb ech_scrn2d \
              $(HELPTASK) ""; do \
           if test -n "$$f" -a -f $(INSTALL_BIN)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_BIN)/$$f .;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the definition files, if installed versions exist.
	- for f in $(DEF_FILES) ""; do \
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
          if test -n "$$f" -a -f $(INSTALL_DATA)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_DATA)/$$f .;\
              rm -f $(INSTALL_DATA)/$$f;\
           else :; fi;\
        done
#
#  Deinstall the arc database files, if installed versions exist.
	- for f in $(INSTALL_ARC) ""; do \
           if test -n "$$f" -a -d "$$f"; then \
              rm -rf $$f;\
           else :; fi;\
        done
#
#  Deinstall the demo files, if installed versions exist.
	- for f in $(INSTALL_DEMO) ""; do \
           if test -n "$$f" -a -d "$$f"; then \
              rm -rf $$f;\
           else :; fi;\
        done
#
#  Deinstall the test files, if installed versions exist.
	- for f in $(INSTALL_TEST) ""; do \
           if test -n "$$f" -a -d "$$f"; then \
              rm -rf $$f;\
           else :; fi;\
        done
#
#  Deinstall the hypertext files, if installed versions exist.
	- for f in $(INSTALL_HYPER) ""; do \
           if test -n "$$f" -a -d "$$f"; then \
              rm -rf $$f;\
           else :; fi;\
        done
#
#  Deinstall the help files, if installed versions exist.
	- for f in $(HELP_FILES) ""; do \
           if test -n "$$f" -a -f $(INSTALL_HELP)/$$f; then \
              rm -f $$f;\
              cp -p $(INSTALL_HELP)/$$f .;\
              rm -f $(INSTALL_HELP)/$$f;\
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Remove the application/task soft links.
	- for f in $(TASKS) ""; do \
           if test -n "$$f"; then \
              rm -f $(INSTALL_BIN)/$$f ;\
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
	@ echo '$(PACK_NAME) does not have a make-driven installation test'
	@ echo
	@ echo 'You can run a comprehensive set of tests using'
	@ echo ' '
	@ echo "     cd /some/empty/directory"
	@ echo "     source `cat .INSTALLED_$(SYSTEM)`/bin/echomop/$(STARTUP_SCRIPT)"
	@ echo "     source `cat .INSTALLED_$(SYSTEM)`/bin/echomop/test/echtests.csh"
	@ echo ' '
	@ echo 'The tests compare their results with reference copies.'
	@ echo 'Some differences (e.g. file names) may be displayed.'
	@ echo ' '
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
           $(ARCHIVE_FILES) $(DATE_STAMP) .BUILT \
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

#  export_dev: Build the source tar files.
#  ---------------------------------------
#
#  This is not a public target, and intended only for the software
#  developer.

#  This target constructs the tar files that constitute the main
#  SOURCE_FILES from their contents.

export_dev: $(PKG_NAME)_source.tar $(PKG_NAME)_arc.tar $(PKG_NAME)_data.tar \
   $(PKG_NAME)_def.tar $(PKG_NAME)_demo.tar  $(HYPERTEXT_DOCS:.htx=.htx_tar) \
   $(PKG_NAME)_hyper.tar $(PKG_NAME)_ifl.tar $(PKG_NAME)_inc.tar \
   $(PKG_NAME)_test.tar

$(PKG_NAME)_source.tar:
	$(TAR_IN) $@ $(PUBLIC_SCRIPTS) $(PUBLIC_INCLUDES) $(F_ROUTINES) \
	   $(BLOCK_DATA) $(C_ROUTINES) $(STARTUP_SCRIPT) $(HELP_SOURCES) \
	   ech_tuner.f ech_decos1.f \
	   ech_decos2.f $(MONO_SOURCE) $(HELPTASK_SOURCE)

$(HYPERTEXT_DOCS:.htx=.htx_tar):
	$(TAR_IN) $@ $(@:.htx_tar=.htx)

$(PKG_NAME)_arc.tar:
	$(TAR_IN) $@ $(ARC_FILES)

$(PKG_NAME)_data.tar:
	$(TAR_IN) $@ $(DATA_FILES)

$(PKG_NAME)_def.tar:
	$(TAR_IN) $@ $(DEF_FILES)

$(PKG_NAME)_demo.tar:
	$(TAR_IN) $@ $(DEMO_FILES)

$(PKG_NAME)_hyper.tar:
	cd hyper; $(TAR_IN) ../$@ .

$(PKG_NAME)_ifl.tar:
	$(TAR_IN) $@ $(IFL_FILES) $(MONO_IFL)

$(PKG_NAME)_inc.tar:
	$(TAR_IN) $@ $(PRIVATE_INCLUDES)

$(PKG_NAME)_test.tar:
	$(TAR_IN) $@ $(TEST_FILES)

#  clean: Clean up the source directory.
#  ------------------------------------
#
#  Remove all intermediate files.  Do not remove built files.

clean:
	@- rm -f $(INCLUDE_LINKS) $(PRIVATE_INCLUDES) $(F_ROUTINES) \
              $(PLATFORM_F) $(BLOCK_DATA) $(IFL_FILES) $(MONO_IFL) \
              $(C_ROUTINES) $(OBJECT_FILES) $(MONO_SOURCE) \
              $(MONO_OBJECT) $(HELP_SOURCES) $(HELPTASK_SOURCE) junk.sdf \
              1>/dev/null 2>/dev/null
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
	strip $(MONOLITH)
	strip ech_ftrdb
	strip ech_scrn2d
	strip $(HELPTASK)
	@ echo '*** Monolith stripped'

#-------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files).  These are normally generated automatically from the source
#  files.
aaa_indexing.o: AAA_COMMON.INC
ech_2dext_opt.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_QUALITIES.INC ECH_REPORT.INC
ech_access_data_file.o: ECH_INIT_RDCTN.INC ECH_MODULES.INC ECH_REPORT.INC \
   ECH_USE_DIMEN.INC ECH_USE_RDCTN.INC SAE_PAR
ech_access_object.o: DAT_PAR ECH_CONTEXT.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_REPORT.INC ECH_SERVER.INC ECH_USE_DIMEN.INC ECH_USE_RDCTN.INC NDF_PAR \
   PRM_PAR SAE_PAR
ech_access_reduction_file.o: ECH_CONTEXT.INC ECH_ECHOMOP.INC ECH_MAPPING.INC \
   ECH_MODULES.INC ECH_REPORT.INC ECH_USE_DIMEN.INC ECH_USE_RDCTN.INC
ech_access_workspace.o: ECH_REPORT.INC ECH_USE_RDCTN.INC SAE_PAR
ech_active_tune.o: ECH_ECHOMOP.INC ECH_MODULES.INC ECH_REPORT.INC \
   ECH_USE_RDCTN.INC PAR_ERR
ech_arfit.o: ECH_ENVIRONMENT.INC ECH_GRAPHICS.INC
ech_calc_meta_ftrs.o: ECH_FEATURE.INC ECH_REPORT.INC
ech_calc_order_idnums.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_REPORT.INC
ech_calc_profsamp.o: ECH_REPORT.INC
ech_calc_trace.o: ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_check_blends.o: ECH_FEATURE.INC
ech_check_frame.o: ECH_QUALITIES.INC ECH_REPORT.INC
ech_check_id_ftrs.o: ECH_DATA_CONSTRAINTS.INC ECH_FEATURE.INC ECH_REPORT.INC
ech_clip_2d_fit.o: ECH_ENVIRONMENT.INC ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_clip_trace.o: ECH_ENVIRONMENT.INC ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_closedown.o: ECH_USE_RDCTN.INC
ech_count_orders.o: ECH_CONTEXT.INC ECH_DATA_CONSTRAINTS.INC \
   ECH_ENVIRONMENT.INC ECH_GRAPHICS.INC ECH_REPORT.INC SAE_PAR
ech_create_reduction_file.o: ECH_REPORT.INC ECH_USE_RDCTN.INC
ech_datafile_tidyup.o: ECH_USE_RDCTN.INC
ech_decos1.o: ECH_CONTEXT.INC ECH_INIT_RDCTN.INC ECH_MAPPING.INC \
   ECH_MODULES.INC ECH_REPORT.INC ECH_USE_RDCTN.INC
ech_decos2.o: ECH_CONTEXT.INC ECH_INIT_RDCTN.INC ECH_MAPPING.INC \
   ECH_MODULES.INC ECH_REPORT.INC ECH_USE_RDCTN.INC
ech_decos_estimate_sky.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_REPORT.INC
ech_decos_fit_profinc.o: ECH_DATA_CONSTRAINTS.INC ECH_REPORT.INC \
   ECH_ENVIR_CONSTANTS.INC
ech_decos_predict.o: ECH_DATA_CONSTRAINTS.INC ECH_QUALITIES.INC ECH_REPORT.INC \
   ECH_ENVIR_CONSTANTS.INC
ech_decos_prob.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIRONMENT.INC \
   ECH_QUALITIES.INC ECH_REPORT.INC
ech_decos_skyline.o: ECH_QUALITIES.INC ECH_REPORT.INC
ech_decos_theoretical.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIRONMENT.INC \
   ECH_REPORT.INC
ech_decosmic_1.o: ECH_ENVIRONMENT.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_QUALITIES.INC ECH_REPORT.INC
ech_decosmic_2.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_QUALITIES.INC ECH_REPORT.INC
ech_define_modules.o: ECH_DATA_CONSTRAINTS.INC ECH_ECHOMOP.INC \
   ECH_ENVIRONMENT.INC ECH_ENVIR_CONSTANTS.INC ECH_FEATURE.INC \
   ECH_MAPPING.INC ECH_MODULES.INC ECH_MODLUT.INC SAE_PAR
ech_dekker_limits.o: ECH_REPORT.INC
ech_determine_slope.o: ECH_DATA_CONSTRAINTS.INC ECH_REPORT.INC
ech_dfeval.o: ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC ECH_POLYSPLINE.INC
ech_disable_order.o: ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_dmp_mem_use.o: ECH_REPORT.INC ECH_USE_RDCTN.INC
ech_echomop_option.o: ECH_ECHOMOP.INC ECH_INIT_RDCTN.INC ECH_MAPPING.INC \
   ECH_MODULES.INC ECH_REPORT.INC ECH_SERVER.INC
ech_edit_id_waves.o: ECH_CONTEXT.INC ECH_DATA_CONSTRAINTS.INC \
   ECH_ENVIRONMENT.INC ECH_ENVIR_CONSTANTS.INC ECH_FEATURE.INC ECH_REPORT.INC
ech_edit_parameters.o: ECH_ECHOMOP.INC ECH_MODULES.INC ECH_REPORT.INC \
   ECH_USE_RDCTN.INC PAR_ERR
ech_edit_profile.o: ECH_ENVIRONMENT.INC ECH_REPORT.INC
ech_estimate_waves.o: ECH_REPORT.INC
ech_eval_meta_dist.o: ECH_FEATURE.INC ECH_FEATURE_DB.INC ECH_REPORT.INC
ech_ext_opt.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_extr_2decosopt.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_REPORT.INC
ech_extr_2dincprof.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_REPORT.INC
ech_extr_2doptimal.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_REPORT.INC
ech_extr_2dsimple.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_REPORT.INC
ech_extr_covariance.o: ECH_DATA_CONSTRAINTS.INC ECH_REPORT.INC
ech_extr_decosopt.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_REPORT.INC
ech_extr_incprof.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_REPORT.INC
ech_extr_optimal.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_REPORT.INC
ech_extr_profile.o: ECH_DATA_CONSTRAINTS.INC ECH_REPORT.INC
ech_extr_quick.o: ECH_CONTEXT.INC ECH_DATA_CONSTRAINTS.INC \
   ECH_ENVIRONMENT.INC ECH_ENVIR_CONSTANTS.INC ECH_GRAPHICS.INC ECH_REPORT.INC
ech_extr_simple.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_REPORT.INC
ech_fatal_error.o: ECH_REPORT.INC PAR_ERR
ech_feval.o: ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC ECH_POLYSPLINE.INC
ech_find_centre.o: ECH_REPORT.INC
ech_find_order_peaks.o: ECH_DATA_CONSTRAINTS.INC ECH_REPORT.INC
ech_fit_2d_distortion.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIRONMENT.INC \
   ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_fit_gaussian.o: ECH_REPORT.INC
ech_fit_order_blaze.o: ECH_ENVIRONMENT.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_REPORT.INC
ech_fit_order_trace.o: ECH_CONTEXT.INC ECH_ENVIRONMENT.INC \
   ECH_ENVIR_CONSTANTS.INC ECH_MAPPING.INC ECH_REPORT.INC
ech_fit_ref_fwhms.o: ECH_FEATURE.INC ECH_REPORT.INC
ech_fit_ref_lines.o: ECH_REPORT.INC
ech_fit_ref_waves.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIRONMENT.INC \
   ECH_FEATURE.INC ECH_REPORT.INC
ech_fitter.o: ECH_ENVIRONMENT.INC ECH_REPORT.INC ECH_POLYSPLINE.INC
ech_flatten_orders.o: ECH_REPORT.INC
ech_flatten_yblaze.o: ECH_REPORT.INC
ech_ftrdb.o: ECH_MAPPING.INC ECH_REPORT.INC
ech_gen_flat.o: ECH_ENVIRONMENT.INC ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_gen_rebin_scale.o: ECH_REPORT.INC
ech_generate_fdb.o: ECH_FTR_DB.INC ECH_REPORT.INC
ech_get_arc_fwhm.o: ECH_ENVIRONMENT.INC ECH_INIT_RDCTN.INC ECH_WORKSPACE.INC
ech_get_data_path.o: ECH_MODULES.INC ECH_REPORT.INC ECH_USE_RDCTN.INC
ech_get_dims.o: ECH_USE_RDCTN.INC SAE_PAR
ech_get_object_path.o: ECH_MODULES.INC ECH_REPORT.INC ECH_USE_RDCTN.INC \
   SAE_PAR
ech_get_parameter.o: ECH_ENVIR_CONSTANTS.INC ECH_INIT_RDCTN.INC \
   ECH_MODULES.INC ECH_REPORT.INC ECH_USE_RDCTN.INC PAR_ERR
ech_get_ref_fwhm.o: ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_get_wave_window.o: ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_help.o: ECH_REPORT.INC ECH_USE_RDCTN.INC
ech_id_interact.o: ECH_REPORT.INC
ech_id_ref_features.o: ECH_DATA_CONSTRAINTS.INC ECH_FEATURE.INC \
   ECH_FEATURE_DB.INC ECH_REPORT.INC
ech_image_cosmic.o: ECH_ENVIR_CONSTANTS.INC ECH_QUALITIES.INC ECH_REPORT.INC
ech_image_trace.o: ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_import_echarc.o: ECH_REPORT.INC
ech_init_obj_paths.o: ECH_USE_RDCTN.INC
ech_initialise.o: ECH_CONTEXT.INC ECH_ECHOMOP.INC ECH_MAPPING.INC \
   ECH_MODULES.INC ECH_REPORT.INC ECH_SERVER.INC
ech_locate_orders.o: ECH_ENVIRONMENT.INC ECH_REPORT.INC
ech_locate_ref_lines.o: ECH_ENVIRONMENT.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_REPORT.INC
ech_meta_candidate.o: ECH_FEATURE.INC ECH_FEATURE_DB.INC ECH_REPORT.INC
ech_model_back.o: ECH_ENVIRONMENT.INC ECH_REPORT.INC
ech_model_fitvar.o: ECH_ENVIRONMENT.INC
ech_model_flat.o: ECH_ENVIRONMENT.INC ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_model_profile.o: ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_model_scatter.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIRONMENT.INC ECH_REPORT.INC
ech_model_sky.o: ECH_ENVIRONMENT.INC ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_model_ssky.o: ECH_ENVIRONMENT.INC ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_module_init.o: ECH_MAPPING.INC ECH_MODULES.INC ECH_REPORT.INC \
   ECH_SERVER.INC ECH_USE_RDCTN.INC SAE_PAR
ech_module_name_index.o: ECH_MODULES.INC
ech_module_props.o: ECH_MODULES.INC
ech_module_tidyup.o: ECH_ECHOMOP.INC ECH_MAPPING.INC ECH_MODULES.INC \
   ECH_REPORT.INC ECH_SERVER.INC ECH_USE_RDCTN.INC
ech_multi_merge.o: ECH_ENVIRONMENT.INC ECH_REPORT.INC ECH_USE_DIMEN.INC \
   ECH_USE_RDCTN.INC
ech_normal_scrunched.o: ECH_REPORT.INC
ech_normal_yblaze.o: ECH_DATA_CONSTRAINTS.INC ECH_REPORT.INC
ech_obj_ind.o: ECH_MODULES.INC ECH_MODLUT.INC
ech_object_limits.o: ECH_REPORT.INC
ech_objinfo.o: ECH_USE_RDCTN.INC
ech_open_file.o: SAE_PAR
ech_pack_results.o: AAA_COMMON.INC ECH_MAPPING.INC ECH_PSX.INC ECH_REPORT.INC \
   ECH_USE_RDCTN.INC
ech_parameter_init.o: ECH_MODULES.INC ECH_REPORT.INC ECH_USE_RDCTN.INC
ech_parse_env.o: ECH_PSX.INC
ech_plot_graph.o: ECH_ENVIRONMENT.INC ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC \
   ECH_SERVER.INC ECH_PLOT_PARS.INC SAE_PAR
ech_plot_profile.o: ECH_ENVIRONMENT.INC ECH_REPORT.INC ECH_GRAPHICS.INC
ech_plot_traces.o: ECH_ENVIRONMENT.INC ECH_GRAPHICS.INC ECH_INIT_RDCTN.INC
ech_plotter.o: ECH_ENVIRONMENT.INC ECH_ENVIR_CONSTANTS.INC ECH_INIT_RDCTN.INC \
   ECH_REPORT.INC ECH_USE_DIMEN.INC ECH_USE_RDCTN.INC
ech_plotter_rembad.o: ECH_ENVIR_CONSTANTS.INC
ech_plotter_style.o: ECH_ENVIRONMENT.INC ECH_PLOT_PARS.INC
ech_plotter_unmap.o: ECH_USE_RDCTN.INC
ech_poly_profile.o: ECH_ENVIRONMENT.INC ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_profile_attrib.o: ECH_REPORT.INC
ech_read_ftr_src.o: ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_read_grph_cursor.o: ECH_ENVIRONMENT.INC
ech_read_keyboard.o: ECH_ENVIRONMENT.INC
ech_read_type.o: DAT_PAR SAE_PAR
ech_rebin_arcorder.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIRONMENT.INC \
   ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_rebin_order.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIRONMENT.INC \
   ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_report.o: ECH_ENVIRONMENT.INC ECH_MAPPING.INC ECH_REPORT.INC \
   ECH_SERVER.INC ECH_REPORT.INC
ech_save_ascii.o: ECH_REPORT.INC
ech_save_orders.o: ECH_REPORT.INC
ech_save_dipstk.o: MSG_PAR ECH_REPORT.INC
ech_scrn2d.o: ECH_MAPPING.INC ECH_REPORT.INC
ech_scrorders.o: ECH_ENVIRONMENT.INC ECH_MAPPING.INC ECH_REPORT.INC
ech_scrunch_2d_order.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIRONMENT.INC \
   ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_scrunch_order.o: ECH_ENVIRONMENT.INC ECH_INIT_RDCTN.INC
ech_set_context.o: ECH_REPORT.INC ECH_CONTEXT.INC ECH_ECHOMOP.INC \
   ECH_MODULES.INC
ech_set_parameter.o: ECH_REPORT.INC ECH_USE_RDCTN.INC
ech_setup_graphics.o: ECH_ENVIRONMENT.INC ECH_MODULES.INC ECH_REPORT.INC
ech_setup_object_ref.o: ECH_FOREIGN_PAR.INC ECH_IMG_NAMES.INC \
   ECH_INIT_RDCTN.INC ECH_MAPPING.INC ECH_MODULES.INC ECH_REPORT.INC \
   ECH_SERVER.INC ECH_USE_RDCTN.INC
ech_skyfit_weights.o: ECH_DATA_CONSTRAINTS.INC
ech_spatial_avgprof.o: ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_spatial_profile.o: ECH_ENVIR_CONSTANTS.INC ECH_REPORT.INC
ech_test_meta_ftrs.o: ECH_FEATURE.INC ECH_REPORT.INC
ech_top_meta_cands.o: ECH_FEATURE.INC ECH_REPORT.INC
ech_trace_consistency.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIRONMENT.INC \
   ECH_ENVIR_CONSTANTS.INC ECH_GRAPHICS.INC ECH_REPORT.INC
ech_trace_order.o: ECH_ENVIRONMENT.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_GRAPHICS.INC ECH_REPORT.INC
ech_tune_batch.o: ECH_REPORT.INC
ech_tune_quick.o: ECH_REPORT.INC
ech_tuner.o: ECH_CONTEXT.INC ECH_REPORT.INC
ech_typeinfo.o: ECH_DTATCON.INC
ech_update_object_ref.o: ECH_FOREIGN_PAR.INC ECH_IMG_NAMES.INC \
   ECH_MAPPING.INC ECH_REPORT.INC ECH_USE_RDCTN.INC
ech_wave_consistency.o: ECH_DATA_CONSTRAINTS.INC ECH_ENVIRONMENT.INC \
   ECH_ENVIR_CONSTANTS.INC ECH_FEATURE.INC ECH_REPORT.INC
ech_wave_polyfit.o: ECH_REPORT.INC ECH_ENVIR_CONSTANTS.INC ECH_FEATURE.INC
ech_wavelength_calib.o: ECH_CONTEXT.INC ECH_FEATURE.INC ECH_FEATURE_DB.INC \
   ECH_MAPPING.INC ECH_REPORT.INC ECH_DATA_CONSTRAINTS.INC
ech_write_results.o: ECH_MAPPING.INC ECH_REPORT.INC ECH_IMG_NAMES.INC
ech_xchng_echarc.o: ECH_REPORT.INC
ech_zero_init.o: ECH_REPORT.INC
echmono.o: ECH_CONTEXT.INC ECH_ECHOMOP.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_MAPPING.INC ECH_MODULES.INC ECH_REPORT.INC ECH_SERVER.INC \
   ECH_ENVIRONMENT.INC SAE_PAR
echomop.o: ECH_CONTEXT.INC ECH_ECHOMOP.INC ECH_ENVIR_CONSTANTS.INC \
   ECH_MAPPING.INC ECH_MODULES.INC ECH_PSX.INC ECH_REPORT.INC ECH_SERVER.INC \
   ECH_USE_RDCTN.INC
#
#  End of makefile.
