#+
#  Name:
#     makefile
#
#  Version:
#     Application makefile Mk V
#
#  Purpose:
#     Build and install the SURF package.
#
#  Type of Module:
#     Description file for the make utility.
#
#  Description:
#     This description file is used by the make utility to build the
#     SURF package from the distributed source files, to install
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
#     The SURF package depends on the following other Starlink
#     packages which must previously have been installed into the
#     appropriate sub-directories of the $STARLINK directory (/star is
#     used if the environment variable STARLINK is not defined).
#        agi
#           Graphics-database routines.
#        ary
#           Array access routines.
#        chr
#           Character-handling Routines.
#        ems                                                
#           Error message service                           
#        fio
#           Fortran I/O routines.
#        gks
#           GKS routines.
#        gns
#           Graphics name service.
#        grp
#           Group handling
#        hlp
#           Portable help system
#        ndf
#           N-Dimensional Data Format
#        hds                                      # Part of NDF
#           Hierarchical data system                        
#        pda
#           Public Domain Algorithms library
#	 pgp
#           PGPLOT graphics library
#        primdat
#           Primitive Numerical-data-processing routines.
#        sae_par                                            
#           Global include files                            
#        sla
#           SLALIB.
#        sgs
#           SGS routines.
#
#     This makefile can also use the ira (IRAS90 astrometry routines) 
#     library if it exists.
#     The mk script automatically detects the presence of ira_link
#     in /star/iras90 - if this file is not found then the system
#     will build without iras90 support.
#
#     This package comes with some files from the Kappa software.
#     These routines are used in SCUOVER and SCUHELP. I felt it best
#     to include the necessary files explicitly rather than linking
#     against libkapgen and libkapsub (means that the user to not
#     need to have the kappa libraries installed before building).
#
#     The graphics libraries (pgp, sgs, gks) are only used for SCUOVER
#     and DESPIKE.
#     I probably should add the ability to build without SCUOVER.
#     them.
#
#     The perl scripts have the following dependencies:
#
#      They all must have the perl NDF module available (either as 
#      a module or as ndfperl). The scripts assume there is a program
#      in /star/bin called ndfperl - this can be a link to your system
#      perl if NDF has been built as a sharable module.
#
#      qdraw requires Kappa (specifically STATS, DRAWSIG and LINPLOT)
#      sigclip requires Kappa (STATS and THRESH)
#
#      These scripts rely on knowing the location of Kappa (/star/bin/kappa)
#      since they can not run a shell initialisation script.
#      scuquick relies on knowing the location of the surf direcory.
#
#      The scripts handle this by processing $KAPPA_DIR and $SURF_DIR

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
#     TIMJ: Tim Jenness (JAC, Hawaii)
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
#     12-DEC-1996 (BLY):
#        Various minor modifications.
#     13-JUN-1997 (TIMJ):
#        Add support for *.F files via FCPPFLAGS
#        Modify for SURF package.
#     12-Nov-1997 (TIMJ):
#        Extend for release 1.1 of SURF
#      3-Mar-1997 (TIMJ):
#        Increment version to 1.1-1
#      10-Jun-1998 (TIMJ)
#        Prepare for release v1.2beta
#        Update file list. Add NDFPERL_SCRIPTS variable
#        and perform substitution of STAR_BIN for /star/bin
#        so that the ndfperl binary can be located even
#        on systems that dont put it in /star/bin.
#      15-Nov-1998 (TIMJ)
#        Prepare for release v1.3beta
#        Add REDUCE_NOISE
#      30-Apr-1999 (TIMJ)
#        Include PERL symbol. - no longer uses NDFPERL
#        Add -DPKG_VERS to automatically write version number to
#        monolith application name for history writing
#        Version 1.4
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

# Location of PERL binary
PERL = $(STARLINK)/Perl/bin/perl

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

#-------------------------------------------------------------------------------
################################################################################
#
#  Define package source files.
#  ===========================
#
#  This section defines the set of source files for the package.

#  Name of the package as specified in documentation
#  The value is used in messages from make to the user.

PACK_NAME = SURF

#  Prefix for the package in lower-case as used in filenames etc.
PKG_NAME = surf

#  Prefix for the package in upper-case as used in include file links.
PKG_LINK = SURF

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

PKG_VERS = 1.4-0

#  List of files comprising the distributed source-only system.  This
#  defines the minimum set of files required to rebuild completely the
#  package from source (including this makefile, the associated mk
#  script and any documentation files).  It is advisable to use several
#  smaller source archives in place of one large one to speed up build
#  times.

SOURCE_FILES = $(PKG_NAME)_source.tar makefile mk $(DOCUMENTATION) \
	$(PKG_NAME)_sub.tar $(PKG_NAME)_ifls.tar $(PKG_NAME)_sculib.tar \
	$(PKG_NAME)_surflib.tar $(PKG_NAME)_kap.tar INSTALL

#  List of perl scripts. These will be processed during install
#  so that instances of STAR_BIN will be replaced by the necessary
#  location so that ndfperl is picked up correctly.

SCULOG = sculog
NDFPERL_SCRIPTS = $(SCULOG) scuquick qdraw sigclip change_nacentre.pl \
remdbm.pl scushift setbolwt.pl

#  List of public script files.  These are scripts which form part of
#  the package and will be required by users of it.  They will be
#  installed in the $(INSTALL_BIN) directory with execute permission
#  set.

SCUPLOT = scuplot.csh
PUBLIC_SCRIPTS = sdip.csh scupa.csh $(SCUPLOT) scunoise scusetenv.csh

# These are all incarnations of sculog. They are invoked by using
# soft links to sculog. SCULOG uses the name of the softlink to work
# out what to do. This is better than duplicating code.

SCULOG_SCRIPTS = photsum mapsum pointsum skysum

# These are all the incarnations of SCUPLOT. They are invoked by using
# soft links to SCUPLOT.ions of sculog. They are invoked by using
# soft links to sculog. SCUPLOT uses the name of the softlink to work
# out what to do. This is better than duplicating code.

SCUPLOT_SCRIPTS = dspbol pltbol rlinplot


#  Startup script.  This is the file that must be executed (sourced)
#  by a user of this package in order to define appropriate aliases 
#  and environment variables.  It is listed separately from the public 
#  scripts as it is edited by the installation procedure.

STARTUP_SCRIPT = $(PKG_NAME).csh 

#  Link script.  This script does not form part of the package but is 
#  required for building it.  It will not be installed.
 
LINK_SCRIPT = $(PKG_NAME)_link_adam

#  Startup ICL script.  This is the file that must be loaded by ICL
#  for a user of this package,  in order to define the package command
#  set within ICL.  It is listed separately from the public scripts as 
#  it is edited by the installation procedure.

ICL_SCRIPT = $(PKG_NAME).icl 

#  List of private include files.  These are include files
#  which form part of the package and are required in order to build
#  it, but which are not required by users of it.

PRIVATE_INCLUDES = $(SURF_INC) $(KAP_INC)

SURF_INC = surf_par

KAP_INC = ctm_com ctm_par hlpcmd

#  List of external include files.  These are files which are required
#  in order to build the package but form part of other, externally
#  installed packages.  This list should contain the names used to
#  reference the files within the source code, not the actual names of
#  the files.

EXTERNAL_INCLUDES = SAE_PAR DAT_PAR GKS_PAR GRP_PAR MSG_PAR NDF_PAR \
	            NUM_DEC_CVT NUM_DEF_CVT PAR_ERR PAR_PAR \
	            PRM_PAR DAT_ERR GNS_PAR NDF_FUNC AST_PAR NDF_ERR

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines and the main routine, which are treated separately).  

F_ROUTINES = $(SURF_F_ROUTINES) $(SURF_FCPP_ROUTINES) \
	$(KAP_F_ROUTINES) $(SURFLIB_F_ROUTINES) $(SCULIB_F_ROUTINES) \
	$(SCULIB_FCPP_ROUTINES) $(MONO_SOURCE)

SURF_F_ROUTINES = \
surf_chgdata.f surf_chgflat.f surf_chgpnt.f surf_chgqual.f surf_despike.f \
surf_extflat.f surf_extinction.f surf_flatfield.f surf_grid_despike.f \
surf_read_rebin_ndf.f surf_rebin.f surf_recurse_read.f surf_reduce_switch.f \
surf_remsky.f surf_restore.f surf_scan_rlb.f surf_scucat.f surf_scuclip.f \
surf_scuhelp.f surf_scuover.f surf_scuphot.f surf_skydip.f surf_write_data.f \
surf_write_photom.f surf_write_photom_header.f surf_scuba2mem.f \
surf_make_weight.f surf_grid_calcsky.f surf_reduce_noise.f \
surf_add_dbm.f surf_remip.f

SURF_FCPP_ROUTINES = surf_write_map_info.F surf_set_app_name.F

KAP_F_ROUTINES = \
gethlp.f kpg1_agref.f kpg1_macol.f kpg1_qidat.f gthlpi.f kpg1_asref.f \
kpg1_nmcol.f kpg1_scrsz.f kpg1_agatc.f kpg1_hmsg.f kpg1_palci.f \
pthlpo.f kpg1_agfnd.f kpg1_ivci.f kpg1_qcol.f sread.f

SURFLIB_F_ROUTINES = \
surflib_calc_gridij.f surflib_diff_despike.f surflib_fill_grid.f \
surflib_calc_ijpos.f surflib_histogram_grid.f \
surflib_calc_output_grid.f surflib_plot_grid.f surflib_clip_grid.f \
surflib_stats_grid.f surflib_decode_remsky_string.f \
surflib_median_regrid.f surflib_remove_dc_from_exp.f surflib_rem_timeseries.f \
surflib_rem_grid.f surflib_2dft_chop.f surflib_calc_dual_beam.f \
surflib_process_bols.f surflib_fill_polpack_angles.f surflib_remove_ip.f \
surflib_fill_wplate.f surflib_read_ipfile.f surflib_calc_polpack_angrot.f \
surflib_trim_image.f

SCULIB_FCPP_ROUTINES = sculib_bessel_wtinit.F sculib_skydip_temperatures.F

SCULIB_F_ROUTINES = \
sculib_1d2_jiggle.f sculib_2pos_confn.f sculib_2pos_deconv.f \
sculib_3pos_confn.f sculib_3pos_deconv.f sculib_add_demod_exposure.f \
sculib_addare.f sculib_addcad.f sculib_addcai.f sculib_addcar.f \
sculib_airmass.f sculib_analyse_photom_jiggle.f sculib_apparent_2_mp.f \
sculib_apparent_2_tp.f sculib_bessj1.f sculib_bits.f sculib_boldecode.f \
sculib_bolname.f sculib_bolselect.f sculib_calc_apparent.f \
sculib_calc_bol_coords.f \
sculib_calc_flatfield.f sculib_calc_grid.f sculib_calc_output_coords.f \
sculib_calc_skydip_temps.f sculib_calc_sub_bols.f sculib_cfillb.f \
sculib_cfilld.f sculib_cfilli.f sculib_cfillr.f sculib_clip_bol.f \
sculib_coadd.f sculib_coadd_maps.f sculib_coadd_remove.f \
sculib_compress_demod.f sculib_construct_out.f sculib_convolve.f \
sculib_copy_demod_switch.f sculib_copy_good.f sculib_copyb.f \
sculib_copyd.f sculib_copyi.f sculib_copyr.f sculib_correct_extinction.f \
sculib_covsrt.f sculib_crosstalk.f sculib_day.f \
sculib_decode_angle.f sculib_decode_component.f sculib_decode_filter.f \
sculib_decode_rebin_line.f sculib_decode_spec.f sculib_div_calibrator.f \
sculib_div_calibrator_2.f sculib_extract_2dim_b.f \
sculib_extract_2dim_d.f sculib_extract_2dim_r.f sculib_extract_bol.f \
sculib_find_switch.f sculib_fit_2d_gaussian.f sculib_fit_2d_parabola.f \
sculib_fit_d2xisq_daj2.f sculib_fit_d2xisq_dajk.f sculib_fit_dxisq_daj.f \
sculib_fit_function.f sculib_fit_makealpha.f sculib_fit_makebeta.f \
sculib_fit_mult.f sculib_fit_plane.f sculib_fit_skydip.f \
sculib_flatfield_data.f sculib_flatfield_sequence.f sculib_free.f \
sculib_gaussian_xisq.f sculib_gaussj.f sculib_gensyconfn.f \
sculib_get_bol_desc.f sculib_get_dem_pntr.f sculib_get_filename.f \
sculib_get_fits_c.f sculib_get_fits_d.f sculib_get_fits_i.f \
sculib_get_fits_l.f sculib_get_fits_r.f sculib_get_jiggle.f \
sculib_get_lst_strt.f sculib_get_mjd.f sculib_get_raster.f \
sculib_get_sub_bols.f sculib_get_sub_inst.f sculib_insert_bol.f \
sculib_integrate_photom_jiggle.f sculib_invert_matrix.f \
sculib_j_theoretical.f sculib_jnu.f sculib_linear_wtinit.f \
sculib_lst.f sculib_malloc.f sculib_map_allan_variance.f \
sculib_mask_data.f sculib_mrqcof.f sculib_mrqmin.f \
sculib_mulcar.f sculib_multare.f sculib_nfilli.f sculib_nfillr.f \
sculib_par_get.f sculib_photom_bolselect.f sculib_power2.f \
sculib_put_fits_c.f sculib_put_fits_d.f \
sculib_put_fits_i.f sculib_ranged.f sculib_read_jiggle.f \
sculib_read_numbers.f sculib_read_sky.f sculib_read_tauz.f \
sculib_readbols.f sculib_reduce_switch.f sculib_remove_demod_int.f \
sculib_remove_linear_baseline.f sculib_remove_opacity.f sculib_remsky.f \
sculib_rewrite_fits_c.f sculib_rewrite_fits_i.f sculib_rewrite_fits_r.f \
sculib_scan_apparent_tp_2_azna.f sculib_search_datadir.f \
sculib_set_data.f sculib_set_data_bit.f sculib_set_data_ub.f \
sculib_set_qual.f sculib_set_quality.f sculib_set_user.f sculib_sinc.f \
sculib_skycon_1.f sculib_skydip_allan_variance.f sculib_skydip_bols.f \
sculib_skydip_xisq.f sculib_skyfunc.f sculib_skyfunc_1.f \
sculib_skyfunc_2.f sculib_skyfuncd.f sculib_spline_pda_idbvip.f \
sculib_spline_pda_idsfft.f sculib_spline_pda_surfit.f \
sculib_spline_regrid.f sculib_spline_regrid_1.f sculib_split_file_spec.f \
sculib_sqrootr.f sculib_standard_apparent.f sculib_statr.f \
sculib_sub_tauz.f sculib_subare.f sculib_sumad.f sculib_tidy_line.f \
sculib_unpack.f sculib_unpack_chopscan.f sculib_unpack_jiggle.f \
sculib_unpack_jiggle_separates.f sculib_ut1.f sculib_wtfn_regrid.f \
sculib_wtfn_regrid_1.f sculib_wtfn_regrid_2.f sculib_wtfn_regrid_3.f \
sculib_scan_2_rd.f sculib_add_chop.f sculib_mulcad.f \
sculib_gauss_wtinit.f sculib_skydip_var.f \
sculib_fix_scan_v10.f sculib_noise_mean.f


#  List of Fortran BLOCK DATA routines.

BLOCK_DATA = $(KAPGEN_BLOCK_DATA)
 
KAPGEN_BLOCK_DATA = ctm_xcols.f

#  List of platform specific Fortran routines.  The source tar file will
#  contain a version of these files for each set of platforms.  The names
#  given here are the general version of the file, e.g. pkg_open.f.  The
#  names in the tar file will be pkg_open.f_sun4, pkg_open.f_mips, etc.

PLATFORM_F = $(PF_SUB1) $(PF_SUB2)

PF_SUB1 =

PF_SUB2 =

#  C routines required for building the package.  This is just a list of
#  all the C source files.

C_ROUTINES = $(KAP_C_ROUTINES) $(SCULIB_C_ROUTINES)

KAP_C_ROUTINES = kpg1_trmsz.c

SCULIB_C_ROUTINES = sculib_linux.c

#  List of applications/tasks in monolith.

TASKS = \
bolrebin calcsky  change_data change_flat change_pointing change_quality \
despike despike2 extinction extract_data extract_flat flatfield intrebin \
rebin reduce_switch remsky restore scucat scan_rlb scuclip scuhelp scuover \
scuphot skydip scuba2mem scumakewt reduce_noise add_dbm remip

#  Monolith routine source name.

MONO_SOURCE = $(PKG_NAME)_mon.f


#  Monolith binary name.

MONOLITH = $(PKG_NAME)_mon

#  Monolith interface file name.

MONO_IFL = $(PKG_NAME)_mon.ifl


#  List of task interface files.

IFL_FILES = $(TASKS:=.ifl)

#  List of help library sources.

HELP_SOURCES = $(PKG_NAME).hlp

#  List of ancilliary data files.
# These are the calibrator signal files

DATA_FILES = calsig_450_850_photom.sdf calsig_450_850_bp2.sdf \
	calsig_450_map.sdf calsig_450_850_map.sdf calsig_850_map.sdf \
	ipfile.dat polimp.scuba


#  Lists of Latex and hypertext documents.

LATEX_DOCS = sun216.tex \
sun216_arrays.eps  sun216_logo.eps    sun216_remsky.eps \
sun216_qdraw.eps   sun216_skydip.eps sun216_3c279.eps \
sun216_fitsdip.eps sun216_rawsdip.eps sun216_rawsdip2.eps \
sun216_shift_diag.eps sun216_despikemodes.eps sun216_despike_eg.eps \
sun216_4chops.eps sun216_flow_photom.eps sun216_flow_jig.eps \
sun216_flow_scan.eps sun216_scansky.eps sun216_m82_remdbm.eps \
sc10.tex \
sc10_fig1.eps sc10_fig2.eps sc10_fig3.eps sc10_fig4.eps sc10_logo.eps \
sc11.tex \
sc11_fig1.eps sc11_fig2.eps sc11_fig3.eps sc11_fig4.eps sc11_fig5.eps \
sc11_fig6.eps sc11_fig7.eps sc11_fig8.eps sc11_fig9.eps sc11_fig10.eps \
sc11_fig11.eps sc11_fig12.eps sc11_flow.eps sc11_logo.eps


HYPERTEXT_DOCS = sun216.htx sc10.htx sc11.htx

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
.SUFFIXES: .o .c .f .ifc .ifl .shl .hlp .F .sdf

#  List of files which must be built from the source files before the
#  package can be installed for use.  This should comprise all the files
#  that are required to use the package (but excluding the date stamp
#  file).

BUILT_FILES = $(PUBLIC_SCRIPTS) $(OBJECT_LIBRARIES) $(MONOLITH) \
$(IFC_FILES) $(HELP_FILES) $(ICL_SCRIPT) $(DATA_FILES) $(STARTUP_SCRIPT) \
$(NDFPERL_SCRIPTS)


#  List of links used to access include files during compilation.  This
#  should comprise all the external include files and any other include
#  files whose names do not exactly match the names used in the source
#  code.

INCLUDE_LINKS = $(EXTERNAL_INCLUDES)  HLPCMD CTM_COM CTM_PAR \
  $(PKG_LINK)_PAR

#  Rules to set up links to locate each of the above include files.

$(PKG_LINK)_PAR:   $(PKG_NAME)_par;               $(LINK) $? $@
CTM_COM:     ctm_com;                 $(LINK) $? $@
CTM_PAR:     ctm_par;                 $(LINK) $? $@
DAT_ERR:     $(STAR_INC)/dat_err;     $(LINK) $? $@
DAT_PAR:     $(STAR_INC)/dat_par;     $(LINK) $? $@
HLPCMD:      hlpcmd;                  $(LINK) $? $@
GKS_PAR:     $(STAR_INC)/gks_par;     $(LINK) $? $@
GNS_PAR:     $(STAR_INC)/gns_par;     $(LINK) $? $@
GRP_PAR:     $(STAR_INC)/grp_par;     $(LINK) $? $@
MSG_PAR:     $(STAR_INC)/msg_par;     $(LINK) $? $@
NDF_FUNC:    $(STAR_INC)/ndf_func;     $(LINK) $? $@
NDF_PAR:     $(STAR_INC)/ndf_par;     $(LINK) $? $@
NDF_ERR:     $(STAR_INC)/ndf_err;     $(LINK) $? $@
NUM_DEC_CVT: $(STAR_INC)/num_dec_cvt; $(LINK) $? $@
NUM_DEF_CVT: $(STAR_INC)/num_def_cvt; $(LINK) $? $@
PAR_PAR:     $(STAR_INC)/par_par;     $(LINK) $? $@
PAR_ERR:     $(STAR_INC)/par_err;     $(LINK) $? $@
PRM_PAR:     $(STAR_INC)/prm_par;     $(LINK) $? $@
SAE_PAR:   $(STAR_INC)/sae_par;   $(LINK) $? $@
AST_PAR:   $(STAR_INC)/ast_par;  $(LINK) $? $@
FIO_ERR:   $(STAR_INC)/fio_err;  $(LINK) $? $@


#  Rules for extracting source files from the source archive.

$(PUBLIC_SCRIPTS) $(PRIVATE_INCLUDES) $(LINK_SCRIPT) $(MONO_SOURCE) \
$(NDFPERL_SCRIPTS) \
$(HELP_SOURCES) $(MONO_IFL) $(STARTUP_SCRIPT) $(ICL_SCRIPT) $(DATA_FILES):
	$(TAR_OUT) $(PKG_NAME)_source.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

# These are the ifls
$(IFL_FILES):
	$(TAR_OUT) $(PKG_NAME)_ifls.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

# These are the surf_ routines
$(SURF_F_ROUTINES) $(SURF_FCPP_ROUTINES):
	$(TAR_OUT) $(PKG_NAME)_sub.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

# These are the kappa library routines
$(KAP_F_ROUTINES) $(KAP_C_ROUTINES) $(KAPGEN_BLOCK_DATA):
	$(TAR_OUT) $(PKG_NAME)_kap.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

# This is sculib
$(SCULIB_F_ROUTINES) $(SCULIB_FCPP_ROUTINES) $(SCULIB_C_ROUTINES):
	$(TAR_OUT) $(PKG_NAME)_sculib.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi

# This is SURFLIB
$(SURFLIB_F_ROUTINES):
	$(TAR_OUT) $(PKG_NAME)_surflib.tar $@
	@ if test -f $@; then :;\
           else echo $@ is not in the tar file; exit 1; fi




#  Rules for extracting platform specific source files from the source
#  archives.

#  It is quite likely that there are no platform-specific source files
#  and that the macro PF_SUBn is empty. To prevent a syntax error in
#  make, a dummy target is present.

$(PF_SUB1) dummy_target1:
	$(TAR_OUT) $(PKG_NAME)_sub1.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

$(PF_SUB2) dummy_target2:
	$(TAR_OUT) $(PKG_NAME)_sub2.tar $@_$(SOURCE_VARIANT)
	@ if test -f $@_$(SOURCE_VARIANT); then :;\
           else echo $@_$(SOURCE_VARIANT) is not in the tar file; exit 1; fi
	mv $@_$(SOURCE_VARIANT) $@

#  List of object files produced by compiling the source code and rules
#  for performing the compilations.

OBJECT_FILES = $(SURF_OBJECT_FILES) $(KAP_OBJECT_FILES) \
	$(SCULIB_OBJECT_FILES) $(SURFLIB_OBJECT_FILES) $(MONO_OBJECT)

SURF_OBJECT_FILES = $(SURF_F_ROUTINES:.f=.o) $(SURF_FCPP_ROUTINES:.F=.o)

KAP_OBJECT_FILES = $(KAP_F_ROUTINES:.f=.o) $(KAP_C_ROUTINES:.c=.o) \
	$(KAPGEN_BLOCK_DATA:.f=.o)

SCULIB_OBJECT_FILES = $(SCULIB_F_ROUTINES:.f=.o) \
	$(SCULIB_FCPP_ROUTINES:.F=.o) $(SCULIB_C_ROUTINES:.c=.o)

SURFLIB_OBJECT_FILES = $(SURFLIB_F_ROUTINES:.f=.o)

MONO_OBJECT = $(MONO_SOURCE:.f=.o)

.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<
.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -DPKG_VERS="'$(PKG_VERS)'" -c $<


#  List of object library files to be built and rules for building
#  them.

OBJECT_LIBRARIES = libsurf.a libsurflib.a libsculib.a  libsurf_kap.a

libsurf.a: $(SURF_OBJECT_FILES) $(SURF_FCPP_OBJECTS)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libsculib.a: $(SCULIB_OBJECT_FILES) $(SCULIB_FCPP_OBJECTS)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libsurflib.a: $(SURFLIB_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

libsurf_kap.a: $(KAP_OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

#  Rule and dependencies for building the monolith(s).

$(MONOLITH): $(MONO_OBJECT) $(OBJECT_LIBRARIES) $(LINK_SCRIPT)
	$(ALINK) $(MONO_OBJECT) -o $(MONOLITH) $(OBJECT_LIBRARIES) \
            $(ADDLIBS) `./$(LINK_SCRIPT)`

#  Rules for building the help libraries.

HELP_FILES = $(HELP_SOURCES:.hlp=.shl)

.hlp.shl:
	$(STAR_BIN)/hlib $<

#  Rules for compiling the interface files.

IFC_FILES = $(IFL_FILES:.ifl=.ifc) $(MONO_IFL:.ifl=.ifc)

.ifl.ifc:
	$(STAR_BIN)/compifl $<

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
INSTALL_HELP = $(INSTALL)/help/$(PKG_NAME)

#  List of directories actually used for installation (selected from
#  those above) and rules to create them.

INSTALL_DIRS = $(INSTALL_BIN) $(INSTALL_DATES) $(INSTALL_DOCS) $(INSTALL_HELP)

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
	@ echo '   FCPPFLAGS: $(FCPPFLAGS)'   >>$(DATE_STAMP)
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
#  Setup the soft links to sculog
	for f in $(SCULOG_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              $(LINK) ./$(SCULOG) $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done
#
#  Setup the soft links to scuplot
	for f in $(SCUPLOT_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              $(LINK) ./$(SCUPLOT) $(INSTALL_BIN)/$$f;\
           else :; fi;\
        done

#
#  Install the monolith, giving it world execute permission.
	for f in $(MONOLITH) ""; do \
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
#
#  Create the application/task name soft links.
	for f in $(TASKS) ""; do \
           if test -n "$$f"; then \
              $(LINK) ./$(MONOLITH) $(INSTALL_BIN)/$$f ;\
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
               -e s#STARLINK#$(STARLINK)# \
               -e s#PKG_VERS#$(PKG_VERS)# \
               -e s#STAR_BIN#$(STAR_BIN)# \
               -e s#STAR_PERL#$(PERL)# \
              $(STARTUP_SCRIPT) >$(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
           chmod 755 $(INSTALL_BIN)/$(STARTUP_SCRIPT) ;\
        else :; fi
#
#  Install the package ICL script.  The package version number must be 
#  edited into this, and read permission given.  Leave the original file
#  in place.
	if test -n "$(ICL_SCRIPT)"; then \
           sed -e s#PKG_VERS#$(PKG_VERS)# \
              $(ICL_SCRIPT) >$(INSTALL_BIN)/$(ICL_SCRIPT) ;\
           chmod 755 $(INSTALL_BIN)/$(ICL_SCRIPT) ;\
        else :; fi
#
#  Install the NDFPERL scripts. The location of the NDFPERL binary
#  is taken to be $STAR_BIN. The original file is left in place.
	for f in $(NDFPERL_SCRIPTS) ""; do \
           if test -n "$$f"; then \
              sed -e s#STAR_BIN#$(STAR_BIN)# \
                  -e s#STAR_PERL#$(PERL)# \
                $$f > $(INSTALL_BIN)/$$f ;\
              chmod 755 $(INSTALL_BIN)/$$f;\
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
#  Deinstall the soft links to $(SCULOG)
	- for f in $(SCULOG_SCRIPTS) ""; do \
             if test -n "$$f"; then \
                rm -f $(INSTALL_BIN)/$$f ;\
           else :; fi;\
        done
#
#  Deinstall the soft links to $(SCUPLOT)
	- for f in $(SCUPLOT_SCRIPTS) ""; do \
             if test -n "$$f"; then \
                rm -f $(INSTALL_BIN)/$$f ;\
           else :; fi;\
        done
#
#  Deinstall the monolith, if an installed version exists.
	- for f in $(MONOLITH) ""; do \
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
#  Deinstall the package ICL file.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists.
	- if test -n "$(ICL_SCRIPT)"; then \
           if test -f $(INSTALL_BIN)/$(ICL_SCRIPT); then\
              rm -f $(INSTALL_BIN)/$(ICL_SCRIPT);\
           else :; fi ;\
           $(MAKE) $(ICL_SCRIPT);\
        else :; fi
#
#  Deinstall the NDFPERL scripts.  Since it will have been edited
#  during installation, we remove the installed copy, if present, and
#  then ensure that the original exists
	- for f in $(NDFPERL_SCRIPTS) ""; do \
	  if test -n "$$f"; then \
           if test -f $(INSTALL_BIN)/$$f; then \
              rm -f $(INSTALL_BIN)/$$f;\
           else :; fi ;\
           $(MAKE) $$f;\
          else :; fi \
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
	@ echo '$(PACK_NAME) does not have a make driven installation test'
	@ echo
	@ echo 'To test that $(PACK_NAME) is installed correctly, run the'
	@ echo 'startup script, then invoke the help facility:'
	@ echo
	@ echo ' % setenv SURF_DIR $(INSTALL_BIN)'
	@ echo ' % setenv SURF_HELP $(INSTALL_HELP)/surf'
	@ echo ' % source $(INSTALL_BIN)/surf.csh'
	@ echo ' % scuhelp'
	@ echo
#	@ echo 'To test $(PACK_NAME), you should run the demonstation script,'
#	@ echo 'thus (% is the shell prompt):'
#	@ echo
#	@ echo ' % cd /empty_directory'
#	@ echo ' % source /star/etc/cshrc'
#	@ echo ' % xdisplay'
#	@ echo ' % $(PKG_NAME)setup'
#	@ echo ' % command_name'
#	@ echo
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
              $(MONO_IFL) $(HELP_SOURCES) $(PLATFORM_F) $(BLOCK_DATA) \
              $(C_ROUTINES) $(LINK_SCRIPT) 1>/dev/null 2>/dev/null
	@- rm -f $(F_ROUTINES) dtask_applic.f 1>/dev/null 2>/dev/null
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

#  strip: strip the monolith(s) of debug information.
#  -------------------------------------------------

strip:
	strip $(MONOLITH)
	@ echo
	@ echo '*** Binaries stripped'
	@ echo

#-------------------------------------------------------------------------------

#  Include file dependencies.
#  =========================

#  Object file dependencies on include files (or links to those include
#  files).  These are normally generated automatically from the source
#  files.
#  Create with:
#    foreach i (*.f)
#      set incs = `grep INCLUDE $i | awk -F\' '{print $2}'`
#      echo ${i:r}.o: $incs
#    end


surf_add_dbm.o: SAE_PAR SURF_PAR NDF_ERR DAT_PAR PAR_ERR
surf_reduce_noise.o: SAE_PAR MSG_PAR SURF_PAR DAT_PAR PRM_PAR PAR_ERR
surf_write_map_info.o: DAT_PAR PRM_PAR SAE_PAR SURF_PAR AST_PAR
surf_chgdata.o: DAT_PAR MSG_PAR NUM_DEC_CVT NUM_DEF_CVT PRM_PAR SAE_PAR SURF_PAR
surf_chgflat.o: DAT_PAR MSG_PAR SAE_PAR SURF_PAR
surf_chgpnt.o: DAT_PAR MSG_PAR PAR_ERR SAE_PAR SURF_PAR
surf_chgqual.o: DAT_PAR MSG_PAR PRM_PAR SAE_PAR SURF_PAR
surf_despike.o: DAT_PAR MSG_PAR SAE_PAR SURF_PAR
surf_extflat.o: DAT_PAR MSG_PAR SAE_PAR SURF_PAR
surf_extinction.o: DAT_PAR MSG_PAR PRM_PAR SAE_PAR SURF_PAR
surf_flatfield.o: DAT_PAR MSG_PAR SAE_PAR SURF_PAR
surf_grid_calcsky.o: DAT_PAR MSG_PAR PAR_ERR PRM_PAR SAE_PAR SURF_PAR
surf_grid_despike.o: MSG_PAR PAR_ERR PRM_PAR SAE_PAR
surf_make_weight.o: DAT_PAR MSG_PAR SAE_PAR SURF_PAR
surf_mon.o: PAR_PAR SAE_PAR
surf_read_rebin_ndf.o: DAT_PAR MSG_PAR PRM_PAR SAE_PAR SURF_PAR
surf_rebin.o: DAT_PAR MSG_PAR NDF_PAR PAR_ERR PAR_PAR PRM_PAR SAE_PAR SURF_PAR
surf_recurse_read.o: DAT_PAR MSG_PAR NDF_PAR SAE_PAR SURF_PAR
surf_reduce_switch.o: DAT_PAR MSG_PAR NDF_PAR PRM_PAR SAE_PAR SURF_PAR
surf_remsky.o: DAT_PAR MSG_PAR PRM_PAR SAE_PAR SURF_PAR
surf_restore.o: DAT_PAR MSG_PAR SAE_PAR SURF_PAR
surf_scan_rlb.o: DAT_PAR MSG_PAR SAE_PAR SURF_PAR
surf_scuba2mem.o: DAT_PAR PRM_PAR SAE_PAR SURF_PAR
surf_scucat.o: DAT_PAR GRP_PAR MSG_PAR NDF_PAR PAR_ERR PRM_PAR SAE_PAR SURF_PAR
surf_scuclip.o: DAT_PAR MSG_PAR PRM_PAR SAE_PAR SURF_PAR
surf_scuhelp.o: PAR_ERR SAE_PAR
surf_scuover.o: DAT_PAR GKS_PAR MSG_PAR NDF_PAR PAR_ERR PAR_PAR PRM_PAR SAE_PAR SURF_PAR
surf_scuphot.o: DAT_PAR MSG_PAR NDF_PAR PAR_ERR PRM_PAR SAE_PAR SURF_PAR
surf_set_app_name.o: SAE_PAR PAR_PAR SURF_PAR
surf_skydip.o: DAT_PAR MSG_PAR NDF_FUNC PAR_ERR PRM_PAR SAE_PAR SURF_PAR
surf_write_data.o: PRM_PAR SAE_PAR
surf_write_photom.o: PRM_PAR SAE_PAR
surf_write_photom_header.o: PRM_PAR SAE_PAR SURF_PAR
sculib_noise_mean.o: SAE_PAR
sculib_1d2_jiggle.o: SAE_PAR
sculib_2pos_confn.o: SAE_PAR
sculib_2pos_deconv.o: MSG_PAR PRM_PAR SAE_PAR
sculib_3pos_confn.o: SAE_PAR
sculib_3pos_deconv.o: MSG_PAR PRM_PAR SAE_PAR
sculib_add_chop.o: PRM_PAR SAE_PAR
sculib_add_demod_exposure.o: SAE_PAR
sculib_addare.o: PRM_PAR SAE_PAR
sculib_airmass.o: SAE_PAR
sculib_analyse_photom_jiggle.o: NDF_FUNC PRM_PAR SAE_PAR
sculib_apparent_2_mp.o: SAE_PAR
sculib_apparent_2_tp.o: SAE_PAR
sculib_bessj1.o: PRM_PAR SAE_PAR
sculib_bits.o: NUM_DEC_CVT NUM_DEF_CVT
sculib_boldecode.o: SAE_PAR
sculib_bolname.o: SAE_PAR
sculib_bolselect.o: PRM_PAR SAE_PAR
sculib_calc_apparent.o: SAE_PAR
sculib_calc_bol_coords.o: PRM_PAR SAE_PAR
sculib_calc_flatfield.o: PRM_PAR SAE_PAR
sculib_calc_grid.o: SAE_PAR
sculib_calc_output_coords.o: SAE_PAR
sculib_calc_skydip_temps.o: MSG_PAR PRM_PAR SAE_PAR
sculib_calc_sub_bols.o: SAE_PAR
sculib_clip_bol.o: NDF_FUNC PRM_PAR SAE_PAR
sculib_coadd.o: NDF_FUNC PRM_PAR
sculib_coadd_maps.o: PRM_PAR SAE_PAR
sculib_coadd_remove.o:
sculib_compress_demod.o: PRM_PAR SAE_PAR
sculib_construct_out.o: SAE_PAR
sculib_convolve.o: NDF_FUNC PRM_PAR SAE_PAR
sculib_copy_demod_switch.o: SAE_PAR
sculib_copy_good.o: PRM_PAR SAE_PAR
sculib_correct_extinction.o: SAE_PAR
sculib_covsrt.o: SAE_PAR
sculib_crosstalk.o: SAE_PAR
sculib_day.o: SAE_PAR
sculib_decode_angle.o: SAE_PAR
sculib_decode_component.o: SAE_PAR
sculib_decode_filter.o: SAE_PAR
sculib_decode_rebin_line.o: PRM_PAR SAE_PAR
sculib_decode_spec.o: PRM_PAR SAE_PAR
sculib_div_calibrator.o: SAE_PAR
sculib_div_calibrator_2.o: SAE_PAR
sculib_extract_2dim_b.o: SAE_PAR
sculib_extract_2dim_d.o: SAE_PAR
sculib_extract_2dim_r.o: SAE_PAR
sculib_extract_bol.o: PRM_PAR SAE_PAR
sculib_find_switch.o: SAE_PAR
sculib_fit_2d_gaussian.o: PRM_PAR SAE_PAR
sculib_fit_2d_parabola.o: NDF_FUNC SAE_PAR
sculib_fit_d2xisq_daj2.o: SAE_PAR
sculib_fit_d2xisq_dajk.o: SAE_PAR
sculib_fit_dxisq_daj.o: SAE_PAR
sculib_fit_function.o: SAE_PAR
sculib_fit_makealpha.o: SAE_PAR
sculib_fit_makebeta.o: SAE_PAR
sculib_fit_mult.o: SAE_PAR
sculib_fit_plane.o: SAE_PAR
sculib_fit_skydip.o: MSG_PAR PRM_PAR SAE_PAR
sculib_fix_scan_v10.o: SAE_PAR
sculib_flatfield_data.o: SAE_PAR
sculib_flatfield_sequence.o: PRM_PAR SAE_PAR
sculib_free.o: PRM_PAR SAE_PAR
sculib_gauss_wtinit.o: SAE_PAR
sculib_gaussian_xisq.o: SAE_PAR
sculib_gaussj.o: SAE_PAR
sculib_gensyconfn.o: SAE_PAR
sculib_get_bol_desc.o: SAE_PAR
sculib_get_dem_pntr.o: SAE_PAR
sculib_get_filename.o: SAE_PAR
sculib_get_fits_c.o: SAE_PAR
sculib_get_fits_d.o: PRM_PAR SAE_PAR
sculib_get_fits_i.o: PRM_PAR SAE_PAR
sculib_get_fits_l.o: PRM_PAR SAE_PAR
sculib_get_fits_r.o: PRM_PAR SAE_PAR
sculib_get_jiggle.o: SAE_PAR
sculib_get_lst_strt.o: SAE_PAR
sculib_get_mjd.o: SAE_PAR
sculib_get_raster.o: SAE_PAR
sculib_get_sub_bols.o: SAE_PAR
sculib_get_sub_inst.o: PRM_PAR SAE_PAR
sculib_insert_bol.o: PRM_PAR SAE_PAR
sculib_integrate_photom_jiggle.o: SAE_PAR
sculib_invert_matrix.o: SAE_PAR
sculib_j_theoretical.o: SAE_PAR
sculib_jnu.o: PRM_PAR SAE_PAR
sculib_linear_wtinit.o: SAE_PAR
sculib_lst.o: SAE_PAR
sculib_malloc.o: PRM_PAR SAE_PAR
sculib_map_allan_variance.o: SAE_PAR
sculib_mask_data.o: PRM_PAR SAE_PAR
sculib_mrqcof.o: SAE_PAR
sculib_mrqmin.o: SAE_PAR
sculib_mulcad.o: PRM_PAR SAE_PAR
sculib_mulcar.o: PRM_PAR
sculib_multare.o: SAE_PAR
sculib_par_get.o: PRM_PAR SAE_PAR
sculib_photom_bolselect.o: PRM_PAR SAE_PAR
sculib_put_fits_c.o: SAE_PAR
sculib_put_fits_d.o: SAE_PAR
sculib_put_fits_i.o: SAE_PAR
sculib_read_jiggle.o: SAE_PAR
sculib_read_numbers.o: SAE_PAR
sculib_read_sky.o: PRM_PAR SAE_PAR
sculib_read_tauz.o: PRM_PAR SAE_PAR
sculib_readbols.o: SAE_PAR
sculib_reduce_switch.o: SAE_PAR
sculib_remove_demod_int.o: SAE_PAR
sculib_remove_linear_baseline.o: MSG_PAR NDF_FUNC PRM_PAR SAE_PAR
sculib_remove_opacity.o: PRM_PAR SAE_PAR
sculib_remsky.o: MSG_PAR NDF_FUNC PRM_PAR SAE_PAR
sculib_rewrite_fits_c.o: SAE_PAR
sculib_rewrite_fits_i.o: SAE_PAR
sculib_rewrite_fits_r.o: SAE_PAR
sculib_scan_2_rd.o: SAE_PAR
sculib_scan_apparent_tp_2_azna.o: PRM_PAR SAE_PAR
sculib_search_datadir.o: DAT_PAR MSG_PAR NDF_PAR PAR_ERR SAE_PAR
sculib_set_data.o: PRM_PAR SAE_PAR
sculib_set_data_bit.o: PRM_PAR SAE_PAR
sculib_set_data_ub.o: PRM_PAR SAE_PAR
sculib_set_qual.o: SAE_PAR
sculib_set_quality.o: SAE_PAR
sculib_skydip_allan_variance.o: SAE_PAR
sculib_skydip_bols.o: SAE_PAR
sculib_skydip_var.o: SAE_PAR
sculib_skydip_xisq.o: SAE_PAR
sculib_spline_pda_idbvip.o: PRM_PAR SAE_PAR
sculib_spline_pda_idsfft.o: PRM_PAR SAE_PAR
sculib_spline_pda_surfit.o: MSG_PAR PRM_PAR SAE_PAR
sculib_spline_regrid.o: MSG_PAR PRM_PAR SAE_PAR
sculib_spline_regrid_1.o: PRM_PAR SAE_PAR
sculib_split_file_spec.o: SAE_PAR
sculib_sqrootr.o: PRM_PAR SAE_PAR
sculib_standard_apparent.o: SAE_PAR
sculib_statd.o: NDF_FUNC PRM_PAR SAE_PAR
sculib_statr.o: NDF_FUNC PRM_PAR SAE_PAR
sculib_sub_tauz.o: PRM_PAR SAE_PAR
sculib_subare.o: PRM_PAR SAE_PAR
sculib_sumad.o: SAE_PAR
sculib_tidy_line.o: SAE_PAR
sculib_unpack.o: SAE_PAR
sculib_unpack_chopscan.o: SAE_PAR
sculib_unpack_jiggle.o: PRM_PAR SAE_PAR
sculib_unpack_jiggle_separates.o: NDF_FUNC PRM_PAR SAE_PAR
sculib_ut1.o: SAE_PAR
sculib_wtfn_regrid.o: MSG_PAR PRM_PAR SAE_PAR
sculib_wtfn_regrid_1.o: PRM_PAR SAE_PAR
sculib_wtfn_regrid_2.o: PRM_PAR SAE_PAR
sculib_wtfn_regrid_3.o: PRM_PAR SAE_PAR
sculib_bessel_wtinit.o: SAE_PAR
sculib_skydip_temperatures.o: SAE_PAR
surflib_2dft_chop.o: SAE_PAR
surflib_calc_gridij.o: PRM_PAR SAE_PAR
surflib_calc_ijpos.o: PRM_PAR SAE_PAR
surflib_calc_output_grid.o: PRM_PAR SAE_PAR
surflib_clip_grid.o: MSG_PAR PAR_ERR PRM_PAR SAE_PAR
surflib_construct_cube.o: PRM_PAR SAE_PAR
surflib_decode_remsky_string.o: MSG_PAR PRM_PAR SAE_PAR
surflib_diff_despike.o: MSG_PAR NDF_FUNC PRM_PAR SAE_PAR
surflib_fill_grid.o: NDF_FUNC PRM_PAR SAE_PAR
surflib_histogram_grid.o: NDF_FUNC PRM_PAR SAE_PAR
surflib_median_regrid.o: MSG_PAR PAR_ERR PRM_PAR SAE_PAR
surflib_plot_grid.o: MSG_PAR PAR_ERR PRM_PAR SAE_PAR
surflib_rem_grid.o: PRM_PAR SAE_PAR
surflib_rem_timeseries.o: PRM_PAR SAE_PAR
surflib_remove_dc_from_exp.o: MSG_PAR PRM_PAR SAE_PAR
surflib_stats_grid.o: PRM_PAR SAE_PAR
surflib_calc_dual_beam.o: PRM_PAR SAE_PAR
surflib_process_bols.o: PRM_PAR SAE_PAR
surflib_read_ipfile.o: FIO_ERR SAE_PAR PRM_PAR
surflib_trim_image.o: SAE_PAR NDF_FUNC
ctm_xcols.o: CTM_PAR
gethlp.o: HLPCMD SAE_PAR
gthlpi.o: HLPCMD SAE_PAR
kpg1_agatc.o: DAT_PAR SAE_PAR
kpg1_agfnd.o: DAT_PAR SAE_PAR
kpg1_agref.o: DAT_ERR DAT_PAR SAE_PAR
kpg1_asref.o: DAT_PAR NDF_PAR PAR_PAR SAE_PAR
kpg1_hmsg.o: SAE_PAR
kpg1_ivci.o: CTM_PAR GNS_PAR SAE_PAR
kpg1_macol.o: SAE_PAR
kpg1_nmcol.o: CTM_COM CTM_PAR SAE_PAR
kpg1_palci.o: CTM_PAR SAE_PAR
kpg1_qcol.o: GKS_PAR SAE_PAR
kpg1_qidat.o: GKS_PAR SAE_PAR
kpg1_scrsz.o: SAE_PAR
pthlpo.o: HLPCMD SAE_PAR

#-------------------------------------------------------------------------------
#
#  End of makefile.
#.
