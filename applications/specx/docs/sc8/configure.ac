dnl    Process this file with autoconf to produce a configure script
AC_REVISION($Revision$)
 
dnl    Initialisation: package name and version number
AC_INIT(specx-sc8, 6.8-5, ussc@star.rl.ac.uk)
 
dnl    Require autoconf-2.50 at least
AC_PREREQ(2.50)
dnl    Require Starlink automake
AM_INIT_AUTOMAKE(1.8.2-starlink)
 
dnl    Sanity-check: name a file in the source directory -- if this
dnl    isn't found then configure will complain
AC_CONFIG_SRCDIR([sc8.tex])

dnl    Include defaults for Starlink configurations
dnl    Need to find out whether we need this
STAR_DEFAULTS
 
dnl    Find required versions of the programs we need for configuration
AC_PROG_MAKE_SET
STAR_LATEX_DOCUMENTATION(sc8)
STAR_DOC_FILES(sc8_badbaseline.ps sc8_dm_dirty.ps sc8_fsw_reduction.ps sc8_rlb.ps sc8_cgm.ps sc8_dm_drop.ps sc8_gray.ps sc8_s-l-r-f.ps sc8_chann.ps sc8_dm_orig.ps sc8_gr_sp.ps sc8_smooth.ps sc8_concat.ps sc8_duffy.ps sc8_hdo_freq.ps sc8_spectrum.ps sc8_cont.ps sc8_fgm.ps sc8_hdo-lsb.ps sc8_spiked.ps sc8_dasmerge_yn.ps sc8_fpb.ps sc8_hdo-orig.ps sc8_despiked.ps sc8_fpb-result.ps sc8_hdo-usb.ps sc8_dm_dasmerge.ps sc8_fsw-fig.ps sc8_merge_diffs.ps)

dnl    If you wish to configure extra files, you can add them to this
dnl    declaration.
AC_CONFIG_FILES(Makefile)

dnl    This is the bit that does the actual work
AC_OUTPUT
