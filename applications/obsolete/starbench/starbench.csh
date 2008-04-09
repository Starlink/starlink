#!/bin/csh -f
#+
# Name:
#   starbench.csh 
#
# Purpose:
#    Setup for the Benchmarking Utilities. 
#    N.B. This file is edited by the installation target in the makefile.
#         Any changes should be made to the template file NOT to the
#         installed file.
#
# Language:
#    C shell
#
# Invocation:
#    source $BENCH_DIR/starbench.csh    
#
# Description:
#    Make some initial checks prior to running the benchmark utilities.
#    This routine also defines local constants for the benchmarks and
#    sets up an environment from which benchmarks can be run.
#
# Authors:
#    Tim Gledhill (tmg@star.herts.ac.uk)
#
# History:
#     21-Dec-1995 (tmg)
#       Implemented at v0.9 of the benchmark suite.
#     18-Jul-1996 (tmg)
#       V1.0 incorporating IRAF benchmarks.
#     12-Nov-1997 (tmg)
#       V1.1 for new Starlink software distribution (ussc207). Change
#       the mechanism for checking package installations (no .INSTALLED
#       files anymore).
#     24-Nov-1998 (tmg)
#       V1.2 Define variables BENCH_USSC_VER and BENCH_IRAF_VER to identify
#       the currently installed versions of USSC and IRAF software.
#-
# 
# Initialisation: perform some checks and define local constants.
# ==============
#
#
# Set interrupt handling.
#
      onintr abort_script
#
# Define local constants. These are filled in at install-time by the
# makefile.
#
      setenv BENCH_VERSION PKG_VERS              #Benchmark version number
      setenv BENCH_DOC LATEX_DOC                 #Latex doc for this package
      setenv BENCH_REVISION_DATE PKG_VERS        #Last revision of this package
      setenv BENCH_DIR INSTALL_BIN               #Installation directory
      setenv BENCH_HELP INSTALL_HELP             #Help directory
      setenv INSTALL_FILE .INSTALLED             #Installed marker file
#
# The software is now expected to reside in a directory specified by the
# environment variable BENCH_DIR. Check the installation.
#
      if ( ! -e ${BENCH_DIR}/$INSTALL_FILE ) then
        echo ""
        echo " The Benchmark Utilites have not been correctly installed in"
        echo " ${BENCH_DIR}. Please check that the installation was"
        echo " successful and, if necessary, deinstall and install the"
        echo " package. See $BENCH_DOC:r for further help"
        echo ""
        goto end_script
      endif
#
# Check the current working directory and make sure it is not the same as
# the installation directory. Running the benchmarks from the installation 
# directory can cause problems - so prevent it.
#
      if ( $cwd == $BENCH_DIR ) then
        echo ""
        echo " The Benchmark Utilities should not be run from the installation"
        echo " directory. Please change directory and try again."
        echo ""
        goto end_script
      endif
#
# Define shell aliases for the benchmark commands.
#
      alias bench      $BENCH_DIR/bench
      alias submit     $BENCH_DIR/submit
      alias scan       $BENCH_DIR/scan
#
# Print an intoductory message.
#
      echo ""
      echo " Starlink Benchmark Utility v$BENCH_VERSION" 
      echo ""            
#
# Check for presence of the benchmarked packages. At version 1.1 it is
# no longer possible to check for the package .INSTALLED files, since 
# these will not be present on run-only installations. Just do a basic
# check for the package bin directory. If the benchmarks fail due to 
# an incorrect installation then the falure will be logged during the
# benchmark execution anyway.
#
      echo -n "   USSC packages: " 
      unsetenv BENCH_KAPPA BENCH_PISA BENCH_FIG BENCH_CCDPACK BENCH_USSC
      if ( $?KAPPA_DIR == 1 ) then
         if ( -e $KAPPA_DIR ) setenv BENCH_KAPPA YES
      endif
      if ( $?PISA_DIR == 1 ) then
         if ( -e $PISA_DIR ) setenv BENCH_PISA YES
      endif
      if ( $?FIG_DIR == 1 ) then
         if ( -e $FIG_DIR ) setenv BENCH_FIG YES
      endif
      if ( $?CCDPACK_DIR == 1 ) then
         if ( -e $CCDPACK_DIR ) setenv BENCH_CCDPACK YES
      endif
      if ( $?BENCH_KAPPA == 1 && \
           $?BENCH_PISA == 1 && \
           $?BENCH_FIG == 1 && \
           $?BENCH_CCDPACK == 1 ) then
        setenv BENCH_USSC YES
      endif
      if ( $?BENCH_USSC == 1 ) then
        echo " Available"
#
# If the USSC packages are installed then set a version number for the 
# installtion.
#
        set ussc_ver = `tail -1 /star/admin/status`
        setenv BENCH_USSC_VER  $ussc_ver[1] 
      else
        echo " Not available"
      endif
#
# Check whether IRAF is available or not. Just a quick check for presence 
# of the root directory. The iraf environment variable indicates the
# location of the IRAF root directory (which may not always be /iraf/iraf).
#
      echo -n "   IRAF packages: "
      unsetenv BENCH_IRAF
      if ( $?iraf == 0 ) then
        setenv iraf "/iraf/iraf/"
      endif
      if ( -e $iraf ) then
        setenv BENCH_IRAF YES
        echo " Available"
#
# Try to set a version number for the IRAF installation.
#
        if ( -e $iraf/unix/hlib/zzsetenv.def ) then
          set defs = $iraf/unix/hlib/zzsetenv.def
          set iraf_ver = `cat $defs | awk '{if($2=="version")print$4" "$5}'`
          setenv BENCH_IRAF_VER "$iraf_ver"
        endif
      else
        echo " Not available"
      endif
      echo ""
#
# Trap interrupts. Return to command prompt.
#
abort_script:
#
# End of script.
#
end_script:
      exit
