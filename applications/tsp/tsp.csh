#!/bin/csh
#+          
#  Name:
#     tsp.csh

#  Purpose:
#     Set up aliases for the TSP package.

#  Type of Module:
#     C shell script.

#  Invocation:
#     source tsp.csh

#  Description:
#     This procedure defines an alias for each TSP command. The string
#     INSTALL_BIN is replaced by the path of the directory containing the 
#     package executable files when the package is installed. The string
#     INSTALL_HELP is likewise replaced by the path to the directory containing 
#     help libraries.

#  Authors:
#     JAB: Jeremy Bailey (AAO)
#     BLY: M.J. Bly (Starlink, RAL)
#     {enter_new_authors_here}

#  History:
#     21-Feb-1994 (JAB):
#        Original Version
#     23-JUN-1995 (BLY):
#        New version using template.
#     23-AUG-1997 (BLY):
#        Added missing IRISPOLC commands.
#        Revised TSP_SOURCE and FIGARO_SOURCE directories.
#     {enter_changes_here}

#-

#  Prepare to run ADAM applications if this has not been done already.
#  ===================================================================
#
#  Here look to see if there is an ADAM_USER directory.  If there is not
#  check whether or not there is an adam file that is not a directory.
#  If there is, issue a warning and exit.  Otherwise create the required
#  directory.

if (-d ${HOME}/adam) then
   echo -n
else
   if (-f ${HOME}/adam) then
      echo "You have a file called adam in your home directory.  Please rename "
      echo "since adam must be a directory for ADAM files."
      exit
   else
      mkdir ${HOME}/adam
   endif
endif

#
#  Define the *_DIR environment variable if not already defined.
#  ============================================================

if !( ${?TSP_DIR} ) then
   setenv TSP_DIR INSTALL_BIN
endif

#
#  Define the *_HELP environment variable if not already defined.
#  =============================================================

#if !( ${?TSP_HELP} ) then
#   setenv TSP_HELP INSTALL_HELP
#endif

#
#  Define the *_SOURCE environment variable if not already defined.
#  ===============================================================
#  If the environment variable TSP_SOURCE does not exist, use a value
#  of /star/sources/tsp (edited in by makefile during installation).
 
if !( ${?TSP_SOURCE} ) then
   setenv TSP_SOURCE STAR_SOURCES/tsp
endif
 
# If the source directory does not exist, warn user.
 
if !( -d ${TSP_SOURCE} ) then
   echo "*** The TSP source directory ${TSP_SOURCE} does not exist"
endif

#
#  Define the FIGARO *_SOURCE environment variable if not already defined.
#  =================+++++++==============================================
#  If the environment variable FIGARO_SOURCE does not exist, use a value
#  of /star/sources/figaro
 
if !( ${?FIGARO_SOURCE} ) then
   setenv FIGARO_SOURCE STAR_SOURCES/figaro
endif
 
# If the source directory does not exist, warn user.
 
if !( -d ${FIGARO_SOURCE} ) then
   echo "*** The FIGARO source directory ${FIGARO_SOURCE} does not exist"
endif

#
#  Define symbols for the applications and scripts.
#  ===============================================

#
#  Define aliases for each application.
#
      alias build3d ${TSP_DIR}/build3d
      alias tsp_build3d ${TSP_DIR}/build3d
#
      alias calfit ${TSP_DIR}/calfit
      alias tsp_calfit ${TSP_DIR}/calfit
#
      alias calfitpa ${TSP_DIR}/calfitpa
      alias tsp_calfitpa ${TSP_DIR}/calfitpa
#
      alias calib ${TSP_DIR}/calib
      alias tsp_calib ${TSP_DIR}/calib
#
      alias calpa ${TSP_DIR}/calpa
      alias tsp_calpa ${TSP_DIR}/calpa
#
      alias ccd2pol ${TSP_DIR}/ccd2pol
      alias tsp_ccd2pol ${TSP_DIR}/ccd2pol
#
      alias ccd2stokes ${TSP_DIR}/ccd2stokes
      alias tsp_ccd2stokes ${TSP_DIR}/ccd2stokes
#
      alias ccdphot ${TSP_DIR}/ccdphot
      alias tsp_ccdphot ${TSP_DIR}/ccdphot
#
      alias ccdpol ${TSP_DIR}/ccdpol
      alias tsp_ccdpol ${TSP_DIR}/ccdpol
#
      alias cgs4pol ${TSP_DIR}/cgs4pol
      alias tsp_cgs4pol ${TSP_DIR}/cgs4pol
#
      alias cmult ${TSP_DIR}/cmult
      alias tsp_cmult ${TSP_DIR}/cmult
#
      alias combine ${TSP_DIR}/combine
      alias tsp_combine ${TSP_DIR}/combine
#
      alias display ${TSP_DIR}/display
      alias tsp_display ${TSP_DIR}/display
#
      alias divide ${TSP_DIR}/divide
      alias tsp_divide ${TSP_DIR}/divide
#
      alias dstokes ${TSP_DIR}/dstokes
      alias tsp_dtokes ${TSP_DIR}/dstokes
#
      alias eplot ${TSP_DIR}/eplot
      alias tsp_eplot ${TSP_DIR}/eplot
#
      alias extin ${TSP_DIR}/extin
      alias tsp_extin ${TSP_DIR}/extin
#
      alias floconv ${TSP_DIR}/flconv
      alias tsp_flconv ${TSP_DIR}/flconv
#
      alias flip ${TSP_DIR}/flip
      alias tsp_flip ${TSP_DIR}/flip
#
      alias fplot ${TSP_DIR}/fplot
      alias tsp_fplot ${TSP_DIR}/fplot
#
      alias imotion ${TSP_DIR}/imotion
      alias tsp_imotion ${TSP_DIR}/imotion
#
      alias ipcs2stokes ${TSP_DIR}/ipcs2stokes
      alias tsp_ipcs2stokes ${TSP_DIR}/ipcs2stokes
#
      alias irflux ${TSP_DIR}/irflux
      alias tsp_irflux ${TSP_DIR}/irflux
#
      alias irisap ${TSP_DIR}/irisap
      alias tsp_irisap ${TSP_DIR}/irisap
#
      alias irisapc ${TSP_DIR}/irisapc
      alias tsp_irisapc ${TSP_DIR}/irisapc
#
      alias irispol ${TSP_DIR}/irispol
      alias tsp_irispol ${TSP_DIR}/irispol
#
      alias irispolc ${TSP_DIR}/irispolc
      alias tsp_irispolc ${TSP_DIR}/irispolc
#
      alias lhatpol ${TSP_DIR}/lhatpol
      alias tsp_lhatpol ${TSP_DIR}/lhatpol
#
      alias lmerge ${TSP_DIR}/lmerge
      alias tsp_lmerge ${TSP_DIR}/lmerge
#
      alias ltcorr ${TSP_DIR}/ltcorr
      alias tsp_ltcorr ${TSP_DIR}/ltcorr
#
      alias phaseplot ${TSP_DIR}/phaseplot
      alias tsp_phaseplot ${TSP_DIR}/phaseplot
#
      alias pplot ${TSP_DIR}/pplot
      alias tsp_pplot ${TSP_DIR}/pplot
#
      alias ptheta ${TSP_DIR}/ptheta
      alias tsp_ptheta ${TSP_DIR}/ptheta
#
      alias qplot ${TSP_DIR}/qplot
      alias tsp_qplot ${TSP_DIR}/qplot
#
      alias qumerge ${TSP_DIR}/qumerge
      alias tsp_qumerge ${TSP_DIR}/qumerge
#
      alias quplot ${TSP_DIR}/quplot
      alias tsp_quplot ${TSP_DIR}/quplot
#
      alias qusub ${TSP_DIR}/qusub
      alias tsp_qusub ${TSP_DIR}/qusub
#
      alias rccdts ${TSP_DIR}/rccdts
      alias tsp_rccdts ${TSP_DIR}/rccdts
#
      alias rcgs2 ${TSP_DIR}/rcgs2
      alias tsp_rcgs2 ${TSP_DIR}/rcgs2
#
      alias reverse ${TSP_DIR}/reverse
      alias tsp_reverse ${TSP_DIR}/reverse
#
      alias rfigaro ${TSP_DIR}/rfigaro
      alias tsp_rfigaro ${TSP_DIR}/rfigaro
#
      alias rhathsp ${TSP_DIR}/rhathsp
      alias tsp_rhathsp ${TSP_DIR}/rhathsp
#
      alias rhatpol ${TSP_DIR}/rhatpol
      alias tsp_rhatpol ${TSP_DIR}/rhatpol
#
      alias rhdsplot ${TSP_DIR}/rhdsplot
      alias tsp_rhdsplot ${TSP_DIR}/rhdsplot
#
      alias rhsp3 ${TSP_DIR}/rhsp3
      alias tsp_rhsp3 ${TSP_DIR}/rhsp3
#
      alias rirps ${TSP_DIR}/rirps
      alias tsp_rirps ${TSP_DIR}/rirps
#
      alias rotpa ${TSP_DIR}/rotpa
      alias tsp_rotpa ${TSP_DIR}/rotpa
#
      alias rturku ${TSP_DIR}/rturku
      alias tsp_rturku ${TSP_DIR}/rturku
#
      alias scrunch ${TSP_DIR}/scrunch
      alias tsp_scrunch ${TSP_DIR}/scrunch
#
      alias shiftadd ${TSP_DIR}/shiftadd
      alias tsp_shiftadd ${TSP_DIR}/shiftadd
#
      alias skysub ${TSP_DIR}/skysub
      alias tsp_skysub ${TSP_DIR}/skysub
#
      alias slist ${TSP_DIR}/slist
      alias tsp_slist ${TSP_DIR}/slist
#
      alias spflux ${TSP_DIR}/spflux
      alias tsp_spflux ${TSP_DIR}/spflux
#
      alias splot ${TSP_DIR}/splot
      alias tsp_splot ${TSP_DIR}/splot
#
      alias subset ${TSP_DIR}/subset
      alias tsp_subset ${TSP_DIR}/subset
#
      alias subtract ${TSP_DIR}/subtract
      alias tsp_subtract ${TSP_DIR}/subtract
#
      alias tbin ${TSP_DIR}/tbin
      alias tsp_tbin ${TSP_DIR}/tbin
#
      alias tcadd ${TSP_DIR}/tcadd
      alias tsp_tcadd ${TSP_DIR}/tcadd
#
      alias tderiv ${TSP_DIR}/tderiv
      alias tsp_tderiv ${TSP_DIR}/tderiv
#
      alias textin ${TSP_DIR}/textin
      alias tsp_textin ${TSP_DIR}/textin
#
      alias tlist ${TSP_DIR}/tlist
      alias tsp_tlist ${TSP_DIR}/tlist
#
      alias tmerge ${TSP_DIR}/tmerge
      alias tsp_tmerge ${TSP_DIR}/tmerge
#
      alias tsetbad ${TSP_DIR}/tsetbad
      alias tsp_tsetbad ${TSP_DIR}/tsetbad
#
      alias tsextract ${TSP_DIR}/tsextract
      alias tsp_tsextract ${TSP_DIR}/tsextract
#
      alias tshift ${TSP_DIR}/tshift
      alias tsp_tshift ${TSP_DIR}/tshift
#
      alias tsplot ${TSP_DIR}/tsplot
      alias tsp_tsplot ${TSP_DIR}/tsplot
#
      alias tsprofile ${TSP_DIR}/tsprofile
      alias tsp_tsprofile ${TSP_DIR}/tsprofile
#
      alias xcopy ${TSP_DIR}/xcopy
      alias tsp_xcopy ${TSP_DIR}/xcopy
#

#
# Test script
      alias tsp_test ${TSP_DIR}/tsp_test

#
#  Tell the user that TSP commands are now available.

echo ""
echo "   TSP commands are now available -- (Version PKG_VERS)"
echo ""

#
# end
