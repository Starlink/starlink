#!/bin/csh
#+
#  Name:
#     starman.csh
#
#  Purpose:
#     Starts STARMAN from the UNIX shell.
#
#  Type of Module:
#     C-shell script
#
#  Invocation
#     starman
#
#  Description:
#     This procedure starts STARMAN for use from UNIX by defining
#     the aliases needed to execute each application or command.
#     It also initialises ADAM, if this has not already been
#     done.
#
#  Authors:
#     AJP: Alan J. Penny  (RAL)
#     PWM: Patrick W. Morris (Leeds)
#     BLY: M.J.Bly (Starlink, RAL)
#     {enter_new_authors_here}
#     
#  History:
#     1992 June 12 (AJP):
#        Original version.
#     1994 June 15 (AJP):
#        Unix version (Version 2.0) made
#     23-NOV-1995 (BLY):
#        Setups modified for correct Starlink practice.
#     {enter_further_changes_here}
#
#  Bugs:
#     {note_any_bugs_here}
#
#-
#
#  Prepare to run ADAM applications if this has not been done already.
#  ===================================================================
#
#  Here look to see if there is ADAM_USER directory.  If there is not
#  check whether or not there is an adam file that is not a directory.
#  If there is, issue a warning and exit.  Otherwise create the required
#  directory.
#
if (-d ${HOME}/adam) then
   echo -n
else
   if (-f ${HOME}/adam) then
      echo "You have a file called adam in your home directory.  Please rename"
      echo "it since adam must be a directory for ADAM files."
      exit
   else
      mkdir ${HOME}/adam
   endif
endif
#
#  Define environment variables.
#  ============================
#
setenv STARMAN_DIR             INSTALL_BIN
setenv STARMAN_HELP            INSTALL_HELP
setenv STARMAN_HYPER           INSTALL_HYPER
setenv STARMAN_DATA            INSTALL_DATA

setenv STARMAN_HELP_A          ${STARMAN_HELP}/starman_help_a
setenv STARMAN_HELP_M          ${STARMAN_HELP}/starman_help_m
 
#  Check that path for s_refresh is there
#  ======================================

set foundit = "no"
if (`echo ${PATH} | grep -c ${STARMAN_DIR}` != 0) set foundit = "yes"
if ( $foundit == "no" ) setenv PATH ${PATH}:${STARMAN_DIR}
unset foundit

#  Define program names
#  ====================

alias addstars       ${STARMAN_DIR}/addstars
alias automag        ${STARMAN_DIR}/automag
alias average        ${STARMAN_DIR}/average
alias chi            ${STARMAN_DIR}/chi
alias diagram        ${STARMAN_DIR}/diagram
alias dustring       ${STARMAN_DIR}/dustring
alias imcalc         ${STARMAN_DIR}/imcalc
alias imcube         ${STARMAN_DIR}/imcube
alias imcut          ${STARMAN_DIR}/imcut
alias imdes          ${STARMAN_DIR}/imdes
alias imfits_dr      ${STARMAN_DIR}/imfits_dr
alias imflash        ${STARMAN_DIR}/imflash
alias imjoin         ${STARMAN_DIR}/imjoin
alias imkey          ${STARMAN_DIR}/imkey
alias import         ${STARMAN_DIR}/import
alias imrotate       ${STARMAN_DIR}/imrotate
alias imsmooth       ${STARMAN_DIR}/imsmooth
alias imstat         ${STARMAN_DIR}/imstat
alias imtype         ${STARMAN_DIR}/imtype
alias imweed         ${STARMAN_DIR}/imweed
alias interact       ${STARMAN_DIR}/interact
alias measure        ${STARMAN_DIR}/measure
alias profile        ${STARMAN_DIR}/profile
alias simplemag      ${STARMAN_DIR}/simplemag
alias sprinkle       ${STARMAN_DIR}/sprinkle
alias starfind       ${STARMAN_DIR}/starfind
alias tbplot         ${STARMAN_DIR}/tbplot
alias tbcalc         ${STARMAN_DIR}/tbcalc
alias tbchart        ${STARMAN_DIR}/tbchart
alias tbcomps        ${STARMAN_DIR}/tbcomps
alias tbcut          ${STARMAN_DIR}/tbcut
alias tbdes          ${STARMAN_DIR}/tbdes
alias tbjoin         ${STARMAN_DIR}/tbjoin
alias tbkey          ${STARMAN_DIR}/tbkey
alias tblist         ${STARMAN_DIR}/tblist
alias tbload         ${STARMAN_DIR}/tbload
alias tbmatch        ${STARMAN_DIR}/tbmatch
alias tbnative       ${STARMAN_DIR}/tbnative
alias tbnmatch       ${STARMAN_DIR}/tbnmatch
alias tbplot         ${STARMAN_DIR}/tbplot
alias tbpmatch       ${STARMAN_DIR}/tbpmatch
alias tbrenum        ${STARMAN_DIR}/tbrenum
alias tbsheet        ${STARMAN_DIR}/tbsheet
alias tbsort         ${STARMAN_DIR}/tbsort
alias tbstat         ${STARMAN_DIR}/tbstat
alias tbtran_auto    ${STARMAN_DIR}/tbtran_auto
alias tbtran_do      ${STARMAN_DIR}/tbtran_do
alias tbtran_load    ${STARMAN_DIR}/tbtran_load
alias tbtran_make    ${STARMAN_DIR}/tbtran_make
alias tbvalue        ${STARMAN_DIR}/tbvalue
alias tbweed         ${STARMAN_DIR}/tbweed
alias unccd          ${STARMAN_DIR}/unccd

alias starmanhelp       ${STARMAN_DIR}/starmanhelp
alias starmangripe      ${STARMAN_DIR}/starmangripe
alias starmanhyperhelp  source ${STARMAN_DIR}/starmanhyperhelp
alias starmanhypergripe source ${STARMAN_DIR}/starmanhypergripe

alias starmandemo     source ${STARMAN_DIR}/starmandemo

#
# Duplicate aliases
#
alias stm_addstars       ${STARMAN_DIR}/addstars
alias stm_automag        ${STARMAN_DIR}/automag
alias stm_average        ${STARMAN_DIR}/average
alias stm_chi            ${STARMAN_DIR}/chi
alias stm_diagram        ${STARMAN_DIR}/diagram
alias stm_dustring       ${STARMAN_DIR}/dustring
alias stm_imcalc         ${STARMAN_DIR}/imcalc
alias stm_imcube         ${STARMAN_DIR}/imcube
alias stm_imcut          ${STARMAN_DIR}/imcut
alias stm_imdes          ${STARMAN_DIR}/imdes
alias stm_imfits_dr      ${STARMAN_DIR}/imfits_dr
alias stm_imflash        ${STARMAN_DIR}/imflash
alias stm_imjoin         ${STARMAN_DIR}/imjoin
alias stm_imkey          ${STARMAN_DIR}/imkey
alias stm_import         ${STARMAN_DIR}/import
alias stm_imrotate       ${STARMAN_DIR}/imrotate
alias stm_imsmooth       ${STARMAN_DIR}/imsmooth
alias stm_imstat         ${STARMAN_DIR}/imstat
alias stm_imtype         ${STARMAN_DIR}/imtype
alias stm_imweed         ${STARMAN_DIR}/imweed
alias stm_interact       ${STARMAN_DIR}/interact
alias stm_measure        ${STARMAN_DIR}/measure
alias stm_profile        ${STARMAN_DIR}/profile
alias stm_simplemag      ${STARMAN_DIR}/simplemag
alias stm_sprinkle       ${STARMAN_DIR}/sprinkle
alias stm_starfind       ${STARMAN_DIR}/starfind
alias stm_tbplot         ${STARMAN_DIR}/tbplot
alias stm_tbcalc         ${STARMAN_DIR}/tbcalc
alias stm_tbchart        ${STARMAN_DIR}/tbchart
alias stm_tbcomps        ${STARMAN_DIR}/tbcomps
alias stm_tbcut          ${STARMAN_DIR}/tbcut
alias stm_tbdes          ${STARMAN_DIR}/tbdes
alias stm_tbjoin         ${STARMAN_DIR}/tbjoin
alias stm_tbkey          ${STARMAN_DIR}/tbkey
alias stm_tblist         ${STARMAN_DIR}/tblist
alias stm_tbload         ${STARMAN_DIR}/tbload
alias stm_tbmatch        ${STARMAN_DIR}/tbmatch
alias stm_tbnative       ${STARMAN_DIR}/tbnative
alias stm_tbnmatch       ${STARMAN_DIR}/tbnmatch
alias stm_tbplot         ${STARMAN_DIR}/tbplot
alias stm_tbpmatch       ${STARMAN_DIR}/tbpmatch
alias stm_tbrenum        ${STARMAN_DIR}/tbrenum
alias stm_tbsheet        ${STARMAN_DIR}/tbsheet
alias stm_tbsort         ${STARMAN_DIR}/tbsort
alias stm_tbstat         ${STARMAN_DIR}/tbstat
alias stm_tbtran_auto    ${STARMAN_DIR}/tbtran_auto
alias stm_tbtran_do      ${STARMAN_DIR}/tbtran_do
alias stm_tbtran_load    ${STARMAN_DIR}/tbtran_load
alias stm_tbtran_make    ${STARMAN_DIR}/tbtran_make
alias stm_tbvalue        ${STARMAN_DIR}/tbvalue
alias stm_tbweed         ${STARMAN_DIR}/tbweed
alias stm_unccd          ${STARMAN_DIR}/unccd

#  Put out start notice

echo ' '
echo '   ____________________________________________________________________ '
echo '  |  Starman - Stellar Photometry               Version PKG_VERS  Dec 95  |'
echo '  |            Image and Table Handling                                |'
echo '  |                                                                    |'
echo '  |1) Help:                         starmanhyperhelp / starmanhelp     |'
echo '  |2) Advice/Bug Reports:           starmanhypergripe / starmangripe   |'
echo '  |                                   - or - mail alan.penny@rl.ac.uk  |'
echo '  |3) Prompts in programs:          ? or ?? for help : \!\! to exit      |'
echo '  |4) Default data for a prompt:    $STARMAN_DATA/progam_parameter     |'
echo '  |                                    e.g. $STARMAN_DATA/measure_in   |'
echo '  |5) Demonstration:                starmandemo                        |'
echo '  |                                                                    |'
echo '  |All rights reserved. Starlink copyright and disclaimer notices apply|'
echo '   --------------------------------------------------------------------'
echo ' '

#  Exit the procedure.

