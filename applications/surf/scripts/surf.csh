#!/bin/csh -f
#+
#  Name:
#     surf.csh
#
#  Purpose:
#     Start the SURF package from the Unix shell
#
#  Type of Module:
#     C shell script.
#
#  Invocation:
#     source surf.csh
#
#  Description:
#     This procedure defines the aliases needed to run 
#     each application monolith.
#
#  Notes:
#     The version should be set outside of this script, and edited at
#     install time to indicate the version number.
#
#  Authors:
#     BLY: M.J.Bly (Starlink, RAL)
#     TIMJ: T. Jenness (JACH)
#     {enter_new_authors_here}
#
#  History:
#     13-OCT-1994 (BLY);
#       Original for PHOTOM
#     06-JUN-1997 (TIMJ);
#       Converted for SURF
#     {enter_changes_here}
#
#-
#


#
#  Define aliases for the applications.

alias change_data     ${SURF_DIR}/change_data
alias change_flat     ${SURF_DIR}/change_flat
alias change_pointing ${SURF_DIR}/change_pointing
alias change_quality  ${SURF_DIR}/change_quality
alias extinction      ${SURF_DIR}/extinction
alias flatfield       ${SURF_DIR}/flatfield
alias rebin           ${SURF_DIR}/rebin
alias reduce_switch   ${SURF_DIR}/reduce_switch
alias remsky          ${SURF_DIR}/remsky
alias restore         ${SURF_DIR}/restore
alias scucat          ${SURF_DIR}/scucat
alias scuhelp         ${SURF_DIR}/scuhelp
alias scuphot         ${SURF_DIR}/scuphot
alias scuover         ${SURF_DIR}/scuover
alias skydip          ${SURF_DIR}/skydip

#
# These are the aliases for the scripts
# May want to include a check for the existence of the NDF module
# or ndfperl

alias sculog        ${SURF_DIR}/sculog
alias scuquick      ${SURF_DIR}/scuquick
alias photsum       ${SURF_DIR}/photsum
alias pointsum      ${SURF_DIR}/pointsum
alias mapsum        ${SURF_DIR}/mapsum
alias obssum        "${SURF_DIR}/sculog -summary"

alias qdraw         ${SURF_DIR}/qdraw
alias sigclip       ${SURF_DIR}/sigclip 

# A csh script
alias scupa       ${SURF_DIR}/scupa.csh
alias sdip        ${SURF_DIR}/sdip.csh


# These are really kappa commands
alias kstest        ${SURF_DIR}/kstest
alias drawsig       ${SURF_DIR}/drawsig

# Announce the availability of the SURF commands

echo " "
echo "   SURF - SCUBA User Reduction Facility"
echo "     Commands are now available -- (Version PKG_VERS)"
echo " "
echo "     Type scuhelp for help on SURF commands."
#echo '     Type "showme sun217" to browse the hypertext documentation.'
echo " "

#
# end
#



