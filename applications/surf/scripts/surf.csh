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

#  Copyright:
#     Copyright (C) 1997,1998,1999 Particle Physics and Astronomy
#     Research Council. All Rights Reserved.

#
#  History:
#     13-OCT-1994 (BLY);
#        Original for PHOTOM
#     06-JUN-1997 (TIMJ);
#        Converted for SURF
#     01-JUL-1997
#        Add SKYSUM alias
#     01-AUG-1997 (BLY):
#        Added aliases: bolrebin,intrebin,extract_data
#        Added `surf_' variants for aliases so that SURF commands are
#           always available if other package command names clash.
#        Added test for NDFPERL when defining aliases for commands which
#           need it and produce polite message if not available.
#     12-NOV-1997 (TIMJ):
#        Add new commands for release v1.1
#          extract_flat, scuclip, despike, despike2, scan_rlb, scushift
#     30-NOV-1997 (TIMJ):
#        Add some more commands for release 1.1: pltbol, rlinplot,
#        scuplot and dspbol
#     26-JAN-1997 (TIMJ)
#        Add SCUBA2MEM
#     10-Jun-1998 (TIMJ)
#        Add CALCSKY, REMDBM and CHANGE_NACENTRE. SCUNOISE and SETBOLWT
#     20-Nov-1998 (TIMJ)
#        Add REDUCE_NOISE
#     January 1999 (TIMJ)
#        Add ADD_DBM and SCUSETENV
#     08-Mar-1999 (TIMJ)
#        Add REMIP. Use the STAR_PERL symbol for perl location
#     {enter_changes_here}
#
#-
#

#
#  Define aliases for the applications.

alias add_dbm         ${SURF_DIR}/add_dbm
alias bolrebin        ${SURF_DIR}/bolrebin
alias calcsky         ${SURF_DIR}/calcsky
alias change_data     ${SURF_DIR}/change_data
alias change_flat     ${SURF_DIR}/change_flat
alias change_pointing ${SURF_DIR}/change_pointing
alias change_quality  ${SURF_DIR}/change_quality
alias despike         ${SURF_DIR}/despike
alias despike2        ${SURF_DIR}/despike2
alias extinction      ${SURF_DIR}/extinction
alias extract_data    ${SURF_DIR}/extract_data
alias extract_flat    ${SURF_DIR}/extract_flat
alias flatfield       ${SURF_DIR}/flatfield
alias intrebin        ${SURF_DIR}/intrebin
alias rebin           ${SURF_DIR}/rebin
alias reduce_noise    ${SURF_DIR}/reduce_noise
alias reduce_switch   ${SURF_DIR}/reduce_switch
alias remip           ${SURF_DIR}/remip
alias remsky          ${SURF_DIR}/remsky
alias restore         ${SURF_DIR}/restore
alias scan_rlb        ${SURF_DIR}/scan_rlb
alias scuba2mem       ${SURF_DIR}/scuba2mem
alias scucat          ${SURF_DIR}/scucat
alias scuclip         ${SURF_DIR}/scuclip
alias scuhelp         ${SURF_DIR}/scuhelp
alias scuphot         ${SURF_DIR}/scuphot
alias scuover         ${SURF_DIR}/scuover
alias skydip          ${SURF_DIR}/skydip

alias surf_add_dbm         ${SURF_DIR}/add_dbm
alias surf_bolrebin        ${SURF_DIR}/bolrebin
alias surf_calcsky         ${SURF_DIR}/calcsky
alias surf_change_data     ${SURF_DIR}/change_data
alias surf_change_flat     ${SURF_DIR}/change_flat
alias surf_change_pointing ${SURF_DIR}/change_pointing
alias surf_change_quality  ${SURF_DIR}/change_quality
alias surf_despike         ${SURF_DIR}/despike
alias surf_despike2        ${SURF_DIR}/despike2
alias surf_extinction      ${SURF_DIR}/extinction
alias surf_extract_data    ${SURF_DIR}/extract_data
alias surf_extract_flat    ${SURF_DIR}/extract_flat
alias surf_flatfield       ${SURF_DIR}/flatfield
alias surf_intrebin        ${SURF_DIR}/intrebin
alias surf_rebin           ${SURF_DIR}/rebin
alias surf_reduce_noise    ${SURF_DIR}/reduce_noise
alias surf_reduce_switch   ${SURF_DIR}/reduce_switch
alias surf_remip           ${SURF_DIR}/remip
alias surf_remsky          ${SURF_DIR}/remsky
alias surf_restore         ${SURF_DIR}/restore
alias surf_scan_rlb        ${SURF_DIR}/scan_rlb
alias surf_scuba2mem       ${SURF_DIR}/scuba2mem
alias surf_scucat          ${SURF_DIR}/scucat
alias surf_scuclip         ${SURF_DIR}/scuclip
alias surf_scuhelp         ${SURF_DIR}/scuhelp
alias surf_scuphot         ${SURF_DIR}/scuphot
alias surf_scuover         ${SURF_DIR}/scuover
alias surf_skydip          ${SURF_DIR}/skydip

#
# These are the aliases for the PERL scripts.
# Need to use the Starlink perl distribution in /star/Perl/bin

# Check for perl module.
if ( -e STAR_PERL ) then
   set myperl = STAR_PERL
   alias change_nacentre  "$myperl ${SURF_DIR}/change_nacentre.pl"
   alias sculog           "$myperl ${SURF_DIR}/sculog"
   alias scuquick         "$myperl ${SURF_DIR}/scuquick"
   alias photsum          "$myperl ${SURF_DIR}/photsum"
   alias pointsum         "$myperl ${SURF_DIR}/pointsum"
   alias mapsum           "$myperl ${SURF_DIR}/mapsum"
   alias remdbm           "$myperl ${SURF_DIR}/remdbm.pl"
   alias scushift         "$myperl -s ${SURF_DIR}/scushift"
   alias scunoise         "$myperl -s ${SURF_DIR}/scunoise"
   alias setbolwt         "$myperl ${SURF_DIR}/setbolwt.pl"
   alias skysum           "$myperl ${SURF_DIR}/skysum"
   alias obssum           "$myperl ${SURF_DIR}/sculog -summary"
   alias qdraw            "$myperl -s ${SURF_DIR}/qdraw"
   alias sigclip          "$myperl -s ${SURF_DIR}/sigclip" 
   alias surf_change_nacentre "$myperl ${SURF_DIR}/change_nacentre.pl"
   alias surf_sculog      "$myperl ${SURF_DIR}/sculog"
   alias surf_scuquick    "$myperl ${SURF_DIR}/scuquick"
   alias surf_photsum     "$myperl ${SURF_DIR}/photsum"
   alias surf_pointsum    "$myperl ${SURF_DIR}/pointsum"
   alias surf_mapsum      "$myperl ${SURF_DIR}/mapsum"
   alias surf_remdbm      "$myperl ${SURF_DIR}/remdbm.pl"
   alias surf_scunoise    "$myperl -s ${SURF_DIR}/scunoise"
   alias surf_scushift    "$myperl -s ${SURF_DIR}/scushift"
   alias surf_setbolwt    "$myperl ${SURF_DIR}/setbolwt.pl"
   alias surf_skysum      "$myperl ${SURF_DIR}/skysum"
   alias surf_obssum      "$myperl ${SURF_DIR}/sculog -summary"
   alias surf_qdraw       "$myperl -s ${SURF_DIR}/qdraw"
   alias surf_sigclip     "$myperl -s ${SURF_DIR}/sigclip" 
else
   alias change_nacentre  echo 'Command not available - needs Starlink PERL'
   alias sculog         echo 'Command not available - needs Starlink PERL'
   alias scuquick       echo 'Command not available - needs Starlink PERL'
   alias photsum        echo 'Command not available - needs Starlink PERL'
   alias pointsum       echo 'Command not available - needs Starlink PERL'
   alias mapsum         echo 'Command not available - needs Starlink PERL'
   alias remdbm         echo 'Command not available - needs Starlink PERL'
   alias scushift       echo 'Command not available - needs Starlink PERL'
   alias setbolwt       echo 'Command not available - needs Starlink PERL'
   alias skysum         echo 'Command not available - needs Starlink PERL'
   alias obssum         echo 'Command not available - needs Starlink PERL'
   alias qdraw          echo 'Command not available - needs Starlink PERL'
   alias sigclip        echo 'Command not available - needs Starlink PERL'
   alias surf_change_nacentre  echo 'Command not available - needs Starlink PERL'
   alias surf_sculog    echo 'Command not available - needs Starlink PERL' 
   alias surf_scuquick  echo 'Command not available - needs Starlink PERL'
   alias surf_photsum   echo 'Command not available - needs Starlink PERL'
   alias surf_pointsum  echo 'Command not available - needs Starlink PERL'
   alias surf_mapsum    echo 'Command not available - needs Starlink PERL'
   alias surf_remdbm    echo 'Command not available - needs Starlink PERL'
   alias surf_scushift  echo 'Command not available - needs Starlink PERL'
   alias surf_setbolwt  echo 'Command not available - needs Starlink PERL'
   alias surf_skysum    echo 'Command not available - needs Starlink PERL'
   alias surf_obssum    echo 'Command not available - needs Starlink PERL'
   alias surf_qdraw     echo 'Command not available - needs Starlink PERL'
   alias surf_sigclip   echo 'Command not available - needs Starlink PERL'
endif

# csh scripts
alias scupa       ${SURF_DIR}/scupa.csh
alias sdip        ${SURF_DIR}/sdip.csh
alias dspbol      ${SURF_DIR}/dspbol
alias pltbol      ${SURF_DIR}/pltbol
alias rlinplot    ${SURF_DIR}/rlinplot
alias scuplot     ${SURF_DIR}/scuplot.csh
alias scusetenv   'source ${SURF_DIR}/scusetenv.csh'

alias surf_scupa       ${SURF_DIR}/scupa.csh
alias surf_sdip        ${SURF_DIR}/sdip.csh
alias surf_dspbol      ${SURF_DIR}/dspbol
alias surf_pltbol      ${SURF_DIR}/pltbol
alias surf_rlinplot    ${SURF_DIR}/rlinplot
alias surf_scuplot     ${SURF_DIR}/scuplot.csh
alias surf_scusetenv   'source ${SURF_DIR}/scusetenv.csh'

# Announce the availability of the SURF commands

echo " "
echo "     SURF - SCUBA User Reduction Facility"
echo "     Commands are now available -- (Version PKG_VERS)"
echo " "
echo "     Type scuhelp for help on SURF commands."
echo '     Type "showme sun216" to browse the hypertext documentation.'
echo " "

#
# end
#.
