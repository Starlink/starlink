#!/bin/csh
#+
#  Name:
#     fluxnow.csh
#
#  Purpose:
#     Start the FLUXES system from Unix shell in such
#     a way that the current values are used.
#
#  Type of Module:
#     C shell script.
#
#  Invocation:
#     fluxnow.csh
#
#  Description:
#     This procedure starts the FLUXES system for use from Unix by 
#     defining some environmental variables appropriately and then 
#     setting the parameters to those that generate the current values.
#
#  Notes:
#     The file JPLEPH is *required* as name for ephemeris file.
#
#  Authors:
#     GJP: G.J. Privett (Starlink, Cardiff)
#     ACC: A.C. Charles (Starlink, RAL)
#     BLY: M.J. Bly (Starlink, RAL)
#
#  History:
#     13-DEC-1996 (GJP):
#       Original Version
#     03-FEB-1997 (ACC/BLY):
#       Changed definition of FLUXES to INSTALL_BIN.
#       Modified startup to use link to JPL ephemeris instead of
#          requiring to be run from ephemeris directory.
#     07-FEB-1997 (BLY):
#       Modified setting of FLUXPWD since $PWD not guranteed on Dec-Unix.
#-

# Flag an interrupt for bugging out.
onintr goto quit

# Set up the environmental variables.
 
# Define the location of fluxes and its data files.
# This is edited into this script during installation.
setenv FLUXES INSTALL_BIN
 
# Define a link to the jpleph.dat file in the current directory.
if ( -f JPLEPH ) then
   echo \
"File JPLEPH already exists - assuming it is, or points to, the JPL ephemeris."
   set jpl_ok = "no"
else
   ln -s JPL_DATA/jpleph.dat JPLEPH
   set jpl_ok = "yes"
endif

# Find out working directory and set FLUXPWD to it, so
# that the code works in JACH style without change.
unalias pwd
setenv FLUXPWD `pwd`

# Run the main program.
$FLUXES/fluxes pos=y flu=y screen=y ofl=y outfile=fluxes.dat now=y apass=n planet=all

# Exit
quit:
if ( ${jpl_ok} == "yes" ) then
   rm -f JPLEPH
endif

exit

# end
#.
