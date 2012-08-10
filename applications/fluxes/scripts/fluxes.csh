#!@csh@
#+
#  Name:
#     fluxes.csh
#
#  Purpose:
#     Start the FLUXES program from Unix shell.
#
#  Type of Module:
#     C shell script.
#
#  Invocation:
#     fluxes.csh
#
#  Description:
#     This procedure starts the FLUXES system for use from Unix by
#     defining some environmental variables appropriately.
#
#  Authors:
#     GJP: G.J. Privett (Starlink, Cardiff)
#     ACC: A.C. Charles (Starlink, RAL)
#     BLY: M.J. Bly (Starlink, RAL)
#     RPT: R.P. Tilanus (JACH)
#     TIMJ: Tim Jenness (JACH)
#
#  History:
#     13-DEC-1996 (GJP):
#       Original Version.
#     03-FEB-1997 (ACC/BLY):
#       Changed definition of FLUXES to INSTALL_BIN.
#       Modified startup to use link to JPL ephemeris instead of
#          requiring to be run from ephemeris directory.
#     07-FEB-1997 (BLY):
#       Modified setting of FLUXPWD since $PWD not guranteed on Dec-Unix.
#     04-SEP-1998 (RPT)
#       Forward arguments to fluxes executable and print fewer messages
#       if the arglist contains screen=n.
#     14-JUL-2004 (TIMJ):
#       Uses @bindir@ rather than INSTALL_BIN
#       Uses FLUXES_DIR and JPL_DIR environment variable if set
#     2012-08-10 (TIMJ):
#       Remove JPL logic.
#-

# Flag an interrupt for bugging out.
onintr goto quit

# No screen output?
set silent = 0
if (`echo "$argv" | fgrep -c 'screen=n'` != 0) then
  set silent = 1
endif

# Set up the environmental variables.

# Define the location of fluxes and its data files.
# This is edited into this script during installation.
if ($?FLUXES_DIR) then
  if (-d $FLUXES_DIR) then
    # FLUXES_DIR is fine
  else
    echo FLUXES_DIR environment variable not set to a directory.
    echo Using default fluxes location
    setenv FLUXES_DIR @bindir@
  endif
else
  setenv FLUXES_DIR @bindir@
endif

# Run the main program.
$FLUXES_DIR/fluxes $argv

# Exit
quit:
exit

# end
#.
