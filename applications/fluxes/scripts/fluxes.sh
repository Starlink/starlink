#!@sh@
#+
#  Name:
#     fluxes.sh
#
#  Purpose:
#     Start the FLUXES program from Unix shell.
#
#  Type of Module:
#     Bourne shell script.
#
#  Invocation:
#     fluxes.sh
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

# Set up the environmental variables.

# Define the location of fluxes and its data files.
# This is edited into this script during installation.
if [ -n "$FLUXES_DIR" ]; then
  if [ ! -d "$FLUXES_DIR" ]; then
    echo FLUXES_DIR environment variable not set to a directory.
    echo Using default fluxes location
    export FLUXES_DIR=@bindir@
  fi
else
  export FLUXES_DIR=@bindir@
fi

# Run the main program.
$FLUXES_DIR/fluxes ${1+"$@"}

# end
#.
