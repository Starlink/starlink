#!@sh@
#+
#  Name:
#     fluxnow.sh
#
#  Purpose:
#     Start the FLUXES system from Unix shell in such
#     a way that the current values are used.
#
#  Type of Module:
#     Bourne shell script.
#
#  Invocation:
#     fluxnow.sh
#
#  Description:
#     This procedure starts the FLUXES system for use from Unix by
#     defining some environmental variables appropriately and then
#     setting the parameters to those that generate the current values.
#
#  Authors:
#     GJP: G.J. Privett (Starlink, Cardiff)
#     ACC: A.C. Charles (Starlink, RAL)
#     BLY: M.J. Bly (Starlink, RAL)
#     TIMJ: Tim Jenness (JACH)
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
$FLUXES_DIR/fluxes pos=y flu=y screen=y ofl=y outfile=fluxes.dat now=y apass=n planet=all

# end
#.
