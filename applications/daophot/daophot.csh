#!/bin/csh -f
#+
#  Name:
#     daophot_init
#
#  Purpose:
#     Start the DAOPHOT system from Unix shell.
#
#  Type of Module:
#     C shell script.
#
#  Invocation:
#     source daophot_init
#
#  Description:
#     This procedure defines the aliases needed to run 
#     each application monolith or executable.
#
#  Notes:
#     The installation target INSTALL_BIN is set outside of this script,
#     and edited at install time to reflect the installation location.
#
#     The target PKG_VERS is set outside of this script, and edited at
#     install time to indicate the version number.
#
#  Authors:
#     BLY: M.J.Bly (Starlink, RAL)
#     {enter_new_authors_here}
#
#  History:
#     13-OCT-1994 (BLY);
#       Original for DAOPHOT
#     {enter_changes_here}
#
#-
#

#
#  Define aliases for the applications.

alias daophot INSTALL_BIN/daophot

alias allstar INSTALL_BIN/allstar

alias daogrey INSTALL_BIN/daogrey

alias daoplot INSTALL_BIN/daoplot

alias daocurs INSTALL_BIN/daocurs

#
#  Announce the availability of the DAOPHOT commands.
#
      echo " "
      echo "   DAOPHOT applications are now available -- (Version PKG_VERS)"
      echo " "
#
# end
#

