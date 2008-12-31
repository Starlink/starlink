#!/bin/csh -f
#+
#  Name:
#     polmap.csh
#
#  Purpose:
#     Start the POLMAP system from Unix shell.
#
#  Type of Module:
#     C shell script.
#
#  Invocation:
#     source polmap.csh
#
#  Description:
#     This procedure defines the environment and aliases needed to
#     run POLMAP from the current directory.
#
#  Notes:
#     The installation target is set outside of this script. 
#
#  Authors:
#     BLY: M.J.Bly (Starlink, RAL)
#     {enter_new_authors_here}
#
#  History:
#     24-NOV-1995 (BLY):
#       Original Version.
#     {enter_changes_here}
#
#-
#

#  Define the locations of the POLMAP files.

if !($?POLMAP_DIR) then
   echo ""
   echo "   Environment Variable POLMAP_DIR is not set.  Setting" 
   echo "   POLMAP_DIR to be INSTALL_BIN"
   echo ""
   setenv POLMAP_DIR INSTALL_BIN
endif

#
#  Define alias for the POLMAP.

alias polmap '$POLMAP_DIR/polmap'

#
#  Announce the availability of POLMAP.
#

echo
echo "   POLMAP is now available -- (Version PKG_VERS)"
echo 
echo "   type polmap to start POLMAP"

#
# end
exit
#.
