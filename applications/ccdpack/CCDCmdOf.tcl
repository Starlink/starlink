   proc CCDCmdOf { window } {
#+
#  Name:
#     CCDCmdOf

#  Type of Module:
#     Tcl/Tk commands

#  Purpose:
#     Finds the command name corresponding to a window.

#  Description:
#     Given a window pathname which may refer either to a genuine Tk widget
#     or to a Ccd::* widget-like thing, this command returns the command
#     name which can be used for manipulating it.  This may or may not
#     be the same as the pathname.  See also CCDTkWidget and CCDCcdWidget.
#
#     The current implementation of this relies on knowledge of how the
#     window names are constructed from command names by CCDTkWidget and
#     CCDCcdWidget.  That information ought ideally to be entirely 
#     private to the Ccd::base class, but I don't see how to get round it.
#     The whole approach is messy, and forced by having to upgrade a 
#     lot of code from Itcl2/Tcl7 to Itcl3/Tcl8.

#  Arguments:
#     cmd = string
#        The command name of a Tk widget or Ccd::* widget-like thing.

#  Return Value:
#     The pathname of the window corresponding to the command.

#  Authors:
#     MBT: Mark Taylor (STARLINK)

#  History:
#     1-APR-2000 (MBT):
#        Original version.

#-
      regsub ^\.- $window . cmd
      return $cmd
   }
# $Id$
