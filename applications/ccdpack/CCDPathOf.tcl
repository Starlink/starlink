   proc CCDPathOf { cmd } {
#+
#  Name:
#     CCDPathOf

#  Type of Module:
#     Tcl/Tk commands

#  Purpose:
#     Finds the pathname of the window for an object.

#  Description:
#     Given a command name which may refer either to a genuine Tk widget
#     or a Ccd::* widget-like thing, this command returns the pathname
#     of the corresponding window.  This may or may not be the same as
#     the command name.  See also CCDTkWidget and CCDCcdWidget.

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
      if { [catch { set path [$cmd pathname] }] } {
         return $cmd
      } else {
         return $path
      }
   }
# $Id$
