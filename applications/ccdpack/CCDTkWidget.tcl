   proc CCDTkWidget { cmdvar pathvar objtype objname args } {
#+
#  Name:
#     CCDTkWidget

#  Type of Module:
#     Tcl/Tk commands

#  Purpose:
#     Creates a new normal Tk widget.

#  Description:
#     This function creates a Tk widget and sets variables giving the
#     pathname of the window and the command name of the widget.  The 
#     purpose of this is so that widgets and Ccd::* widget-type-things
#     can be manipulated using the same form of command, without having
#     to remember which is being manipulated.  This whole approach is
#     messy, and forced by the necessity of converting a lot of Itcl2/Tcl7
#     code to run under Itcl3/Tcl8.  See also CCDCcdWdiget.

#  Arguments:
#     cmdvar = string
#        Name of the variable in the calling context which is to be filled
#        with the command name of the widget.
#     pathvar = string
#        Name of the variable in the calling context which is to be filled 
#        with the pathname of the window.
#     objtype = string
#        The Tk widget type which is to be created.
#     objname = string
#        The pathname of the widget to be created.
#     args = strings
#        Additional arguments to be appended to the widget creation command.

#  Authors:
#     MBT: Mark Taylor (STARLINK)
#     PDRAPER: Peter W. Draper (STARLINK)

#  History:
#     1-APR-2000 (MBT):
#        Original version.
#     1-JAN-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).

#-
      upvar $cmdvar cmd
      upvar $pathvar path
      regsub ^.-? $objname .- cmd
      set path $cmd
      uplevel $objtype $cmd $args
   }

# $Id$
