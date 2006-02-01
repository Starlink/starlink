   proc CCDCcdWidget { cmdvar pathvar objtype objname args } {
#+
#  Name:
#     CCDCcdWidget

#  Type of Module:
#     Tcl/Tk commands

#  Purpose:
#     Creates a new Ccd::* widget-type thing.

#  Description:
#     This function creates a CCDPACK superwidget and sets variables 
#     giving the pathname of the window and the command name of the widget.
#     These variables will not have the same value.  The purpose of this 
#     is so that genuine Tk widgets and Ccd::* widget-type-things
#     can be manipulated using the same form of command, without having
#     to remember which is being manipulated.  This whole approach is
#     messy, and forced by the necessity of converting a lot of Itcl2/Tcl7
#     code to run under Itcl3/Tcl8.  See also CCDTkWidget.

#  Arguments:
#     cmdvar = string
#        Name of the variable in the calling context which is to be filled
#        with the command name of the superwidget.
#     pathvar = string
#        Name of the variable in the calling context which is to be filled 
#        with the pathname of the window.
#     objtype = string
#        The Ccd::* superwidget type which is to be created.
#     objname = string
#        The pathname of the superwidget to be created.
#     args = strings
#        Additional arguments to be appended to the superwidget creation 
#        command.

#  Authors:
#     MBT: Mark Taylor (STARLINK)

#  History:
#     1-APR-2000 (MBT):
#        Original version.

#-
      upvar $cmdvar cmd
      upvar $pathvar path
      regsub ^.-? $objname . cmd
      regsub ^.-? $objname .- path
      uplevel $objtype $cmd $args
   }

# $Id$
