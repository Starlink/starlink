   proc CCDNameTail { name } {
#+
#  Name:
#     CCDNameTail

#  Type of Module:
#     Tcl/Tk commands

#  Purpose:
#     Gets the final part of a fully qualified namespace name.

#  Description:
#     This function returns the last part (after the last ::) of a fully
#     qualified name of a Tcl variable.  It does the same as 
#     the 'info namespace tail' command used to do in Itcl2.2/Tcl7.6,
#     which command does not exist in Itcl3/Tcl8.

#  Authors:
#     MBT: Mark Taylor (STARLINK)

#  History:
#     1-APR-2000 (MBT):
#        Original version.
#-

      regsub .*:: $name "" tail
      return $tail
}
# $Id$
