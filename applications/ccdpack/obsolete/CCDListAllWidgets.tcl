   proc CCDListAllWidgets { parent } {
#+
#  Name:
#     CCDListAllWidgets

#  Purpose:
#     Returns a list of all the widgets in a tree rooted by the given
#     window.

#  Type of Module:
#     Tcl/Tk procedure

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     4-MAR-1994 (PDRAPER):
#     	 Original version.
#     {enter_changes_here}

#-
      set children [ winfo children $parent ]
      foreach oneof $children {
         append children " [CCDListAllWidgets $oneof]"
      }
   return $children
   }
# $Id$
