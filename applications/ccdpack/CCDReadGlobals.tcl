   proc CCDReadGlobals { topwin } {
#+
#  Name:
#     CCDReadGlobals
   
#  Type of Module:
#     Tcl/Tk procedure.
   
#  Purpose:
#     Reads and restores a setup file.
   
#  Description:
#     First a prompt is made for the name of a file from which to read
#     a saved setup. This defaults to $HOME/.ccdpack. Then this file is
#     sourced restoring the state. (State files are written out by the
#     CCDSaveGlobals procedure and consist of Tcl commands to restore
#     the names and values of all the CCDxxxxx global parameters).
   
#  Arguments:
#     topwin = window (read)
#        The name of the top-level window in which to make a prompt for
#        a filename.
   
#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}
   
#  History:
#     6-MAY-1994 (PDRAPER):
#        Original version.
#     9-MAY-1994 (PDRAPER):
#     	 Reads a state file.
#     21-AUG-1995 (PDRAPER):
#        Changed to use sensible defaults for CCDimportfilter.
#     29-AUG-1995 (PDRAPER):
#        Changed to use .ccdpack in the current directory as the
#        default. 
#     {enter_further_changes_here}
   
#-
   
#  Global parameters:
      global CCDimportfile
      global CCDimportexists
      global CCDimportfilter
   
#.
   
#  Get the name of a file to receive the current status.
      set CCDimportfile ".ccdpack"
      if { [ info exists CCDimportfilter] } { 
         set oldfilt $CCDimportfilter 
      } else {
         set oldfilt "*"
      }
      set CCDimportfilter ".*"
      CCDGetFileName $topwin "Restore state from file"
      if { $CCDimportexists } {

#  Source the file in the global scope.
         uplevel #0 source $CCDimportfile
      }

#  Restore input file filter.
      set CCDimportfilter $oldfilt

#  End of procedure.
   }
# $Id$
