   proc CCDGetNDFs { topwin ftype } {
#+
#  Name:
#     CCDGetNDFs

#  Purpose:
#     Obtains a list of NDFs.

#  Description:
#     This procedure produces an instance of a form CCDGetNDFNames which
#     gets a list of NDFs under the control of the global variable
#     CCDshowframe($ftype). If this variable is set then an instance
#     will be created, if false any existing instance will be destroyed.

#  Type of Module:
#     Tcl/Tk procedure.

#  Arguments:
#     topwin = window (read)
#        The top-level window to parent the top-levels created by this
#	 procedure.
#     ftype = string (read)
#	 The frame type of the data. This is normally something like
#	 bias. This indexes the global array CCDshowframe($ftype) which
#	 indicates whether the frame of this type are expected to be
#	 chosen.

#  Global variables:
#     CCDshowframe = array (read)
#        This is an array of boolean values which indicate whether or
#	 not frame of the given type (which are the indices of the
#	 array) are expected to be given or not.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     27-APR-1994 (PDRAPER):
#     	 Original version.
#     28-APR-1994 (PDRAPER):
#     	 Now uses CCDGetColourNDFNames, instead of multiple instances of
#	 CCDGetNDFNames.
#     28-APR-1994 (PDRAPER):
#     	 Just gets ordinary frames. No colour information allowed.
#     {enter_further_changes_here}

#-

#  Global variables:
      global CCDshowframe
#.

#  Look for presence of global variable which indicates whether or not
#  to enabled a form for getting the NDF names. (This variable should be
#  set prior to calling this procedure).
      if { $CCDshowframe($ftype) } {
         if { ! [ winfo exists $topwin.$ftype ] } { 
            CCDGetNDFNames \
               $topwin.$ftype\
               "NDFs ($ftype)" \
               "NDFs ($ftype)" \
                $ftype CCDshowframe($ftype)
         }
      } else {

#  Remove the window if it exists.
         if { [ winfo exists $topwin.$ftype ] } {
            $topwin.$ftype kill $topwin.$ftype 
         }
      }

#  End of procedure.
   }
# $Id$
