   proc CCDGetColourNDFs { topwin ftype } {
#+
#  Name:
#     CCDGetColourNDFs

#  Purpose:
#     Obtains lists of NDFs which may have different colours.

#  Description:
#     This procedure produces an instance of a forms,
#     CCDGetNDFColourNames or CCDGetNDFNames which each get a list of
#     NDFs -- one for each different filter type, or just a single
#     list, together with a possible set of factors (darks and/or
#     pre-flash exposures). Control uses the global variable
#     CCDshowframe($ftype). If this variable is set then an instance
#     will be created, if false any existing instance will be destroyed.

#  Type of Module:
#     Tcl/Tk procedure.

#  Arguments:
#     topwin = window (read)
#        The top-level window to parent the top-levels created by this
#	 procedure.
#     ftype = string (read)
#        The frame type of the colour data. This is normally something
#	 like "target" or "flatfield". This indexes the global array
#	 CCDhaveframe($ftype) which indicates whether the frame of this
#	 type are expected to be chosen.

#  Global variables:
#     CCDshowframe = array (read)
#        This is an array of boolean values which indicate whether or
#	 not frames of the given type (which are the indices of the
#	 array) should be displayed or removed.
#     CCDsame = array (read)
#	 The element CCDsame(filter) indicates whether or not the data
#	 have the same filter type or not. If this is false then
#	 CCDfilternames should contain the filter types.
#     CCDfilternames = string (read)
#        If CCDsame(filter) is false then this should contain the names of
#	 the filters which are present. These are space separated.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     27-APR-1994 (PDRAPER):
#     	 Original version.
#     28-APR-1994 (PDRAPER):
#     	 Now uses CCDGetColourNDFNames, instead of multiple instances of
#	 CCDGetNDFNames.
#     {enter_further_changes_here}

#-

#  Global variables:
      global CCDshowframe
      global CCDsame
      global CCDfilternames
#.

#  Look for presence of global variable which indicates whether or not
#  to enabled a form for getting the NDF names. (This variable should be
#  set prior to calling this procedure).
      if { $CCDshowframe($ftype) } {

#  Have we any filters to accomodate.
         if { ! $CCDsame(filter) } {

#  Yes create an instance of CCDGetColourNDFNames
            if { [ info exists CCDfilternames ] } {
               if { $CCDfilternames != {} } {
                  if { ! [ winfo exists $topwin.$ftype ] } {
                     CCDGetNDFColourNames \
                        $topwin.$ftype \
                        "Filter dependent NDFs ($ftype)" \
                        "Filter dependent NDFs ($ftype)" \
                        $ftype CCDshowframe($ftype)
                  }
	       } else {
		  CCDIssueInfo "No known filters"
	       }
	    }
         } else {

#  No filter no use CCDGetNDFNames.
            if { ! [ winfo exists $topwin.$ftype ] } {
               CCDGetNDFNames \
                  $topwin.$ftype\
                  "NDFs ($ftype)" \
                  "NDFs ($ftype)" \
                  $ftype CCDshowframe($ftype)
            }
	 }
      } else {

#  Remove all windows.
         if { [ winfo exists $topwin.$ftype ] } {
            $topwin.$ftype kill $topwin.$ftype
         }
      }

#  End of procedure.
   }
# $Id$
