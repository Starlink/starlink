   proc CCDGetFactorNDFs { topwin ftype } {
#+
#  Name:
#     CCDGetFactorNDFs

#  Purpose:
#     Obtains a list of NDFs which may have associated factor(s).

#  Description:
#     This procedure produces an instance of a forms, CCDGetNDFNames,
#     or CCDGetNDFwithFactors which each get a list of NDFs.
#     CCDGetNDFwithFactors also gets lists of associated (exposure)
#     factors. Control is performed using the global variable
#     CCDshowframe($ftype). If this variable is set then an instance
#     will be created, if false any existing instance will be
#     destroyed.

#  Type of Module:
#     Tcl/Tk procedure.

#  Arguments:
#     topwin = window (read)
#        The top-level window to parent the top-levels created by this
#	 procedure.
#     ftype = string (read)
#	 The frame type of the data. This is normally "darks" or "flashes". 
#        This indexes the global array CCDshowframe($ftype) which indicates 
#        whether the frame of this type are expected to be chosen. If ftype is
#        "dark" then only one factor is obtained. If ftype is "flash" and the
#        variable CCDhaveframe(darks) is true then two factors are obtained
#        (a dark and a flash time).

#  Global variables:
#     CCDshowframe = array (read)
#        This is an array of boolean values which indicate whether or
#	 not frames of the given type are to be shown.
#     CCDhaveframe = array (read)
#        This is an array of boolean values which indicate whether or
#	 not frame of the given type (which are the indices of the
#	 array) are expected to be given or not.
#     CCDsame = array (read)
#        This is an array which indicates if the data have the same
#	 factor. It is indexed by the ftype variable. So if the NDFs to
#	 be obtained have the same exposes $CCDsame($ftype) will be true
#	 (1).

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
#     	 Now gets associated factors.
#     10-MAY-1994 (PDRAPER):
#     	 Added gvar.
#     26-MAY-1994 (PDRAPER):
#        Now correctly deals with the pre-flash needing dark counts correct
#        case.
#     {enter_further_changes_here}

#-

#  Global variables:
      global CCDhaveframe
      global CCDshowframe
      global CCDsame
#.

#  Look for presence of global variable which indicates whether or not
#  to enable a form for getting the NDF names. (This variable should be
#  set prior to calling this procedure).
      if { $CCDshowframe($ftype) } {

#  Have we an associated factor(s) to obtain?
         if { $ftype == "darks" } { 

#  Yes create an instance of CCDGetNDFwithFactors
	    if { ! [ winfo exists $topwin.$ftype ] } {
               CCDGetNDFwithFactors \
                  $topwin.$ftype \
                  "Dark count NDFs" \
                  "Dark count NDFs" \
                  $ftype CCDshowframe($ftype)
	    }
         } else { 

#  Pre-flash NDFs.
	    if { ! [ winfo exists $topwin.$ftype ] } {
	       CCDGetNDFwithFactors \
		  $topwin.$ftype \
		  "Pre-flash NDFs" \
		  "Pre-flash NDFs" \
		  $ftype CCDshowframe($ftype)
	    }
	 }
      } else {

#  Remove window if it exists.
         if { [ winfo exists $topwin.$ftype ] } {
            $topwin.$ftype kill $topwin.$ftype
         }
      }

#  End of procedure.
   }
# $Id$
