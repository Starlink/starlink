   proc CCDChangeFilter { box ndf dark flash ftype args } {
#+
#  Name:
#     CCDChangeFilter

#  Purpose:
#     Redraws the current filter-related NDFs into a scrollbox.

#  Description:
#     This procedure redraws a scrollbox (or listbox) with the contents
#     of the global array CCDndfs for the given frame type and the
#     current global filter. It also updates a labelled entry widget
#     with the pattern of NDF names which may have been used to
#     generate the NDF names. This is used by the routine
#     CCDGetNDFColourNames, when switching between filters, which should
#     be consulted. This is now also extended to deal with the case
#     when dark and/or pre-flash exposure times are required.

#  Type of Module:
#     Tcl/Tk procedure.

#  Arguments:
#     box = window (read)
#        The name of the listbox or scrollbox which is to be updated
#	 with the appropriate contents of the global NDF names array.
#     ent = window (read)
#        The name of the labelled entry widget which will have the NDF
#	 names pattern updated. This is controlled by setting to a
#	 textvariable which is related to these NDFs
#	 "CCDNDFsel$ftype($CCDcurrentfilter)"
#     dark = window (read)
#        The name of the labelled entry widget which will have the dark
#        count expression updated. This is controlled by setting to a
#	 textvariable "CCDNDFseldarks($CCDcurrentfilter)". Only used if
#        the global variable CCDsame(darks) is false.
#     flash = window (read)
#        The name of the labelled entry widget which will have the flash
#        exposure expression updated. This is controlled by setting to a
#	 textvariable "CCDNDFselflashes($CCDcurrentfilter)". Only used if
#        the global variable CCDsame(flashes) is false.
#     ftype = string (read)
#        This is the first index of the CCDndfs array and points to the
#	 NDF names to be written.
#     args = list (read)
#        Unused arguments. This procedure is called by "trace" which
#	 appends extra arguments about the variable changed etc. These
#	 are not used.

#  Global variables:
#     CCDcurrentfilter = string (read)
#        The name of the current filter. This is the second index of the
#	 CCDndfs array.
#     CCDndfs = array (read)
#        The names of the NDFs imported into the package at this time.
#	 Filter dependent names are indexed:
#	    CCDndfs($ftype,$filter)
#        NDFs which are not filter dependent are indexed otherwise.
#	 (Usually just by the ftype).
#     CCDNDFsel${ftype} = array (read)
#	 The pattern of NDF names which may have been used to generate
#	 the list of NDF names. The element used corresponds to the
#	 current filter. This is configured to be the entry widget
#	 textvariable.
#     CCDNDFseldarks = array (read)
#	 The pattern of dark counts which may have been used to generate
#	 the associated list. The element used corresponds to the
#	 current filter. This is configured to be the entry widget
#	 textvariable.
#     CCDNDFselflashes = array (read)
#	 The pattern of NDF names which may have been used to generate
#	 the list of NDF names. The element used corresponds to the
#	 current filter. This is configured to be the entry widget
#	 textvariable.
#      CCDfactors = array (write)
#         This array contains the lists of exposure factors for dark
#         counts or pre-flash. The indices are 
#         ($ftype,$CCDcurrentfilter,darks) and 
#         ($ftype,$CCDcurrentfilter,flashes) if used.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     29-APR-1994 (PDRAPER):
#     	 Original version.
#     26-MAY-1994 (PDRAPER):
#        Now controls dark and flash list and entries as well.
#     {enter_changes_here}

#-

#  Global variables:
      global CCDNDFsel$ftype.
      global CCDNDFseldarks
      global CCDNDFselflashes
      global CCDcurrentfilter
      global CCDfactors
      global CCDndfs
      global CCDsame

#.

#  First clear the scrollbox(es)
      $box clear 0 end

#  If we have any values...
      if { [ info exists CCDndfs($ftype,$CCDcurrentfilter) ] } {

#  Branch according to the entry widgets and number of listboxes we
#  have to deal with.
         if { $CCDsame(darks)  && $CCDsame(flashes) } {

#  Simple insert into one listbox.
	    eval $box insert 0 $CCDndfs($ftype,$CCDcurrentfilter)
	 } else {
	    
#  Have dark and/or flash entries to deal with.
            if { !$CCDsame(darks) && !$CCDsame(flashes) } { 

#  Both dark and flash exposures are in use.
               set i 0
               foreach item $CCDndfs($ftype,$CCDcurrentfilter) {
		  $box insert end \
		     $item \
		     [lindex $CCDfactors($ftype,$CCDcurrentfilter,darks) $i ] \
		     [lindex $CCDfactors($ftype,$CCDcurrentfilter,flashes) $i ]
                  incr i
	       }
	    } else {

#  Either just darks or flashes.
               if { !$CCDsame(darks) } {
                  set i 0
		  foreach item $CCDndfs($ftype,$CCDcurrentfilter) {
		     $box insert end \
			$item \
			[lindex $CCDfactors($ftype,$CCDcurrentfilter,darks) $i ] 
		     incr i
		  }
	       } else {
                  set i 0 
		  foreach item $CCDndfs($ftype,$CCDcurrentfilter) {
		     $box insert end \
			$item \
			[lindex $CCDfactors($ftype,$CCDcurrentfilter,flashes) $i ] 
		     incr i
		  }
               }
	    }
	 }
      }

#  Configure the entry boxes to show and use the appropriate textvariable.
      $ndf configure -textvariable CCDNDFsel${ftype}($CCDcurrentfilter)
      if { ! $CCDsame(darks) } { 
	 $dark configure -textvariable CCDNDFseldarks($CCDcurrentfilter)
      }
      if { ! $CCDsame(flashes) } { 
	 $flash configure -textvariable CCDNDFselflashes($CCDcurrentfilter)
      }

#  End of procedure.
   }
# $Id$
