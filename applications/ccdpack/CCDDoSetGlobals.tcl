proc CCDDoSetGlobals { Top type } {

#+
#  Name:
#     CCDDoSetGlobals

#  Purpose:
#     Checks that the global parameters are minimally available.

#  Language:
#     Tcl/Tk procedure

#  Description:
#     Thus routine checks that we have a minimally correct set of
#     global parameters available for the given type.
#
#     The type of parameters checked and cleared are defined by the
#     argument type. These separate into two parts "general" variables
#     that define the way that the package works and "CCD" specific
#     variables, these define the CCD characteritics.
#
#     The general variables are:
#        LOGTO, LOGFILE, SATURATE, SETSAT, GENVAR and PRESERVE
#
#     The CCD specific variables are:
#        EXTENT, DIRECTION, BOUNDS, ADC, RNOISE, MASK, DEFERRED and
#        SATURATION

#  Return value:
#     Returns 0 if MASK file is not located, 1 otherwise.

#  Arguments:
#     Top = window (read)
#        Name of the top-level window that is running this
#        routine. This is put on hold (together with all the other
#        active top-level windows) until the application completes.
#     type = string (read)
#        One of "general" or "CCD". This defines which parameters will
#        be checked by this call.

#  Global variables:
#      CCDglobalpars = array (read)
#         The array of all the currently defined global
#         parameters. The indices are ADC, MASK etc.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     1-JUN-1994 (PDRAPER):
#        Original version.
#     24-AUG-1995 (PDRAPER):
#        Recoded to check various things and removed CCDTaskWait.
#     20-OCT-1995 (PDRAPER):
#        Recoded again. This time to remove CCDSETUP as I task does
#        not set the global parameters anyway. These must now be 
#        passed to the applications as used. Nearly missed this point.
#     21-OCT-1995 (PDRAPER):
#        Recoded again. Now doesn't run any applications. The globals
#        are now cleared when the GUI runs up.
#     {enter_changes_here}


#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables:
   global env
   global CCDglobalpars

#  Local parameters:
   set genscalars "LOGTO,LOGFILE,SATURATE,SETSAT,GENVAR,PRESERVE"
   set ccdscalars "DIRECTION,ADC,RNOISE,MASK,DEFERRED,SATURATION"
   set ccdvectors "EXTENT,BOUNDS"
#.

#  Define the variables we'll use.
   if { $type == "CCD" } {
      set scalars $ccdscalars
      set vectors $ccdvectors
   } else {
      set scalars $genscalars
      set vectors ""
   }

#  First check that the mask file exists (this is the most likely error).
   if { $type == "CCD" && [ info exists CCDglobalpars(MASK) ] } {
      if { $CCDglobalpars(MASK) != "" && $CCDglobalpars(MASK) != "!" } {
         if { ! [file readable $CCDglobalpars(MASK)] } {
            CCDIssueInfo \
               "Cannot read MASK data file ($CCDglobalpars(MASK)). Check\
appropriate CCD characteristics field."
            return 0
         }
      }
   }

#  Check that LOGFILE and LOGTO are set as these are used everywhere.
   if { ! [info exists CCDglobalpars(LOGTO) ] } { 
      set CCDglobalpars(LOGTO) BOTH
   }
   if { ! [info exists CCDglobalpars(LOGFILE) ] } { 
      set CCDglobalpars(LOGFILE) CCDPACK.LOG
   } elseif { $CCDglobalpars(LOGFILE) == "" } { 
      set CCDglobalpars(LOGFILE) CCDPACK.LOG
   }

#  End of procedure.
   return 1
}
# $Id$
