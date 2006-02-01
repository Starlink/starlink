proc CCDGeomSetPercent { Top args } {

#+
#  Name:
#     CCDGeomPercent

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Set the display percentile range.

#  Arguments:
#     Top = window (read)
#        Name of the top-level widget created by this form.
#     args = list (read)
#        A command to run if the percentiles change and are accepted.

#  Global variables:
#     PERCENTILES = string (read and write)
#        The selected display percentile, elements (low) and (high)

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     24-OCT-1995 (PDRAPER):
#     	 Original version.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     01-FEB-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_further_changes_here}

#-

#  Global variables.
   global PERCENTILES

#.

#  Set defaults.
   if { ![info exists PERCENTILES(low)] } { 
      set PERCENTILES(low) 5
   }
   if { ![info exists PERCENTILES(high)] } { 
      set PERCENTILES(high) 95
   }

#-----------------------------------------------------------------------------
#  Widget creation.
#-----------------------------------------------------------------------------
   CCDCcdWidget Topwin topwin \
      Ccd::toplevel $Top -title "Select display percentiles"
   CCDTkWidget Label label \
      label $topwin.label -text "Select display percentiles"
   CCDTkWidget Sep sep frame $topwin.sep -height 3
   CCDTkWidget Low low \
      scale $topwin.low -from 0 -to 100 -label Lower -showvalue 1 \
             -variable PERCENTILES(low) -orient hori -resolution 0.25
   CCDTkWidget High high \
      scale $topwin.high -from 0 -to 100 -label Upper -showvalue 1 \
             -variable PERCENTILES(high) -orient hori -resolution 0.25
   CCDCcdWidget Choice choice Ccd::choice $Topwin.choice
	      
#-----------------------------------------------------------------------------
#  Extra configuration.
#-----------------------------------------------------------------------------
   $Choice addcommand  OK \
      "$Topwin kill $Topwin
       global PERCENTILES
       if { $PERCENTILES(low) != \$PERCENTILES(low) || \
            $PERCENTILES(high) != \$PERCENTILES(high) } { 
          eval $args
       }
      "
   $Choice addcommand  Cancel \
      "$Topwin kill $Topwin
       global PERCENTILES
       set PERCENTILES(low)  $PERCENTILES(low)
       set PERCENTILES(high) $PERCENTILES(high)
      "

#------------------------------------------------------------------------------
#  Widget packing.
#------------------------------------------------------------------------------
   pack $choice -side bottom -fill x
   pack $label -side top -fill x
   pack $sep -side top -fill x
   pack $high -side top -fill x
   pack $low -side top -fill x

#  End of procedure.
}

# $Id$
