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
#     {enter_new_authors_here}

#  History:
#     24-OCT-1995 (PDRAPER):
#     	 Original version.
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
   Ccd_toplevel $Top -title {Select display percentiles}
   set Label [label $Top.label -text {Select display percentiles}]
   set Sep [frame $Top.sep -height 3]
   set Low  [scale $Top.low -from 0 -to 100 -label {Lower} -showvalue 1 \
		-variable PERCENTILES(low) -orient hori -resolution 0.25]
   set High [scale $Top.high -from 0 -to 100 -label {Upper} -showvalue 1 \
		-variable PERCENTILES(high) -orient hori -resolution 0.25]
   set Choice [Ccd_choice $Top.choice]
	      
#-----------------------------------------------------------------------------
#  Extra configuration.
#-----------------------------------------------------------------------------
   $Choice addcommand  OK \
      "$Top kill $Top
       global PERCENTILES
       if { $PERCENTILES(low) != \$PERCENTILES(low) || \
            $PERCENTILES(high) != \$PERCENTILES(high) } { 
          eval $args
       }
      "
   $Choice addcommand  Cancel \
      "$Top kill $Top
       global PERCENTILES
       set PERCENTILES(low)  $PERCENTILES(low)
       set PERCENTILES(high) $PERCENTILES(high)
      "

#------------------------------------------------------------------------------
#  Widget packing.
#------------------------------------------------------------------------------
   pack $Choice -side bottom -fill x
   pack $Label -side top -fill x
   pack $Sep -side top -fill x
   pack $High -side top -fill x
   pack $Low -side top -fill x

#  End of procedure.
}

# $Id$
