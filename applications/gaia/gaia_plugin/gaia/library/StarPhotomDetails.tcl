#+
#  Name:
#     StarPhotomDetails

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class for creating a widget for displaying and
#     photometry information and changing the aperture details.

#  Description:
#     This class creates a mega-widget for displaying the current
#     values of a given StarPhotomObject. It displays them in a 
#     series of labelled entries. This class also creates a series of
#     labelled scale widgets for controlling the size and orientation
#     of the aperture. If the aperture is circular then only the
#     semi-major axis and the annulus scale factors may be changed.
#
#     When a scale value is changed a callback is made to the
#     StarPhotomList object that create this object. If the photometry
#     results are in counts then the displayed data is changed to reflect
#     this. 

#  Invocations:
#
#        StarPhotomDetails object_name [configuration options]
#
#     This creates an instance of a StarPhotom object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#     See itk_option definitions below.

#  Methods:
#     See method statements below.

#  Inheritance:
#     FrameWidget: basic container class for mega-widgets.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     28-MAR-1996 (PDRAPER):
#        Original version.
#     8-JUL-1996 (PDRAPER):
#        Converted to itcl2.0.
#     {enter_further_changes_here}

#-

#.
itk::usual StarPhotomDetails {}

class gaia::StarPhotomDetails {

   #  Inheritances:
   #  -------------

   inherit FrameWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Deal with the input arguments (do this now to initialize the
      #  itk_options to the correct values).
      eval itk_initialize $args

      #  Width of labels & value fields.
      set lwidth 17
      set vwidth 5

      #  Create and pack all the widgets that are necessary for
      #  displaying the values of the current StarPhotomObject.
      itk_component add MainLabel {
         label $w_.label -text "Object details\n" -anchor c
      }
      itk_component add Index {
         LabelValue $w_.index -text {Aperture index:} \
            -value 0 \
            -labelwidth $lwidth
      }

      #  Semimajor axis.
      itk_component add Semimajor {
         LabelEntryScale $w_.semimajor \
            -text {Semimajor axis:} \
            -value $itk_option(-semimajor) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -from 0.25 \
            -to $itk_option(-maxsemimajor) \
            -increment 1 \
            -resolution 0.25 \
            -show_arrows 1 \
            -anchor w \
            -command [code $this configure -semimajor]
      }

      #  Eccentricity (0 for circles).
      itk_component add Eccen {
         LabelEntryScale $w_.eccen \
            -text {Eccentricity:} \
            -value $itk_option(-eccentricity) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -from 0.0 \
            -to 0.9999 \
            -increment 0.001 \
            -resolution 0.001 \
            -show_arrows 1 \
            -anchor w \
            -command [code $this configure -eccentricity]
      }

      #  Position angle (0 for circles).
      itk_component add Angle {
         LabelEntryScale $w_.angle \
            -text {Position angle:} \
            -value $itk_option(-angle) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -from -180.0 \
            -to 180.0 \
            -increment 0.25 \
            -resolution 0.25 \
            -show_arrows 1 \
            -anchor w \
            -command [code $this configure -angle]
      }

      #  Inner scale factor.
      itk_component add InnerScale {
         LabelEntryScale $w_.inner \
            -text {Annulus inner scale:} \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -from 1.0 \
            -to 20.0 \
            -increment 0.1 \
            -show_arrows 1 \
            -resolution 0.1 \
            -value $itk_option(-innerscale) \
            -anchor w \
            -command [code $this configure -innerscale]
      }

      #  Outer scale factor.
      itk_component add OuterScale {
         LabelEntryScale $w_.outer \
            -text {Annulus outer scale:} \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -from 1.5 \
            -to 20.5 \
            -increment 0.1 \
            -resolution 0.1 \
            -show_arrows 1 \
            -value $itk_option(-outerscale) \
            -anchor w \
            -command [code $this configure -outerscale]
      }

      #  Labels for results of photometry measurement.
      itk_component add Sep1 {
         frame $w_.sep1 -height 3
      }
      itk_component add X {
         LabelValue $w_.x -text {X position:} \
            -value 0 \
            -labelwidth $lwidth
      }
      itk_component add Y {
         LabelValue $w_.y -text {Y position:} \
            -value 0 \
            -labelwidth $lwidth
      }
      if { $itk_option(-usemags) } { 
         set label {Magnitude:}
      } else {
         set label {Mean count:}
      }
      itk_component add Mag {
         LabelValue $w_.mag -text "$label" \
            -value 0 \
            -labelwidth $lwidth
      }
         
      if { $itk_option(-usemags) } { 
         set label {Magnitude error:} 
      } else {
         set label {Error in count:} 
      }
      itk_component add MagErr {
         LabelValue $w_.magerr -text $label \
            -value 0 \
            -labelwidth $lwidth
      }
      itk_component add Sky {
         LabelValue $w_.sky -text {Sky value:} \
            -value 0 \
            -labelwidth $lwidth 
      }
      itk_component add Signal {
         LabelValue $w_.signal -text {Sum in aperture:} \
            -value 0 \
            -labelwidth $lwidth
      }
      itk_component add Code {
         LabelValue $w_.code -text {Error code:} \
            -value OK \
            -labelwidth $lwidth
      }

      #  Pack up widget for display.
      pack $itk_component(MainLabel) -anchor center -fill x
      pack $itk_component(Index) -fill x
      pack $itk_component(X) -fill x
      pack $itk_component(Y) -fill x
      pack $itk_component(Mag) -fill x
      pack $itk_component(MagErr) -fill x
      pack $itk_component(Sky) -fill x
      pack $itk_component(Signal) -fill x
      pack $itk_component(Code) -fill x
      pack $itk_component(Sep1) -fill x -ipadx 5
      pack $itk_component(Semimajor) -fill x
      pack $itk_component(Eccen) -fill x
      pack $itk_component(Angle) -fill x
      pack $itk_component(InnerScale) -fill x
      pack $itk_component(OuterScale) -fill x
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Update the display to reflect the values of the given
   #  StarPhotomObject.
   method update_display {object} {
      #  Stop changes from leaving this object. Otherwise changes to
      #  these values could be propagated back to whoever caused this
      #  update, ending in a positive feedback loop!
      set propagate_ 0

      lassign [$object aperture_details] index x y mag magerr sky \
         signal code itk_option(-semimajor) itk_option(-eccentricity)\
         itk_option(-angle) positions itk_option(-innerscale) \
	 itk_option(-outerscale)
      $itk_component(Index) configure -value $index
      $itk_component(X) configure -value $x
      $itk_component(Y) configure -value $y
      $itk_component(Mag) configure -value $mag
      $itk_component(MagErr) configure -value $magerr
      $itk_component(Sky) configure -value $sky
      $itk_component(Signal) configure -value $signal
      $itk_component(Code) configure -value $code

      $itk_component(Semimajor) configure -value $itk_option(-semimajor)
      $itk_component(Angle) configure -value $itk_option(-angle)
      $itk_component(Eccen) configure -value $itk_option(-eccentricity)

      $itk_component(InnerScale) configure -value $itk_option(-innerscale)
      $itk_component(OuterScale) configure -value $itk_option(-outerscale)

      if { $itk_option(-positions_cmd) != {} } {
         if { $positions == "annulus" } {
            eval $itk_option(-positions_cmd) annulus
         } else {
            eval $itk_option(-positions_cmd) regions
         }
      }

      #  If annulus is a circle then switch off ellipticity.
      if { [$object cget -shape] == "circle" } { 
         set_for_circles 1
      } else {
         set_for_circles 0
      }
      ::update idletasks ;#  Needed to propagate changes now!
      set propagate_ 1
   }

   #  Reset the displayed values back to their defaults.
   method reset_display {} {
      $itk_component(Index) configure -value 0
      $itk_component(X) configure -value 0
      $itk_component(Y) configure -value 0
      $itk_component(Mag) configure -value 0
      $itk_component(MagErr) configure -value 0
      $itk_component(Sky) configure -value 0
      $itk_component(Signal) configure -value 0
      $itk_component(Code) configure -value OK

      set itk_option(-semimajor) 5
      $itk_component(Semimajor) configure -value 5
      set itk_option(-angle) 0.0
      $itk_component(Angle) configure -value 0.0
      set itk_option(-eccentricity) 0.0
      $itk_component(Eccen) configure -value 0.0
   }

   #  Method to disable the eccentricity and angle widgets if
   #  requested (useful for circular apertures.) 
   method set_for_circles {ok} {
      if { $ok } { 
         $itk_component(Eccen) configure -value 0.0
         $itk_component(Eccen) configure -state disabled
         $itk_component(Angle) configure -value 0.0
         $itk_component(Angle) configure -state disabled
      } else {
         $itk_component(Eccen) configure -state normal
         $itk_component(Angle) configure -state normal
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of object that controls the list photometry objects that
   #  require updating.
   itk_option define -object_list object_list Object_list {} {}

   #  Current semimajor axes of apertures (== radius for circles).
   itk_option define -semimajor semimajor SemiMajor {5} {
      if { $itk_option(-object_list) != {} } {
         if { $propagate_ } {
            $itk_option(-object_list) configure -semimajor $itk_option(-semimajor)
         }
      }
   }

   #  Current scale of inner annular region.
   #  Keep the smallest possible outer scale slightly larger than
   #  this.
   itk_option define -innerscale innerscale InnerScale {1.5} {
      if { $itk_option(-object_list) != {} } {
         if { [info exists itk_component(OuterScale)] } {
            $itk_component(OuterScale) configure \
               -from [expr $itk_option(-innerscale)*1.001]
         }
         if { $propagate_ } {
            $itk_option(-object_list) configure -innerscale $itk_option(-innerscale)
         }
      }
   }

   #  Current scale of outer annular region.
   #  Keep the smallest possible outer scale slightly larger than
   #  this.
   itk_option define -outerscale outerscale OuterScale {2.0} {
      if { $itk_option(-object_list) != {} } {
         if { [info exists itk_component(OuterScale)] } {
            $itk_component(OuterScale) configure \
               -from [expr $itk_option(-innerscale)*1.001]
         }
         if { $propagate_ } {
            $itk_option(-object_list) configure -outerscale $itk_option(-outerscale)
         }
      }
   }

   #  Current eccentricity (constrained to line in range 0-1 by scale).
   itk_option define -eccentricity eccentricity Eccentricity {0.0} {
      if { $itk_option(-object_list) != {} } {
         if { $propagate_ } {
            $itk_option(-object_list) configure -eccentricity $itk_option(-eccentricity)
         }
      }
   }

   #  Current position angle (constrained to line in range -180 to 180
   #  by scale).
   itk_option define -angle angle Angle {0.0} {
      if { $itk_option(-object_list) != {} } {
         if { $propagate_ } {
            $itk_option(-object_list) configure -angle $itk_option(-angle)
         }
      }
   }

   #  Maximum value that semimajor axis can have.
   itk_option define -maxsemimajor maxsemimajor MaxSemimajor {250} {
      if {[info exists itk_component(Semimajor)] } {
         $itk_component(Semimajor) configure -to $itk_option(-maxsemimajor)
      }
   }

   #  Command to set according to the value of positions.
   itk_option define -positions_cmd positions_cmd Positions_cmd {} {}

   #  Whether calculations are in magnitudes or counts.
   itk_option define -usemags usemags UseMags 1 {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Whether to inhibit non-interactive changes from propagating
   #  back to list (where they may have originated).
   protected variable propagate_ 1


   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
