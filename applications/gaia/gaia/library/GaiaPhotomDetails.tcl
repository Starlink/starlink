#+
#  Name:
#     GaiaPhotomDetails

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class for creating a widget for displaying
#     photometry information and changing the aperture details.

#  Description:
#     This class creates a mega-widget for displaying the current
#     values of a given GaiaPhotomObject. These objects represent the
#     current state of either an aperture or optimal photometry
#     measurement (or in the case of optimal photometry this may also
#     be a PSF reference object).
#
#     For apertures and PSF objects this class also creates a series of
#     labelled scale widgets for controlling the size and orientation
#     of the aperture. If the aperture is circular then only the
#     semi-major axis/clipping radius and the annulus scale factors
#     may be changed.
#
#     When a scale value is changed a callback is made to the
#     GaiaPhotomList object that create this object. If the photometry
#     results are in counts then the displayed data is changed to
#     reflect this.
#
#     Note the "type" of photometry object that this widget can deal
#     with is decided when it is created (by the state of the
#     controlling GaiaPhotomList), all objects consequently displayed
#     by it, must be of this type.

#  Invocations:
#
#        GaiaPhotomDetails object_name [configuration options]
#
#     This creates an instance of a GaiaPhotomDetails object. The
#     return is the name of the object.
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

#  Copyright:
#     Copyright (C) 1996-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     28-MAR-1996 (PWD):
#        Original version.
#     8-JUL-1996 (PWD):
#        Converted to itcl2.0.
#     {enter_further_changes_here}

#-

#.
itk::usual GaiaPhotomDetails {}

itcl::class gaia::GaiaPhotomDetails {

   #  Inheritances:
   #  -------------

   inherit util::FrameWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Deal with the input arguments (do this now to initialize the
      #  itk_options to the correct values).
      eval itk_initialize $args

      #  Create the widgets for display the values of the type of
      #  photometry object that we have.
      add_controls_
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Add controls for displaying the GaiaPhotomObject details and
   #  modifying the possible parameters.
   protected method add_controls_ {} {
      if { $phottype_ == "aperture" } {
         add_ap_controls_
      } else {
         if { $psf_ } {
            add_psf_controls_
         } else {
            add_opt_controls_
         }
      }
   }

   #  Add controls for a normal aperture photometry object.
   protected method add_ap_controls_ {} {

      #  Width of labels & value fields.
      set lwidth 17
      set vwidth 5

      itk_component add MainLabel {
         gaia::LabelRule $w_.label -text "Current object details"
      }
      itk_component add Index {
         util::LabelValue $w_.index -text {Aperture index:} \
            -value 0 \
            -labelwidth $lwidth
      }

      #  Semimajor axis.
      itk_component add Semimajor {
         util::LabelEntryScale $w_.semimajor \
            -text {Semimajor axis:} \
            -value $itk_option(-semimajor) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -from 0.25 \
            -to $itk_option(-maxsemimajor) \
            -increment 1 \
            -resolution 0.01 \
            -show_arrows 1 \
            -anchor w \
            -command [code $this configure -semimajor]
      }

      #  Eccentricity (0 for circles).
      itk_component add Eccen {
         util::LabelEntryScale $w_.eccen \
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
         util::LabelEntryScale $w_.angle \
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
         util::LabelEntryScale $w_.inner \
            -text {Annulus inner scale:} \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -from 1.0 \
            -to 20.0 \
            -increment 0.1 \
            -show_arrows 1 \
            -resolution 0.01 \
            -value $itk_option(-innerscale) \
            -anchor w \
            -command [code $this configure -innerscale]
      }

      #  Outer scale factor.
      itk_component add OuterScale {
         util::LabelEntryScale $w_.outer \
            -text {Annulus outer scale:} \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -from 1.5 \
            -to 20.5 \
            -increment 0.1 \
            -resolution 0.01 \
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
         util::LabelValue $w_.x -text {X position:} \
            -value 0 \
            -labelwidth $lwidth
      }
      itk_component add Y {
         util::LabelValue $w_.y -text {Y position:} \
            -value 0 \
            -labelwidth $lwidth
      }
      if { $itk_option(-usemags) } {
         set label {Magnitude:}
      } else {
         set label {Mean count:}
      }
      itk_component add Mag {
         util::LabelValue $w_.mag -text "$label" \
            -value 0 \
            -labelwidth $lwidth
      }

      if { $itk_option(-usemags) } {
         set label {Magnitude error:}
      } else {
         set label {Error in count:}
      }
      itk_component add MagErr {
         util::LabelValue $w_.magerr -text $label \
            -value 0 \
            -labelwidth $lwidth
      }
      itk_component add Sky {
         util::LabelValue $w_.sky -text {Sky value:} \
            -value 0 \
            -labelwidth $lwidth
      }
      itk_component add Signal {
         util::LabelValue $w_.signal -text {Sum in aperture:} \
            -value 0 \
            -labelwidth $lwidth
      }
      itk_component add Code {
         util::LabelValue $w_.code -text {Error code:} \
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

   #  Add controls for a normal optimal photometry object. These are
   #  always circular and have a fixed inner radius (defined as part
   #  of the PSF object).
   protected method add_opt_controls_ {} {

      #  Width of labels & value fields.
      set lwidth 17
      set vwidth 5

      itk_component add MainLabel {
         gaia::LabelRule $w_.label -text "Current object details"
      }
      itk_component add Index {
         util::LabelValue $w_.index -text {Aperture index:} \
            -value 0 \
            -labelwidth $lwidth
      }

      #  Inner scale factor.
      itk_component add InnerScale {
         util::LabelEntryScale $w_.inner \
            -text {Annulus inner scale:} \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -from 1.0 \
            -to 20.0 \
            -increment 0.1 \
            -show_arrows 1 \
            -resolution 0.01 \
            -value $itk_option(-innerscale) \
            -anchor w \
            -command [code $this configure -innerscale]
      }

      #  Outer scale factor.
      itk_component add OuterScale {
         util::LabelEntryScale $w_.outer \
            -text {Annulus outer scale:} \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -from 1.5 \
            -to 20.5 \
            -increment 0.1 \
            -resolution 0.01 \
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
         util::LabelValue $w_.x -text {X position:} \
            -value 0 \
            -labelwidth $lwidth
      }
      itk_component add Y {
         util::LabelValue $w_.y -text {Y position:} \
            -value 0 \
            -labelwidth $lwidth
      }
      if { $itk_option(-usemags) } {
         set label {Magnitude:}
      } else {
         set label {Mean count:}
      }
      itk_component add Mag {
         util::LabelValue $w_.mag -text "$label" \
            -value 0 \
            -labelwidth $lwidth
      }

      if { $itk_option(-usemags) } {
         set label {Magnitude error:}
      } else {
         set label {Error in count:}
      }
      itk_component add MagErr {
         util::LabelValue $w_.magerr -text $label \
            -value 0 \
            -labelwidth $lwidth
      }
      itk_component add Sky {
         util::LabelValue $w_.sky -text {Sky value:} \
            -value 0 \
            -labelwidth $lwidth
      }
      itk_component add Signal {
         util::LabelValue $w_.signal -text {Sum in aperture:} \
            -value 0 \
            -labelwidth $lwidth
      }
      itk_component add Code {
         util::LabelValue $w_.code -text {Error code:} \
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
      pack $itk_component(InnerScale) -fill x
      pack $itk_component(OuterScale) -fill x
   }

   #  Add controls for a PSF optimal photometry object. These are
   #  always circular and have an aperture index of 0.
   protected method add_psf_controls_ {} {

      #  Width of labels & value fields.
      set lwidth 17
      set vwidth 5

      itk_component add MainLabel {
         gaia::LabelRule $w_.label -text "PSF object details"
      }

      #  Clipping radius (masquerades as semi-major axis).
      itk_component add Clip {
         util::LabelEntryScale $w_.clip \
            -text {Clipping radius:} \
            -value $itk_option(-semimajor) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -from 0.25 \
            -to $itk_option(-maxsemimajor) \
            -increment 0.1 \
            -resolution 0.01 \
            -show_arrows 1 \
            -anchor w \
            -command [code $this configure -semimajor]
      }

      #  Seeing estimate.
      itk_component add Seeing {
         util::LabelEntryScale $w_.seeing \
            -text {Seeing estimate:} \
            -value $itk_option(-seeing) \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -from 0.1 \
            -to [expr $itk_option(-maxsemimajor)*0.1] \
            -increment 0.1 \
            -resolution 0.01 \
            -show_arrows 1 \
            -anchor w \
            -command [code $this configure -seeing]
      }

      #  Inner scale factor.
      itk_component add InnerScale {
         util::LabelEntryScale $w_.inner \
            -text {Annulus inner scale:} \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -from 1.0 \
            -to 20.0 \
            -increment 0.1 \
            -show_arrows 1 \
            -resolution 0.01 \
            -value $itk_option(-innerscale) \
            -anchor w \
            -command [code $this configure -innerscale]
      }

      #  Outer scale factor.
      itk_component add OuterScale {
         util::LabelEntryScale $w_.outer \
            -text {Annulus outer scale:} \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -from 1.5 \
            -to 20.5 \
            -increment 0.1 \
            -resolution 0.01 \
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
         util::LabelValue $w_.x -text {X position:} \
            -value 0 \
            -labelwidth $lwidth
      }
      itk_component add Y {
         util::LabelValue $w_.y -text {Y position:} \
            -value 0 \
            -labelwidth $lwidth
      }

      itk_component add Fwhm1 {
         util::LabelValue $w_.fwhm1 -text {Fwhm X:} \
            -value 0 \
            -labelwidth $lwidth
      }

      itk_component add Fwhm2 {
         util::LabelValue $w_.fwhm2 -text {Fwhm Y:} \
            -value 0 \
            -labelwidth $lwidth
      }

#      itk_component add Sky {
#         util::LabelValue $w_.sky -text {Sky value:} \
#            -value 0 \
#            -labelwidth $lwidth
#      }
      itk_component add Angle {
         util::LabelValue $w_.angle -text {Rotation:} \
            -value 0 \
            -labelwidth $lwidth
      }
      itk_component add Code {
         util::LabelValue $w_.code -text {Error code:} \
            -value OK \
            -labelwidth $lwidth
      }

      #  Pack up widget for display.
      pack $itk_component(MainLabel) -anchor center -fill x
      pack $itk_component(X) -fill x
      pack $itk_component(Y) -fill x
      pack $itk_component(Fwhm1) -fill x
      pack $itk_component(Fwhm2) -fill x
      pack $itk_component(Angle) -fill x
      pack $itk_component(Code) -fill x
      pack $itk_component(Sep1) -fill x -ipadx 5
      pack $itk_component(Clip) -fill x
      pack $itk_component(Seeing) -fill x
      pack $itk_component(InnerScale) -fill x
      pack $itk_component(OuterScale) -fill x
   }

   #  Update the display to reflect the values of the given
   #  GaiaPhotomObject.
   public method update_display {object} {
      if { $phottype_ == "aperture" } {
         update_ap_ $object
      } else {
         if { $psf_ } {
            update_psf_ $object
         } else {
            update_opt_ $object
         }
      }
      ::update idletasks ;#  Needed to propagate changes now!
   }

   #  Update values of aperture photometry object.
   protected method update_ap_ {object} {
      lassign [{*}$object object_details] index x y mag magerr sky \
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
      if { [{*}$object cget -shape] == "circle" } {
         set_for_circles 1
      } else {
         set_for_circles 0
      }
   }

   #  Update values of optimal photometry object.
   protected method update_opt_ {object} {
      lassign [{*}$object object_details] index x y mag magerr sky \
         signal code positions itk_option(-innerscale) \
	 itk_option(-outerscale)

      $itk_component(Index) configure -value $index
      $itk_component(X) configure -value $x
      $itk_component(Y) configure -value $y
      $itk_component(Mag) configure -value $mag
      $itk_component(MagErr) configure -value $magerr
      $itk_component(Sky) configure -value $sky
      $itk_component(Signal) configure -value $signal
      $itk_component(Code) configure -value $code

      $itk_component(InnerScale) configure -value $itk_option(-innerscale)
      $itk_component(OuterScale) configure -value $itk_option(-outerscale)

      if { $itk_option(-positions_cmd) != {} } {
         if { $positions == "annulus" } {
            eval $itk_option(-positions_cmd) annulus
         } else {
            eval $itk_option(-positions_cmd) regions
         }
      }
   }

   #  Update values of PSF photometry object.
   protected method update_psf_ {object} {
      lassign [{*}$object object_details] index x y fwhm1 fwhm2 \
         itk_option(-angle) code itk_option(-semimajor) \
         itk_option(-seeing) positions itk_option(-innerscale) \
	 itk_option(-outerscale)

      $itk_component(X) configure -value $x
      $itk_component(Y) configure -value $y
      $itk_component(Fwhm1) configure -value $fwhm1
      $itk_component(Fwhm2) configure -value $fwhm2
      $itk_component(Angle) configure -value $itk_option(-angle)
      $itk_component(Code) configure -value $code
      $itk_component(Clip) configure -value $itk_option(-semimajor)
      $itk_component(Seeing) configure -value $itk_option(-seeing)
      $itk_component(Angle) configure -value $itk_option(-angle)
      $itk_component(InnerScale) configure -value $itk_option(-innerscale)
      $itk_component(OuterScale) configure -value $itk_option(-outerscale)

      if { $itk_option(-positions_cmd) != {} } {
         if { $positions == "annulus" } {
            eval $itk_option(-positions_cmd) annulus
         } else {
            eval $itk_option(-positions_cmd) regions
         }
      }
   }

   #  Reset the displayed values back to their defaults.
   method reset_display {} {
      if { $phottype_ == "aperture" } {
         reset_ap_
      } else {
         if { $psf_ } {
            reset_psf_
         } else {
            reset_opt_
         }
      }
   }

   #  Reset aperture photometry object.
   protected method reset_ap_ {} {
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

   #  Reset optimal photometry object.
   protected method reset_opt_ {} {
      $itk_component(Index) configure -value 0
      $itk_component(X) configure -value 0
      $itk_component(Y) configure -value 0
      $itk_component(Mag) configure -value 0
      $itk_component(MagErr) configure -value 0
      $itk_component(Sky) configure -value 0
      $itk_component(Signal) configure -value 0
      $itk_component(Code) configure -value OK
   }

   #  Reset PSF photometry object.
   protected method reset_psf_ {} {
      $itk_component(X) configure -value 0
      $itk_component(Y) configure -value 0
      $itk_component(Fwhm1) configure -value 0
      $itk_component(Fwhm2) configure -value 0
      $itk_component(Code) configure -value OK

      set itk_option(-semimajor) 5.0
      $itk_component(Clip) configure -value 5.0
      set itk_option(-angle) 0.0
      $itk_component(Angle) configure -value 0.0
      set itk_option(-seeing) 2.0
      $itk_component(Seeing) configure -value 2.0
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
   itk_option define -object_list object_list Object_list {} {
      if { $itk_option(-object_list) != {} } {
         set phottype_ [{*}$itk_option(-object_list) cget -phottype]
         set psf_ [{*}$itk_option(-object_list) cget -psf]
      }
   }

   #  Current semimajor axes of apertures (== radius for circles).
   itk_option define -semimajor semimajor SemiMajor {5} {
      if { $itk_option(-object_list) != {} } {
         {*}$itk_option(-object_list) configure -semimajor $itk_option(-semimajor)
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
         {*}$itk_option(-object_list) configure -innerscale $itk_option(-innerscale)
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
         {*}$itk_option(-object_list) configure -outerscale $itk_option(-outerscale)
      }
   }

   #  Current eccentricity (constrained to line in range 0-1 by scale).
   itk_option define -eccentricity eccentricity Eccentricity {0.0} {
      if { $itk_option(-object_list) != {} } {
         {*}$itk_option(-object_list) configure -eccentricity $itk_option(-eccentricity)
      }
   }

   #  Current position angle (constrained to line in range -180 to 180
   #  by scale).
   itk_option define -angle angle Angle {0.0} {
      if { $itk_option(-object_list) != {} } {
         {*}$itk_option(-object_list) configure -angle $itk_option(-angle)
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

   #  Optimal photometry parameters (clip is semimajor axis).
   itk_option define -seeing seeing Seeing 2.0 {
      if { $itk_option(-object_list) != {} } {
         {*}$itk_option(-object_list) configure -seeing $itk_option(-seeing)
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  The type of photometry objects described in the photometry list.
   protected variable phottype_ aperture
   protected variable psf_ 0

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
