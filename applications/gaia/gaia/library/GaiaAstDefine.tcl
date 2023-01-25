#+
#  Name:
#     GaiaAstDefine

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class for directly creating an image FITS WCS.

#  Description:
#     This class create a toolbox which allows the definition of an
#     AST WCS. The method used is to accept a set of FITS related
#     values from the user and then create an AST FITS channel using
#     these. These are then read to create an WCS object. This can
#     then be used to replace the current WCS object used by GAIA and
#     checked for the correct correspondence. Finally when the user is
#     happy the WCS object can be retained with the image.
#
#     Note that the system of FITS headers used is losely based on the
#     AIPS system, so there is no direct control over PC term, just
#     a single rotation (unless the user is "relaxed") with reference
#     positions and scale factors. The projections allowed are most of
#     those available in FITS-WCS (which may need PROJP values, and
#     LATPOLE/LONGPOLE -- see the FITS-WCS document for more about these).

#  Invocations:
#
#        GaiaAstDefine object_name [configuration options]
#
#     This creates an instance of a GaiaAstDefine object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Configuration options:
#     See itk_option define statements.

#  Methods:
#     See below

#  Inheritance:
#     TopLevelWidget

#  Copyright:
#     Copyright (C) 1997 Central Laboratory of the Research Councils
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
#     08-DEC-1997 (PWD):
#        Original version.
#     23-MAY-2000 (PWD):
#        Renamed GaiaAstDefine.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaAstDefine {}
itcl::class gaia::GaiaAstDefine {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: Known astrometry calibration ($itk_option(-number))"

      #  One off initializations.
      set_projp_
      set_pairings_

      #  Create the short help window.
      make_short_help

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0
      add_short_help $itk_component(menubar).file {File menu: close window}

      #  Add option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      $short_help_win_ add_menu_short_help $File \
         {New window} {Create a new toolbox}

      #  Set the exit menu items.
      $File add command -label {Cancel changes and close window} \
         -command [code $this cancel] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this cancel]
      $File add command -label {Accept changes and close window}\
         -command [code $this accept] \
         -accelerator {Control-a}
      bind $w_ <Control-a> [code $this accept]

      #  Add the options menu
      set Options [add_menubutton "Options"]
      configure_menubutton Options -underline 0
      add_short_help $itk_component(menubar).file {Options menu: expert functions}

      #  Only option is "relax" mode. Using this all state =
      #  disabled functions are disabled (so that for instance two
      #  rotations can be given .
      set offstate_($this) disabled
      $Options add checkbutton  -label {Relax} \
         -variable [scope offstate_($this)] \
         -onvalue normal \
         -offvalue disabled

      #  Add window help.
      add_help_button astrometry "Astrometry Overview..."
      add_help_button define "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      #  Add entry widgets to accept values for all the known
      #  fields. Most of these go into a frame that that is managed by
      #  a table.
      itk_component add f1 {
	  frame $w_.frame1 -borderwidth 3 -relief flat
      }

      #  Get the date of the observations.
      itk_component add space1 {
         LabelRule $itk_component(f1).space1 -text "Dates:"
      }
      itk_component add dateobs {
         util::LabelEntry $itk_component(f1).dateobs \
            -text "Date of observation:" \
            -labelwidth $lwidth_ \
            -valuewidth $vwidth_ \
            -textvariable [scope values_($this,dateobs)]
      }
      add_short_help $itk_component(dateobs) \
         {Format ccyy-mm-ddThh:mm:ss[.sss...]Z (post 2000 dd/mm/yy pre)}

      #  CTYPE, CDELT, CROTA and CRPIX for both axes,
      #  plus LONGPOLE and LATPOLE
      itk_component add space2 {
         LabelRule $itk_component(f1).space2 -text "Transformation:"
      }
      set vwby2 [expr $vwidth_/2]

      #  X axis:
      itk_component add space3 {
         LabelRule $itk_component(f1).space3 -text "X axis:"
      }
      itk_component add ctypelab {
	  label $itk_component(f1).lab1 -text "Coordinate type:"
      }
      itk_component add ctype1 {
         util::LabelMenu $itk_component(f1).ctype1 \
            -relief raised \
            -valuewidth $vwby2
      }
      add_short_help $itk_component(ctype1) \
         {Type of the world coordinate system (FITS synonym)}
      foreach {label value} $ctypemap_ {
         $itk_component(ctype1) add \
            -label "$label ($value)" \
            -value $value \
            -command [code $this set_ctype_ 1 $value]
      }
      itk_component add crpixlab {
	  label $itk_component(f1).lab2 -text "Reference pixel:"
      }
      itk_component add crpix1 {
         util::LabelEntry $itk_component(f1).crpix1 \
            -textvariable [scope values_($this,crpix1)]
      }
      add_short_help $itk_component(crpix1) \
         {X coordinate of reference pixel (1,1 is centre of first pixel)}
      itk_component add crvallab {
	  label $itk_component(f1).lab3 -text "Reference coordinate:"
      }
      itk_component add crval1 {
         util::LabelEntry $itk_component(f1).crval1 \
            -textvariable [scope values_($this,crval1)]
      }
      add_short_help $itk_component(crval1) \
         {World coordinate of reference pixel (hh/dd:mm:ss or degrees)}
      itk_component add cdeltlab {
	  label $itk_component(f1).lab4 -text "Reference increment:"
      }
      itk_component add cdelt1 {
         util::LabelEntry $itk_component(f1).cdelt1 \
            -textvariable [scope values_($this,cdelt1)]
      }
      add_short_help $itk_component(cdelt1) \
         {World coordinate increment at reference pixel (arcsecs)}
      itk_component add crotalab {
	  label $itk_component(f1).lab5 -text "Angle:"
      }
      itk_component add crota1 {
         util::LabelEntryScale $itk_component(f1).crota1 \
            -valuewidth $vwby2 \
            -increment 0.1  \
            -resolution 0.1 \
            -from -180.0 \
            -to 180.0 \
            -show_arrows 1 \
            -anchor w \
            -value 0.0 \
            -command [code $this set_angle_ 1]
      }
      add_short_help $itk_component(crota1) \
         {Rotation of world coordinates at reference pixel (degrees)}

      # Y axis:
      itk_component add space4 {
         LabelRule $itk_component(f1).space4 -text "Y axis:"
      }
      itk_component add ctype2 {
         util::LabelMenu $itk_component(f1).ctype2 \
            -relief raised \
            -valuewidth $vwby2
      }
      add_short_help $itk_component(ctype2) \
         {Type of the world coordinate system (FITS synonym)}
      foreach {label value} $ctypemap_ {
         $itk_component(ctype2) add \
            -label "$label ($value)" \
            -value $value \
            -command [code $this set_ctype_ 2 $value]
      }
      itk_component add crpix2 {
         util::LabelEntry $itk_component(f1).crpix2 \
            -textvariable [scope values_($this,crpix2)]
      }
      add_short_help $itk_component(crpix2) \
         {Y coordinate of reference pixel (1,1 is centre of first pixel)}
      itk_component add crval2 {
         util::LabelEntry $itk_component(f1).crval2 \
            -textvariable [scope values_($this,crval2)]
      }
      add_short_help $itk_component(crval2) \
         {World coordinate of reference pixel (hh/dd:mm:ss or degrees)}
      itk_component add cdelt2 {
         util::LabelEntry $itk_component(f1).cdelt2 \
            -textvariable [scope values_($this,cdelt2)]
      }
      add_short_help $itk_component(cdelt2) \
         {World coordinate increment at reference pixel (arcsecs)}
      itk_component add crota2 {
         util::LabelEntryScale $itk_component(f1).crota2 \
            -valuewidth $vwby2 \
            -increment 0.1  \
            -resolution 0.1 \
            -from -180.0 \
            -to 180.0 \
            -show_arrows 1 \
            -anchor w \
            -value 0.0 \
            -command [code $this set_angle_ 2]
      }
      add_short_help $itk_component(crota2) \
         {Rotation of world coordinates at reference pixel (degrees)}

      # LONGPOLE and LATPOLE
      itk_component add longpole {
         util::LabelEntry $itk_component(f1).longpole \
            -text "Longpole:" \
            -labelwidth $lwidth_ \
            -textvariable [scope values_($this,longpole)]
      }
      add_short_help $itk_component(longpole) \
         {Longitude of system northpole}
      itk_component add latpole {
         util::LabelEntry $itk_component(f1).latpole \
            -text "Latpole:" \
            -labelwidth $lwidth_ \
            -textvariable [scope values_($this,latpole)]
      }
      add_short_help $itk_component(latpole) \
         {Latitude of system northpole}

      #  RADECSYS and EQUINOX.
      itk_component add space5 {
         LabelRule $itk_component(f1).space5 -text "Fundamental coordinate system:"
      }

      #  radecsys.
      itk_component add system {
         util::LabelMenu $itk_component(f1).system \
            -text "System:" \
            -relief raised \
            -labelwidth $lwidth_ \
      }
      add_short_help $itk_component(system) \
         {Frame of reference for RA/Dec systems}
      foreach {system needequinox} $systemattrib_ {
         $itk_component(system) add \
            -command [code $this set_system_ $system $needequinox] \
            -label $system \
            -value $system
      }

      #  Equinox, J2000 or B1950 usually.
      itk_component add equinox {
         LabelEntryMenu $itk_component(f1).equinox \
            -text "Equinox:" \
            -labelwidth $lwidth_ \
            -valuewidth $vwidth_ \
            -textvariable [scope values_($this,equinox)]
      }
      add_short_help $itk_component(equinox) \
         {Epoch of mean equator and equinox (decimal years)}
      foreach equinox $equinoxmap_ {
         $itk_component(equinox) add \
            -label $equinox \
            -value $equinox \
            -command [code $this set_equinox_ $equinox]
      }

      #  Add a section to get the projection type and any necessary
      #  PROJP parameters (note we don't allow ZPN as this requires
      #  another 8 PROJP's!
      itk_component add space6 {
         LabelRule $itk_component(f1).space6 -text "Projection:"
      }
      itk_component add proj {
         util::LabelMenu $w_.proj \
            -text "Type:" \
            -relief raised \
            -labelwidth $lwidth_ \
      }
      add_short_help $itk_component(proj) \
         {Method used to project celestial sphere onto image}
      foreach {label value} $projectmap_ {
         $itk_component(proj) add \
            -label "$label ($value)" \
            -value $value \
            -command [code $this set_proj_ $value]
      }

      itk_component add projp1 {
         util::LabelEntry $itk_component(f1).projp1 \
            -text "projp1:" \
            -labelwidth $lwidth_ \
            -valuewidth $vwidth_ \
            -textvariable [scope values_($this,projp1)]
      }
      add_short_help $itk_component(projp1) \
         {First projection parameter (PROJP1)}

      itk_component add projp2 {
         util::LabelEntry $itk_component(f1).projp2 \
            -text "projp2:" \
            -labelwidth $lwidth_ \
            -valuewidth $vwidth_ \
            -textvariable [scope values_($this,projp2)]
      }
      add_short_help $itk_component(projp2) \
         {Second projection parameter (PROJP2)}

      #  Create the button bar
      itk_component add actionframe {frame $w_.action}

      #  Add a button to close window and accept the new WCS.
      itk_component add accept {
         button $itk_component(actionframe).accept -text Accept \
            -command [code $this accept]
      }
      add_short_help $itk_component(accept) \
         {Accept astrometric calibration and close window}

      #  Add a button to test the WCS.
      itk_component add test {
         button $itk_component(actionframe).test -text Test \
            -command [code $this test]
      }
      add_short_help $itk_component(test) \
         {Write new astrometric calibration to image for test purposes}

      #  Add a button to close window and not accept the new WCS.
      itk_component add cancel {
         button $itk_component(actionframe).cancel -text Cancel \
            -command [code $this cancel]
      }
      add_short_help $itk_component(cancel) \
         {Close window and restore original astrometric calibration}

      #  Add a button to reset the entries and return to the original
      #  image WCS.
      itk_component add reset {
         button $itk_component(actionframe).reset -text Reset \
            -command [code $this reset_]
      }
      add_short_help $itk_component(reset) {Reset window and image to defaults}

      #  Reset all fields (thereby initialising them).
      reset_

      #  Now create and pack a table with all the above.
      blt::blttable $itk_component(f1) \
	      $itk_component(space1) 0,0 -cspan 3 -fill x \
	      $itk_component(dateobs) 1,0 -cspan 2 -fill x \
	      $itk_component(space2) 2,0 -cspan 3 -fill x \
	      $itk_component(ctypelab) 4,0 -anchor w \
	      $itk_component(crpixlab) 5,0 -anchor w \
	      $itk_component(crvallab) 6,0 -anchor w \
	      $itk_component(cdeltlab) 7,0 -anchor w \
	      $itk_component(crotalab) 8,0 -anchor w \
	      $itk_component(space3) 3,1 -fill x \
	      $itk_component(ctype1) 4,1 -fill x \
	      $itk_component(crpix1) 5,1 -fill x \
	      $itk_component(crval1) 6,1 -fill x \
	      $itk_component(cdelt1) 7,1 -fill x \
	      $itk_component(crota1) 8,1 -fill x \
	      $itk_component(space4) 3,2 -fill x \
	      $itk_component(ctype2) 4,2 -fill x \
	      $itk_component(crpix2) 5,2 -fill x \
	      $itk_component(crval2) 6,2 -fill x \
	      $itk_component(cdelt2) 7,2 -fill x \
	      $itk_component(crota2) 8,2 -fill x \
	      $itk_component(longpole) 9,0 -cspan 2 -fill x \
	      $itk_component(latpole) 10,0 -cspan 2 -fill x \
              $itk_component(space5)  11,0 -cspan 3 -fill x \
	      $itk_component(system)  12,0 -cspan 2 -fill x \
	      $itk_component(equinox) 13,0 -cspan 2 -fill x \
	      $itk_component(space6)  14,0 -cspan 3 -fill x \
	      $itk_component(proj)    15,0 -cspan 3 -fill x \
	      $itk_component(projp1)  16,0 -cspan 2 -fill x \
	      $itk_component(projp2)  17,0 -cspan 2 -fill x


      #  Pack all other widgets into place.
      pack $itk_component(f1) -side top -fill both -expand 1

      pack $itk_component(actionframe) -side bottom -fill x -pady 5 -padx 5
      pack $itk_component(accept) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(cancel) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(reset)  -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(test)   -side right -expand 1 -pady 3 -padx 3
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Create a new instance of this object.
   protected method clone_me_ {} {
      if { $itk_option(-clone_cmd) != {} } {
         eval $itk_option(-clone_cmd)
      }
   }

   #  Withdraw this window without accepting any new WCS information.
   public method cancel {} {

      #  Restore WCS system to the original (if available).
      if { $itk_option(-rtdimage) != {} } {
         catch {
            $itk_option(-rtdimage) astrestore original
         }
         notify_
      }
      if { $itk_option(-really_die) } {
         delete object $this
      } else {
         wm withdraw $w_
      }
   }

   #  Withdraw window and write new WCS to image permanently.
   public method accept {} {
      set_wcs_
      $itk_option(-rtdimage) astfix
      if { $itk_option(-really_die) } {
         delete object $this
      } else {
         wm withdraw $w_
      }
   }

   #  Test the WCS by making it the current system.
   public method test {} {
      set_wcs_
   }

   #  Reset all fields to their default values and retore the original
   #  image WCS.
   protected method reset_ {} {
      set_angle_ 1 0.0
      set_ctype_ 1 {RA--}

      set values_($this,crpix1) [expr [$itk_option(-rtdimage) width]/2]
      $itk_component(crpix1) configure -value  $values_($this,crpix1)
      set values_($this,crpix2) [expr [$itk_option(-rtdimage) height]/2]
      $itk_component(crpix2) configure -value  $values_($this,crpix2)

      set values_($this,crval1) 0.0
      $itk_component(crval1) configure -value 0.0
      set values_($this,crval2) 0.0
      $itk_component(crval2) configure -value 0.0

      set values_($this,cdelt1) 1.0
      $itk_component(cdelt1) configure -value 1.0
      set values_($this,cdelt2) 1.0
      $itk_component(cdelt2) configure -value 1.0

      set_proj_ -TAN
      $itk_component(proj) configure -value -TAN

      set_system_ FK5 1
      set values_($this,equinox) {}

      set values_($this,projp1) {}
      $itk_component(projp1) configure -value {}
      set values_($this,projp2) {}
      $itk_component(projp2) configure -value {}

      set values_($this,longpole) {}
      $itk_component(longpole) configure -value {}
      set values_($this,latpole) {}
      $itk_component(latpole) configure -value {}

      #  The date defaults.
      set values_($this,date) {}

      #  Restore WCS system to the original (if available).
      if { $itk_option(-rtdimage) != {} } {
         catch {
            $itk_option(-rtdimage) astrestore original
         }
         notify_
      }
   }

   #  Set the WCS system of the main image to be one based on the
   #  values given in the entry fields.
   protected method set_wcs_ {} {

      #  Method is simple. Read all the values and construct a FITS
      #  channel filled with cards. Then attempt to create an AST
      #  object which can be used to replace the existing WCS system.
      if { $itk_option(-rtdimage) != {} } {
         set image $itk_option(-rtdimage)
         set chan [$image astcreate]
         $image aststore $chan CTYPE1 "'$values_($this,ctype1)$values_($this,proj)'"
         $image aststore $chan CTYPE2 "'$values_($this,ctype2)$values_($this,proj)'"
         $image aststore $chan CRVAL1 [wcs_convert_ $values_($this,ctype1) \
                                          $values_($this,crval1)]
         $image aststore $chan CRVAL2 [wcs_convert_ $values_($this,ctype2) \
                                          $values_($this,crval2)]
         $image aststore $chan CDELT1 [expr $values_($this,cdelt1)/3600.0]
         $image aststore $chan CDELT2 [expr $values_($this,cdelt2)/3600.0]
         $image aststore $chan CRPIX1 $values_($this,crpix1)
         $image aststore $chan CRPIX2 $values_($this,crpix2)
         $image aststore $chan CROTA1 $values_($this,crota1)
         $image aststore $chan CROTA2 $values_($this,crota2)
         if { $values_($this,dateobs) != {} } {
            $image aststore $chan DATE-OBS "'$values_($this,dateobs)'"
         }
         if { $values_($this,system) != {} } {
            $image aststore $chan RADECSYS "'$values_($this,system)'"
            if { $values_($this,equinox) != {} } {
               $image aststore $chan EQUINOX $values_($this,equinox)
            }
         }

         #  If needed for this projection, and available add the PROJP
         #  values.
         if { $projpmap_($values_($this,proj),1) } {
            if { $values_($this,projp1) != {} } {
               $image aststore $chan PROJP1 $values_($this,projp1)
            }
         }
         if { $projpmap_($values_($this,proj),2) } {
            if { $values_($this,projp2) != {} } {
               $image aststore $chan PROJP2 $values_($this,projp2)
            }
         }

         #  If set add LONGPOLE and LATPOLE.
         if { $values_($this,longpole) != {} } {
            $image aststore $chan LONGPOLE $values_($this,longpole)
         }
         if { $values_($this,latpole) != {} } {
            $image aststore $chan LATPOLE $values_($this,longpole)
         }

         #  Read the channel to create an AST object and then replace
         #  the current WCS using it.
         $image astread $chan
         $image astreplace
         notify_
         $image astdelete $chan
      }
   }

   #  Record the value of the selected RADECSYS and configure the
   #  equinox entry window appropriately.
   protected method set_system_ {value needequinox} {
      set values_($this,system) $value
      $itk_component(system) configure -value $value
      if { ! $needequinox } {
         $itk_component(equinox) configure -value {}
         $itk_component(equinox) configure -state $offstate_($this)
      } else {
         $itk_component(equinox) configure -state normal
      }
   }

   #  Set the value of the given axis coordinate type and also reset
   #  the corresponding axis to the proper pair and disable the
   #  longitude angle field. Finally disable the system selection
   #  unless the ctype is "RA--", "DEC-".
   protected method set_ctype_ {axis value} {

      set values_($this,ctype$axis) $value
      if { $axis == 1 } {
         set values_($this,ctype2) $ctypepairs_($value)
         $itk_component(ctype2) configure -value $ctypepairs_($value)
         if { $ctypepairs_($value,type) == "lat" } {
            $itk_component(crota2) configure -value 0.0
            set values_($this,crota2) 0.0
            $itk_component(crota1) configure -state normal
            $itk_component(crota2) configure -state $offstate_($this)
         } else {
            $itk_component(crota1) configure -value 0.0
            set values_($this,crota1) 0.0
            $itk_component(crota1) configure -state $offstate_($this)
            $itk_component(crota2) configure -state normal
         }
      } else {
         set values_($this,ctype1) $ctypepairs_($value)
         $itk_component(ctype1) configure -value $ctypepairs_($value)
         if { $ctypepairs_($value,type) == "lat" } {
            $itk_component(crota1) configure -value 0.0
            set values_($this,crota1) 0.0
            $itk_component(crota1) configure -state $offstate_($this)
            $itk_component(crota2) configure -state normal
         } else {
            $itk_component(crota2) configure -value 0.0
            set values_($this,crota2) 0.0
            $itk_component(crota1) configure -state normal
            $itk_component(crota2) configure -state $offstate_($this)
         }
      }
      if { $value == {RA--} || $value == {DEC-} } {
         $itk_component(system) configure -state normal
         set text [[$itk_component(system) component mb] cget -text]
         if { $text == "Ecliptic" || $text == "Galactic" } {
            set_system_ FK5 1
         }
      } else {
         $itk_component(system) configure -state disabled
	  if { $value == {ELON} || $value == {ELAT} } {
	      [$itk_component(system) component mb] configure -text Ecliptic
	  } else {
	      [$itk_component(system) component mb] configure -text Galactic
	  }
	  set values_($this,system) {}
      }
   }

   #  Set the value of the equinox entry window.
   protected method set_equinox_ {value} {
      set values_($this,equinox) $value
      $itk_component(equinox) configure -value $value
   }

   #  Set the valid latitude/longitude pairings and record if latitude
   #  or longitude
   protected method set_pairings_ {} {
      if { ! [info exists ctypepairs_(RA--)] } {
         set ctypepairs_(RA--) DEC-
         set ctypepairs_(RA--,type) long
         set ctypepairs_(DEC-) RA--
         set ctypepairs_(DEC-,type) lat
         set ctypepairs_(GLON) GLAT
         set ctypepairs_(GLON,type) long
         set ctypepairs_(GLAT) GLON
         set ctypepairs_(GLAT,type) lat
         set ctypepairs_(ELON) ELAT
         set ctypepairs_(ELON,type) long
         set ctypepairs_(ELAT) ELON
         set ctypepairs_(ELAT,type) lat
         set ctypepairs_(SLON) SLAT
         set ctypepairs_(SLON,type) long
         set ctypepairs_(SLAT) SLON
         set ctypepairs_(SLAT,type) lat
         set ctypepairs_(HLON) HLAT
         set ctypepairs_(HLON,type) long
         set ctypepairs_(HLAT) HLON
         set ctypepairs_(HLAT,type) lat
      }
   }

   #  Set the value of the rotation of the latitude axis (AIPS
   #  convention for now).
   protected method set_angle_ {axis value} {
      set values_($this,crota$axis) $value
      if { $axis == 1 } {
         set values_($this,crota2) 0.0
         $itk_component(crota2) configure -value 0.0
      } else {
         set values_($this,crota1) 0.0
         $itk_component(crota1) configure -value 0.0
      }
   }

   #  Set the projection type. Also enables the PROJP section if
   #  necessary.
   protected method set_proj_ {proj} {
      set values_($this,proj) $proj
      if { $projpmap_($proj,1) } {
         $itk_component(projp1) configure -state normal
      } else {
	 $itk_component(projp1) configure -value {}
         $itk_component(projp1) configure -state $offstate_($this)
      }
      if { $projpmap_($proj,2) } {
         $itk_component(projp2) configure -state normal
      } else {
	 $itk_component(projp2) configure -value {}
         $itk_component(projp2) configure -state $offstate_($this)
      }
   }

   #  Set which PROJP parameters are required for which projections.
   protected method set_projp_ {} {
      if { ![info exists projpmap_(-TAN,1)] } {
         set projpmap_(-TAN,1) 0
         set projpmap_(-TAN,2) 0
         set projpmap_(-SIN,1) 1
         set projpmap_(-SIN,2) 1
         set projpmap_(-ARC,1) 0
         set projpmap_(-ARC,2) 0
         set projpmap_(-AZP,1) 1
         set projpmap_(-AZP,2) 0
         set projpmap_(-STG,1) 0
         set projpmap_(-STG,2) 0
         set projpmap_(-ZEA,1) 0
         set projpmap_(-ZEA,2) 0
         set projpmap_(-AIR,1) 1
         set projpmap_(-AIR,2) 0
         set projpmap_(-CYP,1) 1
         set projpmap_(-CYP,2) 1
         set projpmap_(-CAR,1) 0
         set projpmap_(-CAR,2) 0
         set projpmap_(-MER,1) 0
         set projpmap_(-MER,2) 0
         set projpmap_(-CEA,1) 1
         set projpmap_(-CEA,2) 0
         set projpmap_(-COP,1) 1
         set projpmap_(-COP,2) 1
         set projpmap_(-COD,1) 1
         set projpmap_(-COD,2) 1
         set projpmap_(-COE,1) 1
         set projpmap_(-COE,2) 1
         set projpmap_(-COO,1) 1
         set projpmap_(-COO,2) 1
         set projpmap_(-BON,1) 1
         set projpmap_(-BON,2) 0
         set projpmap_(-PCO,1) 0
         set projpmap_(-PCO,2) 0
         set projpmap_(-GLS,1) 0
         set projpmap_(-GLS,2) 0
         set projpmap_(-PAR,1) 0
         set projpmap_(-PAR,2) 0
         set projpmap_(-AIT,1) 0
         set projpmap_(-AIT,2) 0
         set projpmap_(-MOL,1) 0
         set projpmap_(-MOL,2) 0
         set projpmap_(-CSC,1) 0
         set projpmap_(-CSC,2) 0
         set projpmap_(-QSC,1) 0
         set projpmap_(-QSC,2) 0
         set projpmap_(-TSC,1) 0
         set projpmap_(-TSC,2) 0
      }
   }

   #  Convert an hh/dd:mm:ss string to degrees if necessary.
   protected method wcs_convert_ {ctype value} {
      if { [regexp {([\+\-]*)(.*):(.*):(.*)} $value all sign hhdd mm ss] } {

         #  Check hhdd, mm and ss for leading 0. This chokes expr as
         #  it is taken to mean an octal value.
         if { [string index $hhdd 0] == 0 && [string length $hhdd] > 1 } {
            set hhdd [string index $hhdd 1]
         }
         if { [string index $mm 0] == 0 && [string length $mm] > 1 } {
            set mm [string index $mm 1]
         }
         if { [string index $ss 0] == 0 && [string length $ss] > 1 } {
            set ss [string index $ss 1]
         }
         set result [expr (($ss/60.0)+$mm)/60.0+$hhdd]
         if { $sign == "-" } {
            set result [expr -1.0*$result]
         } else {
            if { $ctype == "RA--" } {
               set result [expr $result*15.0]
            }
         }
         return $result
      } else {
         return $value
      }
   }

   #  Do the notify_cmd option if needed.
   protected method notify_ {} {
      if { $itk_option(-notify_cmd) != {} } {
         eval $itk_option(-notify_cmd)
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of starrtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {} {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Command to execute when the WCS is changed.
   itk_option define -notify_cmd notify_cmd Notify_Cmd {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  If this is a clone, then it should die rather than be withdrawn.
   itk_option define -really_die really_die Really_Die 0

   #  Protected variables: (available to instance)
   #  --------------------

   #  Width of labels & value fields.
   protected variable lwidth_ 18
   protected variable vwidth_ 18

   #  The known types of coordinates and their long descriptions.
   protected variable ctypemap_ \
      { {Right Ascension} {RA--} {Declination} {DEC-}
         {Galactic Longitude} GLON {Galactic Latitude} GLAT
         {Ecliptic Longitude} ELON {Ecliptic Latitude} ELAT
         {Supergalatic Longitude} SLON {Supergalactic Latitude} SLAT}
# -- not implemented {Helioecliptic Longitude} HLON {Helioecliptic Latitude} HLAT

   #  The available projections and their long descriptions (note no
   #  ZPN, requires too many PROJP's).
   protected variable projectmap_ \
      { {Gnomic (tangent plane)} -TAN {Orthographic} -SIN
         {Zenithal equidistant} -ARC {Zenithal perspective} -AZP
         {Sterographic} -STG
         {Zenithal equal-area} -ZEA {Airy} -AIR
         {Cylindrical perspective} -CYP {Cartesian} -CAR {Mercator} -MER
         {Cylindrical equal area} -CEA {Conical perspective} -COP
         {Conical equidistant} -COD {Conical equal-area} -COE
         {Conical orthomorthic} -COO {Bonne's equal area} -BON
         {Polyconic} -PCO {Sinusoidal} -GLS {Parabolic} -PAR
         {Hammer-Aitoff} -AIT {Mollweide} -MOL
         {Cobe Quadtrilaterized Spherical Cube} -CSC
         {Quadtrilaterized Spherical Cube} -QSC
         {Tangential Spherical Cube} -TSC}

   #  Names of all the possible RA/DEC fundamental coordinate
   #  systems. The values following these are the need for an equinox.
   protected variable systemattrib_ {FK5 1 FK4 1 FK4-NO-E 1 GAPPT 0 {} 0}

   #  Names of sensible some equinoxes.
   protected variable equinoxmap_ {2000.0 1950.0}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  The pairings of the coordinate types (RA only with DEC etc).
   common ctypepairs_

   #  Which PROJP parameters are required for which projections.
   common projpmap_

   #  Variable to share amongst all widgets. This is indexed by the
   #  object name ($this)
   common values_

   #  Relax mode. State of potentially disabled functions.
   common offstate_

#  End of class definition.
}
