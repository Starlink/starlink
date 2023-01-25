#+
#  Name:
#     GaiaAstReference

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class that create a toolbox for creating a WCS system
#     for an image using reference positions.

#  Description:
#     This class create a toolbox which allows the creation of an
#     AST WCS. The method used is to get a minimal set of system and
#     projection information from the user (including the Equinox) and
#     then to relate a series of positions on the image with X, Y and
#     Lat, Long. Using the projection and table coordinate system
#     information a simple FrameSet to map from sky coordinates to
#     image coordinates is created, which is in turn refined (twice)
#     using a linear fit from the image coordinates that this produces
#     to the X and Y positions actually given. Note that little
#     control over the linear fit is given (needs at least an offset and
#     magnification anyway), except to exclude shear terms or define
#     that the axes are already aligned (hence no PC matrix).
#
#     Note that the celestial coordinate system of the fit is
#     restricted to being that of the reference positions (which are
#     displayed in the table). If this needs to be changed then an
#     additional level of control is available via the GaiaAstSystem
#     toolbox.

#  Invocations:
#
#        GaiaAstReference object_name [configuration options]
#
#     This creates an instance of a GaiaAstReference object. The return is
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
#     See method definitions below.

#  Inheritance:
#     TopLevelWidget.

#  Copyright:
#     Copyright (C) 1998-2000 Central Laboratory of the Research Councils
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
#     19-DEC-1997 (PWD):
#        Original version.
#     20-JAN-1998 (PWD):
#        Rewrite to separate image coordinate system information from
#        image related stuff.
#     23-MAY-2000 (PWD):
#        Changed name to GaiaAstReference.
#     10-APR-2001 (PWD):
#        Change to report RMS of fit in mean arcsecs.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaAstReference {}

itcl::class gaia::GaiaAstReference {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ \
         "GAIA: Fit astrometry reference positions ($itk_option(-number))"

      #  One off initializations.
      set_projp_

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

      #  And the options to save and read reference positions.
      $File add command -label {Write positions to file...} \
         -command [code $this save_positions_] \
         -accelerator {Control-s}
      bind $w_ <Control-s> [code $this save_positions_]
      $short_help_win_ add_menu_short_help $File \
         {Write positions to file...}\
         {Write the displayed positions out to an ordinary text file}

      $File add command -label {Read positions from a file...} \
         -command [code $this read_positions_] \
         -accelerator {Control-r}
      bind $w_ <Control-r> [code $this read_positions_]
      $short_help_win_ add_menu_short_help $File \
         {Read positions from a file...} \
      {Read a suitable set of positions from an ordinary text file}

      #  Set the exit menu items.
      $File add command -label {Cancel changes and close window   } \
         -command [code $this cancel] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this cancel]
      $File add command -label {Accept changes and close window   } \
         -command [code $this accept] \
         -accelerator {Control-a}
      bind $w_ <Control-a> [code $this accept]

      #  Add window help.
      add_help_button astrometry "Astrometry Overview..."
      add_help_button reference "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      # Edit menu
      set Edit [add_menubutton Edit]

      #  Options menu. Set the fittype. Used to refine the initial
      #  guess to the reference positions, only control offerred is to
      #  stop any shear terms.
      set m [add_menubutton Options]
      add_short_help $itk_component(menubar).options {Set additional options}
      $m add cascade -label {Fit options} -menu [menu $m.fittype]
      $short_help_win_ add_menu_short_help $m {Fit options} \
         {Set the type of fit used to match the positions}
      set fithelp(3) {Assume axes are aligned}
      set fithelp(4) {Use one image scale with rotation}
      set fithelp(5) {Allow shearing terms}
      foreach i {3 4 5} {
         $m.fittype add radiobutton \
            -value $i \
            -label $fithelp($i) \
            -variable [scope values_($this,fittype)] \
            -command [code $this configure -fittype $i]
      }

      #  Add option to define a different centroid maxshift/search box.
      set values_($this,isize) $itk_option(-isize)
      $m add cascade -label {Centroid search box} -menu [menu $m.isize]
      $short_help_win_ add_menu_short_help $m {Centroid search box} \
         {Change the search box used when locating centroids (pixels)}
      for {set i 3} {$i < 22} {incr i} {
         $m.isize add radiobutton \
            -value $i \
            -label $i \
            -variable [scope values_($this,isize)] \
            -command [code $this configure -isize $i]
      }
      set values_($this,maxshift) $itk_option(-maxshift)
      $m add cascade -label {Centroid max shift} -menu [menu $m.maxshift]
      $short_help_win_ add_menu_short_help $m {Centroid max shift} \
         {Change the maximum shift from initial position (pixels)}
      for {set i 3.5} { $i < 22} {set i [expr $i+1.0]} {
         $m.maxshift add radiobutton \
            -value $i \
            -label $i \
            -variable [scope values_($this,maxshift)] \
            -command [code $this configure -maxshift $i]
      }

      #  Allow the user to define the less useful values.
      $m add command -label {Additional parameters...   } \
	      -command [code $this show_additional_] \
	      -accelerator {Control-p}
      bind $w_ <Control-p> [code $this show_additional_]
      $short_help_win_ add_menu_short_help $m \
	      {Additional parameters...   } \
	      {Set reference pixel positions, latpole and longpole}

      #  Markers menu
      set Markers [add_menubutton Markers]

      #  Add the table for displaying the reference coordinate
      #  positions and some basic controls for convenience. Also
      #  adds to the edit menu and the markers menu which controls
      #  the apperance of the graphics markers.
      itk_component add table {
         GaiaAstTable $w_.table \
		 -editmenu $Edit \
		 -markmenu $Markers \
		 -rtdimage $itk_option(-rtdimage) \
		 -canvas $itk_option(-canvas) \
		 -image $itk_option(-image) \
		 -notify_cmd [code $this fix_equinox_]
      }
      add_short_help $itk_component(table) \
         {Reference sky positions and their ideal/current X,Y places}

      #  Add a switch for controlling if the graphics markers move
      #  one-by-one or all together.
      itk_component add coupled {
	  StarLabelCheck $w_.coupled \
		  -text "Move markers individually:" \
		  -onvalue 0 \
		  -offvalue 1 \
		  -variable [scope values_($this,coupled)] \
		  -command [code $this set_coupled_]
      }
      set default_(coupled) 0
      add_short_help $itk_component(coupled) \
	      {Move markers individually or all together}

      #  Add control for pop-up window that allows the selection of
      #  reference positions in other images.
      itk_component add transfer {
         button $w_.transfer \
            -text "Transfer" \
            -command [code $this start_transfer_]
      }
      add_short_help $itk_component(transfer) \
         {Transfer reference RA/Dec positions from other images}

      #  Add entry widgets that specify the coordinate types, system
      #  etc. of the table values.
      itk_component add space1 {
         LabelRule $w_.space1 -text "Parameters for table coordinates:"
      }

      #  Coordinate type for the table data (both CTYPES).
      itk_component add ctype {
         util::LabelMenu $w_.ctype -relief raised \
	    -valuewidth $vwidth_ \
	    -labelwidth $lwidth_ \
	    -text "Coordinate type:"
      }
      add_short_help $itk_component(ctype) \
         {Type of coordinates in table}
      foreach {lname value ctype1 ctype2} $ctypemap_ {
         $itk_component(ctype) add \
            -label $lname \
            -value $value \
            -command [code $this set_ctype_ $value]
      }

      #  RADECSYS, EQUINOX and EPOCH of table values.
      itk_component add system {
         util::LabelMenu $w_.system \
            -relief raised \
	    -labelwidth $lwidth_ \
	    -valuewidth $vwidth_ \
	    -text "Coordinate system:"
      }
      add_short_help $itk_component(system) \
	      {Table coordinates frame of reference for RA/Dec systems}
      foreach {system needequinox needepoch} $systemattrib_ {
	  $itk_component(system) add \
		  -command [code $this set_system_ $system $needequinox $needepoch] \
		  -label $system \
		  -value $system
      }

      #  Equinox, J2000 or B1950 usually.
      itk_component add equinox {
         LabelEntryMenu $w_.equinox \
            -textvariable [scope values_($this,equinox)] \
	    -labelwidth $lwidth_ \
	    -valuewidth $vwidth_ \
	    -text "Equinox:"
      }
      add_short_help $itk_component(equinox) \
         {Equinox of table coordinates ((B/J)decimal years)}
      foreach equinox $equinoxmap_ {
         $itk_component(equinox) add \
            -label $equinox \
            -value $equinox \
            -command [code $this set_equinox_ $equinox]
      }

      #  Epoch, only needed for FK4 really.
      itk_component add epoch {
         LabelEntryMenu $w_.epoch \
            -textvariable [scope values_($this,epoch)] \
	    -labelwidth $lwidth_ \
	    -valuewidth $vwidth_ \
	    -text "Epoch:"
      }
      add_short_help $itk_component(epoch) \
         {Epoch of table coordinates ((B/J)decimal years)}
      foreach equinox $equinoxmap_ {
         $itk_component(epoch) add \
            -label $equinox \
            -value $equinox \
            -command [code $this set_epoch_ $equinox]
      }

      #  Add a section to get the projection type and any necessary
      #  PROJP parameters (note we don't allow ZPN as this requires
      #  another 8 PROJP's!
      itk_component add space2 {
         LabelRule $w_.space2 -text "Image parameters:"
      }
      itk_component add proj {
	  util::LabelMenu $w_.proj -relief raised \
		  -labelwidth $lwidth_ \
		  -valuewidth 40 \
		  -text "Projection type:"
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
         util::LabelEntry $w_.projp1 \
		 -textvariable [scope values_($this,projp1)] \
		 -labelwidth $lwidth_ \
		 -valuewidth $vwidth_ \
		 -text "projp1:"
      }
      add_short_help $itk_component(projp1) \
	      {First projection parameter (PROJP1)}

      itk_component add projp2 {
	  util::LabelEntry $w_.projp2 \
		  -textvariable [scope values_($this,projp2)] \
		  -labelwidth $lwidth_ \
		  -valuewidth $vwidth_ \
		  -text "projp2:"
      }
      add_short_help $itk_component(projp2) \
         {Second projection parameter (PROJP2)}

      #  Check if X axes is to be associated with longitude or
      #  latitude axis.
      itk_component add xislong {
         util::LabelMenu $w_.xislong -relief raised \
            -labelwidth $lwidth_ \
            -valuewidth $vwidth_ \
            -text "X coordinate type:"
      }
      add_short_help $itk_component(xislong) \
         {Celestial coordinate type of image X axis}
      $itk_component(xislong) add -label "RA/Longitude" -value 1 \
         -command [code $this set_xislong_ 1]
      $itk_component(xislong) add -label "Dec/Latitude" -value 0 \
         -command [code $this set_xislong_ 0]

      #  Make window for additional items.
      make_additional_

      #  Create the button bar
      itk_component add actionframe {frame $w_.action}

      #  Add a button to close window and accept the new WCS.
      itk_component add accept {
         button $itk_component(actionframe).accept -text Accept \
            -command [code $this accept]
      }
      add_short_help $itk_component(accept) \
         {Accept new astrometric calibration and close window}

      #  Add a button to test the WCS.
      itk_component add test {
         button $itk_component(actionframe).test -text {Fit/Test} \
            -command [code $this test]
      }
      add_short_help $itk_component(test) \
         {Perform astrometric calibration fit and assign result to image}

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
      add_short_help $itk_component(reset) \
         {Reset image and window to defaults}

      #  Add a scrollbox to display results of fit.
      itk_component add results {
         Scrollbox $w_.results -height 3 -singleselect 0
      }
      add_short_help $itk_component(results) {Results of fit}

      #  Reset all fields (thereby initialising them).
      reset_

      #  Pack all other widgets into place.
      pack $itk_component(table) -side top -fill both -expand 1
      pack $itk_component(transfer) -side top -pady 1 -padx 1
      pack $itk_component(coupled) -side top -fill x -pady 3 -padx 3
      pack $itk_component(space1) -side top -fill x -pady 5 -padx 5
      pack $itk_component(ctype) -side top -pady 2 -padx 2 -anchor w
      pack $itk_component(system) -side top -pady 2 -padx 2 -anchor w
      pack $itk_component(equinox) -side top -pady 2 -padx 2 -anchor w
      pack $itk_component(epoch) -side top -pady 2 -padx 2 -anchor w
      pack $itk_component(space2) -side top -fill x -pady 5 -padx 5
      pack $itk_component(proj) -side top -pady 2 -padx 2 -anchor w
      pack $itk_component(projp1) -side top -pady 2 -padx 2 -anchor w
      pack $itk_component(projp2) -side top -pady 2 -padx 2 -anchor w
      pack $itk_component(xislong) -side top -pady 2 -padx 2 -anchor w

      pack $itk_component(actionframe) -fill x -side top -pady 3 -padx 3
      pack $itk_component(accept) -side right -expand 1 -pady 2 -padx 2
      pack $itk_component(cancel) -side right -expand 1 -pady 2 -padx 2
      pack $itk_component(reset)  -side right -expand 1 -pady 2 -padx 2
      pack $itk_component(test)   -side right -expand 1 -pady 2 -padx 2

      pack $itk_component(results) -side bottom -fill x -pady 3 -padx 3
   }

   #  Destructor:
   #  -----------
   destructor  {
      if { [winfo exists $add_] } {
         delete object $add_
      }
      if { [winfo exists $trantop_] } {
         delete object $trantop_
      }
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
            notify_
         }
      }
      set testing_ 0
      clear_
      if { $itk_option(-really_die) } {
         delete object $this
      } else {
         wm withdraw $w_
      }
   }

   #  Withdraw window and write new WCS to image -- permanently.
   public method accept {} {
      if { !$testing_ } {
         set_wcs_
      }
      clear_
      set testing_ 0
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
      set testing_ 1
   }

   #  Reset all fields to their default values and retore the original
   #  image WCS.
   protected method reset_ {} {
      $itk_component(xislong) configure -value 1
      set_xislong_ 1

      $itk_component(ctype) configure -value Equatorial
      set_ctype_ Equatorial

      set_proj_ -TAN
      $itk_component(proj) configure -value -TAN

      set_equinox_ 2000
      set_epoch_ {}
      set_system_ FK5 1 0

      set values_($this,projp1) {}
      $itk_component(projp1) configure -value {}
      set values_($this,projp2) {}
      $itk_component(projp2) configure -value {}

      set values_($this,crpix1) [expr [$itk_option(-rtdimage) width]/2]
      $add_.crpix1 configure -value $values_($this,crpix1)
      set values_($this,crpix2) [expr [$itk_option(-rtdimage) height]/2]
      $add_.crpix2 configure -value $values_($this,crpix2)

      set values_($this,longpole) {}
      $add_.longpole configure -value {}
      set values_($this,latpole) {}
      $add_.latpole configure -value {}

      set values_($this,coupled) 0
      set_coupled_

      #  Restore WCS system to the original (if available).
      if { $itk_option(-rtdimage) != {} } {
         catch {
            $itk_option(-rtdimage) astrestore original
         }
	 $itk_component(table) update_x_and_y
         notify_
      }
      set testing_ 0
   }

   #  Set the WCS system of the main image to be one based on the
   #  values given in the entry fields and then do a fit to the X and
   #  Y positions.
   protected method set_wcs_ {} {

      #  Method is simple. Read all the values and construct a FITS
      #  channel filled with cards. Then attempt to create an AST
      #  object which can be used to replace the existing WCS system.
      #  Then refine it a couple of times more to get an accurate
      #  reference pixel position.
      set nrows [$itk_component(table) total_rows]
      if { $itk_option(-rtdimage) != {} && $nrows > 0 } {

         #  See if preferred longitude direction is along the X axis.
         if { $values_($this,xislong) } {
            set reversed 0
         } else {
            set reversed 1
         }
         lassign [wcs_stats_ $reversed] mean1 mean2 xscale_ yscale_
         if { $itk_option(-fittype) == 4 } {
            #  Just one magnification, so make same for both axes
            #  preserving sign.
            set yscale_ [expr $xscale_*$yscale_/abs($yscale_)]
         }
         create_wcs_ $reversed $mean1 $mean2 $xscale_ $yscale_ 0.0

         #  Now refine the WCS to produce a good solution. Note we
         #  reset the angle as each refinement is an incremental
         #  change in this.
         set angle_ 0.0
	 refine_ 0

         #  Now we repeat the above, but using the better fit derive
         #  the initial conditions. This refines the reference
         #  position to be correct and adds in the rotation angle.
         lassign [$itk_option(-rtdimage) astpix2wcs \
                     $values_($this,crpix1) \
                     $values_($this,crpix2)] crval1 crval2
         create_wcs_ $reversed $crval1 $crval2 $xscale_ $yscale_ [expr -1.0*$angle_]
	 refine_ 1
      }
   }


   #  Create a WCS from the FITS description that we've got and some
   #  initial values.
   protected method create_wcs_ {reversed crval1 crval2 xscale yscale angle} {
      set image $itk_option(-rtdimage)
      set chan [$image astcreate]

      #  Set the basic elements. If needed swap the sense.
      if { $reversed } {
         $image aststore $chan CTYPE1 "'$values_($this,ctype2)$values_($this,proj)'"
         $image aststore $chan CTYPE2 "'$values_($this,ctype1)$values_($this,proj)'"
         $image aststore $chan CRVAL1 $crval2
         $image aststore $chan CRVAL2 $crval1
         $image aststore $chan CROTA1 $angle
      } else {
         $image aststore $chan CTYPE1 "'$values_($this,ctype1)$values_($this,proj)'"
         $image aststore $chan CTYPE2 "'$values_($this,ctype2)$values_($this,proj)'"
         $image aststore $chan CRVAL1 $crval1
         $image aststore $chan CRVAL2 $crval2
         $image aststore $chan CROTA2 $angle
      }
      $image aststore $chan CDELT1 $xscale_
      $image aststore $chan CDELT2 $yscale_

      #  Add the reference point (usually approximate).
      $image aststore $chan CRPIX1 $values_($this,crpix1)
      $image aststore $chan CRPIX2 $values_($this,crpix2)

      #  Only record a RADECSYS if needed.
      if { $values_($this,system) != {} } {
         if { $values_($this,ctype) == "Equatorial" } {
            $image aststore $chan RADECSYS "'$values_($this,system)'"
         }
      }

      #  Most systems need an equinox (if B/J present use string).
      if { $values_($this,equinox) != {} } {
         if { [regexp {J(.*)|B(.*)} $values_($this,equinox) all value] } {
            $image aststore $chan EQUINOX "'$values_($this,equinox)'"
         } else {
            $image aststore $chan EQUINOX $values_($this,equinox)
         }
      }

      #  And a MJD to go with it (note conversion of B/J/G date to MJD).
      if { $values_($this,epoch) != {} &&
           [$itk_component(epoch) cget -state] != "disabled" } {
         $image aststore $chan MJD-OBS [find_epoch_ $values_($this,epoch)]
      } else {
         #  No epoch specified, or none applicable to the astrometry.
         #  Preserve the MJD-OBS or DATE-OBS if present, otherwise we get the
         #  AST default of 2000.0. If both are present they are supposed to be
         #  the same value, so don't worry about that.
         set mjdobs [$image fits get MJD-OBS]
         if { $mjdobs != {} } {
            $image aststore $chan MJD-OBS "$mjdobs"
         } else {
            set dateobs [$image fits get DATE-OBS]
            if { $dateobs != {} } {
               $image aststore $chan DATE-OBS "$dateobs" \
                  "Date of observation" 1
            }
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
         $image aststore $chan LATPOLE $values_($this,latpole)
      }

      #  Read the channel to create an AST object and then replace
      #  the current WCS using it.
      $image astread $chan
      $image astreplace
      $image astdelete $chan
   }

   #  Perform a refinement on the current positions.
   protected method refine_ {update} {

      #  Extract the contents of the TableList and create the
      #  current projected image coordinates. Also keep a list of
      #  positions which these are expected to correspond to.
      set nrows [$itk_component(table) total_rows]
      set contents [$itk_component(table) get_contents]
      set newcoords {}
      set oldcoords {}
      set npoint 0

      #  If the association X-RA is reversed then flip x and y.
      for { set i 0 } { $i < $nrows } { incr i } {
         lassign [lindex $contents $i] id ra dec newx newy
         if { [ catch { $itk_option(-rtdimage) \
                           astwcs2pix $ra $dec } msg ] == 0 } {
            lassign $msg oldx oldy
            incr npoint
            lappend newcoords $newx $newy
            lappend oldcoords $oldx $oldy
         }
      }
      if { $npoint > 0 } {
         set ret 1
         busy {
            if { [catch { $itk_option(-rtdimage) astrefine image \
                             $itk_option(-fittype) \
                             $oldcoords $newcoords } errmsg] == 0 } {

               #  Succeeded, so record the new estimates of the image
               #  scale and then, if requested update the X and Y
               #  positions and make a report on the fit.
               $itk_option(-rtdimage) astreplace
               notify_
               set xscale_ [expr $xscale_/[lindex $errmsg 9]]
               set yscale_ [expr $yscale_/[lindex $errmsg 10]]
               set angle_  [expr [lindex $errmsg 12]+$angle_]
               if { $update } {
                  set fitqual [$itk_component(table) update_x_and_y 1]
                  set xs [expr $xscale_*3600.0]
                  set ys [expr $yscale_*3600.0]
                  set rms [expr $fitqual*0.5*(abs($xs)+abs($ys))]

                  #  Report the results of the fit.
                  report_result_ \
                     "Rms of fit = $rms (arcsec), $fitqual (pixels)" \
                     "x,y scales = $xs, $ys (arcsec/pixel)" \
                     "orientation = $angle_ (degrees)"
               }
            } else {
               error_dialog "$errmsg"
               set ret 0
            }
         }
      } else {
         error_dialog {Sorry there are no valid positions available}
      }
   }

   #  Record the value of the selected RADECSYS and configure the
   #  equinox entry window appropriately.
   protected method set_system_ {value needequinox needepoch} {
       set values_($this,system) $value
       $itk_component(system) configure -value $value
       if { ! $needequinox } {
	   $itk_component(equinox) configure -value {}
	   $itk_component(equinox) configure -state disabled
       } else {
	   set default [lindex $systemmap_ [expr [lsearch $systemmap_ $value]+1]]
	   $itk_component(equinox) configure -value $default
	   $itk_component(equinox) configure -state normal
       }
       if { ! $needepoch } {
	   $itk_component(epoch) configure -value {}
	   $itk_component(epoch) configure -state disabled
       } else {
	   set default [lindex $systemmap_ [expr [lsearch $systemmap_ $value]+1]]
	   $itk_component(epoch) configure -value $default
	   $itk_component(epoch) configure -state normal
       }
   }

   #  Set the value of the table coordinate types. Also disable the
   #  system selection unless the type is Equatorial (this means we
   #  should also check the equinox and epoch fields for dependency).
   protected method set_ctype_ {value} {

      #  Get the ctypes associated with this system.
      set ctype1 [lindex $ctypemap_ [expr [lsearch $ctypemap_ $value]+1]]
      set ctype2 [lindex $ctypemap_ [expr [lsearch $ctypemap_ $value]+2]]
      set values_($this,ctype) $value
      set values_($this,ctype1) $ctype1
      set values_($this,ctype2) $ctype2
      if { $value == "Equatorial" } {
         $itk_component(system) configure -state normal
         set text [[$itk_component(system) component mb] cget -text]
         if { $text == "Ecliptic" || $text == "Galactic" } {
            set_system_ FK5 1 0
         }
      } else {
         $itk_component(system) configure -state disabled
         if { $value == "Ecliptic" } {
            set_system_ {} 1 0
            [$itk_component(system) component mb] configure -text Ecliptic
         } else {
            set_system_ {} 0 0
            [$itk_component(system) component mb] configure -text Galactic
         }
         set values_($this,system) {}
      }
   }

   #  Set the value of the equinox entry window.
   protected method set_equinox_ {value} {
      set values_($this,equinox) $value
      $itk_component(equinox) configure -value $values_($this,equinox)
      $itk_component(table) configure -equinox $values_($this,equinox)
   }

   #  Set the value of the epoch entry window.
   protected method set_epoch_ {value} {
      set values_($this,epoch) $value
      $itk_component(epoch) configure -value $values_($this,epoch)
   }

   #  Translate an epoch like B1950 to an MJD.
   protected method find_epoch_ {value} {
      if {[regexp {J(.*)} $value all decyears]} {
         set julian 1
      } elseif {[regexp {B(.*)} $value all decyears]} {
         set julian 0
      } elseif { $value < 1984.0 } {
         set julian 0
         set decyears $value
      } else {
         set julian 1
         set decyears $value
      }
      if { $julian } {
         return  [expr 51544.5+($decyears-2000.0)*365.25]
      } else {
         return  [expr 15019.81352+($decyears-1900.0)*365.242198781]
      }
   }

   #  Fix the value of the equinox to a catalogue value. Note epoch
   #  assumed to be the same.
   protected method fix_equinox_ {equinox} {
      set_equinox_ $equinox
      set_epoch_ $equinox
      if { $values_($this,equinox) < 1984 } {
         set_system_ FK4 1 1
      }  else {
         set_system_ FK5 1 0
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
         $itk_component(projp1) configure -state disabled
      }
      if { $projpmap_($proj,2) } {
         $itk_component(projp2) configure -state normal
      } else {
	 $itk_component(projp2) configure -value {}
         $itk_component(projp2) configure -state disabled
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

   #  Derive useful statistics about the RA/Dec and X,Y positions
   #  (meanra, meandec, xscale, yscale). Note that if requested then
   #  the association between X-Y and RA-Dec is reversed.
   protected method wcs_stats_ {{reversed 0}} {
      set nrows [$itk_component(table) total_rows]
      set contents [$itk_component(table) get_contents]
      set radec ""
      set xy ""
      if { ! $reversed } {
         for { set i 0 } { $i < $nrows } { incr i } {
            lassign [lindex $contents $i] id ra dec x y
            append radec "$ra $dec "
            append xy "$x $y "
         }
      } else {
         for { set i 0 } { $i < $nrows } { incr i } {
            lassign [lindex $contents $i] id ra dec x y
            append radec "$ra $dec "
            append xy "$y $x "
         }
      }
      return [$itk_option(-rtdimage) astbootstats \
                 $radec $xy $values_($this,crpix1) $values_($this,crpix2)]
   }

   #  Clear the graphics markers from canvas.
   protected method clear_ {} {
       $itk_component(table) clear_marks
   }

   #  Enter a report in the results window.
   protected method report_result_ {args} {
      $itk_component(results) clear all
      foreach line "$args" {
         $itk_component(results) insert end $line
      }
   }

   #  Read and write reference positions to a file.
   protected method read_positions_ {} {
      $itk_component(table) read_from_file
   }
   protected method save_positions_ {} {
      $itk_component(table) write_to_file
   }

   #  Do the notify_cmd option if needed.
   protected method notify_ {} {
      if { $itk_option(-notify_cmd) != {} } {
         eval $itk_option(-notify_cmd)
      }
   }

   #  Create a toplevel windows displaying additional options.
   protected method make_additional_ {} {
      set add_ [TopLevelWidget $w_.add \
                   -transient $itk_option(-transient) \
                   -withdraw 1 \
                   -center 0]

      #  Set the top-level window title.
      wm title $add_ \
         "GAIA: Additional parameters ($itk_option(-number))"

      #  Create the short help window.
      $add_ make_short_help

      #  Add the File menu.
      $add_ add_menubar
      set File [$add_ add_menubutton "File"]
      $add_ configure_menubutton File -underline 0
      $add_ add_short_help [$add_ component menubar].file \
         {File menu: close window}
      $File add command -label {Close window   } \
         -command [code $this hide_additional_] \
         -accelerator {Control-c}
      bind $add_ <Control-c> [code $this hide_additional_]

      $add_ itk_component add space1 {
         LabelRule $add_.space1 -text "Additional parameters:"
      }
      #  Reference pixels
      $add_ itk_component add crpix1 {
         util::LabelEntry $add_.crpix1 \
            -labelwidth $lwidth_ \
            -valuewidth $vwidth_ \
            -text "X Reference pixel:" \
            -textvariable [scope values_($this,crpix1)]
      }
      $add_ add_short_help $add_.crpix1 \
         {Estimated X coordinate of image reference pixel (1,1 is centre of first pixel)}

      $add_ itk_component add crpix2 {
         util::LabelEntry $add_.crpix2 \
            -labelwidth $lwidth_ \
            -valuewidth $vwidth_ \
            -text "Y Reference pixel:" \
            -textvariable [scope values_($this,crpix2)]
      }
      $add_ add_short_help $add_.crpix2 \
         {Estimated Y coordinate of image reference pixel (1,1 is centre of first pixel)}

      # Longpole AND latpole.
      $add_ itk_component add longpole {
         util::LabelEntry $add_.longpole \
            -text "Longpole:" \
            -labelwidth $lwidth_ \
            -valuewidth $vwidth_ \
            -textvariable [scope values_($this,longpole)]
      }
      $add_ add_short_help $add_.longpole \
         {Longitude of table system northpole}
      $add_ itk_component add latpole {
         util::LabelEntry $add_.latpole \
            -text "Latpole:" \
            -labelwidth $lwidth_ \
            -valuewidth $vwidth_ \
            -textvariable [scope values_($this,latpole)]
      }
      $add_ add_short_help $add_.latpole \
         {Latitude of table system northpole}

      #  Add a button to close window.
      $add_ itk_component add accept {
         button $add_.close -text Close \
            -command [code $this hide_additional_]
      }
      $add_ add_short_help $add_.close {Close window}

      pack $add_.space1 -side top -pady 1 -padx 1 -fill x
      pack $add_.crpix1 -side top -pady 1 -padx 1 -anchor w
      pack $add_.crpix2 -side top -pady 1 -padx 1 -anchor w
      pack $add_.longpole -side top -pady 1 -padx 1 -anchor w
      pack $add_.latpole -side top -pady 1 -padx 1 -anchor w
      pack $add_.close -expand 1 -side bottom -pady 1 -padx 1
   }

   #  Show the additional parameters window.
   protected method show_additional_ {} {
      if { ! [winfo exists $add_] } {
         make_additional_
      }
      $add_ configure -center 1
   }

   #  Hide the additional parameters window.
   protected method hide_additional_ {} {
      if { [winfo exists $add_] } {
         wm withdraw $add_
      }
   }

   #  Set xislong variable to a value.
   protected method set_xislong_ {value} {
      set values_($this,xislong) $value
   }

   #  Set marker movement policy.
   protected method set_coupled_ {} {
       $itk_component(table) configure -coupled $values_($this,coupled)
   }

   #  Start transfer dialog.
   protected method start_transfer_ {} {
      blt::busy hold $w_
      if { ! [winfo exists $trantop_] } {
         set trantop_ [GaiaAstTransfer $w_.trantop \
                          -rtdimage $itk_option(-rtdimage) \
                          -canvas $itk_option(-canvas) \
                          -image $itk_option(-image) \
                          -update_cmd [code $this stop_transfer_]\
                          -isize $itk_option(-isize) \
                          -maxshift $itk_option(-maxshift)]
      } else {
         wm deiconify $trantop_
      }
      set content [$itk_component(table) get_contents]
      if { $content != {} } {
         $trantop_ clear_table
         $itk_component(table) clear_marks
         eval $trantop_ set_contents $content
      }
   }

   #  Transfer completed.
   protected method stop_transfer_ {args} {
      if { $args != {{}} } {
         $itk_component(table) clear_table
         eval $itk_component(table) set_contents $args
      }
      blt::busy release $w_
   }

   #  Configuration options
   #  =====================

   #  Name of starrtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {}

   #  Name of the canvas holding the starrtdimage widget.
   itk_option define -canvas canvas Canvas {}

   #  Name of the RtdImage widget or derived class.
   itk_option define -image image Image {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0

   #  The type of fit to be used when refining the coordinate system.
   itk_option define -fittype fittype Fittype 5 {
      set values_($this,fittype) $itk_option(-fittype)
   }

   #  The centroid search box and maximum shift.
   itk_option define -isize isize Isize 9 {
      set itk_option(-isize) [expr min(21,max(3,$itk_option(-isize)))]
      set values_($this,isize) $itk_option(-isize)
      if { [info exists itk_component(table) ] } {
         $itk_component(table) configure -isize $itk_option(-isize)
      }
      if { $trantop_ != {} && [winfo exists $trantop_] } {
         $trantop_ configure -isize $itk_option(-isize)
      }
   }

   #  Need to be 3.5->21.5, steps of 1.
   itk_option define -maxshift maxshift Maxshift 5.5 {
      set maxshift [expr min(21.5,max(3.5,$itk_option(-maxshift)))]
      set itk_option(-maxshift) [expr int($maxshift)+0.5]
      set values_($this,maxshift) $itk_option(-maxshift)
      if { [info exists itk_component(table) ] } {
         $itk_component(table) configure -maxshift $itk_option(-maxshift)
      }
      if { $trantop_ != {} && [winfo exists $trantop_] } {
         $trantop_ configure -maxshift $itk_option(-maxshift)
      }
   }

   #  Command to execute when the WCS is changed.
   itk_option define -notify_cmd notify_cmd Notify_Cmd {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  If this is a clone, then it should die rather than be withdrawn.
   itk_option define -really_die really_die Really_Die 0

   #  Protected variables: (available to instance)
   #  --------------------

   #  Default values of the controls.
   protected variable default_

   #  The known types of coordinates and their FITS synonyms.
   protected variable ctypemap_ \
      { {Equatorial (RA/Dec)} {Equatorial} {RA--} {DEC-} \
        {Ecliptic (Long/Lat)} {Ecliptic} {ELON} {ELAT} \
        {Galactic (Long/Lat)} {Galactic} {GLON} {GLAT} \
        {SuperGalactic (Long/Lat)} {SuperGalactic} {SLON} {SLAT}}
   #  Not implemented helioecliptic.

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
   #  systems. The values following these are the need for an equinox
   #  and an epoch.
   protected variable systemattrib_ \
      {FK5 1 0 FK4 1 1 FK4-NO-E 1 1 GAPPT 0 1}

   #  List of the various system names and their default
   #  equinoxes and the initialising list.
   protected variable systemmap_ {FK5 J2000 FK4 B1950 FK4-NO-E B1950 GAPPT {} }

   #  Names of some sensible equinoxes.
   protected variable equinoxmap_ {J2000.0 B1950.0}

   #  Widths of various fields.
   protected variable vwidth_ 20
   protected variable lwidth_ 20

   #  Whether a WCS system is being tested or not.
   protected variable testing_ 0

   #  Scales used in initial WCS system.
   protected variable xscale_ 1.0
   protected variable yscale_ 1.0

   #  Rotation angle.
   protected variable angle_ 0.0

   #  Name of additional toplevel window.
   protected variable add_ {}

   #  Name of RA/Dec transfer window.
   protected variable trantop_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Which PROJP parameters are required for which projections.
   common projpmap_

   #  Variable to share amongst all widgets. This is indexed by the
   #  object name ($this)
   common values_

#  End of class definition.
}
