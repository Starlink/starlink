#+
#  Name:
#     GaiaAstRefine

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Refines an existing WCS system.

#  Description:
#     Allows an existing WCS system to be refined by adding a
#     linear transformation to the image coordinates. The positions
#     can be defined by using a list selected from a displayed
#     catalogue, or by entering directly by hand. Auto-refinement is
#     provided by using centroiding on the current image positions.
#
#     Rough transpositions of the object positions can be provided
#     through sliders that allow changes in the offsets, scale and
#     rotations to be made. Once applied (using the assign button) the
#     new WCS becomes the position from which transformations are made
#     (note that if reference positions are available a transformation
#     using these is derived, otherwise the transformation as it
#     appears in the controls is blindly applied).

#  Invocations:
#
#        GaiaAstRefine object_name [configuration options]
#
#     This creates an instance of a StarAstRefince object. The return is
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
#     TopLevelWidget

#  Copyright:
#     Copyright (C) 1997-2005 Central Laboratory of the Research Councils.
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
#     11-DEC-1997 (PWD):
#        Original version.
#     23-MAY-2000 (PWD):
#        Renamed GaiaAstRefine.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaAstRefine {}

itcl::class gaia::GaiaAstRefine {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate all options.
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: Tweak astrometry calibration ($itk_option(-number))"

      #  Create the short help window.
      make_short_help

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0
      add_short_help $itk_component(menubar).file \
         {File menu: close window, read/write reference positions}

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
         {Write the displayed positions out to an ordinary text file} \

      $File add command -label {Read positions from a file...} \
         -command [code $this read_positions_] \
         -accelerator {Control-r}
      bind $w_ <Control-r> [code $this read_positions_]
      $short_help_win_ add_menu_short_help $File \
         {Read positions from a file...} \
      {Read a suitable set of positions from an ordinary text file} \

      #  Set the exit menu items.
      $File add command -label {Cancel changes and close window}\
         -command [code $this cancel] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this cancel]
      $File add command -label {Accept changes and close window}\
         -command [code $this accept] \
         -accelerator {Control-a}
      bind $w_ <Control-a> [code $this accept]

      # Edit menu
      set Edit [add_menubutton Edit]

      #  Add window help.
      add_help_button astrometry "Astrometry Overview..."
      add_help_button refine "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      #  Options menu. Set the fittype.
      set m [add_menubutton Options]
      add_short_help $m {Set additional options}
      $m add cascade -label {Fit options} -menu [menu $m.fittype]
      $short_help_win_ add_menu_short_help $m Fittype \
         {Set the type of fit used to match the positions}
      set fithelp(1) {shift of origin only}
      set fithelp(2) {shift of origin and rotation}
      set fithelp(3) {shift of origin and magnification}
      set fithelp(4) {shift of origin, rotation and magnification}
      set fithelp(5) {full six parameter fit (includes shear)}
      foreach i {1 2 3 4 5} {
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

      #  Markers menu
      set Marker [add_menubutton Markers]

      #  Create a GaiaAstTable to display the positions that we are using
      #  to refine the WCS.
      itk_component add table {
          gaia::GaiaAstTable $w_.table \
             -editmenu $Edit \
             -markmenu $Marker \
             -rtdimage $itk_option(-rtdimage) \
             -canvas $itk_option(-canvas) \
             -image $itk_option(-image) \
             -notify_cmd [code $this fix_equinox_] \
             -centre_cmd [code $this fix_centre_] \
             -maxshift $itk_option(-maxshift) \
             -isize $itk_option(-isize)
      }
      add_short_help $itk_component(table) \
	      {Reference positions and their ideal X,Y places}

      #  Add a switch for controlling if the graphics markers move
      #  one-by-one or all together.
      itk_component add coupled {
         gaia::StarLabelCheck $w_.coupled \
            -text "Move markers individually:" \
            -onvalue 0 \
            -offvalue 1 \
            -variable [scope values_($this,coupled)] \
            -command [code $this set_coupled_]
      }
      set default_(coupled) 0
      add_short_help $itk_component(coupled) \
         {Move markers individually or all together}

      #  Add scales for changing the offsets, scale and rotations of
      #  the projected positions.
      itk_component add rule1 {
         gaia::LabelRule $w_.rule1 -text "Simple transformations:"
      }
      set width [$itk_option(-rtdimage) width]
      set height [$itk_option(-rtdimage) height]
      set iwidth [expr $width*0.25]
      set iheight [expr $height*0.25]
      set default_(xoffset) 0.0
      set last_(xoffset) 0.0
      itk_component add xoffset {
         util::LabelEntryScale $w_.xoffset \
            -text {X offset:} \
            -labelwidth 10 \
            -increment 0.05 \
            -resolution 0.05 \
            -from -$iwidth \
            -to $iwidth \
            -show_arrows 1 \
            -value 0.0 \
            -anchor w \
            -command [code $this xoffset_]
      }
      add_short_help $itk_component(xoffset) \
         {X displacement from current positions}
      set default_(yoffset) 0.0
      set last_(yoffset) 0.0
      itk_component add yoffset {
         util::LabelEntryScale $w_.yoffset \
            -text {Y offset:} \
            -labelwidth 10 \
            -increment 1.0 \
            -resolution 0.5 \
            -from -$iheight \
            -to $iheight \
            -show_arrows 1 \
            -value 0.0 \
            -anchor w \
            -command [code $this yoffset_]
      }
      add_short_help $itk_component(yoffset) \
         {Y displacement from current positions}
      set default_(scale) 1.0
      set last_(scale) 1.0
      itk_component add scale {
         util::LabelEntryScale $w_.scale \
            -text {Scale:} \
            -labelwidth 10 \
            -increment 0.005 \
            -resolution 0.005 \
            -from 0.1 \
            -to 10.0 \
            -show_arrows 1 \
            -value 1.0 \
            -anchor w \
            -command [code $this scale_]
      }
      add_short_help $itk_component(scale) \
         {Magnification factor about centre}
      set default_(rotate) 0.0
      set last_(rotate) 0.0
      itk_component add rotate {
         util::LabelEntryScale $w_.rotate \
            -text {Rotation:} \
            -labelwidth 10 \
            -increment 0.05 \
            -resolution 0.05 \
            -from -180 \
            -to 180.0 \
            -show_arrows 1 \
            -value 1.0 \
            -anchor w \
            -command [code $this rotate_]
      }
      add_short_help $itk_component(rotate) \
         {Rotation about centre (degrees)}
      set default_(xcentre) [expr $width/2]
      itk_component add xcentre {
         util::LabelEntryScale $w_.xcentre \
            -text {X centre:} \
            -labelwidth 10 \
            -increment 0.05 \
            -resolution 0.05 \
            -from 0.0 \
            -to $width \
            -show_arrows 1 \
            -value $default_(xcentre) \
            -anchor w \
            -command [code $this set_xcentre_]
      }
      set_xcentre_ $default_(xcentre)
      add_short_help $itk_component(xcentre) \
         {X centre for rotation and scaling}
      set default_(ycentre) [expr $height/2]
      itk_component add ycentre {
         util::LabelEntryScale $w_.ycentre \
            -text {Y centre:} \
            -labelwidth 10 \
            -increment 0.05 \
            -resolution 0.05 \
            -from 0.0 \
            -to $height \
            -show_arrows 1 \
            -value $default_(ycentre) \
            -anchor w \
            -command [code $this set_ycentre_]
      }
      set_ycentre_ $default_(ycentre)
      add_short_help $itk_component(ycentre) \
         {Y centre for rotation and scaling}

      #  Create the button bar
      itk_component add actionframe {
         frame $w_.action
      }

      #  Add a button to close window and accept the new WCS.
      itk_component add accept {
         button $itk_component(actionframe).accept -text Accept \
            -command [code $this accept]
      }
      add_short_help $itk_component(accept) \
         {Accept new astrometry calibration and close window}

      #  Add a button to refine the WCS given the current positions.
      itk_component add refine {
         button $itk_component(actionframe).refine -text {Fit/Test} \
            -command [code $this refine]
      }
      add_short_help $itk_component(refine) \
         {Perform fit and assign new astrometric calibration to image}

      #  Add a button to assign the current transforms to the WCS system.
      itk_component add assign  {
         button $itk_component(actionframe).assign -text Assign \
            -command [code $this assign]
      }
      add_short_help $itk_component(assign) \
         {Assign the simple transform value to astrometry calibration}

      #  Add a button to close window and not accept the new WCS.
      itk_component add cancel {
         button $itk_component(actionframe).cancel -text Cancel \
            -command [code $this cancel]
      }
      add_short_help $itk_component(cancel) \
         {Close window and restore original astrometry calibration}

      #  Add a button to reset the entries and return to the original
      #  image WCS.
      itk_component add reset {
         button $itk_component(actionframe).reset -text Reset \
            -command [code $this reset_]
      }
      add_short_help $itk_component(reset) {Reset image and window to defaults}

      #  Add a scrollbox to display results of fit.
      itk_component add results {
         gaia::Scrollbox $w_.results -height 4 -singleselect 0
      }
      add_short_help $itk_component(results) \
         {Results of fit (input X,Y to post fit X,Y positions)}

      #  Reset image to original WCS.
      reset_

      #  Pack all widgets into place.
      pack $itk_component(table) -side top -fill both -expand 1 -pady 5 -padx 5
      pack $itk_component(coupled) -side top -fill x -pady 3 -padx 3
      pack $itk_component(rule1) -side top -fill x
      pack $itk_component(xoffset) -side top -fill x -pady 3 -padx 3
      pack $itk_component(yoffset) -side top -fill x -pady 3 -padx 3
      pack $itk_component(scale) -side top -fill x -pady 3 -padx 3
      pack $itk_component(rotate) -side top -fill x -pady 3 -padx 3
      pack $itk_component(xcentre) -side top -fill x -pady 3 -padx 3
      pack $itk_component(ycentre) -side top -fill x -pady 3 -padx 3

      pack $itk_component(actionframe) -side top -fill x -pady 5 -padx 5
      pack $itk_component(accept) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(cancel) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(reset) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(assign) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(refine) -side right -expand 1 -pady 3 -padx 3

      pack $itk_component(results) -side bottom -fill x -pady 5 -padx 5
   }

   #  Destructor:
   #  -----------
   destructor  {
       delete object $itk_component(table)
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
            restore_
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

   #  Withdraw window and retain new WCS -- permanently.
   public method accept {} {
      if { ! $testing_ } {
         warning_dialog "There are no changes to the WCS"
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

   #  Reset the WCS of the displayed image back to the original
   #  version.
   protected method reset_ {} {
      catch {restore_}
      $itk_component(table) update_x_and_y
      reset_controls_
      set testing_ 0
   }

   #  Restore the original WCS of the displayed image.
   protected method restore_ {} {
      $itk_option(-rtdimage) astrestore original
      notify_
   }

   #  Perform a refinement on the current positions.
   public method refine {} {
      set testing_ 1

      #  Extract the contents of the TableList and create the
      #  current projected image coordinates. Also keep a list of
      #  positions which these are expected to correspond to.
      set nrows [$itk_component(table) total_rows]
      set contents [$itk_component(table) get_contents]
      set newcoords {}
      set oldcoords {}
      set npoint 0
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

               #  Succeeded replace the x and y coordinates by the new
               #  estimates and redraw the positions.
               $itk_option(-rtdimage) astreplace
               notify_
               $itk_component(table) update_x_and_y

               #  Report the results of the fit.
               report_result_ "Rms of fit = [lindex $errmsg 0] (pixels)" \
                  "x,y shifts = [lindex $errmsg 7] [lindex $errmsg 8]" \
                  "x,y scales = [lindex $errmsg 9] [lindex $errmsg 10]" \
                  "orientation = [lindex $errmsg 12] (degrees)" \
                  "non-perpendicularity = [lindex $errmsg 11]"
               } else {
               error_dialog "$errmsg"
               set ret 0
            }
         }
         reset_controls_
         return $ret
      } else {
         error_dialog {Sorry there are no valid positions available}
         return 0
      }
   }

   #  Assign the current transform information to the WCS.  Assumes
   #  that where we are is a solid body transformation from where we
   #  were if a table of positions is available. Otherwise the
   #  transformation as it exists is assigned directly to the image.
   public method assign {} {
      set nrows [$itk_component(table) total_rows]
      if { $nrows > 0 } {

        #  Just use a refine with fittype of 6.
         set fittype $itk_option(-fittype)
         set itk_option(-fittype) 6
         refine
         set itk_option(-fittype) $fittype
      } else {

         #  Get the current offsets, scales and rotations and append
         #  these to the current base transform. Note we scale, rotate
         #  and finally shift.
         set xoff [$itk_component(xoffset) get]
         set yoff [$itk_component(yoffset) get]
         set scale [$itk_component(scale) get]
         set angle [$itk_component(rotate) get]
         set xcen [$itk_component(xcentre) get]
         set ycen [$itk_component(ycentre) get]
         set cs [expr cos($angle*$d2r_)]
         set sn [expr sin($angle*$d2r_)]

         set tr1(1) [expr $xcen*(1.0-$scale)]
         set tr1(2) $scale
         set tr1(3) 0.0
         set tr1(4) [expr $ycen*(1.0-$scale)]
         set tr1(5) 0.0
         set tr1(6) $scale

         set tr2(1) [expr $xcen*(1.0-$cs)+$ycen*$sn]
         set tr2(2) $cs
         set tr2(3) [expr -$sn]
         set tr2(4) [expr $ycen*(1.0-$cs)-$xcen*$sn]
         set tr2(5) $sn
         set tr2(6) $cs

         set tr3(1) $xoff
         set tr3(2) 1.0
         set tr3(3) 0.0
         set tr3(4) $yoff
         set tr3(5) 0.0
         set tr3(6) 1.0

         trconcat_ tr1 tr2 tr4
         trconcat_ tr4 tr3 tr5

         $itk_option(-rtdimage) astassign image \
            $tr5(1) $tr5(2) $tr5(3) $tr5(4) $tr5(5) $tr5(6)

         $itk_option(-rtdimage) astreplace
         notify_
         set testing_ 1

         #  Reset the controls to their defaults and also reset the last_
         #  values.
         reset_controls_
      }
   }

   #  Methods for performing globals reorientations of the displayed
   #  positions. Note that we use displacements, scales and rotations
   #  relative to the position used last time (this ensures that we
   #  get back to where we started when returning to the initial
   #  values).
   protected method xoffset_ {dx} {
      set reldx [expr $dx-$last_(xoffset)]
      $itk_component(table) offset $reldx 0.0
      set last_(xoffset) $dx
   }
   protected method yoffset_ {dy} {
      set reldy [expr $dy-$last_(yoffset)]
      $itk_component(table) offset 0.0 $reldy
      set last_(yoffset) $dy
   }

   #  Scale the X and Y coordinates about the centre.
   protected method scale_ {scale} {
      $itk_component(table) configure -xcentre [$itk_component(xcentre) get]
      $itk_component(table) configure -ycentre [$itk_component(ycentre) get]
      set relscale [expr $scale/$last_(scale)]
      $itk_component(table) scale $relscale
      set last_(scale) $scale
   }

   #  Rotate positions about the centre.
   protected method rotate_ {angle} {
      $itk_component(table) configure -xcentre [$itk_component(xcentre) get]
      $itk_component(table) configure -ycentre [$itk_component(ycentre) get]
      set relangle [expr $angle-$last_(rotate)]
      $itk_component(table) rotate $relangle
      set last_(rotate) $angle
   }

   #  Reset the transformation controls.
   protected method reset_controls_ {{ignore none}} {
       foreach control {xoffset yoffset scale rotate xcentre ycentre} {
	   if { $control != $ignore } {
	       $itk_component($control) configure -value $default_($control)
	       set last_($control) $default_($control)
	   }
       }
       if { $ignore != "xcentre" } {
	   set_xcentre_ $default_(xcentre)
       }
       if { $ignore != "ycentre" } {
	   set_ycentre_ $default_(ycentre)
       }
       if { $ignore != "coupled" } {
	   set values_($this,coupled) $default_(coupled)
	   set_coupled_
       }
   }

   #  Concatenate two linear transformations into one (note using
   #  upvar to get a reference to arrays at the callers level).
   protected method trconcat_ {reftr1 reftr2 reftr3} {
      upvar $reftr1 tr1
      upvar $reftr2 tr2
      upvar $reftr3 tr3
      set tr3(1) [expr $tr2(1) + $tr2(2)*$tr1(1) + $tr2(3)*$tr1(4)]
      set tr3(2) [expr $tr2(2)*$tr1(2) + $tr2(3)*$tr1(5)]
      set tr3(3) [expr $tr2(2)*$tr1(3) + $tr2(3)*$tr1(6)]
      set tr3(4) [expr $tr2(4) + $tr2(5)*$tr1(1) + $tr2(6)*$tr1(4)]
      set tr3(5) [expr $tr2(5)*$tr1(2) + $tr2(6)*$tr1(5)]
      set tr3(6) [expr $tr2(5)*$tr1(3) + $tr2(6)*$tr1(6)]
   }

   #  Trap an attempt by the GaiaAstTable to convert the equinox (used
   #  when grabbing a catalogue).
   protected method fix_equinox_ {equinox} {
       $itk_component(table) change_equinox [$itk_option(-rtdimage) wcsequinox]
   }

   #  Deal with notification that table has changed the centre.
   protected method fix_centre_ {x y} {
       $itk_component(xcentre) configure -value $x
       $itk_component(ycentre) configure -value $y
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

   #  Set the xcentre and ycentre of table.
   protected method set_xcentre_ {value} {
       $itk_component(table) configure -xcentre $value
   }
   protected method set_ycentre_ {value} {
       $itk_component(table) configure -ycentre $value
   }

   #  Set marker movement policy.
   protected method set_coupled_ {} {
       $itk_component(table) configure -coupled $values_($this,coupled)
   }

   #  Public variables: (configuration options).
   #  -----------------

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

   #  Command to execute when the WCS is changed.
   itk_option define -notify_cmd notify_cmd Notify_Cmd {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  If this is a clone, then it should die rather than be withdrawn.
   itk_option define -really_die really_die Really_Die 0

   #  The centroid search box and maximum shift.
   itk_option define -isize isize Isize 9 {
      set itk_option(-isize) [expr min(21,max(3,$itk_option(-isize)))]
      set values_($this,isize) $itk_option(-isize)
      if { [info exists itk_component(table) ] } {
         $itk_component(table) configure -isize $itk_option(-isize)
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
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Default values of the controls.
   protected variable default_

   #  Last simple transformation value.
   protected variable last_

   #  Whether a WCS system is being tested or not.
   protected variable testing_ 0

   #  Degrees to radians factor.
   protected variable d2r_ 0.017453292

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Communication with widgets.
   common values_

   #  End of class definition.
}
