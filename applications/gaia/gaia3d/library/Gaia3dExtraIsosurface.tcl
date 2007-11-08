#+
#  Name:
#     Gaia3dExtraIsosurface

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Add additional cubes for rendering as isosurfaces into a scene.

#  Description:
#     Creates a window for choosing cubes and defining levels, colours and
#     opacities to draw isosurfaces of the cube. An instance of this class
#     should be associated with a Gaia3dTool which is used to do the actual
#     rendering and instantiation.

#  Copyright:
#     Copyright (C) 2007 Science and Technology Facilities Council
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     09-OCT-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual Gaia3dExtraIsosurface {}

itcl::class ::gaia3d::Gaia3dExtraIsosurface {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA3D: Extra cubes ($itk_option(-number))"

      #  Make it a decent size (packing doesn't work).
      wm geometry  $w_ 400x400+0+0

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File" left]
      configure_menubutton File -underline 0

      #  Add window help.
      add_help_button extraisosurface "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      #  Set the close menu item.
      $File add command -label Close \
         -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]

      #  Add the options menu
      set Options [add_menubutton "Options" left]
      configure_menubutton Options -underline 0

      #  Add an option to use same colour for all surfaces.
      set Options [get_menu "Options"]
      $Options add checkbutton \
         -label {Use single colour} \
         -variable [scope single_colour_] \
         -onvalue 1 \
         -offvalue 0
      $short_help_win_ add_menu_short_help $Options \
         {Use single colour} \
         {Draw all iso surfaces with the same colour}

      #  Add an option to use same opacity for all surfaces.
      $Options add checkbutton \
         -label {Use single opacity} \
         -variable [scope single_opacity_] \
         -onvalue 1 \
         -offvalue 0
      $short_help_win_ add_menu_short_help $Options \
         {Use single opacity} \
         {Draw all iso surfaces with the same opacity}

      #  Main controls.
      add_controls_

      #  Close the window (removes any associated graphics).
      itk_component add actionframe {
         frame $w_.actions
      }
      pack $itk_component(actionframe) -side bottom -fill x

      itk_component add close {
         button $itk_component(actionframe).close -text "Close" \
            -command [code $this close]
      }
      pack $itk_component(close) -side left -expand 1 -padx 3m -pady 3m \
         -ipadx 2m -ipady 1m
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Release all accessed cubes.
      for { set i 0 } { $i < $itk_option(-maxcubes) } { incr i } {
         if { [info exists cubeaccessor_($i)] } {
            release_cube_ $i
            object delete $cubeaccessor_($i)
         }
      }

      #  Release all VTK objects.
      catch {release_all_iso_objects}
   }

   #  Methods and procedures:
   #  -----------------------

   #  Close this window. Withdraws and removes any associated cube renderings.
   public method close {} {
      wm withdraw $w_
      release_all_iso_objects
   }

   #  Release all data associated with a cube.
   protected method release_cube_ {index} {
      if { [info exists cubeaccessor_($index)] } {
         $cubeaccessor_($index) close
         $imagedata_($index) Delete
         $imageimport_($index) Delete
         set imagedata_($index) {}
         set imageimport_($index) {}
      }
   }

   #  Render any cubes we have into a given renderer. The target coordinates
   #  are defined by the supplied AST FrameSet. XXX handling caching of these
   #  transformations when WCS hasn't changed? Also contours etc.
   public method render {renwindow target_wcs} {

      #  Make sure all cube data is available. Also clear existing contours.
      open_cubes_

      #  Connection should go through the domain of the current coordinates.
      set domain [gaiautils::astget $target_wcs "Domain"]

      #  The relevant coordinates are base to base as we are working in
      #  graphics coordinates, same as GRID. So switch this WCS to that
      #  domain.
      set target_current [gaiautils::astget $target_wcs "Current"]
      set target_base [gaiautils::astget $target_wcs "Base"]
      gaiautils::astset $target_wcs "Current=$target_base"

      #  Don't want to exit this method without resetting the domain.
      catch {

         #  Create an actor for each possible level of each cube.
         for { set i 0 } { $i < $itk_option(-maxcubes) } { incr i } {
            if { [info exists cubename_($i)] && $cubename_($i) != {} } {

               #  Connect the coordinates of this cube with the target.
               set wcs_current [gaiautils::astget $wcs_($i) "Current"]
               set wcs_base [gaiautils::astget $wcs_($i) "Base"]
               gaiautils::astset $wcs_($i) "Current=$wcs_base"

               set connection_wcs \
                  [gaiautils::astconvert $wcs_($i) $target_wcs $domain]
               gaiautils::astset $wcs_($i) "Current=$wcs_current"
               gaiautils::astset $wcs_($i) "Base=$wcs_base"

               for { set j 0 } { $j < $itk_option(-maxcnt) } { incr j } {
                  create_iso_contour_ $i $j $connection_wcs $renwindow
               }

               #  Update colours, levels and opacities.
               for { set j 0 } { $j < $itk_option(-maxcnt) } { incr j } {
                  update_iso_contour_ $i $j
               }
            }
         }
      } msg
      if { $msg != {} } {
         puts "Matched error: $msg"
      }

      gaiautils::astset $target_wcs "Current=$target_current"
      gaiautils::astset $target_wcs "Base=$target_base"
   }

   #  Access all active cubes and wrap these into instances of vtkArrayData.
   protected method open_cubes_ {} {
      for { set i 0 } { $i < $itk_option(-maxcubes) } { incr i } {
         if { [info exists cubename_($i)] && $cubename_($i) != {} } {

            #  Check name of cube data, if changed need to release the
            #  existing cube and access the new data.
            set newname [$cubeaccessor_($i) cget -dataset]
            if { $cubename_($i) != $newname } {

               #  New cube or data has changed. Release existing cube and data.
               if { [info exists imagedata_($i)] && $imagedata_($i) != {} } {
                  release_cube_ $i
               }

               #  And access the new cube.
               $cubeaccessor_($i) configure -dataset $cubename_($i)

               #  For speed of access cache the full WCS.
               set wcs_($i) [$cubeaccessor_($i) getwcs]

               #  Access data, wrapping to an vtkImageData instance.
               lassign [gaia3d::Gaia3dVtk::import_gaia_cube \
                           $cubeaccessor_($i) 0 0] \
                  imagedata_($i) imageimport_($i)
            }
         }

         #  Always release iso objects.
         release_iso_objects $i
      }
   }

   #  Create objects to manage a contour for a cube, i is cube index, j the
   #  index of the level. "wcs" is an AST Mapping/FrameSet connecting the
   #  cube coordinates to the graphics coordinates.
   protected method create_iso_contour_ {i j wcs renwindow} {
      if { $cubename_($i) != {} } {
         set contour_($i,$j) [Gaia3dVtkIso \#auto -imagedata $imagedata_($i) \
                                 -renwindow $renwindow -wcs $wcs]
         $contour_($i,$j) add_to_window
         $contour_($i,$j) set_visible
      }
   }

   #  Update the objects managing a contour. The values are gathered from the
   #  interface. If no level is assigned then that contour is invisible.
   protected method update_iso_contour_ {i j} {

      #  Do nothing if the contour actors don't exist.
      if { ! [info exists contour_($i,$j)] } {
         return
      }

      #  Get attributes. Returns "" if no level.
      set atts [get_level_and_atts $i $j]
      if { $atts == {} } {
         $contour_($i,$j) set_invisible
      } else {
         $contour_($i,$j) set_visible
         eval $contour_($i,$j) set_lco $atts
      }
   }

   #  Get the level and attributes as a list for a contour.
   public method get_level_and_atts {i j} {
      set value [$itk_component(value$i$j) get]
      if { $value != {} } {
         set colour [$itk_component(colour$i$j) get]
         set opacity [$itk_component(opacity$i$j) get]
         return [list $value $colour $opacity]
      }
      return ""
   }

   #  Release all iso contours.
   public method release_all_iso_objects {} {
      if { [info exists contour_] } {
         for {set k 0} {$k < $itk_option(-maxcubes)} {incr k} {
            release_iso_objects $k
         }
         unset contour_
      }
   }

   #  Release iso contours for a cube.
   public method release_iso_objects {i} {
      for {set j 0} {$j < $itk_option(-maxcnt)} {incr j} {
         if { [info exists contour_($i,$j)] } {
            $contour_($i,$j) remove_from_window
            ::delete object $contour_($i,$j)
         }
      }
   }

   #  Add main controls for selecting cubes and defining the attributes for
   #  rendering.
   private method add_controls_ {} {

      #  Scrollpane for all controls.
      itk_component add scrollframe {
         ::iwidgets::scrolledframe $w_.scrollframe \
            -height 200 -hscrollmode none -vscrollmode dynamic
      }
      pack $itk_component(scrollframe) -fill both -expand 1
      set parent [$itk_component(scrollframe) childsite]

      #  Work hard to get a width to the childsite, boy...
      set canvas [winfo parent $parent]
      $canvas itemconfigure frameTag -width 350

      #  Add controls for selecting cubes, and defining their attributes.
      for { set i 0 } { $i < $itk_option(-maxcubes) } { incr i } {

         itk_component add attrule$i {
            LabelRule $parent.attrule$i \
               -text "Cube, contour levels & attributes:"
         }
         pack $itk_component(attrule$i) -side top -fill x -expand 1

         #  Create cubeaccessor.
         set cubeaccessor_($i) [uplevel \#0 gaia::GaiaNDAccess \#auto]

         #  Select cube.
         set cubename_($i) {}
         itk_component add cube$i {
            LabelCubeFileChooser $parent.cube$i \
               -labelwidth 5 \
               -text "Cube:" \
               -textvariable [scope cubename_($i)] \
               -command [code $this set_cubename_ $i] \
               -filter_types $itk_option(-filter_types) \
               -chooser_title "Select cube" \
               -cubeaccessor $cubeaccessor_($i)
         }
         pack $itk_component(cube$i) -side top -fill x -ipadx 1m -ipady 2m
         add_short_help $itk_component(cube$i) \
            {Name of a data file, must be a cube}

         #  Bind <3> to open a pop-up menu containing a list of cubes that
         #  have already been opened.
         set entry [$itk_component(cube$i) component entry]
         ::bind $entry <3> "puts \"you pressed <3>: $i\""

         #  Display or not.

         #  Add controls for line attributes.
         itk_component add tab$i {
            frame $parent.tab$i
         }
         add_att_controls_ $i $itk_component(tab$i)
         pack $itk_component(tab$i) -side top -fill x -pady 3 -padx 3

         #  Add a button to clear all contour levels for this cube.
         itk_component add clear$i {
            button $parent.clear$i -text {Clear levels} \
               -command [code $this clear_levels $i]
         }
         add_short_help $itk_component(clear$i) {Clear all levels and contours}
         pack $itk_component(clear$i) -side top -pady 3 -padx 3
      }
   }

   #  Clear the levels associated with a cube.
   public method clear_levels {i} {
      for {set j 0} {$j < $itk_option(-maxcnt)} {incr j} {
         $itk_component(value$i$j) configure -value {}
      }
   }

   #  Set the name of a cube.
   protected method set_cubename_ {i name} {
      set cubename_($i) $name
      $itk_component(cube$i) add_to_history $name
   }

   #  Add the controls for the contour levels and attributes.
   protected method add_att_controls_ {index w} {

      #  Add headings.
      itk_component add athead1$index {
         label $w.value -text "Level"
      }
      itk_component add athead2$index {
         label $w.colour -text "Colour"
      }
      itk_component add athead3$index {
         label $w.opacity -text "Opacity"
      }

      #  Arrange grid so that extra space goes to the right (anchored left
      #  effect).
      grid columnconfigure $w 0 -weight 0
      grid columnconfigure $w 1 -weight 0
      grid columnconfigure $w 2 -weight 0
      grid columnconfigure $w 3 -weight 1

      grid $itk_component(athead1$index) $itk_component(athead2$index) \
         $itk_component(athead3$index)

      #  Set up the default colours (wrapped at maximum number, avoiding
      #  white and black, pens 0 and 1.
      for {set i 0} {$i < $itk_option(-maxcnt)} {incr i} {
         set ii [expr 2+int(fmod($i,13))]
         set coldefault_($i) [gaia::AstColours::lookup_colour $ii]
      }

      #  Now add the controls for the actual values.
      for {set i 0} {$i < $itk_option(-maxcnt)} {incr i} {

         #  Entry widget for the contour values.
         itk_component add value$index$i {
            LabelEntry $w.value$i \
               -validate real \
               -text "$i:" \
               -labelwidth 5
         }

         #  Menu for selecting the colour.
         itk_component add colour$index$i {
            util::LabelMenu $w.colour$i \
               -relief raised
         }

         #  Now add all the colours to it.
         set colour_menu_($i) \
            [gaia::ColourLabelMenu \#auto $itk_component(colour$index$i) \
                -change_cmd [code $this set_colour_ $index $i] \
                -image $itk_option(-rtdimage) \
                -show_custom 0]

         #  Set to next colour in list.
         $itk_component(colour$index$i) configure -value $coldefault_($i)

         #  Add menu for selecting the opacity.
         itk_component add opacity$index$i {
            util::LabelMenu $w.opacity$i \
               -relief raised
         }

         #  Now add the range of opacities to it.
         for {set j 0.0} {$j <= 1.0} {set j [expr $j+0.1]} {
            $itk_component(opacity$index$i) add \
               -label $j \
               -value $j \
               -command [code $this set_opacity_ $index $i]
         }

         #  Default is transparent.
         $itk_component(opacity$index$i) configure -value 0.5

         #  Need to make geometries up to date, otherwise a user defined
         #  BorderWidth property seems to leave all widgets size 1.
         update idletasks

         #  Add these to the grid.
         grid $itk_component(value$index$i) $itk_component(colour$index$i) \
              $itk_component(opacity$index$i)
      }
   }

   #  Set the colour of all contours if using a single colour.
   protected method set_colour_ {index i colindex} {
      if { $single_colour_ } {
         set colour [gaia::AstColours::lookup_colour $colindex]
         for {set k 0} {$k < $itk_option(-maxcubes)} {incr k} {
            for {set j 0} {$i < $itk_option(-maxcnt)} {incr j} {
               $itk_component(colour$k$j) configure -value $colour
            }
         }
      }
   }

   #  Set the opacity of all contours if using a single value.
   #  Check if redraw is needed.
   protected method set_opacity_ {index i} {
      if { $single_opacity_ } {
         set opacity [$itk_component(opacity$index$i) get]
         for {set k 0} {$k < $itk_option(-maxcubes)} {incr k} {
            for {set j 0} {$i < $itk_option(-maxcnt)} {incr j} {
               if { $j != $i } {
                  $itk_component(opacity$k$j) configure -value $opacity
               }
            }
         }
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Rtdimage instance for managing colours.
   itk_option define -rtdimage rtdimage RtdImage {}

   #  Maximum number of additional cubes.
   itk_option define -maxcubes maxcubes MaxCubes 5

   #  Maximum number of contours, only works once.
   itk_option define -maxcnt maxcnt Maxcnt 3

   #  Filters for selecting files.
   itk_option define -filter_types filter_types Filter_types {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Whether to use a single colour for all contours.
   protected variable single_colour_ 0

   #  Whether to use a single opacity for all contours.
   protected variable single_opacity_ 0

   #  Names of the cubes, index by integer (0->maxcubes-1).
   protected variable cubename_

   #  The cube accessors.
   protected variable cubeaccessor_

   #  The cached AST FrameSet.
   protected variable wcs_

   #  VTK variables for vtkImageData and vtkImageImport instances.
   protected variable imagedata_
   protected variable imageimport_

   #  VTK variables for isosurfaces.
   protected variable contour_

   #  Whether named cube should be rendered.
   protected variable enabled_

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
