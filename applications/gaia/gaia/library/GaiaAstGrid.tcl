#+
#  Name:
#     GaiaAstGrid

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Creates a instance of a toolbox for controlling the Astrometry
#     options.

#  Description:
#     This class creates a top-level window which controls the drawing
#     of the astrometry grid on an instance of RTD with AST
#     extensions. There are many options that can be used to control
#     the appearance of the grid, consequently these are loosely catagorised
#     into pages of controls.

#  Invocations:
#
#        GaiaAstGrid object_name [configuration options]
#
#     This creates an instance of a GaiaAstGrid object. The return is
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
#
#        -rtdimage
#
#     Name of the rtdimage image item.
#
#        -canvas
#
#     Name of the canvas to draw into.
#
#        -number
#
#     Clone number of the controlling RTD widget.

#  Methods:
#
#     public:
#
#        close
#
#     Close the top-level window (destroys everything).
#
#        draw_grid {override 0}
#
#     Draw the grid using the current options, but only if override is
#     true or a grid is already drawn (this method is intended for use
#     when external events mean that a re-draw is needed, if the grid
#     is already drawn).
#
#        remove_grid
#
#     Remove the grid.
#
#        write_file
#
#     Creates a file selection box, in which an existing file can be
#     selected or a new name can be given. A copy of the current
#     options is then written to this file.
#
#        read_file
#
#     Creates a file selection box, in which an existing file can be
#     selected. The contents of this file are then sourced to
#     restore a previous set of options.
#
#        save_options filename
#
#     Writes a copy of the current options to the named file.
#
#        read_options filename
#
#     Restores the options stored in the named file (which should be
#     the output from a save_options operation).
#
#     protected:
#
#        draw_grid_
#
#     Draws the grid.
#
#        remove_grid_
#
#     Removes the grid.
#
#        add_spacing_selections_ parent
#        add_position_selections_ parent
#        add_scaling_selections_ parent
#        add_style_selections_ parent
#        add_element_selections_ parent
#        add_colour_selections_ parent
#        add_font_selections_ parent
#        add_system_selections_ parent
#        add_label_selections_ parent
#        add_format_selections_ parent
#
#     Create the controls for the various pages.
#
#        reset_spacing_
#        reset_position_
#        reset_scaling_
#        reset_style_
#        reset_element_
#        reset_colour_
#        reset_font_
#        reset_system_
#        reset_label_
#        reset_format_
#
#     Reset the controls for the various pages to their default values.
#
#        set_position_config_ name axis value
#        set_size_config_ name value
#        set_width_config_ name value
#        set_font_config_ name value
#        set_colour_config_ name value
#        set_system_config_ name value needepoch needequinox
#        set_spacing_config_ name value
#        set_format_config_ name value
#
#     Set the values of the appropriate arrays when a control has
#     a value change.
#
#        arcsec_to_radian_ value
#
#     Converts a value in arcseconds to radians.
#
#        radec_to_radian_ axis value
#
#     Converts an sexagesimal string to radians.

#  Inheritance:
#     This object inherits TopLevelWidget.

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
#     18-AUG-1997 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaAstGrid {}

itcl::class gaia::GaiaAstGrid {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: Grid overlay ($itk_option(-number))"

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0

      #  Add the options menu
      set Options [add_menubutton "Options"]
      configure_menubutton Options -underline 0

      #  Add window help.
      add_help_button grid "On Window..."

      #  Add option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      $short_help_win_ add_menu_short_help $File \
         {New window} {Create a new toolbox}

      #  Save options to a file.
      $File add command \
         -label {Save options...} \
         -command [code $this write_file] \
         -accelerator {Control-s}
      bind $w_ <Control-s> [code $this write_file]

      #  Restore options from a file.
      $File add command \
         -label {Restore options...} \
         -command [code $this read_file] \
         -accelerator {Control-r}
      bind $w_ <Control-r> [code $this read_file]

      #  Set the exit menu item.
      $File add command -label Exit \
         -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]

      #  Add auto-redraw option.
      set auto_redraw_($this) 0
      $Options add checkbutton  -label {Auto-redraw} \
         -variable [scope auto_redraw_($this)] \
         -onvalue 1 \
         -offvalue 0

      #  Normal or channel map defaults. Start with normal.
      $Options add checkbutton  -label {Chanmap defaults} \
         -variable [scope chanmap_defaults_] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this toggle_chanmap_defaults_]
      set chanmap_defaults_ 0
      set elementattrib_ $normalelementattrib_
      set position_($this,labelling) "interior"

      #  Create the tab notebook for containing each page of options.
      itk_component add TabNoteBook {
         ::iwidgets::tabnotebook $w_.tab -angle 30 -tabpos w \
            -width 350 -height 550
      }
      pack $itk_component(TabNoteBook) -fill both -expand 1

      #  Add a page for the spacing options and get the child site.
      #  and then add a page of controls to allow the selection of
      #  the related elements.
      $itk_component(TabNoteBook) add -label Spacing \
         -command [code $this reveal_ 0]
      set pages_(0) spacing
      set revealed_(0) 0

      #  Add a page for setting a new title and axes labels.
      $itk_component(TabNoteBook) add -label Labelling \
         -command [code $this reveal_ 1]
      set pages_(1) label
      set revealed_(1) 0

      #  Add a page for defining a different astrometry system.
      $itk_component(TabNoteBook) add -label System \
         -command [code $this reveal_ 2]
      set pages_(2) system
      set revealed_(2) 0

      #  Add a page for the colour options.
      $itk_component(TabNoteBook) add -label Colours \
         -command [code $this reveal_ 3]
      set pages_(3) colour
      set revealed_(3) 0

      #  Add a page for the elements options.
      $itk_component(TabNoteBook) add -label Elements \
         -command [code $this reveal_ 4]
      set pages_(4) element
      set revealed_(4) 0

      #  Add a page for the scale options.
      $itk_component(TabNoteBook) add -label Scaling \
                  -command [code $this reveal_ 5]
      set pages_(5) scaling
      set revealed_(5) 0

      #  Add a page for the line style options.
      $itk_component(TabNoteBook) add -label Style \
                  -command [code $this reveal_ 6]
      set pages_(6) style
      set revealed_(6) 0

      #  Add a page for the position options.
      $itk_component(TabNoteBook) add -label Position \
         -command [code $this reveal_ 7]
      set pages_(7) position
      set revealed_(7) 0

      #  Add a page for the font options.
      $itk_component(TabNoteBook) add -label Fonts \
         -command [code $this reveal_ 8]
      set pages_(8) font
      set revealed_(8) 0

      #  Add a page for setting the formats of the axes numbers.
      $itk_component(TabNoteBook) add -label Format \
         -command [code $this reveal_ 9]
      set pages_(9) format
      set revealed_(9) 0

      #  Create the button bar
      itk_component add actionframe {frame $w_.action}

      #  Add a button to close window.
      itk_component add close {
         button $itk_component(actionframe).close -text Close \
            -command [code $this close]
      }

      #  Add a button to reset the current page.
      itk_component add resetpage {
         button $itk_component(actionframe).resetpage -text {Reset Page} \
            -command [code $this reset_page_ 0]
      }

      #  Add a button to reset all page.
      itk_component add resetall {
         button $itk_component(actionframe).resetall -text {Reset All Pages} \
            -command [code $this reset_page_ 1]
      }

      #  And draw the grid.
      itk_component add draw {
         button $itk_component(actionframe).draw -text {Draw} \
            -command [code $this draw_grid_]
      }

      #  Set the symbolic names of the X and Y axes.
      #  set xname_ [$itk_option(-rtdimage) astget symbol(1)]
      #  set yname_ [$itk_option(-rtdimage) astget symbol(2)]

      #  Bind the wm deiconify event to redraw the grid, it
      #  auto_redraw is on.
      bind [winfo toplevel $w_] <Map> [code $this redraw_]

      #  Pack all the components into place.
      pack $itk_component(TabNoteBook) -side top -fill both -expand 1 -pady 5 -padx 5
      pack $itk_component(actionframe) -side bottom -fill x -pady 5 -padx 5
      pack $itk_component(close) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(resetpage) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(resetall) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(draw) -side left -expand 1 -pady 3 -padx 3

      #  Select a page.
      $itk_component(TabNoteBook) select 0

      #  Create a unique tag for this grid.
      set grid_tag_ "grid[incr grid_count_]"
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Remove the grid before exiting in a hard manner.
      catch { remove_grid_ }
   }

   #  Methods:
   #  --------

   #  Create a new instance of this object.
   protected method clone_me_ {} {
      if { $itk_option(-clone_cmd) != {} } {
         eval $itk_option(-clone_cmd)
      }
   }

   #  Close this window, kill it if needed, otherwise withdraw.
   public method close {} {
      if { $itk_option(-really_die) } {
         delete object $this
      } else {
         wm withdraw $w_
         #  Remove the grid.
         catch {remove_grid_}
      }
   }

   #  Public draw_grid method. Only used externally to this class,
   #  which means we can decline to draw the grid unless if has already
   #  been drawn using the button. The auto option controls whether
   #  the grid redraw is also to be controlled by the autoredraw
   #  option.
   public method draw_grid { {override 0} {auto 0} } {
      if { $drawn_ || $override } {

         #  If image been rotated then change the defaults for the
         #  position if necessary.
         if { $drawn_ } {
            reset_edge_
         }
         if { $auto } {
            redraw_
         } else {
            draw_grid_
         }
      }
   }

   #  Public remove_grid method.
   public method remove_grid {} {
      if { $drawn_  } {
         catch {remove_grid_}

         #  Make sure that the default system is correctly labelled.
         set_system_defaults_

         #  And that the format makes sense for this new image.
         reset_format_
      }
   }

   #  Reveal a page of widgets if not already done (these are deferred
   #  to give a better interactive response).
   protected method reveal_ {index {all 0}} {
      if { ! $all } {
         if { !$revealed_($index) } {
            set revealed_($index) 1
            set child [$itk_component(TabNoteBook) childsite $index]
            eval add_$pages_($index)_selections_ $child
         }
      } else {
         #  Reveal all pages.
         for {set i 0} {$i < 10} {incr i} {
            if { !$revealed_($i) } {
               set revealed_($i) 1
               set child [$itk_component(TabNoteBook) childsite $i]
               eval add_$pages_($i)_selections_ $child
            }
         }
      }
   }

   #  Reset the current page to its default values.
   protected method reset_page_ {{all 0}} {
      if { ! $all } {

         #  Get the index of the current page.
         set current [$itk_component(TabNoteBook) index select]
         eval reset_$pages_($current)_
      } else {

         #  Reset all pages.
         reveal_ 0 1
         for {set i 0} {$i < 9} {incr i} {
            eval reset_$pages_($i)_
         }
      }
      redraw_
   }

   #  Redraw the grid if auto_redraw is on (note args are ignored).
   protected method redraw_ {args} {
      if { $auto_redraw_($this) } {
         set auto_redraw_($this) 0
         draw_grid_
         set auto_redraw_($this) 1
      }
   }

   #  Convert state into an AST options list.
   protected method gen_options_ {} {
      set options ""

      #  Add the position options.
      if { $position_($this,edge1) != "default" } {
         lappend options "edge(1)=$position_($this,edge1)"
      }
      if { $position_($this,edge2) != "default" } {
         lappend options "edge(2)=$position_($this,edge2)"
      }
      if { $position_($this,labelat1) != "" } {
         lappend options \
            "labelat(1)=[radec_to_radian_ 2 $position_($this,labelat1)]"
      }
      if { $position_($this,labelat2) != "" } {
         lappend options \
            "labelat(2)=[radec_to_radian_ 1 $position_($this,labelat2)]"
      }

      lappend options "labelup(1)=$position_($this,labelup1)"
      lappend options "labelup(2)=$position_($this,labelup2)"
      lappend options "labelling=$position_($this,labelling)"

      #  Add spacing options.
      foreach {sname lname default} $scaleattrib_ {
         lappend options "$sname=$spacing_($sname)"
      }
      foreach {sname lname default} $spacingattrib_ {
         lappend options "$sname=$spacing_($sname)"
      }
      foreach {sname lname default} $minorattrib_ {
         if { $spacing_($sname) != "0.0" } {
            lappend options "$sname=[expr int($spacing_($sname))]"
         }
      }

      #  Add scaling options.
      foreach {sname lname deffont defsize} $fontattrib_ {
         lappend options "size($sname)=$size_($sname)"
      }
      foreach {sname lname default} $widthattrib_ {
         lappend options "width($sname)=[expr $width_($sname)/200.0]"
      }
      foreach {sname lname default} $lengthattrib_ {
         lappend options "$sname=[expr $size_($sname)/100.0]"
      }

      #  Line style options.
      foreach {sname lname default} $styleattrib_ {
         lappend options "style($sname)=[expr int($style_($sname))]"
      }

      #  Add the elements options.
      foreach {sname lname default} $elementattrib_ {
         lappend options "$sname=$element_($this,$sname)"
      }

      #  Add the colour options.
      foreach {sname lname default} $colourattrib_ {
         lappend options "colour($sname)=$colour_($sname)"
      }

      #  Add the font options.
      foreach {sname lname deffont defsize} $fontattrib_ {
         lappend options "font($sname)=$font_($sname)"
      }

      #  Add the astrometric system that we want to plot in.
      if { $system_(system) != "default" } {
         lappend options "system=$system_(system)"
      }
      set system_(epoch) [$itk_component(Epoch) get]
      if { $system_(epoch) != "default" } {
         lappend options "epoch=$system_(epoch)"
      } elseif { $system_(system) == "azel" && $system_(epoch) == "default" } {
         #  For azel the default epoch needs to be the existing one.
         lappend options "epoch=$system_defaults_(default,epoch)"
      }

      set system_(equinox) [$itk_component(Equinox) get]
      if { $system_(equinox) != "default" } {
         lappend options "equinox=$system_(equinox)"
      }

      #  Add any new labels. Note these are formatted so that
      #  non-keyboard characters can be used (i.e. \000 type
      #  numbers). AST formatting may also be present and introduce
      #  percent characters. These should be protected.
      foreach {sname lname} $labelattrib_ {
         if { $label_($this,$sname) != "" } {
            regsub -all {%} "$label_($this,$sname)" {%%} value
            eval set value [format \"$value\"]

            regsub -all {%s} "$value" {%%s} value
            regsub -all {%f} "$value" {%%f} value
            regsub -all {%c} "$value" {%%c} value
            regsub -all {%t} "$value" {%%t} value
            regsub -all {%w} "$value" {%%w} value
            lappend options "$sname=$value"
         }
      }

      #  And set the axes number formatting to be used. Note this is
      #  quite different for SkyFrames and ordinary Frames.
      if { ! $format_($this,noformat) } {
         if { [$itk_option(-rtdimage) astcelestial] } {
            set format1 "format(1)=$Xformat_($this,sep)"
            set format2 "format(2)=$Yformat_($this,sep)"
            foreach {lname ident} $formatattrib_ {
               if { $Xformat_($this,$ident) } {
                  lappend format1 $ident
               }
               if { $Yformat_($this,$ident) } {
                  lappend format2 $ident
               }
            }
            lappend format1 ".$Xformat_($this,.)"
            lappend format2 ".$Yformat_($this,.)"
         } else {
            #  Simple C-printf precision format only.
            set format1 "format(1)=%%.$Xformat_($this,.)g"
            set format2 "format(2)=%%.$Xformat_($this,.)g"
         }
         lappend options $format1
         lappend options $format2
      }
      return $options
   }

   #  Draw the grid (really).
   protected method draw_grid_ {} {
      busy {
         update idletasks

         #  Make sure all pages are revealed (this makes the widgets
         #  etc. available and sets all default values).
         reveal_ 0 1
         if { $drawn_ } {
            catch {remove_grid_}
         }
         set drawn_ 1

         #  Get the current options.
         set options [gen_options_]

         #  Set the grid tags, ast_tag for general control, grid_tag_
         #  for this particular grid.
         $itk_option(-rtdimage) configure -ast_tag \
            "$itk_option(-ast_tag) $grid_tag_"

         #  If requested just display over the visible canvas +/- a little.
         if { ! $gridsize_($this,whole) } {
            set xf [expr 0.5*(1.0-$gridsize_($this,xfrac))]
            set yf [expr 0.5*(1.0-$gridsize_($this,yfrac))]
            set w [winfo width $itk_option(-canvas)]
            set h [winfo height $itk_option(-canvas)]
            set x0 [$itk_option(-canvas) canvasx 0]
            set y0 [$itk_option(-canvas) canvasy 0]
            set dw [expr $w*$xf]
            set dh [expr $h*$yf]
            set x1 [expr $x0+$w-$dw]
            set y1 [expr $y0+$h-$dh]
            set x0 [expr $x0+$dw]
            set y0 [expr $y0+$dh]

            #  Finally plot the grid, using canvas region if needed.
            $itk_option(-rtdimage) plotgrid $options [list $x0 $y0 $x1 $y1]
         } else {
            $itk_option(-rtdimage) plotgrid $options
         }
      }
   }

   #  Remove the grid.
   protected method remove_grid_ {} {
      $itk_option(-canvas) delete $grid_tag_
      set drawn_ 0
   }

   #  Save the current options to a file.
   public method write_file {} {
      set w [util::FileSelect .\#auto -title "Save options to a file"]
      if {[$w activate]} {
         save_options [$w get]
      }
      destroy $w
   }

   #  Restore options from a file.
   public method read_file {} {
      set w [util::FileSelect .\#auto -title "Read options from a file"]
      if {[$w activate]} {
         read_options [$w get]
      }
      destroy $w
   }

   #  Write the current options to a named file. These are stored in a
   #  format so we can just source the file to reset the values.
   public method save_options {filename} {
      if { $filename != {} } {
         busy {

            #  Make sure all widgets and their values are available.
            reveal_ 0 1

            #  Open the file.
            set fid [::open $filename w]

            #  Position options.
            puts $fid "set position_(\$this,edge1) $position_($this,edge1)"
            puts $fid "set position_(\$this,edge2) $position_($this,edge2)"
            if { $position_($this,labelat1) != "" } {
               puts $fid "set position_(\$this,labelat1) $position_($this,labelat1)"
            }
            if { $position_($this,labelat2) != "" } {
               puts $fid "set position_(\$this,labelat2) $position_($this,labelat2)"
            }
            puts $fid "set position_(\$this,labelup1) $position_($this,labelup1)"
            puts $fid "set position_(\$this,labelup2) $position_($this,labelup2)"
            puts $fid "set position_(\$this,labelling) $position_($this,labelling)"

            #  Add spacing options.
            foreach {sname lname default} \
               "$scaleattrib_ $spacingattrib_ $minorattrib_" {
                  puts $fid "set spacing_($sname) $spacing_($sname)"
               }

            #  Add scaling options.
            foreach {sname lname deffont defsize} $fontattrib_ {
               puts $fid "set size_($sname) $size_($sname)"
            }
            foreach {sname lname default} $widthattrib_ {
               puts $fid "set width_($sname) $width_($sname)"
            }
            foreach {sname lname default} $lengthattrib_ {
               puts $fid "set size_($sname) $size_($sname)"
            }
            puts $fid "set gridsize_(\$this,whole) $gridsize_($this,whole)"
            puts $fid "set gridsize_(\$this,xfrac) $gridsize_($this,xfrac)"
            puts $fid "set gridsize_(\$this,yfrac) $gridsize_($this,xfrac)"

            #  Add style options.
            foreach {sname lname default} $styleattrib_ {
               puts $fid "set style_($sname) $style_($sname)"
            }

            #  Add the elements options.
            foreach {sname lname default} $elementattrib_ {
               puts $fid "set element_(\$this,$sname) $element_($this,$sname)"
            }

            #  Add the colour options.
            foreach {sname lname default} $colourattrib_ {
               puts $fid "set colour_($sname) $colour_($sname)"
            }

            #  Add the font options.
            foreach {sname lname deffont defsize} $fontattrib_ {
               puts $fid "set font_($sname) $font_($sname)"
            }

            #  Add the astrometric system that we want to plot in.
            puts $fid "set system_(system) $system_(system)"
            set system_(epoch) [$itk_component(Epoch) get]
            puts $fid "set system_(epoch) $system_(epoch)"
            set system_(equinox) [$itk_component(Equinox) get]
            puts $fid "set system_(equinox) $system_(equinox)"

            #  Add any new labels.
            foreach {sname lname} $labelattrib_ {
               if { $label_($this,$sname) != "" } {
                  puts $fid "set label_(\$this,$sname) \"$label_($this,$sname)\""
               }
            }

            #  Format strings.
            puts $fid "set Xformat_(\$this,sep) $Xformat_($this,sep)"
            puts $fid "set Yformat_(\$this,sep) $Yformat_($this,sep)"
            foreach {lname ident} $formatattrib_ {
               puts $fid "set Xformat_(\$this,$ident) $Xformat_($this,$ident)"
               puts $fid "set Yformat_(\$this,$ident) $Yformat_($this,$ident)"
            }
            puts $fid "set Xformat_(\$this,.) $Xformat_($this,.)"
            puts $fid "set Yformat_(\$this,.) $Yformat_($this,.)"
            puts $fid "set format_(\$this,noformat) $format_($this,noformat)"
            ::close $fid
         }
      }
   }

   #  Restore any options that have been saved to a file.
   public method read_options {filename} {
      if { [file readable $filename] } {
         busy {
            #  Make sure all widgets and their values are available.
            reveal_ 0 1

            #  Source the file, making sure disabled widgets can be set.
            set format_($this,noformat) 0
            enable_format_all_
            $itk_component(Xfrac) configure -state normal
            $itk_component(Yfrac) configure -state normal
            source $filename

            #  We now need to update all buttons etc. that do not have an
            #  associated variable so as to reflect these changes.

            #  Position options.
            $itk_component(edge1) configure -value $position_($this,edge1)
            $itk_component(edge2) configure -value $position_($this,edge2)

            #  Spacing options.
            foreach {sname lname default} \
               "$scaleattrib_ $spacingattrib_ $minorattrib_" {
                  $itk_component(Spacing$sname) configure -value $spacing_($sname)
               }

            #  Scaling options.
            foreach {sname lname deffont defsize} $fontattrib_ {
               $itk_component(Size$sname) configure -value $size_($sname)
            }
            foreach {sname lname default} $widthattrib_ {
               $itk_component(Width$sname) configure -value $width_($sname)
            }
            foreach {sname lname default} $lengthattrib_ {
               $itk_component(Length$sname) configure -value $size_($sname)
            }
            $itk_component(Xfrac) configure -value $gridsize_($this,xfrac)
            $itk_component(Yfrac) configure -value $gridsize_($this,yfrac)
            set_whole_

            #  Style options.
            foreach {sname lname default} $styleattrib_ {
               $itk_component(Style$sname) configure -value $style_($sname)
            }

            #  Colour options.
            foreach {sname lname default} $colourattrib_ {
               set realcolour [lindex $colourmap_ [expr $colour_($sname)*2+1]]
               $itk_component(Colour$sname) configure -value $realcolour
            }

            #  Font options.
            foreach {sname lname deffont defsize} $fontattrib_ {
               set realfont [lindex $fontmap_ [expr $font_($sname)*3+1]]
               $itk_component(Font$sname) configure -value $realfont
            }

            #  Format options.
            $itk_component(SepX) configure -value $Xformat_($this,sep)
            $itk_component(SepY) configure -value $Yformat_($this,sep)
            $itk_component(DigitX) configure -value $Xformat_($this,.)
            $itk_component(DigitY) configure -value $Yformat_($this,.)
            if { $format_($this,noformat) } {
               $itk_component(Noformat) select
            } else {
               $itk_component(Noformat) deselect
            }
            enable_format_all_

            #  Astrometric system.
            $itk_component(System)  configure -value $system_(system)
            $itk_component(Epoch)   configure -value $system_(epoch)
            $itk_component(Equinox) configure -value $system_(equinox)

            #  Restore the greyed out look correctly by getting the
            #  command back from the appropriate menu item.
            set cmd [$itk_component(System).mb.m entrycget "$system_(system)*" -command]
            eval $cmd

            #  And reset the default system to the correct values for
            #  this image.
            set_system_defaults_
         }
      }
   }

   #  Add a series of controls for setting the various spacings
   #  (grid spacing being the main value).
   protected method add_spacing_selections_ {parent} {

      #  These values are scale factors.
      itk_component add space1 {
         gaia::LabelRule $parent.space1 -text "$scaleannounce_"
      }
      pack $itk_component(space1) -fill x -ipadx 1m
      foreach {sname lname default} $scaleattrib_ {
         set spacing_($sname) $default
         itk_component add Spacing$sname {
            util::LabelEntryScale $parent.spacing$sname \
               -text "$lname:" \
               -labelwidth 9 \
               -valuewidth $vwidth_ \
               -increment 0.1  \
               -resolution 0.1 \
               -from 0.1 \
               -to 4.0 \
               -show_arrows 1 \
               -anchor w \
               -value $default \
               -command [code $this set_spacing_config_ $sname]
         }
         pack $itk_component(Spacing$sname) -side top -fill x -ipadx 1m -ipady 1m
      }

      #  These values are a fraction of the plotting area.
      itk_component add space2 {
         gaia::LabelRule $parent.space2 -text "$spacingannounce_"
      }
      pack $itk_component(space2) -fill x -ipadx 1m
      foreach {sname lname default} $spacingattrib_ {
         set spacing_($sname) $default
         itk_component add Spacing$sname {
            util::LabelEntryScale $parent.spacing$sname \
               -text "$lname:" \
               -labelwidth 9 \
               -valuewidth $vwidth_ \
               -increment 0.005  \
               -resolution 0.005 \
               -from -0.1 \
               -to 0.1 \
               -show_arrows 1 \
               -anchor w \
               -value $default \
               -command [code $this set_spacing_config_ $sname]
         }
         pack $itk_component(Spacing$sname) -side top -fill x -ipadx 1m -ipady 1m
      }

      #  These are integers (number of ticks usually).
      itk_component add space3 {
         gaia::LabelRule $parent.space3 -text "$minorannounce_"
      }
      pack $itk_component(space3) -fill x -ipadx 1m
      foreach {sname lname default} $minorattrib_ {
         set spacing_($sname) $default
         itk_component add Spacing$sname {
            util::LabelEntryScale $parent.spacing$sname \
               -text "$lname:" \
               -labelwidth 9 \
               -valuewidth $vwidth_ \
               -from 0.0 \
               -to 20.0 \
               -increment 1.0  \
               -resolution 1.0 \
               -show_arrows 1 \
               -anchor w \
               -value $default \
               -command [code $this set_spacing_config_ $sname]
         }
         pack $itk_component(Spacing$sname) -side top -fill x -ipadx 1m -ipady 1m
      }
   }

   #  Reset the spacing controls to their default values.
   protected method reset_spacing_ {} {
      foreach {sname lname default} \
         "$scaleattrib_ $spacingattrib_ $minorattrib_"  {
            set spacing_($sname) $default
            $itk_component(Spacing$sname) configure -value $default
      }
   }

   #  Add a series of controls to allow the choice of where elements
   #  are displayed.
   protected method add_position_selections_ {parent} {

      #  Determine which edges the axes are plotted on.
      itk_component add position1 {
         gaia::LabelRule $parent.position1 -text "Label placement:"
      }
      pack $itk_component(position1) -fill x -ipadx 1m

      #  Check if image is rotated. If so then swap lef-right,
      #  top-bottom pairing around.
      if { [$itk_option(-rtdimage) rotate] } {
         set xpos "default left right bottom top"
         set ypos "default bottom top left right"
      } else {
         set xpos "default bottom top left right"
         set ypos "default left right bottom top"
      }
      foreach {axis lname sides} [list 1 {X Label} $xpos 2 {Y Label} $ypos] {
         itk_component add edge$axis {
            util::LabelMenu $parent.edge$axis \
               -text "$lname:" \
               -relief raised \
               -labelwidth $lwidth_
         }
         pack $itk_component(edge$axis) -side top -fill x -ipadx 1m -ipady 1m

         #  Now add all the options.
         foreach side $sides {
            $itk_component(edge$axis) add \
               -command [code $this set_position_config_ edge $axis $side] \
               -label $side \
               -value $side
         }
         set position_($this,edge$axis) [lindex $sides 0]
      }

      #  Labelling of the numbers. Interior or exterior.
      itk_component add position2 {
         gaia::LabelRule $parent.position2 -text "Suggested axis placement:"
      }
      pack $itk_component(position2) -fill x -ipadx 1m
      itk_component add Labelling {
         gaia::StarLabelCheck $parent.labelling \
            -text "Interior:" \
            -onvalue "interior" \
            -offvalue "exterior" \
            -labelwidth $lwidth_ \
            -variable [scope position_($this,labelling)] \
            -command [code $this redraw_]
      }
      pack $itk_component(Labelling) -side top -fill x -ipadx 1m -ipady 1m

      #  LabelAt, where to place the numeric labels. These require
      #  either a RA, Dec set of values or some other sensible value.
      itk_component add position3 {
         gaia::LabelRule $parent.position3 -text "Place numeric labels along:"
      }
      pack $itk_component(position3) -fill x -ipadx 1m
      itk_component add Xat {
         util::LabelEntry $parent.xat \
            -text "X Axis (Y value):" \
            -labelwidth $lwidth_ \
            -textvariable [scope position_($this,labelat1)] \
            -command [code $this redraw_]
      }
      pack $itk_component(Xat) -side top -fill x -ipadx 1m -ipady 1m
      itk_component add Yat {
         util::LabelEntry $parent.yat \
            -text "Y Axis (X value):" \
            -labelwidth $lwidth_ \
            -textvariable [scope position_($this,labelat2)] \
            -command [code $this redraw_]
      }
      pack $itk_component(Yat) -side top -fill x -ipadx 1m -ipady 1m

      #  LabelUp: determine whether the numeric labels are plotted
      #  upright or rotated.
      itk_component add position4 {
         gaia::LabelRule $parent.position4 -text "Non-rotated numeric labels:"
      }
      pack $itk_component(position4) -fill x -ipadx 1m
      itk_component add Upright1 {
         gaia::StarLabelCheck $parent.upright1 \
            -text "X Axis:" \
            -labelwidth $lwidth_ \
            -variable [scope position_($this,labelup1)] \
            -command [code $this redraw_]
      }
      set position_($this,labelup1) 0
      pack $itk_component(Upright1) -side top -fill x -ipadx 1m -ipady 1m

      itk_component add Upright2 {
         gaia::StarLabelCheck $parent.upright2 \
            -text "Y Axis:" \
            -labelwidth $lwidth_ \
            -variable [scope position_($this,labelup2)] \
            -command [code $this redraw_]
      }
      set position_($this,labelup2) 0
      pack $itk_component(Upright2) -side top -fill x -ipadx 1m -ipady 1m
   }

   #  Reset the selections controls.
   protected method reset_position_ {} {
      reset_edge_
      set position_($this,labelling) "interior"
      set position_($this,labelat1) ""
      set position_($this,labelat2) ""
      set position_($this,labelup1) 0
      set position_($this,labelup2) 0
   }

   #  Reset the positioning of the edge labels. These go back to the
   #  AST defaults.
   protected method reset_edge_ {} {
      $itk_component(edge1) configure -value "default"
      set position_($this,edge1) "default"
      $itk_component(edge2) configure -value "default"
      set position_($this,edge2) "default"
   }

   #   Add a series of controls for setting the sizes/scales of the
   #   various parts.
   protected method add_scaling_selections_ {parent} {

      #  Size of various text elements.
      itk_component add style1 {
         gaia::LabelRule $parent.style1 -text "$fontannounce1_"
      }
      pack $itk_component(style1) -fill x -ipadx 1m
      foreach {sname lname deffont defsize} $fontattrib_ {
         set size_($sname) $defsize
         itk_component add Size$sname {
            util::LabelEntryScale $parent.size$sname \
               -text "$lname:" \
               -value $defsize \
               -labelwidth 10 \
               -valuewidth $vwidth_ \
               -from 0.25 \
               -to 4.0 \
               -increment 0.25 \
               -resolution 0.25 \
               -show_arrows 1 \
               -anchor w \
               -command [code $this set_size_config_ $sname]
         }
         pack $itk_component(Size$sname) -side top -fill x -ipadx 1m -ipady 1m
      }

      #  Width of various line elements.
      itk_component add scale2 {
         gaia::LabelRule $parent.scale2 -text "$widthannounce_"
      }
      pack $itk_component(scale2) -fill x -ipadx 1m
      foreach {sname lname default} $widthattrib_ {
         set width_($sname) $default
         itk_component add Width$sname {
            util::LabelEntryScale $parent.width$sname \
               -text "$lname:" \
               -value $default \
               -labelwidth 10 \
               -valuewidth $vwidth_ \
               -from 1.0 \
               -to 10.0 \
               -increment 1.0  \
               -resolution 0.5 \
               -show_arrows 1 \
               -anchor w \
               -command [code $this set_width_config_ $sname]
         }
         pack $itk_component(Width$sname) -side top -fill x -ipadx 1m -ipady 1m
      }

      #  Lengths of various line elements.
      itk_component add scale3 {
         gaia::LabelRule $parent.scale3 -text "$lengthannounce_"
      }
      pack $itk_component(scale3) -fill x -ipadx 1m
      foreach {sname lname default} $lengthattrib_ {
         set size_($sname) $default
         itk_component add Length$sname {
            util::LabelEntryScale $parent.width$sname \
               -text "$lname:" \
               -value $default \
               -labelwidth 10 \
               -valuewidth $vwidth_ \
               -increment 1  \
               -resolution 1 \
               -from -10 \
               -to 10 \
               -show_arrows 1 \
               -anchor w \
               -command [code $this set_size_config_ $sname]
         }
         pack $itk_component(Length$sname) -side top -fill x -ipadx 1m -ipady 1m
      }

      #  Control what area the plot covers. This can be the whole just
      #  or just the part that is displayed.
      itk_component add scale4 {
         gaia::LabelRule $parent.scale4 -text "Grid size:"
      }
      pack $itk_component(scale4) -fill x -ipadx 1m
      itk_component add Whole {
         gaia::StarLabelCheck $parent.whole \
            -text "Whole of image:" \
            -onvalue 1 \
            -offvalue 0 \
            -labelwidth 15 \
            -variable [scope gridsize_($this,whole)] \
            -command [code $this set_whole_]
      }
      set gridsize_($this,whole) 1
      pack $itk_component(Whole) -side top -fill x -ipadx 1m -ipady 1m

      #  Control the fraction of the display that grid covers. Only
      #  relevant if not drawing over the whole image.
      set gridsize_($this,xfrac) 0.8
      itk_component add Xfrac {
         util::LabelEntryScale $parent.xfrac \
            -text "X display fraction:" \
            -labelwidth 15 \
            -valuewidth $vwidth_ \
            -from 0.01 \
            -to 0.99 \
            -increment 0.01  \
            -resolution 0.01 \
            -show_arrows 1 \
            -anchor w \
            -value $gridsize_($this,xfrac) \
            -command [code $this set_gridsize_config_ xfrac]
      }
      pack $itk_component(Xfrac) -side top -fill x -ipadx 1m -ipady 1m

      set gridsize_($this,yfrac) 0.8
      itk_component add Yfrac {
         util::LabelEntryScale $parent.yfrac \
            -text "Y display fraction:" \
            -labelwidth 15 \
            -valuewidth $vwidth_ \
            -from 0.01 \
            -to 0.99 \
            -increment 0.01  \
            -resolution 0.01 \
            -show_arrows 1 \
            -anchor w \
            -value $gridsize_($this,yfrac) \
            -command [code $this set_gridsize_config_ yfrac]
      }
      pack $itk_component(Yfrac) -side top -fill x -ipadx 1m -ipady 1m
      set_whole_
   }

   #  Reset the scaling controls to their defaults.
   protected method reset_scaling_ {} {
      foreach {sname lname deffont defsize} $fontattrib_ {
         set size_($sname) $defsize
         $itk_component(Size$sname) configure -value $defsize
      }
      foreach {sname lname default} $widthattrib_ {
         set width_($sname) $default
         $itk_component(Width$sname) configure -value $default
      }
      foreach {sname lname default} $lengthattrib_ {
         set size_($sname) $default
         $itk_component(Length$sname) configure -value $default
      }
      set gridsize_($this,whole) 1
      set gridsize_($this,xfrac) 0.8
      $itk_component(Xfrac) configure -value 0.8
      set gridsize_($this,yfrac) 0.8
      $itk_component(Yfrac) configure -value 0.8
      set_whole_
   }

   #  Disable the display fraction widget if not allowed.
   protected method set_whole_ {} {
      if { $gridsize_($this,whole) } {
         $itk_component(Xfrac) configure -state disabled
         $itk_component(Yfrac) configure -state disabled
      } else {
         $itk_component(Xfrac) configure -state normal
         $itk_component(Yfrac) configure -state normal
      }
      redraw_
   }

   #  Set the value of gridsize element.
   protected method set_gridsize_config_ {name value} {
      set gridsize_($this,$name) $value
      redraw_
   }

   #   Add a series of controls for setting the styles of line elements.
   protected method add_style_selections_ {parent} {

      #  Width of various line elements.
      itk_component add scale1 {
         gaia::LabelRule $parent.scale1 -text "$styleannounce_"
      }
      pack $itk_component(scale1) -fill x -ipadx 1m

      foreach {sname lname default} $styleattrib_ {
         set style_($sname) $default

         #  Create a menu as the selection is fixed.
         itk_component add Style$sname {
            util::LabelMenu $parent.$sname \
               -text "$lname:" \
               -relief raised \
               -labelwidth $lwidth_
         }
         pack $itk_component(Style$sname) -side top -fill x -ipadx 1m -ipady 1m

         #  Now add the style numbers.
         for { set i 0 } { $i < 4 } { incr i } {
            $itk_component(Style$sname) add \
               -command [code $this set_style_config_ $sname $i] \
               -bitmap style$i \
               -value $i
         }
         $itk_component(Style$sname) configure -value $default
      }
   }

   #  Reset the style controls to their defaults.
   protected method reset_style_ {} {
      foreach {sname lname default} $styleattrib_ {
         set style_($sname) $default
         $itk_component(Style$sname) configure -value $default
      }
   }


   #  Add a series of controls to allow the choice of which elements
   #  to display as part of the plot.
   protected method add_element_selections_ {parent} {
      itk_component add element1 {
         gaia::LabelRule $parent.element1 -text "$elementannounce_"
      }
      pack $itk_component(element1) -fill x -ipadx 1m
      foreach {sname lname default} $elementattrib_ {
         set element_($this,$sname) $default
         itk_component add Element$sname {
            gaia::StarLabelCheck $parent.$sname \
               -text "$lname:" \
               -labelwidth $lwidth_ \
               -variable [scope element_($this,$sname)] \
               -command [code $this redraw_]
         }
         pack $itk_component(Element$sname) -side top -fill x -ipadx 1m -ipady 1m
      }
   }

   #  Reset element selection controls to their default values.
   protected method reset_element_ {} {
      foreach {sname lname default} $elementattrib_ {
         set element_($this,$sname) $default
      }
   }

   #  Add widgets for selecting between the various colours for the
   #  various elements.
   protected method add_colour_selections_ {parent} {
      itk_component add colour1 {
         gaia::LabelRule $parent.colour1 -text "$colourannounce_"
      }
      pack $itk_component(colour1) -fill x -ipadx 1m
      foreach {sname lname default} $colourattrib_ {
         set colour_($sname) $default
         set realcolour [lindex $colourmap_ [expr $colour_($sname)*2+1]]

         #  Create a menu that allows the visual selection of one of the
         #  known colours. Use a LabelMenu widget to host the choice.
         itk_component add Colour$sname {
            util::LabelMenu $parent.$sname \
               -text "$lname:" \
               -relief raised \
               -labelwidth $lwidth_
         }
         pack $itk_component(Colour$sname) -side top -fill x -ipadx 1m -ipady 1m

         #  Now add all the colours.
         foreach {index xname} $colourmap_ {
            $itk_component(Colour$sname) add \
               -command [code $this set_colour_config_ $sname $index] \
               -label {    } \
               -value $xname \
               -background $xname
         }
         $itk_component(Colour$sname) configure -value $realcolour
      }
   }

   #  Reset the colour controls to their default values.
   protected method reset_colour_ {} {
      foreach {sname lname default} $colourattrib_ {
         set colour_($sname) $default
         set realcolour [lindex $colourmap_ [expr $colour_($sname)*2+1]]
         $itk_component(Colour$sname) configure -value $realcolour
      }
   }

   #  Add widgets for selecting between the various fonts for the
   #  various elements.
   protected method add_font_selections_ {parent} {
      itk_component add font1 {
         gaia::LabelRule $parent.font1 -text "$fontannounce2_"
      }
      pack $itk_component(font1) -fill x -ipadx 1m
      foreach {sname lname deffont defsize} $fontattrib_ {
         set font_($sname) $deffont
         set realfont [lindex $fontmap_ [expr $font_($sname)*3+1]]

         #  Create a menu that allows the visual selection of one of the
         #  known fonts. Use a LabelMenu widget to host the choice.
         itk_component add Font$sname {
            util::LabelMenu $parent.$sname \
               -text "$lname:" \
               -relief raised \
               -labelwidth 12 \
               -valuewidth $vwidth_
         }
         pack $itk_component(Font$sname) -side top -fill x -ipadx 1m -ipady 1m

         catch {
            #  Now add all the fonts.
            foreach {index xname desc} $fontmap_ {
               $itk_component(Font$sname) add \
                  -command [code $this set_font_config_ $sname $index] \
                  -label $desc \
                  -value $xname \
                  -font $xname
            }
            $itk_component(Font$sname) configure -value $realfont
         }
      }
   }

   #  Reset the font controls to their default values.
   protected method reset_font_ {} {
      foreach {sname lname deffont defsize} $fontattrib_ {
         set font_($sname) $deffont
         set realfont [lindex $fontmap_ [expr $font_($sname)*3+1]]
         $itk_component(Font$sname) configure -value $realfont
      }
   }

   #  Add controls that allow a different astrometry system to be
   #  drawn.
   protected method add_system_selections_ {parent} {

      #  There are only three elements that can be controlled: epoch,
      #  equinox and system. Each of these defaults to the string
      #  "default" which is taken to mean do not set this option.
      #  The default label (which is always first in the list)
      #  is appended with the string showin the actual value, when a
      #  system is selected.
      itk_component add system1 {
         gaia::LabelRule $parent.system1 -text "$systemannounce_"
      }
      pack $itk_component(system1) -side top -fill x -ipadx 1m -ipady 1m

      #  System.
      itk_component add System {
         util::LabelMenu $parent.system \
            -text "System:" \
            -relief raised \
            -labelwidth 8
      }
      pack $itk_component(System) -side top -fill x -ipadx 1m -ipady 1m
      foreach {system needepoch needequinox} $systemattrib_ {
         $itk_component(System) add \
            -command [code $this set_system_config_ system $system $needepoch $needequinox] \
            -label $system \
            -value $system
      }

      #  Epoch (date of observation usually).
      itk_component add Epoch {
         gaia::LabelEntryMenu $parent.epoch \
            -text "Epoch:" \
            -labelwidth 8
      }
      pack $itk_component(Epoch) -side top -fill x -ipadx 1m -ipady 1m
      foreach epoch $epochmap_ {
         $itk_component(Epoch) add \
            -label $epoch \
            -value $epoch \
            -command [code $this redraw_]
      }
      set system_(epoch) default

      #  Equinox, J2000 or B1950 usually.
      itk_component add Equinox {
         gaia::LabelEntryMenu $parent.equinox \
            -text "Equinox:" \
            -labelwidth 8
      }
      pack $itk_component(Equinox) -side top -fill x -ipadx 1m -ipady 1m
      foreach equinox $equinoxmap_ {
         $itk_component(Equinox) add \
            -label $equinox \
            -value $equinox \
         -command [code $this redraw_]
      }

      #  Set the defaults for all the known systems (these are used to
      #  set the labels for the default identifiers).
      foreach {system epoch equinox} $systemmap_ {
         set system_defaults_($system,epoch)   $epoch
         set system_defaults_($system,equinox) $equinox
      }

      #  Set the first system (default all round).
      set system_(equinox) default
      set_system_config_ system default 1 1

      #  And set the values that system "default" equates on the labels.
      set_system_defaults_
   }

   #  Set the labels of the defaults for the default system.
   protected method set_system_defaults_ {} {
      set system_defaults_(default,system)  [$itk_option(-rtdimage) astget system]
      set system_defaults_(default,epoch)   [$itk_option(-rtdimage) astget epoch]
      set system_defaults_(default,equinox) [$itk_option(-rtdimage) astget equinox]

      #  Update the default labels to reflect the current system (which
      #  may not be the default one).
      $itk_component(System).mb.m entryconfigure 0 -label \
         "default ($system_defaults_(default,system))"
      $itk_component(Epoch).mb.m entryconfigure 0 -label \
         "default ($system_defaults_($system_(system),epoch))"
      $itk_component(Equinox).mb.m entryconfigure 0 -label \
         "default ($system_defaults_($system_(system),equinox))"
   }

   #  Reset the system controls to their defaults.
   protected method reset_system_ {} {
      $itk_component(System) configure -value "default"
      set system_(system) default
      $itk_component(Epoch) configure -value "default"
      set system_(epoch) default
      $itk_component(Equinox) configure -value "default"
      set system_(equinox) default
      set_system_config_ system default 1 1

      #  And set the values that system "default" equates on the labels.
      set_system_defaults_
   }


   #  Add controls that change the title and labels of the plot.
   protected method add_label_selections_ {parent} {

      #  Text content.
      itk_component add label1 {
         gaia::LabelRule $parent.label1 -text "$labelannounce_"
      }
      pack $itk_component(label1) -side top -fill x -ipadx 1m -ipady 1m

      global ::tcl_version

      foreach {sname lname} $labelattrib_ {
         itk_component add Label$sname {
            util::LabelEntry $parent.label$sname \
               -text "$lname:" \
               -labelwidth 8 \
               -textvariable [scope label_($this,$sname)] \
               -command [code $this redraw_]
         }
         pack $itk_component(Label$sname) -side top -fill x -ipadx 1m -ipady 1m
      }
   }

   #  Reset label controls to their defaults.
   protected method reset_label_ {} {
      foreach {sname lname} $labelattrib_ {
         $itk_component(Label$sname) configure -value {}
      }
   }

   #  Add controls that change the format of the labels of the plot.
   protected method add_format_selections_ {parent} {

      #  Add a toggle for controlling if the default AST format
      #  control will be used, or the format string as shown.
      itk_component add Noformat {
         gaia::StarLabelCheck $parent.noformat \
            -text "Use default formatting:" \
            -onvalue 1 \
            -offvalue 0 \
            -labelwidth 25 \
            -variable [scope format_($this,noformat)] \
            -command [code $this enable_format_all_]
      }
      pack $itk_component(Noformat) -side top -fill x -ipadx 1m -ipady 1m

      #  For each axes add the formatting controls.
      foreach axis {X Y} {

         #  Add a label for this axis.
         itk_component add Label$axis {
            gaia::LabelRule $parent.label$axis -text "$axis axis:" -anchor w
         }
         pack $itk_component(Label$axis) -side top -fill x -ipadx 1m -ipady 1m

         #  Add choices for each possible format character.
         foreach {lname ident} $formatattrib_ {
            set ${axis}format_($this,$ident) 0
            itk_component add Label$ident$axis {
               gaia::StarLabelCheck $parent.$ident$axis \
                  -text "$lname:" \
                  -onvalue 1 \
                  -offvalue 0 \
                  -labelwidth $lwidth_ \
                  -variable [scope ${axis}format_($this,$ident)] \
                  -command [code $this redraw_]
            }
            pack $itk_component(Label$ident$axis) -side top -fill x -ipadx 1m -ipady 1m
         }

         #  Create a menu that allows the visual selection of one of the
         #  known separators. Use a LabelMenu widget to host the choice.
         set ${axis}format_($this,sep) i
         itk_component add Sep$axis {
            util::LabelMenu $parent.sep$axis \
               -text "Separator:" \
               -relief raised \
               -labelwidth $lwidth_
         }
         pack $itk_component(Sep$axis) -side top -fill x -ipadx 1m -ipady 1m

         #  Now add all the choices.
         foreach {label value} $sepmap_ {
            $itk_component(Sep$axis) add \
               -command [code $this set_${axis}format_config_ sep $value] \
               -label $label \
               -value $value
         }

         #  Add a scale to select the number of digits.
         itk_component add Digit$axis {
            util::LabelEntryScale $parent.digit$axis \
               -text "Digits:" \
               -labelwidth 9 \
               -valuewidth $vwidth_ \
               -value 0 \
               -increment 1  \
               -resolution 1 \
               -from 0 \
               -to 6 \
               -show_arrows 1 \
               -anchor w \
               -command [code $this set_${axis}format_config_ "."]
         }
         pack $itk_component(Digit$axis) -side top -fill x -ipadx 1m -ipady 1m
      }

      #  Now set all the default values correctly.
      reset_format_
   }

   #  Reset the label format controls to their defaults.
   protected method reset_format_ {} {

      #  Reset main boolean elements to false.
      foreach {lname ident} $formatattrib_ {
         set Xformat_($this,$ident) 0
         set Yformat_($this,$ident) 0
      }
      set Xformat_($this,.) 0
      set Yformat_($this,.) 0
      set Xformat_($this,sep) i
      set Yformat_($this,sep) i
      set format_($this,noformat) 1

      #  Get the format string from the skyframe. The tricky bits are
      #  dealing with the number of digits and picking between
      #  the separators (ibl).
      set format [$itk_option(-rtdimage) astget "format(1)"]
      set keepnext 0
      foreach element [split $format {}] {
         if { $element == "." } {
            set keepnext 1
         } elseif { $keepnext } {
            set keepnext 0
            set Xformat_($this,.) $element
         } else {
            set keepnext 0
            set Xformat_($this,$element) 1
         }
      }

      #  Now check out the presence of the separators and combine
      #  these into a single value.
      if { [info exists Xformat_($this,b)] } {
         set Xformat_($this,sep) b
      }
      if { [info exists Xformat_($this,l)] } {
         set Xformat_($this,sep) l
      }
      if { [info exists Xformat_($this,g)] } {
         set Xformat_($this,sep) g
      }

      set format [$itk_option(-rtdimage) astget "format(2)"]
      set keepnext 0
      foreach element [split $format {}] {
         if { $element == "." } {
            set keepnext 1
         } elseif { $keepnext } {
            set keepnext 0
            set Yformat_($this,.) $element
         } else {
            set keepnext 0
            set Yformat_($this,$element) 1
         }
      }

      if { [info exists Yformat_($this,b)] } {
         set Yformat_($this,sep) b
      }
      if { [info exists Yformat_($this,l)] } {
         set Yformat_($this,sep) l
      }
      if { [info exists Yformat_($this,g)] } {
         set Yformat_($this,sep) g
      }

      #  And set the widgets to the correct values if necessary.
      $itk_component(SepX) configure -value $Xformat_($this,sep)
      $itk_component(DigitX) configure -value $Xformat_($this,.)
      $itk_component(SepY) configure -value $Xformat_($this,sep)
      $itk_component(DigitY) configure -value $Xformat_($this,.)
      if { $format_($this,noformat) } {
         $itk_component(Noformat) select
      } else {
         $itk_component(Noformat) deselect
      }
      enable_format_all_
   }

   #  Methods to set the value of a configuration array option.
   protected method set_position_config_ {name axis value} {
      set position_($this,$name$axis) $value
      redraw_
   }
   protected method set_size_config_ {name value} {
      set size_($name) $value
      redraw_
   }
   protected method set_width_config_ {name value} {
      set width_($name) $value
      redraw_
   }
   protected method set_style_config_ {name value} {
      set style_($name) $value
      redraw_
   }
   protected method set_font_config_ {name value} {
      set font_($name) $value
      redraw_
   }
   protected method set_colour_config_ {name value} {
      set colour_($name) $value
      redraw_
   }
   protected method set_system_config_ {name value needepoch needequinox} {
      set system_($name) $value
      if { ! $needepoch } {
         $itk_component(Epoch) configure -value default
         $itk_component(Epoch) configure -state disabled
      } else {
         $itk_component(Epoch) configure -state normal
      }
      if { ! $needequinox } {
         $itk_component(Equinox) configure -value default
         $itk_component(Equinox) configure -state disabled
      } else {
         $itk_component(Equinox) configure -state normal
      }

      #  Make sure that the default labels are correct.
      set_system_defaults_
      redraw_
   }
   protected method set_spacing_config_ {name value} {
      set spacing_($name) $value
      redraw_
   }
   protected method set_Xformat_config_ {name value} {
      set Xformat_($this,$name) $value
      redraw_
   }
   protected method set_Yformat_config_ {name value} {
      set Yformat_($this,$name) $value
      redraw_
   }

   #  Disabled or enable all entries if required.
   protected method enable_format_all_ {} {
      if { $format_($this,noformat) } {
         set state disabled
      } else {
         set state normal
      }
      foreach axis {X Y} {
         foreach {lname ident} $formatattrib_ {
            $itk_component(Label$ident$axis) configure -state $state
         }
         $itk_component(Sep$axis) configure -state $state
         $itk_component(Digit$axis) configure -state $state
      }
      redraw_
   }

   protected method arcsec_to_radian_ {value} {

      #  Convert arcseconds to radians (default is 60 arcsec).
      set spacing 0.0002908882
      if { [catch {expr $value/3600.0} degval] } {
         warning_dialog "Cannot interpret \"$value\" as arcseconds"
      } else {
         set spacing [expr $degval*0.017453293]
      }
      return $spacing
   }

   protected method radec_to_radian_ {axis value} {

      #  If value contain a "*:*:*" then attempt to convert it into
      #  degrees and then radians. Otherwise we just accept the
      #  value if it is numeric. If an error is reported then
      #  the associated variable is unset to get things back to
      #  hopefully sensible places.
      set newvalue 0.0
      if { [string match {*:*:*} $value] } {
         set adr [$itk_option(-rtdimage) astgetclone]
         if { [catch {gaiautils::astunformat $adr $axis $value} newvalue ] } {
            warning_dialog "Cannot interpret \"$value\" as a valid HH:MM:SS or DD:MM:SS string ($newvalue)"
            set newvalue $value
         }
         gaiautils::astannul $adr
      } else {
         if { [catch {expr $value*1.0}] } {
            warning_dialog "Cannot interpret \"$value\""
         } else {
            set newvalue $value
         }
      }
      return $newvalue
   }


   #  Control the defaults used. These are for channel maps or the usual
   #  single-image gridded form.
   protected method toggle_chanmap_defaults_ {} {
      if { $chanmap_defaults_ } {
         set elementattrib_ $chanmapelementattrib_
         set position_($this,labelling) "exterior"
      } else {
         set elementattrib_ $normalelementattrib_
         set position_($this,labelling) "interior"
      }
      reset_element_
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of canvas.
   itk_option define -canvas canvas Canvas {} {}

   #  Name of starrtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {} {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  If this is a clone, then it should die rather than be withdrawn.
   itk_option define -really_die really_die Really_Die 0

   #  Ast graphics tag used to control general level redraws of all
   #  AST elements.
   itk_option define -ast_tag ast_tag Ast_Tag {} {
      if { $itk_option(-ast_tag) == {} } {
         set itk_option(-ast_tag) "ast_element"
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  True after first draw of grid.
   protected variable drawn_ 0

   #  Array of option names and values (too numerous for normal
   #  methods).
   protected variable colour_
   protected variable font_
   protected variable size_
   protected variable width_
   protected variable system_
   protected variable spacing_
   protected variable style_

   #  Short and long names of elements that can be plotted and their
   #  defaults.
   protected variable elementannounce_ {Display elements:}
   protected variable elementattrib_ {}

   #  Variation on elements for normal grids and those for channel maps.
   protected variable normalelementattrib_ {
      border {Outer Border} 0 grid {Grid Lines} 1 numlab Numbers 1
      drawtitle {Plot Title} 1 drawaxes Axes 1 textlab {Axes Labels} 1
      tickall {Tick All Axes} 0 labelunits {Axes Units} 0}

   protected variable chanmapelementattrib_ {
      border {Outer Border} 1 grid {Grid Lines} 0 numlab Numbers 1
      drawtitle {Plot Title} 1 drawaxes Axes 1 textlab {Axes Labels} 1
      tickall {Tick All Axes} 1 labelunits {Axes Units} 0}

   #  Short and long names of elements that can have their colour
   #  modified and their defaults.
   protected variable colourannounce_ {Element colouring:}
   protected variable colourattrib_ \
      {title {Plot Title}  11 grid {Grid Lines} 6
       border {Border Lines} 0 numlab {Numbers} 3 axes {Axes} 2
       textlab {Axes Labels} 11 ticks {Tick Marks} 0}

   #  Names of the possible colours and their AST index equivalents.
   protected variable colourmap_ {
      0 "#fff" 1 "#000" 2 "#f00" 3 "#0f0" 4 "#00f" 5 "#0ff" 6 "#f0f"
      7 "#ff0" 8 "#f80" 9 "#8f0" 10 "#0f8" 11 "#08f" 12 "#80f"
      13 "#f08" 14 "#512751275127" 15 "#a8b4a8b4a8b4"}

   #  Short and long names of options that require a spacing
   #  value and their defaults and an indication if the value requires
   #  transformation to radians or an integer (note default of 0.0
   #  indicates no value).
   protected variable scaleannounce_ {Relative grid spacing:}
   protected variable scaleattrib_ \
      { {gap(1)} {X Axis} 1.0
        {gap(2)} {Y Axis} 1.0 }
   protected variable minorannounce_ {Number of minor ticks:}
   protected variable minorattrib_ \
      { {mintick(1)} {X Axis} 0.0
        {mintick(2)} {Y Axis} 0.0}
   protected variable spacingannounce_ {Label offsets:}
   protected variable spacingattrib_ \
      { {numlabgap(1)} {X Numbers} 0.01
        {numlabgap(2)} {Y Numbers} 0.01
        {textlabgap(1)} {X Axis} 0.01
        {textlabgap(2)} {Y Axis} 0.01
        titlegap {Main Title} 0.05}

   #  Short and long names of line elements that can be changed in
   #  width and their defaults.
   protected variable widthannounce_ {Widths:}
   protected variable widthattrib_ \
      {grid {Grid Lines} 1.0 border {Border Lines} 1.0 axes {Axes Lines} 1.0
       ticks {Tick Marks} 1.0}

   #  Short and long names of elements that can be changed in length
   #  and their defaults.
   protected variable lengthannounce_ {Lengths:}
   protected variable lengthattrib_ \
      {majticklen {Major Ticks} 3.0 minticklen {Minor Ticks} 1.0}

   #  Short and long names of elements that can have their font
   #  modified and their default colours and scales.
   protected variable fontannounce1_ {Font scales:}
   protected variable fontannounce2_ {Label fonts:}
   protected variable fontattrib_ \
      {title {Plot Title} 6 1.0 numlab {Numbers} 4 1.0
       textlab {Axes Labels} 4 1.0}

   #  Short and long names of line elements that can be changed in
   #  style and their defaults.
   protected variable styleannounce_ {Style:}
   protected variable styleattrib_ \
      {grid {Grid Lines} 0 border {Border Lines} 0 axes {Axes Lines} 0
       ticks {Tick Marks} 0}

   #  Names of the fonts that we will use and their AST indices.
   #  A text string to very briefly describe the font is also set.
   protected variable fontmap_ $::gaia::astfontmap

   #  Names of all the possible systems we can plot in and their
   #  need for epoch and equinox qualifiers.
   protected variable systemannounce_ {Grid celestial coordinate system:}
   protected variable systemattrib_ \
      {default 1 1 fk5 0 1 fk4 1 1 fk4-no-e 1 1 gappt 1 0 ecliptic 0 1
         galactic 0 0 supergalactic 0 0 azel 1 0 pixels 0 0}

   #  Array of the various system names and their default epochs and equinoxes
   #  and the initialising list. The azel epoch is set to current which means
   #  use the same value as the displayed image.
   protected variable systemmap_ \
      {fk5 {} J2000 fk4 B1950 B1950 fk4-no-e B1950 B1950
         gappt J2000 {} ecliptic {} J2000 galactic {} {}
         supergalactic {} {} azel current {} pixels {} {}}
   protected variable system_defaults_

   #  Names of sensible epochs.
   protected variable epochmap_ \
      "default J2000.0 B1950.0 [clock format [clock seconds] -format {%Y-%b-%dT%H:%M:%S}]"

   #  Names of sensible equinoxes.
   protected variable equinoxmap_ {default J2000.0 B1950.0}

   #  Short and long names of any labels that can be modified.
   protected variable labelannounce_ {Plot labels:}
   protected variable labelattrib_ \
      {title {Main Title} {label(1)} {X Axis} {label(2)} {Y Axis}}

   #  Short and long names of format elements that control the
   #  way numbers are plotted along the axes.
   protected variable formatattrib_ \
      {{Plus Sign} {+} {Leading Zeros} z {Degrees} d {Hours} h
       {Minutes} m {Seconds} s}
   protected variable sepmap_ \
      { {:} i  {blank} b {h/dms} l {h/d'"} g}

   #  Width of labels & value fields.
   protected variable lwidth_ 17
   protected variable vwidth_ 5

   #  Symbolic names of pages (used to access names via index).
   protected variable pages_
   protected variable revealed_

   #  Symbol names of the axis (replaces X and Y in the interface).
   #  XXX not yet.
   #  protected variable xname_ X
   #  protected variable yname_ Y

   #  Unique tag for this grid.
   protected variable grid_tag_ {}

   #  Whether to configure to channel map defaults, or not.
   protected variable chanmap_defaults_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Array of option names and values (too numerous for normal
   #  methods). As the protected option variables above, except these
   #  are also indexed by $this so that they can be set by Tk widgets.
   common element_
   common position_
   common label_
   common Xformat_
   common Yformat_
   common format_
   common gridsize_

   #  True for auto redraw of grid when elements are changed.
   common auto_redraw_

   #  Number of grids on display.
   common grid_count_ 0
#  End of class definition.
}
