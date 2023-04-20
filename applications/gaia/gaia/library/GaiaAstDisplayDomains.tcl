#+
#  Name:
#     GaiaAstDisplayDomains

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Creates a toolbox for displaying the current coordinates in all
#     known domains.

#  Description:
#     This class creates a window that contains displays that show the
#     current coordinate in all the known domains. Domains that have more
#     than two axes can also be handled.

#  Invocations:
#
#        GaiaAstDisplayDomains object_name [configuration options]
#
#     This creates an instance of a GaiaAstDisplayDomains object. The return is
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
#    See itk_option define lines below.

#  Methods:
#     See method definitions below.

#  Inheritance:
#     TopLevelWidget.

#  Copyright:
#     Copyright (C) 2001-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     Copyright (C) 2009 Science and Technology Facilities Council.
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
#     12-DEC-2001 (PWD):
#        Original version.
#     04-MAY-2006 (PWD):
#        Converted to display domains that have more than 2 axes.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaAstDisplayDomains {}

itcl::class gaia::GaiaAstDisplayDomains {

   #  Inheritances:
   #  -------------

   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: show coordinates for all systems ($itk_option(-number))"

      #  Create the short help window.
      make_short_help
      $itk_component(short_help) configure -width 40

      #  Add File menu for usual stuff.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0

      #  Add option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      $short_help_win_ add_menu_short_help $File \
         {New window} {Create a new toolbox}

      #  Options to close window.
      $File add command -label {Close window   } \
         -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]

      #  Add window help.
      add_help_button alldomains "On Window..."

      #  Only one element is available. A dropdown box showing the AST
      #  domains for the current image.
      itk_component add rule {
         gaia::LabelRule $w_.rule -text {Built-in coordinate readouts:}
      }

      #  We need read outs for all domains known to the displayed
      #  image.
      itk_component add readoutarea {
         iwidgets::scrolledframe $w_.readoutarea -height 200 -width 550 \
            -vscrollmode dynamic -hscrollmode dynamic
      }
      update_displays_

      #  Add button to close window.
      itk_component add actionframe {frame $w_.action}

      #  Add a button to close window.
      itk_component add close {
         button $itk_component(actionframe).close -text Close \
	       -command [code $this close]
      }
      add_short_help $itk_component(close) {Close window}

      #  Pack window.
      pack $itk_component(rule) -side top -fill x -ipadx 1m -ipady 1m
      pack $itk_component(readoutarea) -side top -fill both -expand 1 -pady 3 -padx 3
      pack $itk_component(actionframe) -side bottom -fill x -pady 3 -padx 3
      pack $itk_component(close) -side right -expand 1 -pady 1 -padx 1

      #  Bind the wm deiconify event to restart tracing updates.
      bind [winfo toplevel $w_] <Map> [code $this start_trace_]
   }

   #  Destructor:
   #  -----------
   destructor  {
      stop_trace_
   }

   #  Methods:
   #  --------

   #  Create a new instance of this object.
   protected method clone_me_ {} {
      if { $itk_option(-clone_cmd) != {} } {
         eval $itk_option(-clone_cmd)
      }
   }

   #  Close this window.
   public method close {} {
      stop_trace_
      if { $itk_option(-really_die) } {
         delete object $this
      } else {
         wm withdraw $w_
      }
   }

   #  Recreate the components needed to display the coordinates.
   protected method update_displays_ {} {

      #  Clear any existing controls.
      if { [info exists domains_] }  {
         set i 0
         foreach {domain dims} "$domains_" {
            incr i
            catch {destroy $itk_component(label$i)}
            for {set j 1} {$j <= $dims} {incr j} {
               catch {destroy $itk_component(value$i$j)}
            }
         }
         unset domains_
      }

      #  Locate all the domains and their dimensionalities.
      set domains_ [$itk_option(-rtdimage) astdomains 1]
      set i 0
      set lwidth 15
      foreach {domain dims} "$domains_" {
         set lwidth [expr int(max([string length $domain],$lwidth))]
      }

      # Get the current base domain frame index.
      set current [$itk_option(-rtdimage) astget current]

      set w [$itk_component(readoutarea) childsite]
      blt::blttable $w
      foreach {domain dims} "$domains_" {
         incr i

         itk_component add label$i {
            label $w.label$i -text "$domain:" -width $lwidth -anchor e
         }
         blt::blttable $w $itk_component(label$i) $i,0 -fill x

         for {set j 1} {$j <= $dims} {incr j} {
            set values_($i,$j) 0.0
            itk_component add value$i$j {
               entry $w.value$i$j -text "Value $j" \
                  -textvariable [scope values_($i,$j)]
            }

            #  Move to this domain so short help displays label and unit.
            $itk_option(-rtdimage) astset current $i
            set label [$itk_option(-rtdimage) astget label\($j\)]
            set unit [$itk_option(-rtdimage) astget unit\($j\)]
            add_short_help $itk_component(value$i$j) "$label ($unit)"

            blt::blttable $w $itk_component(value$i$j) $i,$j -fill x
         }
      }

      #  Restore default domain.
      $itk_option(-rtdimage) astset current $current
      start_trace_
   }

   #  Called when new coordinates are to be shown.
   protected method update_readouts_ { args } {

      # Get the current base domain frame index.
      set current [$itk_option(-rtdimage) astget current]

      # Access variable with current X and Y coordinates.
      set var $itk_option(-rtdimage)
      global ::$var

      # For each domain, switch the image to it and then transform the
      # X and Y coordinates, update the readout. Trap problem domains
      # (bad coordinates etc.) and pass on to later ones.
      set i 0
      set oldx [set ::${var}(X)]
      set oldy [set ::${var}(Y)]
      foreach {domain dims} $domains_ {
         catch {
            incr i
            $itk_option(-rtdimage) astset current $i
            set xylist [$itk_option(-rtdimage) astpix2cur $oldx $oldy]
            set j 0
            foreach value "$xylist" {
               incr j
               set values_($i,$j) $value
            }
         }
      }

      #  Restore default domain.
      $itk_option(-rtdimage) astset current $current
   }

   #  Method to call when the displayed image changes.
   public method image_changed {} {
      if { $trace_active_ } {
         update_displays_
      }
   }

   #  Start tracing changes in coordinates. Trace (Y) as that is set
   #  after (X).
   protected method start_trace_ {} {
      if { ! $trace_active_ } {
         set var $itk_option(-rtdimage)
         global ::$var
         trace variable ::${var}(Y) w [code $this update_readouts_]
         set trace_active_ 1
      }
   }

   #  Stop tracing variable (when closed or destroyed).
   protected method stop_trace_ {} {
      set var $itk_option(-rtdimage)
      global ::$var
      trace vdelete ::${var}(Y) w [code $this update_readouts_]
      set trace_active_ 0
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of starrtdimage widget.
   itk_option define -rtdimage rtdimage Rtdimage {} {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  If this is a clone, then it should die rather than be withdrawn.
   itk_option define -really_die really_die Really_Die 0

   #  Protected variables: (available to instance)
   #  --------------------

   #  Domains that are available.
   protected variable domains_ {}
   protected variable values_

   #  Whether trace is active.
   protected variable trace_active_ 0

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
