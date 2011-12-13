#+
#  Name:
#     FITSChooser

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Allow the choice of one FITS header from all available in a
#     given image.

#  Invocations:
#
#        FITSChooser object_name [configuration options]
#
#     This creates an instance of a FITSChooser object. The return is
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

#  Methods:

#  Inheritance:
#     util::TopLevelWidget

#  Copyright:
#     Copyright (C) 2003-2005 Central Laboratory of the Research Councils.
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
#     10-SEP-2003 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual FITSChooser {}

itcl::class gaia::FITSChooser {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Set the top-level window title.
      wm title $w_ "GAIA: Choose FITS header ($itk_option(-number))"

      #  Add short help window.
      make_short_help

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0
      add_short_help $itk_component(menubar).file {File menu: close window}

      #  Set the exit menu items.
      $File add command -label {Cancel} \
         -command [code $this cancel] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this cancel]
      $File add command -label {Accept} \
         -command [code $this accept] \
         -accelerator {Control-a}
      bind $w_ <Control-a> [code $this accept]

      #  Add the Chooser for listing the FITS headers.
      itk_component add listing {
         util::ListboxWidget $w_.listing \
            -exportselection 0 \
            -title "FITS headers" \
            -relief groove -borderwidth 2 \
            -width 0
      }
      pack $itk_component(listing) \
         -fill both -expand 1 -side top -pady 2 -padx 2 -anchor w

      #  Add frame for holding the window control action buttons.
      itk_component add frame2 {
         frame $w_.frame2
      }

      #  Add a frame of buttons for Accepting and Cancelling the table
      #  of values.
      itk_component add accept {
         button $itk_component(frame2).accept -text Accept \
            -command [code $this accept]
      }
      add_short_help $itk_component(accept) \
         {Accept selected header value and close window}
      pack $itk_component(accept) -side left -expand 1 -pady 2 -padx 2

      itk_component add cancel {
         button $itk_component(frame2).cancel -text Cancel \
            -command [code $this cancel]
      }
      add_short_help $itk_component(cancel) \
         {Close window without accepting selected header}
      pack $itk_component(cancel) -side left -expand 1 -pady 2 -padx 2

      #  Pack button frame.
      pack $itk_component(frame2) \
         -fill x -expand 0 -side top -pady 2 -padx 2 -anchor w

      #  Evaluate any options and reveal the headers.
      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------
   #  Close window. Does nothing.
   public method cancel {} {
      wm withdraw $w_
  }

   #  Close window, also executes the command with the selected FITS
   #  header appended parsed into keyword value comment.
   public method accept {} {
      wm withdraw $w_
      if { $itk_option(-command) != {} } {
         set parts [get_selected_]
         if { $parts != {} } {
            eval $itk_option(-command) $parts
         }
      }
   }

   #  Write the full header description into the list.
   protected method update_headers_ {} {
      if { $itk_option(-rtdimage) != {} } {
         set values [$itk_option(-rtdimage) fits get]
         foreach line [split $values "\n"] {
            if { $line != "" } {
               $itk_component(listing) append $line
            }
         }
      } else {
         #  Clear headers.
         $itk_component(listing) clear
      }
   }

   #  Return a list of the selected header.
   protected method get_selected_ {} {
      set card [$itk_component(listing) get_selected]
      if { $card != {} } {
         eval set card $card
         return [parse_header_card $card]
      }
      return {}
   }

   #  Parse a FITS header card returning a list of its keyword, value
   #  and comment. Note doesn't work for heirarchical keywords.
   public proc parse_header_card {card} {
      set key {}
      set value {}
      set comment {}

      set key [string trim [string range $card 0 7]]

      #  If it is an empty key then the remainder of the card is a comment
      if { $key == {} } {
         set comment [string range $card 9 end]
         return [list $key $value $comment]
      }

      #  Non-key/value pair lines are treated as keyed comments
      set equals [string range $card 8 9]
      if { $equals != "= " } {
         set comment [string range $card 9 end]
         return [list $key $value $comment]
      }

      #  Extract the value/comment part of the string
      set rest [string trim [string range $card 9 end]]

      #  If there is no value/comment part, we are done.
      if { $rest == {} } {
         return [list $key $value $comment]
      }

      #  If we have a ' then find the matching '.
      if { [string range $rest 0 0] == {'} } {
         set matchcomment [string last {'} $rest]
         if { $matchcomment != -1 } {
            set value [string range $rest 1 [expr $matchcomment-1]]
            set slash [string last {/} $rest]
            if { $slash != -1 } {
               set comment [string trim [string range $rest [expr $slash+1] end]]
            } else {
               set comment [string range $rest [expr $matchcomment+1] end]
            }
            set value [string trim $value]
            set commend [string trim $comment]
         } else {
            set comment $rest
         }
      } else {

         # Look for a / to terminate the field.
         set slash [string last {/} $rest]
         if { $slash != -1 } {
            set value [string trim [string range $rest 0 [expr $slash-1]]]
            set comment [string trim [string range $rest [expr $slash+1] end]]
         } else {
            set value $rest
         }
      }
      return [list $key $value $comment]
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of the image we're to use.
   itk_option define -rtdimage rtdimage RtdImage {} {
      if { $itk_option(-rtdimage) != {} } {
         update_headers_
      }
   }

   #  Command to execute if a header is accepted.
   itk_option define -command command Command {} {}

   #  Protected variables: (available to instance)
   #  --------------------


   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
