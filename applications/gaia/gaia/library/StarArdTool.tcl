#+
#  Name:
#     StarArdTool

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Provides a class for controlling ARD descriptions on
#     a canvas.

#  Description:
#     This class creates a class that can populates other frames with
#     buttons for controlling the creation of ARD regions on a canvas
#     with an RTD image displayed. It also provides methods for
#     reading and writing ARD descriptions and converting
#     some to IVOA FITS MOCs.

#  Invocations:
#
#        StarArdTool object_name [configuration options]
#
#     This creates an instance of a StarArdTool object. The return is
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
#
#        -canvasdraw canvas_draw_name
#
#     Sets the name of the StarCanvasDraw object used to control the
#     graphics content.
#
#        -canvas canvas_name
#
#     Sets the name of the canvas used to display the image and graphics.
#
#        -rtdimage rtd_image_name
#
#     Sets the name of the GaiaImageCtrl object used to display the
#     image.
#
#        -selected_colour colour
#
#     Sets the colour of regions when they are selected. Default is
#     white.
#
#        -deselected_colour colour
#
#     Sets the colour of regions when they are not selected. Default is
#     green.
#
#        -continuous_updates boolean
#
#     Sets whether object information is updated as changes occur
#     or just when the action is complete (slower machines
#     may show better interactive response when this is set 0). The
#     default is 1 (true).
#
#        -routine_prefix
#
#     The prefix used to create the name of the class that controls
#     the region list. Should be StarArd or StarArdAnn. The latter
#     form creates regions with annuli.
#
#        -maxcol integer
#
#     The maximum number of button shown in a row.

#  Methods:
#
#        read_file
#
#     Creates a filebrowser in which an ARD file can be selected for
#     reading. If the file contains an ARD description than can be
#     processed then the ARD regions are drawn to the display canvas.
#
#        read_description filename
#
#     Performs the work for read_file, or alternatively an ARD file
#     can be passed directly to this method instead.
#
#        save_file
#
#     Creates a filebrowser in which the name of an file to save the
#     current ARD description is given.
#
#        save_description filename
#
#     Performs the work for save_file, or alternatively a filename can be
#     given directly.
#
#        create_region type
#
#     Creates a new region on the canvas. The type must be one listed
#     in known_types and that is supported by the ARD system (see
#     StarArdList and StarArdAnnList).
#
#        created_object_
#
#     Private method that deals with consequences of object creation.
#
#        make_types_frame
#
#     Creates a frame with buttons for selecting an ARD type to draw.
#
#        disable_types_frame
#
#     Disables (and greys out) all the buttons in the region creation
#     frame (this happens when a region is being created and remains
#     in force until its creation is completed).
#
#        enable_types_frame
#
#     Undoes the effect of disable_types_frame.
#
#        bbox
#
#     Return a bounding box for all ARD objects, in image coordinates.
#
#        known_types
#
#     Sets the known ARD types (allows the buttons shown to be
#     controlled ). See StarArdList for a list of the possible
#     values.

#  Inheritance:
#     This class inherits from no other classes.

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
#     14-MAY-1996 (PWD):
#        Original version.
#     14-JUN-1996 (PWD):
#        Removed top-level controls (was StarArd) to generalize the
#        functionality for reuse in other toolboxes.
#     4-JUL-1996 (PWD):
#        Converted to itcl2.0.
#     24-Mar-1998 (ALLAN)
#        There was a conflict with an rtd bitmap named "rect", use "rectangle"
#     4-FEB-1999 (ALLAN)
#        Added [code ...] to object_list_ for correct scope in tcl8
#     11-JAN-2019 (PWD):
#        Added export to IVOA FITS MOCs.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::StarArdTool {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Process any configuration options.
      eval configure $args

      #  Create the StarArdList object to deal with the ARD objects.
      set object_list_ [code [gaia::${routine_prefix}List \#auto \
                                 -canvasdraw $canvasdraw \
                                 -canvas $canvas \
                                 -rtdimage $rtdimage \
                                 -notify_created_cmd \
                                    [code $this created_object_] \
                                 -selected_colour $selected_colour \
                                 -deselected_colour $deselected_colour \
                                 -continuous_updates $continuous_updates]]
   }

   #  Destructor:
   #  -----------
   destructor  {
      if { $object_list_ != {} } {
         delete object $object_list_
         set object_list_ {}
      }
   }

   #  Methods:
   #  --------

   #  Choose an ARD file and read in its description.
   method read_file {} {
      set w [util::FileSelect .\#auto -title "Choose ARD file"]
      if {[$w activate]} {
         read_description [$w get]
      }
      destroy $w
   }

   #  Parse the ARD description in a file and create the necessary regions.
   method read_description {filename} {
      if { [file readable $filename] } {
         set fid [open $filename r]

         #  Loop over the file skip comments and blank lines.
         set full_line_ {}
         set incomplete_ 0
         set ok 1
         while { $ok  } {
            set llen [gets $fid line]
            if { $llen > 0 } {
               if { ! [string match {\#*} $line] } {
                  set line [string toupper $line]

                  #  If last line was incomplete then append this line to
                  #  it. Otherwise set the local buffer to the line.
                  if { $incomplete_ } {
                     append full_line_ "$line"
                  } else {
                     set full_line_ "$line"
                  }

                  #  Check that line is now complete (matched parentheses).
                  if { [string match {*(*)} $full_line_] } {
                     set incomplete_ 0

                     #  Create the required ARD region.
                     if { ![{*}$object_list_ createard $full_line_] } {
                        error "Unable to interpret \"$full_line_\" as an ARD \
                               region. Check that this file contains an ARD \
                               description that can be processed by \
                               this program."
                     }
                  } else {
                     set incomplete_ 1
                  }
               }
            } elseif { $llen < 0 } {
               set ok 0
            }
         }
         ::close $fid
      } else {
         error "Cannot read file: $filename."
      }
   }

   #  Select a file to save the current ARD description.
   method save_file {} {
      set w [util::FileSelect .\#auto -title "Write ARD description to file"]
      if {[$w activate]} {
         save_description [$w get]
      }
      destroy $w
   }

   #  Write the current description to the named file.
   method save_description {filename} {
      if { $filename != {} } {
         set fid [open $filename w]
         set ok [{*}$object_list_ save_description $fid]
         ::close $fid
      }
      return $ok
   }

   #  Write a description of the currently selected regions to a file.
   method save_selected_description {filename} {
      if { $filename != {} } {
         set fid [open $filename w]
         set ok [{*}$object_list_ save_selected_description $fid]
         ::close $fid
      }
      return $ok
   }

   #  Return a description of all regions as a string.
   method get_description {} {
      return [{*}$object_list_ get_description]
   }

   #  Return a description of the currently selected regions as a string.
   #  with no spaces or newlines if requested.
   method get_selected_description {{oneline 1}} {
      set desc [{*}$object_list_ get_selected_description]
      if {$oneline} {
         regsub -all {\n} $desc {} desc
         regsub -all {\s+} $desc {} desc
      }
      return "$desc"
   }

   #  Set the description of the selected region (first if more than one).
   method set_selected_description {desc} {
      {*}$object_list_ set_selected_description $desc
   }

   #  Find an ARD region that matches the given description and make it
   #  the currently selected region.
   method match_description {desc} {
      return [{*}$object_list_ match_description $desc]
   }

   #  Parse an ARD description stored in a single string and create the
   #  region.
   method parse_description {desc} {
      #  Create the required ARD region.
      if { ![{*}$object_list_ createard $desc] } {
         error "Unable to interpret \"$desc\" as an ARD region."
      }
   }

   #  Create an ARD region of the given type.
   method create_region {type} {
      disable_types_frame
      if { $notify_started_cmd != {} } {
         eval $notify_started_cmd
      }
      {*}$object_list_ create_region $type
   }

   #  Method to deal with ARD object created callback.
   private method created_object_ {index id} {
      enable_types_frame
      if { $notify_created_cmd != {} } {
         eval $notify_created_cmd $index $id
      }
   }

   #  Apply a command to an object, the index is that returned with
   #  notify_created_cmd. The command must clearly be a valid one
   #  for a StarArdPrim object.
   method apply_cmd {index args} {
      eval $object_list_ apply_cmd $index $args
   }

   #  Create the frame for selecting an ARD type to draw.
   method make_types_frame {w} {
      set Buttonbox_ [frame $w -bd 3]
      set row 0
      set col 0
      foreach i [{*}$object_list_ known_types {}] {
         set l [string tolower $i]
         set bitmap $l
         # allan: conflict with rtd bitmap named "rect", use "rectangle" here...
         if {"$bitmap" == "rect"} {
             set bitmap rectangle
         }
         set b [button $Buttonbox_.$l \
                   -bitmap $bitmap \
                   -bd 3 \
                   -command [code $this create_region $i] ]
         blt::blttable $Buttonbox_ $b $row,$col -fill x -ipadx 1m -ipady 1m
         if {$col < $maxcol} {
            incr col
         } else {
            set col 0
            incr row
         }
      }
      return $Buttonbox_
   }

   #  Temporarily disable all the ARD region buttons.
   method disable_types_frame {} {
      if { $Buttonbox_ != {} } {
         foreach i [$object_list_ known_types {}] {
            set l [string tolower $i]
            $Buttonbox_.$l configure -state disabled
         }
      }
   }

   #  And enable all the ARD region buttons.
   method enable_types_frame {} {
      if { $Buttonbox_ != {} } {
         foreach i [$object_list_ known_types {}] {
            set l [string tolower $i]
            $Buttonbox_.$l configure -state normal
         }
      }
   }

   #  Return a bounding box for all ARD objects, in image coordinates.
   method bbox {} {
      if { $object_list_ != {} } {
         return [{*}$object_list_ bbox]
      }
   }

   #  Set the known ARD types (allows the buttons shown to be
   #  controlled ).
   method known_types {newtypes} {
      if { $newtypes != {} } {
         if { $object_list_ != {} } {
            set known_types [{*}$object_list_ known_types $newtypes]
         }
      }
   }

   #  Get the known ARD types (allows the buttons shown to be
   #  controlled ).
   method get_known_types {} {
      if { $object_list_ != {} } {
         return [{*}$object_list_ known_types ""]
      }
      return ""
   }

   #  Clear all ARD objects.
   method clear {} {
      if { $object_list_ != {} } {
         {*}$object_list_ clear
      }
   }

   #  Save the ARD description to a FITS MOC.
   public method save_moc {} {
      if { $routine_prefix == "StarArd" } {
         set w [util::FileSelect .\#auto -title "Save MOC"]
         if {[$w activate]} {
            save_fitsmoc [$w get]
         }
         destroy $w
      } else {
         error "Cannot save Annular regions as MOCs" 
      }
   }

   #  Save the current description to the named file as a FITS MOC.
   method save_fitsmoc {filename} {
      if { $filename != {} } {
         set frmset [$rtdimage astgetclone]
         lassign [$rtdimage wcsset] ra dec secpix nxpix nypix rotate equinox epoch
         set maxres [expr $secpix * 0.25]
         set ok [{*}$object_list_ save_fitsmoc $frmset $maxres $filename]
         gaiautils::astannul $frmset
      }
      return $ok
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of a StarCanvasDraw widget to use to control objects.
   public variable canvasdraw {} {
      if { $object_list_ != {} } {
         {*}$object_list_ configure -canvasdraw $canvasdraw
      }
   }

   #  Name of canvas.
   public variable canvas {} {
      if { $object_list_ != {} } {
         {*}$object_list_ configure -canvas $canvas
      }
   }

   #  Name of starrtdimage widget.
   public variable rtdimage {} {
      if { $object_list_ != {} } {
         {*}$object_list_ configure -rtdimage $rtdimage
      }
   }

   #  Colours of regions when selected/deselected.
   public variable selected_colour white {
      if { $object_list_ != {} } {
         {*}$object_list_ configure -selected_colour $selected_colour
      }
   }
   public variable deselected_colour green {
      if { $object_list_ != {} } {
         {*}$object_list_ configure -deselected_colour $deselected_colour
      }
   }

   #  Command to execute when a new object has been created.
   public variable notify_created_cmd {}

   #  Command to execute when object creation is started by pressing one of
   #  the buttons.
   public variable notify_started_cmd {}

   #  Change continuous updates of object information.
   public variable continuous_updates 1 {
      if { $object_list_ != {} } {
         {*}$object_list_ configure -continuous_updates $continuous_updates
      }
   }

   #  Prefix of routines used to create the various ARD
   #  regions. Normally this is "StarArd", as in StarArdCircle.
   public variable routine_prefix StarArd {
      if { $object_list_ != {} } {
         {*}$object_list_ configure -routine_prefix $routine_prefix
      }
   }

   #  Maximum number of button in a row.
   public variable maxcol {3} {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Name of the object that deals with the full ARD description.
   protected variable object_list_ {}

   #  Contents of a full line (containing a single keyword and its
   #  qualifiers and possibly any logical operators).
   protected variable full_line_ {}

   #  Whether last line read from file contained a incomplete
   #  description. If so then additional lines are appended to it
   #  until the description is complete.
   protected variable incomplete_ 0

   #  Name of frame containing buttons for creating regions.
   protected variable Buttonbox_ {}

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
