#+
#  Name:
#     GaiaImagePick.tcl

#  Purpose:
#     Widget to select an object (star), see stats about it and
#     save/append the details to a file.

#  Type of Module:
#     [incr Tk] class

#  Description:
#     This class extends RtdImagePick to add an ability to save the
#     current values to a file. The name of the file can be specified
#     as well as a comment to include with the record. The results
#     file will be created if it doesn't exist, otherwise records
#     are appended.

#  Invocation:
#     GaiaImagePick name [configuration options]

#  Notes:
#     This will only run with the gaia_wish installed as part of the
#     GAIA package.

#  Authors:
#     PWD: Peter Draper (STARLINK)
#     {enter_new_authors_here}

#  Inherits:
#     Methods and configuration options of SkyCatCtrl.

#  Copyright:
#     Copyright (C) 1998-1999 Central Laboratory of the Research Councils
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     Copyright (C) 2008 Science and Technology Facilities Council.
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

#  History:
#     19-MAR-1999 (PWD):
#        Original version, inspired by Andy Adamson.
#     02-JUL-1999 (PWD):
#        Added log filename fields and comment. A date & time comment
#        is also added to the file.
#     08-MAR-2006 (PWD):
#        Make the default directory for the log file selectable by using
#        the variable GAIAIMAGEPICK_DIR. This can be overridden
#        by using the chooser.
#     {enter_changes_here}

#-

itk::usual GaiaImagePick {}

itcl::class gaia::GaiaImagePick {
   inherit rtd::RtdImagePick

   #  Create a new GaiaImagePick widget
   constructor {args} {
      eval rtd::RtdImagePick::constructor $args
   } {
      eval itk_initialize $args
   }

   #  Destructor
   destructor {
   }

   #  Called after the options have been evaluated
   method init {} {
      rtd::RtdImagePick::init

      #  Set the default name for the log file. This can be changed to a
      #  different directory using env(GAIAIMAGEPICK_DIR).
      global env
      if { [info exists env(GAIAIMAGEPICK_DIR)] } {
         if { [file isdirectory $env(GAIAIMAGEPICK_DIR)] } {
            set logfile_ "$env(GAIAIMAGEPICK_DIR)/$logfile_"
         } else {
            puts stderr \
               "Information: the environment variable GAIAIMAGEPICK_DIR\
                does not specify an existing directory. \
                It has been ignored ($env(GAIAIMAGEPICK_DIR))."
         }
      }

      #  Add a control for selecting the logfile.
      itk_component add logfile {
         gaia::LabelFileChooser $w_.logfile \
            -labelwidth 8 \
            -text "Logfile:" \
            -textvariable [scope logfile_] \
            -value "$logfile_"
      }
      pack $itk_component(logfile) \
         -side top -fill x -padx 2m -pady 2m -before $itk_component(buttons)
      add_short_help $itk_component(logfile) \
         {Filename for saving window values, created if doesn't exist}

      #  And the comment field.
      itk_component add comment {
         util::LabelEntry $w_.comment \
            -labelwidth 8 \
            -text "Comment:"
      }
      pack $itk_component(comment) \
         -side top -fill x -padx 2m -pady 1m -before $itk_component(buttons)
      add_short_help $itk_component(comment) \
         {Comment to add with results in log file}

      #  Toggle to report coordinates in degrees (often useful for scripts).
      itk_component add degrees {
         gaia::StarLabelCheck $w_.degrees \
            -text "Degrees:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth 8 \
            -variable [scope show_degrees_] \
            -command [code $this change_show_degrees_]
      }
      pack $itk_component(degrees) \
         -side top -fill x -padx 2m -pady 1m -before $itk_component(buttons)
      add_short_help $itk_component(degrees) \
         {Show RA and Dec values in decimal degrees}

      #  Add a save/append button.
      itk_component add save {
         button $w_.save  \
            -text "Save/Append" \
            -command [code $this save]
      }
      pack $itk_component(save) \
         -side left -expand 1 -padx 2m -pady 1m -in $itk_component(buttons)
      add_short_help $itk_component(save) \
         {Append values to log file, created if doesn't exist}
   }

   #  save the results to a file "GaiaPick.dat", these are appended
   #  if the file exists already.
   method save {} {
      set init 0
      if { ! [file exists "$logfile_"] } {
         set init 1
      }
      if {[catch {set fd [::open "$logfile_" a+]} msg]} {
         error_dialog $msg
         return
      }

      #  If new file then add headers.
      if { $init } {
         puts $fd "# name \t x \t y \t ra \t dec \t equinox \t angle \t peak \t background \t fwhm (X:Y) \t fwhm (X:Y)"
      }

      #  Add the comment and time stamp.
      set comment [$itk_component(comment) get]
      if { "$comment" != "" } {
         puts $fd "# $comment"
      }
      set time [clock format [clock seconds] -format {%A %B %d %Y - %H:%M:%S}]
      puts $fd "# $time"

      #  Get the image name.
      set name [$target_image_ fullname]

      #  Get all the values.
      set x [$itk_component(x) cget -value]
      set y [$itk_component(y) cget -value]
      set ra [$itk_component(ra) cget -value]
      set dec [$itk_component(dec) cget -value]
      if { $ra == "" || $dec == "" } {
         set ra  "<null>"
         set dec "<null>"
      }
      set equinox [$itk_component(equinox) cget -value]

      set angle [$itk_component(angle) cget -value]
      set object [$itk_component(object) cget -value]
      set background [$itk_component(background) cget -value]
      set fwhm [$itk_component(fwhm) cget -value]
      set fwhmarc [$itk_component(fwhmarc) cget -value]

      #  Make sure we have something to write...
      if { $x == "" } {
         error_dialog "cannot save values, no object picked"
         return
      }

      #  Write them out.
      puts $fd "$name \t $x \t $y \t $ra \t $dec \t $equinox \t $angle \t $object \t $background \t $fwhm \t $fwhmarc"
      $itk_option(-shorthelpwin) short_help {saved values...}

      ::close $fd
   }

   # Format a floating point value (which may also be empty).
   # Overridden to increase accuracy to hundredths.
   protected method format_val {val} {
      if {"$val" == ""} {
         return
      }
      return [format {%.2f} $val]
   }

   #  Return the image statistics. Overrriden for degrees function.
   protected method get_stats_ {} {
      if { $show_degrees_ } {
         return [$image_ statistics degrees]
      }
      return [$image_ statistics]
   }

   #  Handle change in show degrees.
   protected method change_show_degrees_ {} {
      picked_object
      update_now
   }

   #  Protected variables.

   #  Name of log file.
   protected variable logfile_ "GaiaPick.Log"

   #  Show RA and Dec in degrees.
   protected variable show_degrees_ 0
}
