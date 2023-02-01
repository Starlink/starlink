#+
#  Name:
#     StarAppFilter

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Creates a list of "filters" to process the currently displayed
#     image.

#  Description:
#     This class controls a series of Starlink applications that perform
#     simple pipe-line processing of an image (a filter). The filters
#     are defined in two files that exist in the GAIA main directory
#     and in the user's $HOME directory. Both files have the same name:
#     ".gaFilters". The format of the file is simple for the moment,
#     but is expected to increase in complexity as more functionality
#     is folded in.
#
#     The filters defined in these files are labelled in the "Filters"
#     menu created in any GAIA windows.

#   File format:
#     gaFilters must have the following format:
#
#        Filter replace_image filter_binary_name \
#           command_qualifier_string use_prefix
#           output_image_name_modifier label
#
#     So for instance the KAPPA application BLOCK might be run
#     using:
#
#        Filter 1 $env{KAPPA_DIR}/block \
#           "in=$indata box=[10,10] out=$outdata accept" \
#           1 smooth_ "Smooth 10 by 10"
#
#      The symbol "$indata" will be replaced with the name of the
#      currently displayed image as suitable for passing to a Starlink
#      application. The variable "$outdata" will be constructed from
#      the displayed image name, but will be modified using the
#      "smooth_" as a prefix to the current image name (setting
#      use_prefix to 0 would result in a postfix).
#
#      The $env{KAPPA_DIR} variable is changed to the value of the
#      KAPPA_DIR environment variable.
#
#      Since replace is set to 1 (true) the currently displayed image
#      will be erased and the output of this command will be displayed
#      instead. If replace had been 0 (false) then a new display would
#      have been created and the new image displayed in that.

#  Invocations:
#
#        StarAppFilter object_name gaia [configuration options]
#
#     This creates an instance of a StarAppFilter object. The return is
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
#     See public variable defintions.

#  Methods:
#     See method definitions.

#  Inheritance:
#     This object inherits no other classes.

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils
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
#     ALLAN: Allan Brighton (ESO)
#     {enter_new_authors_here}

#  History:
#     19-NOV-1996 (PWD):
#        Original version.
#     24-APR-1998 (ALLAN)
#        Pass command line arguments to "clone" rather than use "after 500".
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::StarAppFilter {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {gaia args} {
      global env gaia_dir

      #  Record the name of the GAIA widget.
      set gaia_ $gaia
      set rtdimage_ [$gaia_ get_image]
      set rtdimage_ [$rtdimage_ get_image]

      #  Evaluate any configuration options.
      eval configure $args

      #  Look for the gaFilter files. First in gaia_dir and then
      #  in the HOME directory.
      if { [file readable $gaia_dir/.gaFilters] } {
         if { ![catch {set ios [open $gaia_dir/.gaFilters]}] } {
            process_filters_ $ios
            close $ios
         }
      }
      if { [file readable $env(HOME)/.gaFilters] } {
         if { ![catch {set ios [open $env(HOME)/.gaFilters]}] } {
            process_filters_ $ios
            close $ios
         }
       }

      # Check that we have some filters and create the menu to run
      # them if so.
      add_menu

      #  Create control object for image names.
      set namer_ [gaia::GaiaImageName \#auto]

   }

   #  Destructor:
   #  -----------
   destructor  {
      if { $namer_ != {} } {
         catch {delete object $namer_}
      }
   }

   #  Methods:
   #  --------

   #  Read in a list of commands from a stream and try to interpret
   #  them as Filters. Commands are extended across lines by the
   #  \ character. Blank lines are ignored. Each part of the file
   #  is interpreted as an input list then quickly stored away (before
   #  things get difficult).
   private method process_filters_ {ios} {
      set nf 0
      set whole ""
      while { [gets $ios line] >= 0 } {
         set newf [llength $line]
         append whole $line
         for { set i 0 } { $i < $newf } { incr i } {
            incr nf
            set newinfo($nf) [lindex $line $i]
         }
         if { $nf > 0 } {
            if { $newinfo($nf) == "\\" || $newinfo($nf) == "" } {
               incr nf -1
            } else {
               # Should have a complete description. Just check
               # number of elements and store it.
               if { $nf == 7 } {
                  set info_($nf_,replace) $newinfo(2)
                  set info_($nf_,binary) $newinfo(3)
                  set info_($nf_,qualifier) $newinfo(4)
                  set info_($nf_,prefix) $newinfo(5)
                  set info_($nf_,output) $newinfo(6)
                  set info_($nf_,label) $newinfo(7)
                  incr nf_
               } else {
                  error_dialog "Cannot interpret \"$whole\" as \
                                a possible filter description"
               }
               set nf 0
               set whole ""
            }
         }
      }
   }

   #  Create the menu for the list of filters.
   method add_menu {} {
       if { $nf_ > 0 } {
	   set m [$gaia_ add_menubutton Filters]
	   for { set i 0 } { $i < $nf_ } { incr i } {
	       $m add command -label $info_($i,label) \
		       -command [code $this run $i]
	   }
       }
   }

   #  Run the command with the given index.
   method run {id} {
      set image [$rtdimage_ fullname]
      if { $image == "" } {
         error_dialog "No image to filter"
         return
      }
      $namer_ configure -imagename $image

      if { [info exists info_($id,replace)] } {
         incr nf_
         global env
         set app [subst $info_($id,binary)]
         set info_($id,app) [gaia::GaiaApp \#auto -application \
                                "$app" -show_output 0 \
                                -notify [code $this completed $id]]

         #  Set name of the "indata" variable to the name used by
         #  Starlink applications. Includes everything but the ".sdf"
         #  extension.
         set indata [$namer_ ndfname 0]

         #  Set the name of the "outdata" variable.
         set outdata [$namer_ modname $info_($id,prefix) $info_($id,output)]

         #  Need to substitute $indata and $outdata, but still protect
         #  [] and form a proper list as an argument to runwith! So
         #  use keep things in a single string and do careful substitution.
         set qual [join $info_($id,qualifier)]
         set qual [subst -nocommands -nobackslashes $qual]
         $info_($id,app) runwiths "$qual"
      }
   }

   #  Method to deal with a completed filter. If creating a new window
   #  then hang around until it exists.
   method completed {id} {
       if { [info exists info_($id,replace) ] } {
	   if { $info_($id,replace) } {
               eval $gaia_ open $outdata
	    } else {
	       set w [$gaia_ clone -file $outdata]
	   }
       }
       set data_ {}
       set datatype_ {}

       #  Don't hang on to the apps. We have no idea how many of these
       #  there might be! UNIX should cache interesting ones.
       $info_($id,app) delete_sometime
   }

   #  Configuration options: (public variables)
   #  ----------------------


   #  Protected variables: (available to instance)
   #  --------------------
   #  Name of the main Gaia widget for placing the menu into and
   #  getting the image name from. Creating clones etc.
   protected variable gaia_ {.gaia1}

   #  Name of rtdimage.
   protected variable rtdimage_ {}

   #  Number of filters known.
   protected variable nf_ 0

   #  Information about filter.
   protected variable info_

   #  Current file extension.
   protected variable datatype_ ""

   #  Name of the file being processed.
   protected variable indata {}

   #  Name of the file being produced.
   protected variable outdata {}

   #  Object for image name nicities.
   protected variable namer_ {}

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
