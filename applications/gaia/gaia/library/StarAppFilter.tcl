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
#           command_qualifier_string output_image_name label
#
#     So for instance the KAPPA application BLOCK might be run
#     using:
#
#        Filter 1 $env{KAPPA_DIR}/block \
#           "in=$data box=[10,10] out=${data}_smooth accept" \
#           ${data}_smooth "Smooth 10 by 10"
#
#      The symbol $data will be replaced with the name of the
#      currently displayed image and $env{KAPPA_DIR} with the value of the
#      environment variable. In this case since replace is set to 1 (true)
#      the currently displayed image will be erased and the output
#      of this command will be displayed instead. If replace had been
#      0 (false) then a new display would have been created and
#      the new image displayed in that.

#  Invocations:
#
#        StarAppFilter object_name rtdwidget [configuration options]
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

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     19-NOV-1996 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#-

#.

class gaia::StarAppFilter {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {rtdwidget args} {
       global env

       #  Record the name of the Gaia widget.
       set rtdwidget_ $rtdwidget

       #  Evaluate any configuration options.
       eval configure $args

       #  Look for the gaFilter files. First in GAIA_DIR and then
       #  in the HOME directory.
       if { [file readable $env(GAIA_DIR)/.gaFilters] } {
	   if { ![catch {set ios [open $env(GAIA_DIR)/.gaFilters]}] } {
	       process_filters_ $ios
	       close $ios
	   }
       }
       if { [file readable $env(HOME)/.gaFilters] } {
	   if { ![catch {set ios [open $env(HOME)/.gaFilters]}] } {
	       process_filters $ios
	       close $ios
	   }
       }

       # Check that we have some filters and create the menu to run
       # them if so.
       add_menu
   }

   #  Destructor:
   #  -----------
   destructor  {
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
       while { [lgets $ios line] >= 0 } {
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
		   if { $nf == 6 } {
		       set info_($nf_,replace) $newinfo(2)
		       set info_($nf_,binary) $newinfo(3)
		       set info_($nf_,qualifier) $newinfo(4)
		       set info_($nf_,output) $newinfo(5)
		       set info_($nf_,label) $newinfo(6)
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
	   set m [$rtdwidget_ add_menubutton Filters]
	   for { set i 0 } { $i < $nf_ } { incr i } {
	       $m add command -label $info_($i,label) \
		       -command [code $this run $i]
	   }
       }
   }

   #  Run the command with the given index.
   method run {id} {
      if { [info exists info_($id,replace) ] } {
         incr nf_
         if { ! [info exists info_($id,app)] } {
            global env
            set app [subst $info_($id,binary)]
            set info_($id,app) [StarApp \#auto -application \
                                   "$app" -show_output 0 \
                                   -notify [code $this completed $id]]
         }
         set data [$rtdwidget_ open]
         set datatype_ ""
         lassign [fileName $data] data slice
         if { [file extension $data] == ".sdf" } {
            set data "[file rootname $data]${slice}"
            set datatype_ ".sdf"
         }
         
         #  Need to substitute $data, but still protect [] and form a 
         #  proper list as an argument to runwith! So use keep things
         #  in a single string and do careful substitution.
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
               eval $rtdwidget_ open $info_($id,output)${datatype_}
	    } else {
	       set w [$rtdwidget_ clone]
	       after 0 [code $w configure -file [subst $info_($id,output)${datatype_}]]
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
   protected variable rtdwidget_ {.c}

   #  Number of filters known.
   protected variable nf_ 0

   #  Information about filter.
   protected variable info_

   #  Current file extension.
   protected variable datatype_ ""

   #  Name of the file being processed.
   protected variable data {}
   
   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
