#-*-tcl-*-
# E.S.O. - VLT project/ESO Archive
# $Id$
#
# graphics_features.tcl - add graphics features to Skycat Graphics
#                         menu.
#
# The add_graphics_features proc is called by the example SkyCat
# plugin script in this directory to add menu items to the Graphics
# menu in Skycat to save and reload line graphics.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 20 Jan 98   created
# P.W.Draper 14 Dec 07   use tcl_precision=17 when saving, needed for
#                        edge values like -DBL_MAX.
#

# Add "Save Graphics" and "Load Graphics" menu items to the Skycat
# Graphics menu so that line graphics may be saved to a file and
# reloaded later. You can save the graphics using world or image
# coordinates and specify when reloading, whether or not the graphics
# should be "sticky" and remain after a new image is loaded.
#
# $w is the name of the top level window.

proc add_graphics_features {w} {
    # get the path name of the Graphics menu
    # (see TopLevelWidget in tclutil)
    set m [$w get_menu Graphics]

    $m add separator

    $w add_menuitem $m command "Save graphics..." \
	{Save line graphics to a file} \
	-command [code save_graphics $w]

    $w add_menuitem $m command "Load graphics..." \
	{Load line graphics from a file} \
	-command [code load_graphics $w]
}


# convert the given coordinates from $from_units to $to_units using the given
# rtdimage handle and return the result. $coords may be a list of an even
# number of values {x1 y1 x2 y2 x3 y3 ...}. The result is the same list,
# converted to the output coordinates.

proc convert_coords {coords from_units to_units image} {
    set result {}
    set len [llength $coords]
    for {set i 0} {$i < $len} {incr i 2} {
	set ix [lindex $coords $i]
	set iy [lindex $coords [expr $i+1]]
	$image convert coords $ix $iy $from_units x y $to_units
	lappend result $x $y
    }
    return $result
}


# Ask for a file name and save line graphics to the given file

proc save_graphics {w} {
   set filename [filename_dialog]
   if {"$filename" == ""} {
      return
   }

   # check if file exists and, if so, ask for confirmation
   if {[file exists $filename]} {
      if {! [confirm_dialog "File `[file tail $filename]' exists. Overwrite it?"]} {
         return
      }
   }

   # ask if we should save the graphics in world or image coords
   set choice [choice_dialog \
                  "Please select the type of coordinates to save the graphics in:" \
                  {{World Coordinates} {Image Coordinates} Cancel} {World Coordinates} $w]
   if {$choice == "Cancel"} {
      return
   } elseif {$choice == "World Coordinates"} {
      set units {deg J2000}
   } else {
      set units image
   }

   # create the file
   if {[catch {set fd [open $filename w]} msg]} {
      error_dialog $msg
      return
   }

   # get the handle of the canvas window for the image
   set canvas [$w component image component canvas]

   # get the handle for the rtdimage item (for converting coordinates)
   set image [$w component image get_image]

   # get the handle for the image' graphic editor (class CanvasDraw)
   # so that we can deselect any selected graphic objects
   set draw [$w component image component draw]
   $draw deselect_objects

   # save the coordinate type: degrees J2000 or image pixel coords
   puts $fd "set units \"$units\""

   # use full precision so that we do not loose any
   set old_tcl_precision $::tcl_precision
   set ::tcl_precision 17
   catch {

      # loop through the canvas items
      foreach item [$canvas find all] {
         set type [$canvas type $item]
         if {"$type" == "image"} {
            continue
         }
         # get item coords and convert from canvas coords to $units
         set coords [convert_coords [$canvas coords $item] canvas $units $image]

         # add a special tag to this item so we can delete it before reloading it
         $canvas addtag $filename withtag $item

         # get list of configuration options for the item
         set config {}
         foreach cfg [$canvas itemconfigure $item] {
            lappend config [list [lindex $cfg 0] [lindex $cfg 4]]
         }
         puts $fd [list $type $coords $config]
      }
   }
   set ::tcl_precision $old_tcl_precision
   close $fd
}


# Ask for a file name and load the line graphics from the given file

proc load_graphics {w} {
    set filename [filename_dialog]
    if {"$filename" == ""} {
	return
    }

    # make sure the file exists
    if {! [file exists $filename]} {
	error_dialog "file `[file tail $filename]' does not exist"
	return
    }

    # open the file for reading
    if {[catch {set fd [open $filename]} msg]} {
	error_dialog $msg
	return
    }

    # get the handle of the canvas window for the image
    set canvas [$w component image component canvas]

    # get the handle for the rtdimage item (for converting coordinates)
    set image [$w component image get_image]

    # get the handle for the image' graphic editor (class CanvasDraw)
    # so that we can set bindings for editing objects
    set draw [$w component image component draw]

    # check for "set units..." line
    set linenum 0
    if {[gets $fd line] != -1} {
	incr linenum
	if {[llength $line] != 3} {
	    error_dialog "bad input file format: line $linenum, expected: set units ..."
	    close $fd
	    return
	}
	lassign $line s1 s2 s3
	# check for "set units..." line
	if {"$s1" == "set" && "$s2" == "units"} {
	    eval $line
	} else {
	    error_dialog "expected first line to be \"set units ...\"" $w
	    close $fd
	    return
	}
    }

    # if using image coords, ask if the graphics should be "sticky"
    # (i.e.: stay after new image is loaded)
    set sticky 0
    if {"$units" == "image"} {
	set choice [choice_dialog \
			"Do you want these items to remain visible after you load a new image?:" \
			{Yes No Cancel} No $w]
	if {$choice == "Cancel"} {
	    return
	} elseif {$choice == "Yes"} {
	    set sticky 1
	} else {
	    set sticky 0
	}
    }

    # delete any items that have already been loaded from this file
    $canvas delete $filename

    # loop through the contents of the file and create canvas items
    while {[gets $fd line] != -1} {
	incr linenum
	if {[llength $line] != 3} {
	    error_dialog "bad input file format: line $linenum, expected: type coords config"
	    close $fd
	    return
	}
	lassign $line type coords config

	# convert coords from $units to canvas coords
	if {[catch {set coords [convert_coords $coords $units canvas $image]} msg]} {
	    puts $msg
	    continue
	}
	set id [eval $canvas create $type $coords]

	foreach cfg $config {
	    lassign $cfg opt arg
	    $canvas itemconfigure $id $opt $arg
	}

	# items with the "objects" tag are always deleted when a new image is loaded
	# so if we want the graphics to be "sticky", we remove that tag.
	# (The util::CanvasDraw class assigns the "objects" tag for graphic objects
	# created interactively and other classes follow the convention also.)
	if {$sticky} {
	    $canvas dtag $id objects
	    $canvas dtag $id $draw.objects
	    $canvas addtag sticky withtag $id
	}

	# add bindings so that the items may be edited and saved again
	$draw add_object_bindings $id
    }

    close $fd
}

