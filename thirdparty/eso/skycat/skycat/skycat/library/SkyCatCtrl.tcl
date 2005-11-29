# E.S.O. - VLT project/ESO Archive
# $Id: SkyCatCtrl.tcl,v 1.1.1.1 2002/04/04 20:11:54 brighton Exp $
#
# SkyCatCtrl.tcl - image display widget with catalog extensions
#
# See man page SkyCatCtrl(1) for a complete description.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 11 Oct 95   created

itk::usual SkyCatCtrl {}

# This class extends the RtdImageCtrl class (see RtdImageCtrl(n)) by
# adding image catalog extensions and user interface dialogs for use
# with astronomical catalogs.

itcl::class skycat::SkyCatCtrl {
    inherit rtd::RtdImageCtrl


    # constructor

    constructor {args} {
	eval itk_initialize $args
    }

    
    # destructor

    destructor {
	catch {kill $subscribe_pid_}
    }

    
    # this method is called from the base class (TopLevelWidget) after all
    # the options have been evaluated

    protected method init {} {
	RtdImageCtrl::init
	# make sure at least an empty history catalog is created
	add_history {}
    }


    # display the skycat logo in the center of the image window
    # with some copywrite text
    # Note that we assume here that the logo was created previously
    # and the name of the image is in the global var skycat_logo.

    protected method display_logo {} {
	global ::about_skycat
	set skycat_logo [tix getimage skycat_logo]

	# center logo
	$canvas_ config -scrollregion "0 0 1 1"
	$canvas_ create image 0 0 \
	    -image $skycat_logo \
	    -tags "objects logo logo_image" \
	    -anchor c

	# place text under logo
	lassign [$canvas_ bbox logo_image] x0 y0 x1 y1
	set y0 $y1
	set iw [expr $x1-$x0]
	set ih [expr $y1-$y0]
	set x [expr ($x0+$x1)/2]
	set y [expr $y1+10]
	$canvas_ create text $x $y \
	    -tags "objects logo logo_text" \
	    -justify center \
	    -anchor n \
	    -fill lightblue \
	    -font -Adobe-Helvetica-Bold-R-Normal--*-120-*-*-*-*-*-* \
	    -text $about_skycat
	
	# center the whole thing
	lassign [$canvas_ bbox logo] x0 y0 x1 y1
	set w [expr $x1-$x0]
	set h [expr $y1-$y0]
	$canvas_ move logo 0 [expr -($h/2-$ih/2)] 
	
	# clicking in it deletes it
	$canvas_ bind logo <1> "$canvas_ delete logo"
    }

    
    # display a popup window with information about this application
    
    public method about {} {
	global ::about_skycat
	set skycat_logo [tix getimage skycat_logo]

	DialogWidget $w_.about \
	    -image $skycat_logo \
	    -messagewidth 6i \
	    -justify center \
	    -text $about_skycat
	$w_.about activate
    }

    
    # send a URL to be displayed by netscape

    public method send_to_netscape {url} {
	if {[catch {exec netscape -remote "openURL($url)"} msg1]} {
	    if {! [string match $msg1 "netscape: warning"]} {
		if {[catch {exec netscape $url &} msg2]} {
		    warning_dialog "couldn't start netscape: $msg1"
		}
	    }
	}
    }
    

    # This method is called by the image code whenever a new image is loaded.

    protected method new_image_cmd {} {
 	RtdImageCtrl::new_image_cmd
	
	# check for saved line graphics
	after idle [code $this load_graphics_from_image]

	# delete temp files once they are loaded
        # PWD: let's not GAIA relies of access to the image for other reasons
        # (like the various toolboxes and catalogue access). These images
        # should be deleted by AstroCat.
	#set filename [$image_ cget -file]
	#if {[string match {/tmp/cat[0-9]*} $filename]} {
	#    catch {file delete $filename}
	#}
    }

    
    # display a popup window listing the HDUs in the current image, if any

    public method display_fits_hdus {} {
        if {[catch {set n [$image_ hdu count]}]} {
            set n 0
        }

	if {$n <= 1} {
	    warning_dialog "There are no FITS extensions" $w_
	    return
	}

	utilReUseWidget skycat::SkyCatHduChooser $w_.hdu \
	    -center 0 \
            -image $this \
            -shorthelpwin $itk_option(-shorthelpwin) \
            -usexshm $itk_option(-usexshm) \
            -usexsync $itk_option(-usexsync) \
            -verbose $itk_option(-verbose)
    }


    # update the toplevel window header and icon name to include the name
    # of the file being displayed

    protected method update_title {} {
	set file "[file tail $itk_option(-file)]"
	set w [winfo toplevel $w_]
	wm title $w "Skycat - version [skycat_version]: $file ([$w cget -number])"
	wm iconname $w $file
    }


    # If the current image is not saved in a file (came from a server, ...)
    # check if it should be saved before loading a new image. Then, if the
    # file exists, add image info for it to the history catalog.
    
    protected method check_save {} {
	if {"$filename_" != ""} {
	    if {! [file exists $filename_] && ! [$image_ isclear]} {
		# XXX this doesn't always work as expected...
		#
		#set s [choice_dialog \
		#	   "Do you want to save the current image to a file\
                #            and add it to the history list first before loading\
                #           a new one?" \
		#	   {Yes No} \
		#	   {No} \
		#	   $w_]
		#if {"$s" == "Yes"} {
		#    save_as
		#} else {
		    # don't add to history
		    set filename_ $itk_option(-file)
		    return
		#}
	    }
	    add_history $filename_
	}
	# set the new file name
	set filename_ $itk_option(-file)
    }


    # This method is redefined here from the base class to include the 
    # file name in the window header and note the filename.
    
    protected method load_fits_ {} {
	check_save
	RtdImage::load_fits_
	update_title
	apply_history $itk_option(-file)
	component colorramp update_colors
    }
    

    # Save the current image or a section of the current image to a file in 
    # FITS format chosen from a file name dialog. If dir and pattern are specified,
    # they are used as defaults for the file selection dialog.
    # If x0, y0, x1 and y1 are specified (canvas coordinates), then a section 
    # of the image is saved.
    #
    # The return value is the name of the new file, if any, or an empty string.
    # (redefined from parent class to set filename_, used in check_save).
    
    public method save_as {{dir "."} {pattern "*"} {x0 ""} {y0 ""} {x1 ""} {y1 ""}} {
	set file [RtdImage::save_as $dir $pattern $x0 $y0 $x1 $y1]
	if {"$x0" == ""} {
	    set filename_ $file
	}
	return $file
    }


    # convert the given coordinates from $from_units to $to_units and return 
    # the result. $coords may be a list of an even number of values 
    # {x1 y1 x2 y2 x3 y3 ...}. The result is the same list, converted to the 
    # output coordinates.

    public method convert_coords {coords from_units to_units} {
	set result {}
	set len [llength $coords]
	for {set i 0} {$i < $len} {incr i 2} {
	    set ix [lindex $coords $i]
	    set iy [lindex $coords [expr $i+1]]
	    $image_ convert coords $ix $iy $from_units x y $to_units
	    lappend result $x $y
	}
	return $result
    }


    # Save the current line graphics in a FITS binary table in the image.
    # The table has 3 columns: "type", "coords", and "config". 
    # "type" gives the shape and is one of the Tk canvas item types.
    # "coords" is a list of coordinates for the item.
    # "config" is a Tcl list of configuration options for the item.
    # There can be one graphics table for each image extension. For each
    # image extension, the graphics table is called "${extname}.GRAPHICS".

    public method save_graphics_with_image {} {
	busy {
	    save_graphics_with_image_
	}
    }
    public method save_graphics_with_image_ {} {
	# deselect any objects
	$w_.draw deselect_objects

	# table headings
	set headings {type coords config}
	
	# table data
	set info {}
	
	# max table column widths
	set width(type) 0
	set width(coords) 0
	set width(config) 0
	
	# loop through the canvas items
	foreach item [$canvas_ find all] {
	    set type [$canvas_ type $item]
	    if {"$type" == "image"} {
		continue
	    }
	    # get item coords and convert from canvas to image coords
	    set coords [convert_coords [$canvas_ coords $item] canvas image]

	    # add a special tag to this item so we can delete it before reloading it
	    $canvas_ addtag "graphics" withtag $item

	    # get list of configuration options for the item
	    set config {}
	    foreach cfg [$canvas_ itemconfigure $item] {
		lappend config [list [lindex $cfg 0] [lindex $cfg 4]]
	    }

	    set width(type) [max $width(type) [string length $type]]
	    set width(coords) [max $width(coords) [string length $coords]]
	    set width(config) [max $width(config) [string length $config]]
	    lappend info [list $type $coords $config]
	}
	
	# set table column formats (FITS style: 16A, for char[16], etc...)
	set tform "$width(type)A $width(coords)A $width(config)A"

	set listheadings [$image_ hdu listheadings]
	set hdu_list [$image_ hdu list]
	set extname [$image_ fits get EXTNAME]
	set table "${extname}.GRAPHICS"

	# Look for an existing graphics table and delete it, so we can
	# replace it with a new one
	foreach row $hdu_list {
	    eval lassign [list $row] $listheadings
	    if {"$ExtName" == "$table"} {
		$image_ hdu delete $HDU
		break
	    }
	}

	# create a new binary table and insert the info
	if {[catch {
	    $image_ hdu create binary $table $headings $tform $info
	} msg]} {
	    error_dialog "error creating FITS table '$table': $msg"
	    return
	}

	# update and display the HDU window
	update_fits_hdus
    }


    # Check if there is a FITS table with the same name as the current
    # image extension, but with ".GRAPHICS" appended.
    # If found, restore the previously saved line graphics from the table. 
    # This method is called automatically when a new image extension is 
    # loaded.

    public method load_graphics_from_image {} {
	busy {
	    load_graphics_from_image_
	}
    }
    public method load_graphics_from_image_ {} {
	# only do this for image extensions
	if {[catch {set type [$image_ hdu type]}]} {
	    return
	}
	if {"$type" != "image"} {
	    return
	}

	# get the name of the graphics table
	set extname [$image_ fits get EXTNAME]
	set name "${extname}.GRAPHICS"

	# get the hdu
	set headings [$image_ hdu listheadings]
	if {[catch {set hdu_list [$image_ hdu list]}]} {
	    # avoid problem when image and header are in separate files...
	    return
	}
	set hdu 0
	foreach i $hdu_list {
	    eval lassign [list $i] $headings
	    if {"$ExtName" == "$name"} {
		set hdu $HDU
		break
	    }
	}
	if {$hdu == 0} {
	    return
	}

	# make sure we don't create 2 of each object
	$canvas_ delete "graphics"

	# Now we have the hdu number of the graphics table. Read it and
	# restore the graphics
	set headings {type coords config}
	foreach row [$image_ hdu get $hdu] {
	    eval lassign [list $row] $headings

	    # convert from image to canvas coords
	    if {[catch {set coords [convert_coords $coords image canvas]} msg]} {
		puts $msg
		continue
	    }
	    set id [eval $canvas_ create $type $coords]

	    foreach cfg $config {
		lassign $cfg opt arg
		$canvas_ itemconfigure $id $opt $arg
	    }

	    # add bindings so that the items may be edited and saved again
	    $w_.draw add_object_bindings $id
	}
    }


    # Add the current image to the history catalog under the given filename.
    # The current FITS header is used to extract information about the image
    # to put in the catalog. The file basename is assumed to be the unique id.

    public method add_history {filename} {
	skycat::SkySearch::add_history $this $filename
	if {"$filename" != "" && [file exists $filename]} {
	    lappend back_list_ $filename
	}
    }
    

    # Check if the given filename is in the history catalog, and if so, 
    # apply the cut levels and color settings for the file.

    public method apply_history {filename} {
	skycat::SkySearch::apply_history $this $filename
    }


    
    # Update the given menu with image history items. $w is the TopLevelWidget
    # containing the menubar
    
    public method update_history_menu {w m} {
	$m delete 0 end

	$w add_menuitem $m command "Back" \
	    {Go back again to the previous image} \
	    -command [code $this previous_image] \
	    -state disabled

	if {[llength $back_list_]} {
	    $m entryconfig Back -state normal
	}

	$w add_menuitem $m command "Forward" \
	    {Go forward again to the next image} \
	    -command [code $this forward_image] \
	    -state disabled

	if {[llength $forward_list_]} {
	    $m entryconfig Forward -state normal
	}

	$m add separator

	skycat::SkySearch::add_history_menu_items $w $this $m 20
    }


    # go back to the previous image

    public method previous_image {} {
	while {[set n [llength $back_list_]]} {
	    set filename [lindex $back_list_ end]
	    if {"$filename" != "$itk_option(-file)" && [file exists $filename]} {
		lappend forward_list_ $itk_option(-file)
		configure -file $filename
		set back_list_ [lrange $back_list_ 0 [expr $n-2]]
		break
	    }
	    set back_list_ [lrange $back_list_ 0 [expr $n-2]]
	}
    }


    # go forward again to the next image

    public method forward_image {} {
	while {[set n [llength $forward_list_]]} {
	    set filename [lindex $forward_list_ end]
	    if {"$filename" != "$itk_option(-file)" && [file exists $filename]} {
		configure -file $filename
		set forward_list_ [lrange $forward_list_ 0 [expr $n-2]]
		break
	    }
	    set forward_list_ [lrange $forward_list_ 0 [expr $n-2]]
	}
    }

    
    # This method is also redefined from the parent class to set the window
    # header info

    public method clear {} {
	check_save
	set filename_ {}
	RtdImageCtrl::clear
	update_title
    }

    
    # subscribe to (or unsubscribe from) the OLAF DHS server images
    # (ESO/Archive On-Line Archive Facility project: use the -dhshost and 
    # -dhsdata options to add this feature.) 
    # The first argument is the name of the trace variable used in the checkbutton menuitem.
    # dhshost is the name of the host running the DHS server, to which we subscribe.
    # dhsdata is the directory to use to hold the images files (temporary files).

    public method subscribe {variable dhshost dhsdata} {
	global ::$variable


	catch {exec kill $subscribe_pid_}
	if {[set $variable]} {
	    # subscribe
	    set port [$image_ remote]
	    if {[catch {set subscribe_pid_ \
			    [exec rtdSubscribe \
				 -rtdport $port \
				 -dhshost $dhshost \
				 -dhsdata $dhsdata \
				 -logpath /tmp >& /dev/null &] \
			} msg]} {
		error_dialog "Couldn't exec rtdSubscribe: $msg"
	    }
	    
	    # try to handle errors written to stderr
	    #set subscribe_fd_ [::open "| tail -f $subscribe_tmpfile_"]
	    #fileevent $subscribe_fd_ readable [code $this rtdSubscribeError $subscribe_fd_]
	} 
    }

    
    # display a rectangle on the image at the given center coords 
    # with the given width and height and return the items tag or id.

    public method mark_image {ra dec width height} {
	# get radius from width and height
	set a $width/2.0
	set radw [expr sqrt(2.0*$a*$a)]
	set a $height/2.0
	set radh [expr sqrt(2.0*$a*$a)]
	
	# combine 2 square boxes to get the rectangle
	lassign [$image_ radecbox $ra $dec $radw] wr0 wd0 wr1 wd1
	lassign [$image_ radecbox $ra $dec $radh] hr0 hd0 hr1 hd1
	
	set eq [$image_ wcsequinox]
	if {[catch {
	    $image_ convert coords $wr0 $hd0 "wcs $eq" x0 y0 canvas
	    $image_ convert coords $wr1 $hd1 "wcs $eq" x1 y1 canvas
	} msg]} {
	    error_dialog $msg $w_
	    return
	}

	set id [$canvas_ create rectangle \
		    $x0 $y0 $x1 $y1 \
		    -width 2 \
		    -tags objects \
		    -fill yellow \
		    -outline white \
		    -stipple pat6]

	$itk_component(draw) add_object_bindings $id
	return $id
    }


    # remove the given mark from the image

    public method unmark_image {id} {
	$canvas_ delete $id
    }


    # Ask the user to select an area of the image by dragging out a region
    # and return as a result a list of the form {x0 y0 x1 y1} in pixels.

    public method select_area {{shape rectangle}} {
	if {[$image_ isclear]} {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}

	# if {[action_dialog \
	#	 "Please select and drag out a region of the image with mouse button 1" \
	#	 $w_]}  

	global ::$w_.select_area
	set $w_.select_area {}
	$itk_component(draw) set_drawing_mode $shape [code $this selected_area]
	tkwait variable $w_.select_area
	return [set $w_.select_area]
    }


    # This method is called when the user has selected an area of the image.
    # The results are in canvas coordinates, clipped to the area of the image.

    public method selected_area {id x0 y0 x1 y1} {
	global ::$w_.select_area

	# make sure the coordinates don't go off the image
	if {"$x0" != ""} {
	    set w [$image_ width]
	    $image_ convert coords $x0 $y0 canvas x0 y0 image
	    $image_ convert coords $x1 $y1 canvas x1 y1 image
	    foreach i {x0 y0 x1 y1} {
		set v [set $i]
		if {$v < 1} {
		    set $i 1 
		} elseif {$v > $w} {
		    set $i $w
		}
	    }
	    $image_ convert coords $x0 $y0 image x0 y0 canvas
	    $image_ convert coords $x1 $y1 image x1 y1 canvas
	}

	set $w_.select_area "$x0 $y0 $x1 $y1"
	after 0 [code $w_.draw delete_object $id]
    }

    
    # Draw a symbol on the image with the given shape at the given coordinates
    # (in the given x,y units), with the given radius (in radius_units), 
    # bg and fg color, canvas tags list, x/y ratio and rotation angle.
    #
    # shape may be one of "circle", "square", "plus", "cross", "triangle",
    # "diamond", "ellipse", "compass", "line", "arrow".
    #
    # x and y are the coordinates in "xy_units", which is one of the units
    # accepted by the Rtd commands (canvas, image, screen, "wcs $equinox", 
    # "deg $equinox").
    #
    # The radius value is interpreted in radius_units.
    # 
    # bg and fg are X color names for the symbol (may be the same).
    #
    # symbol_tags should be a Tcl list of canvas tags for the symbol.
    #
    # ratio and angle are optional and used to stretch/shrink and 
    # rotate the symbol. The default ratio is 1, default angle 0.
    #
    # label is an optional text for a label to place near the symbol.
    #
    # label_tags should be a Tcl list of canvas tags for the label, or
    # an empty or null string, if there is no label.
    #
    # Returns an error if the coordinates or part of the symbol are off 
    # the image.
    #
    # Uses world coordinates, if available, for the rotation and orientation, 
    # for symbols that support it (i.e.: rotation is relative to WCS north).

    public method draw_symbol {shape x y xy_units radius radius_units bg fg 
			       symbol_tags {ratio 1} {angle 0} {label ""} 
			       {label_tags ""}} {
	$image_ symbol $shape $x $y $xy_units $radius $radius_units \
	    $bg $fg $symbol_tags $ratio $angle $label $label_tags
    }


    # -- options --
    
    # flag: if true, run queries in the foreground for better debugging
    itk_option define -debug debug Debug 0

    # see parent class for other options...
    

    # -- protected variables --

    # pid of rtdSubscribe process (OLAF)
    protected variable subscribe_pid_ {}

    # const PI
    protected variable pi_ 3.14159265358979323846

    # const PI/180.
    protected variable rad_ [expr 3.14159265358979323846/180.]

    # the name of the image file, if any
    protected variable filename_ {}

    # used for the Go=>Back/Forward menu itemes
    protected variable back_list_ {}
    protected variable forward_list_ {}
}


