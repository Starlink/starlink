# E.S.O. - VLT project 
# "@(#) $Id: SkyCatHduChooser.tcl,v 1.8 1999/01/07 09:27:23 abrighto Exp $"
#
# SkyCatHduChooser.tcl - Itcl widget for displaying FITS extensions
# 
# See man page SkyCatHduChooser(n) for a complete description.
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  09/11/98   Created
# Peter W. Draper 08/12/00   Slight corrections for missing CRPIX values.
#                  14/11/01  Retro-fitted update from main image button
#                            (from 2.4.7)

itk::usual SkyCatHduChooser {}

# This class defines a widget for displaying the HDUs in the current FITS
# image. The user can select a FITS table or image extension to display 
# by clicking on an entry the list or on one of the small images displayed 
# in a table.

itcl::class skycat::SkyCatHduChooser {
    inherit util::TopLevelWidget


    # constructor: create a toplevel window

    constructor {args} {
	eval itk_initialize $args
    }


    # This method is called after the options have been evaluated.

    protected method init {} {
	wm minsize $w_ 10 10
	wm title $w_ "FITS HDUs ($itk_option(-number))"
	wm iconname $w_ "FITS HDUs ($itk_option(-number))"

	# get internal image handle
	set image_ [$itk_option(-image) get_image]

	make_table
	make_image_table
	make_buttons
	make_short_help
	
	show_hdu_list
    }

    
    # Make the table component for displaying the HDU info

    protected method make_table {} {
	set headings [$image_ hdu listheadings]
	# TableList(n) widget for displaying the list of HDUs
	itk_component add table {
	    set table_ [util::TableList $w_.table \
			    -title {FITS HDUs} \
			    -headings $headings \
			    -width [string length $headings]]
	}
	pack $itk_component(table) \
	    -side top -fill both -expand 1 

	bind $table_.listbox <ButtonRelease-1> [code $this select_hdu]
	bind $table_.listbox <Double-ButtonPress-1> [code $this set_hdu]
    }

    
    # Make a subwindow for displaying miniature versions of image extensions
    
    protected method make_image_table {} {
	# Frame (BLT table) used to display images in FITS extensions
	itk_component add image_table {
	    set imagetab_ [frame $w_.imagetab]
	}	
	pack $itk_component(image_table) \
	    -side top -fill both -expand 1
    }


    # add a row of buttons at bottom

    protected method make_buttons {} {
	# Button frame at bottom of window
	itk_component add buttons {
	    frame $w_.buttons \
		-borderwidth 2 \
		-relief raised
	}

	pack \
	    [button $w_.buttons.open \
		 -text Open \
		 -command [code $this set_hdu]] \
            [button $w_.buttons.settings \
                -text "Use Settings from Main Image" \
                -command [code $this use_settings_from_main_image]] \
	    [button $w_.buttons.delete \
		 -text Delete \
		 -state disabled \
		 -command [code $this delete_hdu]] \
	    [button $w_.buttons.close \
		  -text Close \
		  -command "destroy $w_" ] \
	    -side left -expand 1
	
	pack $itk_component(buttons) \
	    -side top -fill x -expand 1
    }

    
    # add a short help window

    protected method make_short_help {} {
	TopLevelWidget::make_short_help

	add_short_help $itk_component(table) \
	    {Table: Click to select HDU, double-click to display image or table}

	add_short_help $itk_component(buttons).open \
           {Open and display the selected HDU image or table}
	add_short_help $itk_component(buttons).close {Close the selected HDU}
	add_short_help $itk_component(buttons).delete \
           {Delete the selected HDU from the FITS file}
	add_short_help $itk_component(buttons).settings \
           {Set the cut levels and colormap for the preview images to the one used in the main image}
    }

    
    # Update the list of HDUs and the image displays, if needed

    public method show_hdu_list {} {
        set old_filename $filename_
        set filename_ [$image_ cget -file]
	set hdu_list [$image_ hdu list]
	set headings [$image_ hdu listheadings]

	# update the table listing
	$table_ clear
	$table_ config -height [llength $hdu_list] -info $hdu_list

        if {"$filename_" == "$old_filename"} {
            return
        }

	# delete old images
	set w $imagetab_.f
	catch {destroy $w}
	pack [frame $w] -fill both -expand 1
	
	if {"$filename_" == ""} {
	    return
	}

	# See if there is more than one image, otherwise skip it.
	# If there are multiple images and they are the same size,
	# put them in "crpix" order also.
	set num_images_ 0
	catch {unset ext_}
	set max_crpix1 [set max_crpix2 0]
	set naxis1 [set naxis2 0]
	set first_time 1
	set use_crpix 1
	foreach hdu $hdu_list {
	    eval lassign [list $hdu] $headings
	    if {"$Type" == "image" && "$NAXIS" >= 2} {
		set ext_($num_images_,HDU) $HDU
		set ext_($num_images_,ExtName) $ExtName
		set ext_($num_images_,NAXIS1) $NAXIS1
		set ext_($num_images_,NAXIS2) $NAXIS2
		set ext_($num_images_,CRPIX1) $CRPIX1
		set ext_($num_images_,CRPIX2) $CRPIX2
		if {$first_time} {
		    set first_time 0
		    set max_crpix1 $CRPIX1
		    set max_crpix2 $CRPIX2
		    set naxis1 $NAXIS1
		    set naxis2 $NAXIS2
		} else {
		    if {$naxis1 != $NAXIS1 || $naxis2 != $NAXIS2} {
			set use_crpix 0
		    }
		    if {$CRPIX1 > $max_crpix1} {
			set max_crpix1 $CRPIX1
		    }
		    if {$CRPIX2 > $max_crpix2} {
			set max_crpix2 $CRPIX2
		    }
		}
		incr num_images_
	    } 
	}
	if {$num_images_ <= 1} {
	    return
	}

	# determine the table index for each image
	if {$use_crpix} {
	    # put images in correct order based on crpix values 
	    # (reverse of crpix order)
	    for {set i 0} {$i < $num_images_} {incr i} {
               set crpix1 $ext_($i,CRPIX1)
               set crpix2 $ext_($i,CRPIX2)
               set naxis1 $ext_($i,NAXIS1)
               set naxis2 $ext_($i,NAXIS2)
               set row -1
               set col -1
               catch {
                  set row [expr round(($max_crpix2 - $crpix2)/$naxis2)]
                  set col [expr round(($max_crpix1 -
                                       $crpix1)/$naxis1)]
               }
               if {$row<0 || $col<0 || [info exists check($row,$col)]} {
                  # use plan B...
                  set use_crpix 0
                  break
               }
               set check($row,$col) 1
               set ext_($i,idx) $row,$col
	    }
	} 

	if {! $use_crpix} {
	    # different sized images: put in sequential order
	    set num_cols 4
	    set num_rows [expr $num_images_/$num_cols+$num_cols]
	    set n 0
	    for {set row 0} {$row < $num_rows} {incr row} {
		for {set col 0} {$col < $num_cols} {incr col} {
		    if {$n == $num_images_} {
			break
		    }
		    set ext_($n,idx) $row,$col
		    incr n
		}
	    }
	}

	# put the images in the table
	blt::table $w
	for {set i 0} {$i < $num_images_} {incr i} {
	    set f [frame $w.f$i -borderwidth 5 -relief raised]
	    set im [RtdImage $f.im \
			-graphics 0 \
			-file $filename_ \
			-canvaswidth 100 \
			-canvasheight 100 \
			-fitwidth 100 \
			-fitheight 100]
	    pack $im -fill both -expand 1

	    # save widget names for later reference
	    set ext_($i,frame) $f
	    set ext_($i,RtdImage) $im
	    set ext_($i,image) [$im get_image]
	    set ext_($i,canvas) [$im get_canvas]

	    set hdu $ext_($i,HDU)
	    set name $ext_($i,ExtName)

	    # use after to override default bindings, etc.
	    after 0 [code $this add_image_bindings $im $hdu $name]

	    # position the image in the table
	    blt::table $w $f $ext_($i,idx) -fill both
	}

	# Select the HDU being displayed, if any
	select_image_hdu [$image_ hdu]
    }

    
    # This method is called when the user clicks on an image HDU icon.
    # Display the selected image and disable the delete button.
    
    protected method select_image_hdu {hdu} {
	$w_.buttons.delete config -state disabled
	$image_ hdu $hdu
	for {set i 0} {$i < $num_images_} {incr i} {
	    if {[info exists ext_($i,frame)]} {
		if {"$ext_($i,HDU)" == "$hdu"} {
		    $ext_($i,frame) configure -relief sunken
		} else {
		    $ext_($i,frame) configure -relief raised
		}
	    }
	}
	update
	catch "$table_ select_row [expr $hdu-1]"
    }

    
    # Add bindings to the given RtdImage itcl class object and set it to 
    # display the given HDU when clicked on. 
    # The image's extension name is also given.

    protected method add_image_bindings {im hdu name} {
	set image [$im get_image]
	set canvas [$im get_canvas]

	# set the HDU for the image
	$image hdu $hdu

	# need to add 2 bindings: one for the image, one for the background
	bind $canvas <1> [code $this select_image_hdu $hdu]

	# set up a resize handler to change the image size
	bind $canvas <Configure> [code $this resize $im %w %h]

	# add a help message indicating which image it is
	set s $name
	if {"$s" != ""} {
	    set s "($s)"
	}
	add_short_help $canvas "Click here to display HDU $hdu $s"
    }


    # This method is called when the image window is resized.
    # The rtdImage widget and the new width and height are given.

    protected method resize {im new_width new_height} {
	set image [$im get_image]
	$image config -fitwidth $new_width -fitheight $new_height
	$im center
    }


    # Set the HDU to display. Makes the currently selected HDU the current HDU

    protected method set_hdu {} {
	set sel [$table_ get_selected]
	if {[llength $sel]} {
	    lassign [lindex $sel 0] hdu type name
	    if {"$type" == "image"} {
		select_image_hdu $hdu
	    } elseif {"$type" == "ascii" || "$type" == "binary"} {
		display_fits_table $name $hdu
	    }
	}
    }
    

    # This method is called when a line in the HDU list is selected.
    # Update the states of the buttons depending on the selection.

    protected method select_hdu {} {
	set sel [$table_ get_selected]
	if {[llength $sel]} {
	    lassign [lindex $sel 0] hdu type name
	    if {"$type" == "image"} {
		$w_.buttons.delete config -state disabled
	    } elseif {"$type" == "ascii" || "$type" == "binary"} {
		$w_.buttons.delete config -state normal
	    }
	}
    }
    

    # Select an HDU to display. This makes it the current HDU
    # (XXX TO BE DONE: should also delete entry from $catinfo table)

    protected method delete_hdu {} {
	set hdu [$image_ hdu]
	set selected_hdu [lindex [lindex [$table_ get_selected] 0] 0]
	set type [$image_ hdu type $selected_hdu]

	if {"$type" == "image"} {
	    error_dialog "Deleting image HDUs is not allowed"
	    return
	} else {
	    append type " table"
	}
	if {! [confirm_dialog "Do you really want to delete the $type at HDU #$selected_hdu?"]} {
	    return
	}

	if {"$type" == "image"} {
	    catch {destroy $imagetab.f.im$selected_hdu}
	}

	if {[catch {$image_ hdu delete $selected_hdu} msg]} {
	    error_dialog $msg
	}

	set n [$image_ hdu count]
	if {$hdu == $selected_hdu || $n <= 0 || $hdu > $n} {
	    $itk_option(-image) clear
	    return
	}

	# current HDU my have moved up...
	$image_ hdu $hdu

	# update the list
	show_hdu_list
    }


    # Display the current FITS table

    protected method display_fits_table {name hdu} {
	# build the name from the catalog name and the file base name
	set file [file tail [file rootname [$image_ cget -file]]]
	if {[string first "$file-" $name] == 0} {
	    set filename /tmp/$name
	} else {
	    set filename "/tmp/$file-$name"
	}

	# get the catalog config entry from the $catinfo table
	set entry [get_config_entry_from_fits_table $name $filename]

	# copy the FITS table to a temporary local catalog
	if {[catch {$image_ hdu get $hdu $filename $entry} msg]} {
	    error_dialog $msg
	    return
	}

	# display the catalog
	cat::AstroCat::new_catalog $filename $itk_option(-image) ::skycat::SkySearch
    }


    # Return the catalog config entry for the named FITS table, if
    # available, or a default entry. If the current FITS file contains 
    # an HDU named $catinfo, with an entry for the named catalog ($extname), 
    # then extract and return that entry as a Tcl keyed list.
    
    protected method get_config_entry_from_fits_table {extname filename} {
	set headings [$image_ hdu listheadings]

	# the first part of the catalog config entry is always the same
	set entry {}
	lappend entry [list serv_type local]
	lappend entry [list short_name $extname]
	lappend entry [list long_name $extname]
	lappend entry [list url $filename]

	foreach row [$table_ cget -info] {
	    eval lassign [list $row] $headings
	    if {"$ExtName" == "$catinfo"} {
		# found table
		set headings [$image_ hdu headings $HDU]
		foreach row [$image_ hdu get $HDU] {
		    eval lassign [list $row] $headings
		    if {"$SHORT_NAME" == "$extname"} {
			# found entry
			foreach key $headings {
			    set value [set $key]
			    set key [string tolower $key]
			    if {"[string trim $value]" != ""} {
				lappend entry [list $key $value]
			    }
			}
			return $entry
		    }
		}
		break
	    }
	}
	# no entry found, use default (no plotting)
	foreach i {id_col ra_col dec_col x_col y_col} {
	    lappend entry [list $i -1]
	}
	return $entry
    }

    # Set the cut levels and colormap for the image extensions to the ones
    # used in the main image

    method use_settings_from_main_image {} {
	# return if no image
	if {! [info exists ext_(0,image)]} {
	    return
	}

	lassign [$image_ cut] low high
	if { "$low" == "" || "$high" == "" || [$image_ isclear] } {
	    return
	}
	set cmap [$image_ cmap file]
	set itt [$image_ itt file]
	set colorscale [$image_ colorscale]
	busy {
	    for {set i 0} {$i < $num_images_} {incr i} {
		$ext_($i,image) cut $low $high
		$ext_($i,image) colorscale $colorscale
		$ext_($i,image) cmap file $cmap
		$ext_($i,image) itt file $itt
		update idletasks
	    }
	}
    }
    
    # -- options  --

    # target SkyCatCtrl itcl class object
    itk_option define -image image Image {}

    # name of the FITS table containing catalog config info
    public variable catinfo "CATINFO"

    # -- protected vars --
    
    # internal rtdimage object
    protected variable image_

    # table displaying the HDUs
    protected variable table_

    # table displaying image extensions
    protected variable imagetab_

    # name of image file
    protected variable filename_ {}

    # number of image HDUs in the current FITS file
    protected variable num_images_ 0

    # array(HDUIndex,keyword) of image keyword and widget info
    protected variable ext_

    # C++ astrocat object use here to access catalog entries
    common astrocat_ [astrocat ::cat::.cataloginfo]
}

