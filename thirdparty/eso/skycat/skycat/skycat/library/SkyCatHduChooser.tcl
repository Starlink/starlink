# E.S.O. - VLT project 
# "@(#) $Id: SkyCatHduChooser.tcl,v 1.1 1998/11/16 21:26:44 abrighto Exp $"
#
# RtdImageHDUs.tcl - Itcl widget that displays the HDUs in a FITS file.
# 
# See man page SkyCatHduChooser(n) for a complete description.
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  09/11/98   Created

itk::usual SkyCatHduChooser {}

# This class defines a widget for displaying the HDUs in the current FITS
# image. The user can select an HDU to display by clicking on an
# entry. 

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
	itk_component add table {
	    set table_ [util::TableList $w_.table \
			    -title {FITS HDUs} \
			    -headings $headings \
			    -width [string length $headings]]
	}
	pack $itk_component(table) \
	    -side top -fill x 

	bind $table_.listbox <ButtonRelease-1> [code $this select_hdu]
    }

    
    # Make a subwindow for displaying miniature versions of image extensions

    protected method make_image_table {} {
	itk_component add image_table {
	    set imagetab_ [frame $w_.imagetab]
	}	
	pack $itk_component(image_table) \
	    -side top -fill both -expand 1
    }


    # add a row of buttons at bottom

    protected method make_buttons {} {
	itk_component add buttons {
	    frame $w_.buttons \
		-borderwidth 2 \
		-relief raised
	}

	pack [button $w_.buttons.close \
		  -text Close \
		  -command "destroy $w_"]
	
	pack $itk_component(buttons) \
	    -side top -fill x
    }

    
    # add a short help window

    protected method make_short_help {} {
	TopLevelWidget::make_short_help

	add_short_help $itk_component(table) \
	    {Table: lists the available FITS HDUs in the current image: click left to select HDU}

	add_short_help $itk_component(buttons).close {Close the HDU window}
    }

    
    # Update the list of HDUs and the image displays, if needed

    public method show_hdu_list {} {
	# update the image displays
	set old_filename $filename_
	set filename_ [$image_ cget -file]
	if {"$filename_" == "$old_filename"} {
	    return
	}

	set hdu_list [$image_ hdu list]
	set headings [$image_ hdu listheadings]

	# update the table listing
	$table_ clear
	$table_ config -height [llength $hdu_list] -info $hdu_list

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
	set num_images 0
	set max_crpix1 [set max_crpix2 0]
	set naxis1 [set naxis2 0]
	set first_time 1
	set use_crpix 1
	foreach hdu $hdu_list {
	    eval lassign [list $hdu] $headings
	    if {"$Type" == "image" && "$NAXIS" >= 2} {
		set ext($num_images,HDU) $HDU
		set ext($num_images,ExtName) $ExtName
		set ext($num_images,NAXIS1) $NAXIS1
		set ext($num_images,NAXIS2) $NAXIS2
		set ext($num_images,CRPIX1) $CRPIX1
		set ext($num_images,CRPIX2) $CRPIX2
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
		incr num_images
	    }
	}
	if {$num_images <= 1} {
	    return
	}

	# determine the table index for each image
	if {$use_crpix} {
	    # put images in correct order based on crpix values 
	    # (reverse of crpix order)
	    for {set i 0} {$i < $num_images} {incr i} {
		set crpix1 $ext($i,CRPIX1)
		set crpix2 $ext($i,CRPIX2)
		set naxis1 $ext($i,NAXIS1)
		set naxis2 $ext($i,NAXIS2)
		set row [expr int(($max_crpix2 - $crpix2)/$naxis2)]
		set col [expr int(($max_crpix1 - $crpix1)/$naxis1)]
		if {$row<0 || $col<0 || [info exists check($row,$col)]} {
		    # conflict: images might overlap, use plan B...
		    set use_crpix 0
		}
		set check($row,$col) 1
		set ext($i,idx) $row,$col
	    }
	} 

	if {! $use_crpix} {
	    # different sized images: put in sequential order
	    set num_cols 4
	    set num_rows [expr $num_images/$num_cols+$num_cols]
	    set n 0
	    for {set row 0} {$row < $num_rows} {incr row} {
		for {set col 0} {$col < $num_cols} {incr col} {
		    if {$n == $num_images} {
			break
		    }
		    set ext($n,idx) $row,$col
		    incr n
		}
	    }
	}

	# put the images in the table
	blt::table $w
	for {set i 0} {$i < $num_images} {incr i} {
	    set im [RtdImage $w.im$i \
			-graphics 0 \
			-file $filename_ \
			-canvaswidth 100 \
			-canvasheight 100 \
			-fitwidth 100 \
			-fitheight 100]
	    set image [$im get_image]
	    set canvas [$im get_canvas]
	    set hdu $ext($i,HDU)
	    set name $ext($i,ExtName)

	    # use after to override default bindings, etc.
	    after 0 [code $this add_image_bindings $im $hdu $name]

	    # position the image in the table
	    blt::table $w $im $ext($i,idx) -fill both
	}
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
	bind $canvas <1> "$image_ hdu $hdu"
	$canvas bind $image <1> "$image_ hdu $hdu"

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


    # Select an HDU to display. This makes it the current HDU

    protected method select_hdu {} {
	set sel [$table_ get_selected]
	if {[llength $sel]} {
	    lassign [lindex $sel 0] hdu type name
	    if {"$type" == "image"} {
		$image_ hdu $hdu
	    } elseif {"$type" == "ascii" || "$type" == "binary"} {
		# save and restore the HDU setting, since the image is
		# still accessing it
		set saved_hdu [$image_ hdu]
		$image_ hdu $hdu
		display_fits_table $name
		$image_ hdu $saved_hdu
	    }
	}
    }
    

    # Display the current FITS table

    protected method display_fits_table {name} {
	set file [file tail [file rootname [$image_ cget -file]]]
	set filename "/tmp/$file-$name"
	if {[catch {$image_ hdu get $filename} msg]} {
	    error_dialog $msg
	    return
	}
	cat::AstroCat::new_catalog $filename $itk_option(-image) ::skycat::SkySearch
    }

    
    # -- options  --

    # target RtdImage itcl class object
    itk_option define -image image Image {}

    # -- protected vars --
    
    # internal rtdimage object
    protected variable image_

    # table displaying the HDUs
    protected variable table_

    # table displaying image extensions
    protected variable imagetab_

    # name of image file
    protected variable filename_ {}

    # C++ astrocat object use here to access catalog entries
    common astrocat_ [astrocat ::cat::.cataloginfo]
}

