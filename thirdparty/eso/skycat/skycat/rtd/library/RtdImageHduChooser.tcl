# E.S.O. - VLT project 
# "@(#) $Id: RtdImageHduChooser.tcl,v 1.1.1.1 2006/01/12 16:38:21 abrighto Exp $"
#
# RtdImageHduChooser.tcl - Itcl widget for displaying FITS extensions
# 
# See man page RtdImageHduChooser(n) for a complete description.
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  09/11/98   Created
# Peter W. Draper 08/12/00   Slight corrections for missing CRPIX values.
# pbiereic        14/08/01   Added default method display_fits_table()
#                            for binary tables.
#                            Display HDU images only on request (for
#                            faster display of main image)

itk::usual RtdImageHduChooser {}

# This class defines a widget for displaying the HDUs in the current FITS
# image. The user can select an image extension to display 
# by clicking on an entry the list or on one of the small images displayed 
# in a table.

itcl::class rtd::RtdImageHduChooser {
    inherit util::TopLevelWidget


    # constructor: create a toplevel window

    constructor {args} {
	eval itk_initialize $args
        wm protocol $w_ WM_DELETE_WINDOW [code $this quit]
    }


    # This method is called after the options have been evaluated.

    protected method init {} {
	wm title $w_ "FITS HDUs ($itk_option(-number))"
	wm iconname $w_ "FITS HDUs ($itk_option(-number))"

	# get internal image handle
	set image_ [$itk_option(-image) get_image]

	make_table
	make_buttons
	make_short_help
	
	show_hdu_list
    }
    
    # Quit this widget

    public method quit { } {
        destroy $w_
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
        global $w_.show
        set $w_.show $itk_option(-show_images)

	# Button frame at bottom of window
	itk_component add buttons {
	    frame $w_.buttons -borderwidth 2
	}

	button $w_.buttons.open \
		-text Open \
		-command [code $this set_hdu]
	button $w_.buttons.display \
		-text "Display as one Image" \
		-state disabled \
		-command [code $this display_as_one_image]
	checkbutton $w_.buttons.show \
                -text "Show HDU images" \
                -variable $w_.show \
                -onvalue 1 -offvalue 0 \
                -borderwidth 2 \
                -relief raised \
                -state disabled \
                -pady 4 \
                -highlightthickness 0 \
                -command [code $this show_images]
	button $w_.buttons.delete \
		-text Delete \
		-state disabled \
		-command [code $this delete_hdu]
	button $w_.buttons.close \
		-text Close \
		-command "destroy $w_"
	pack $w_.buttons.open $w_.buttons.display $w_.buttons.show $w_.buttons.delete \
		$w_.buttons.close \
		-side left -expand 1 -fill x -padx 1m -ipadx 1m
	
	pack $itk_component(buttons) \
		-side top -fill x
    }

    
    # Set the cut levels for the image extensions to the given percent

    method auto_set_cut_levels {{percent 99}} {
	busy {
	    for {set i 0} {$i < $num_images_} {incr i} {
		$ext_($i,image) autocut -percent $percent
		update idletasks
	    }
	}
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

    # add a short help window

    protected method make_short_help {} {
	##TopLevelWidget::make_short_help

	add_short_help $itk_component(table) \
		{Table: Click to select HDU, double-click to display image or table}

	add_short_help $itk_component(buttons).open \
		{Open and display the selected HDU image or table}

	add_short_help $itk_component(buttons).close {Close the selected HDU}

	add_short_help $itk_component(buttons).display \
		{Display the image extensions as a single image using the CRPIX values from the FITS header}

	add_short_help $itk_component(buttons).delete \
		{Delete the selected HDU from the FITS file}

        add_short_help $itk_component(buttons).show \
		{Show/Hide the display of the miniature versions of the image extensions}
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

        $w_.buttons.show config -state disabled
        $w_.buttons.display config -state disabled

	# cleanup first
        delete_images
	destroy $w_.bintable
	
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
        set first_image -1
	set use_crpix 1
	foreach hdu $hdu_list {
	    eval lassign [list $hdu] $headings
            if {"$CRPIX1" == ""} {
                set CRPIX1 0
            } 
            if {"$CRPIX2" == ""} {
                set CRPIX2 0
            } 
	    if {"$Type" == "image" && "$NAXIS" >= 2 && "$NAXIS1" > 0 && "$NAXIS2" > 0} {
		set ext_($num_images_,HDU) $HDU
		set ext_($num_images_,ExtName) $ExtName
		set ext_($num_images_,NAXIS1) $NAXIS1
		set ext_($num_images_,NAXIS2) $NAXIS2
		set ext_($num_images_,CRPIX1) $CRPIX1
		set ext_($num_images_,CRPIX2) $CRPIX2
		if {$first_image == -1} {
                    set first_image $HDU
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

	if {$num_images_ < 1} {
	    return
	}

	# determine the table index for each image
	if {$use_crpix} {
	    # put images in correct order based on crpix values 
	    set max_row_ 0
	    set max_col_ 0
	    for {set i 0} {$i < $num_images_} {incr i} {
		set crpix1 $ext_($i,CRPIX1)
		set crpix2 $ext_($i,CRPIX2)
		set naxis1 $ext_($i,NAXIS1)
		set naxis2 $ext_($i,NAXIS2)
                set row -1
                set col -1
                catch {
   		   set row [expr {round(($max_crpix2 - $crpix2)/$naxis2)}]
		   set col [expr {round(($max_crpix1 - $crpix1)/$naxis1)}]
                }
		if {$row<0 || $col<0 || [info exists check($row,$col)]} {
		    # put in sequential order
		    set use_crpix 0
		    break
		}
		set check($row,$col) 1
		set ext_($i,row) $row
		set ext_($i,col) $col
		set max_row_ [max $max_row_ $row]
		set max_col_ [max $max_col_ $col]
	    }
	} 

	if {! $use_crpix} {
	    # different sized images: put in sequential order
	    set num_cols 4
	    set num_rows [expr {$num_images_/$num_cols+$num_cols}]
	    set max_row_ 0
	    set max_col_ 0
	    set n 0
	    for {set row 0} {$row < $num_rows} {incr row} {
		for {set col 0} {$col < $num_cols} {incr col} {
		    if {$n == $num_images_} {
			break
		    }
		    set ext_($n,row) $row
		    set ext_($n,col) $col
		    incr n
		    set max_row_ [max $max_row_ $row]
		    set max_col_ [max $max_col_ $col]
		}
	    }
	}

	# Select the HDU being displayed, if any
        if {$first_image == -1} {
            select_image_hdu [$image_ hdu]
        } else {
            select_image_hdu $first_image
        }

        if { $num_images_ > 1 } {
            $w_.buttons.show config -state normal
            if { $use_crpix } {
                $w_.buttons.display config -state normal
            }
        }
	update
	global $w_.show
	set $w_.show $itk_option(-show_images)
	show_images
    }

    # Remove all image minature versions

    method delete_images {} {
	global $w_.show
	set $w_.show 0

        if {[info exists imagetab_]} {
            # delete old images
            destroy $imagetab_
	    unset imagetab_
        }
    }

    # Show all image minature versions (called by checkbutton)

    protected method show_images {} {
        global $w_.show
        if { [set $w_.show] } {
	    create_images
	} else {
	    delete_images
	}
    }

    # put the images in the table

    protected method create_images {} {
	make_image_table

        set w $imagetab_
	for {set i 0} {$i < $num_images_} {incr i} {
	    set f [frame $w.f$i -borderwidth 2 -relief raised]
	    set im [RtdImage $f.im \
		    -graphics 0 \
		    -displaymode 0 \
		    -canvaswidth 100 \
		    -canvasheight 100 \
		    -fitwidth 100 \
		    -fitheight 100 \
                    -verbose $itk_option(-verbose) \
                    -usexsync $itk_option(-usexsync) \
                    -usexshm $itk_option(-usexshm)]
	    pack $im -fill both -expand 1

	    # save widget names for later reference
	    set ext_($i,frame) $f
	    set ext_($i,RtdImage) $im
	    set ext_($i,image) [$im get_image]
	    set ext_($i,canvas) [$im get_canvas]

	    # position the image in the table
	    set row [expr {$max_row_-$ext_($i,row)}]
	    set col $ext_($i,col)
	    blt::table $w $f ${row},${col} -fill both
	}

        button $w.settings \
                -text "Use Settings from Main Image" \
                -command [code $this use_settings_from_main_image]

        add_short_help $w.settings \
                {Set the cut levels and colormap for the preview images to the one used in the main image}

        blt::table $w \
	    $w.settings [expr {$max_row_ + 1}],0 -fill x -columnspan [expr {$max_col_ + 1}]

        update  ; # wait until images are ready (needed)

        # configure the images
	lassign [$image_ cut] low high
	busy {
	    for {set i 0} {$i < $num_images_} {incr i} {
		if { [catch {$ext_($i,image) config -file $filename_} msg] } {
		    error_dialog "HDU $ext_($i,HDU), Extension '$ext_($i,ExtName)':\n$msg"
		    continue
		}
		add_image_bindings $ext_($i,RtdImage) $ext_($i,HDU) $ext_($i,ExtName)
		$ext_($i,image) cut $low $high
		update idletasks
	    }
	}
    }
    
    # This method is called when the user clicks on an image HDU icon.
    # Display the selected image and disable the delete button.
    
    protected method select_image_hdu {hdu} {
	catch "$table_ select_row [expr {$hdu-1}]"
	$w_.buttons.delete config -state disabled

	# display only a real image
	set headings [$image_ hdu listheadings]
	eval lassign [list [lindex [$image_ hdu list] [expr {$hdu -1}] ]] $headings
	if {"$Type" == "image" && "$NAXIS" < 1} {
	    return
	}

	for {set i 0} {$i < $num_images_} {incr i} {
	    if {[info exists ext_($i,frame)] && [winfo exists $ext_($i,frame)]} {
		if {"$ext_($i,HDU)" == "$hdu"} {
		    $ext_($i,frame) configure -relief sunken
		} else {
		    $ext_($i,frame) configure -relief raised
		}
	    }
	}
        update idletasks
	busy { $image_ hdu $hdu }
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
	###bind $canvas <Configure> [code $this resize $im %w %h]

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
    

    # Display all of the image extensions as a single image (combine based 
    # on CRPIX1 and CRPIX2 keywords).

    protected method display_as_one_image {} {
	for {set i 0} {$i < $num_images_} {incr i} {
	    if {[info exists ext_($i,frame)] && [winfo exists $ext_($i,frame)]} {
		$ext_($i,frame) configure -relief raised
	    }
	}
	busy { $image_ hdu display }
    }


    # Select an HDU to display. This makes it the current HDU

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
	if {! [confirm_dialog "Delete the $type at HDU $selected_hdu ?"]} {
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


    # Display the current FITS table. Needs to be redefined by parent class
    # when other tables than binary are to be displayed.

    protected method display_fits_table {name hdu} {
        set sel [$table_ get_selected]
        lassign [lindex $sel 0] hdu type name
        if {"$type" != "binary"} { return }

        set bintbl $w_.bintable
        # toggle the visibility of the binary table
        if { [winfo exists $bintbl] && $hdu == $hdu_bin_ } {
            destroy $bintbl
            return
        }
        
        if { ! [winfo exists $bintbl] } {
            # TableList(n) widget for displaying the binary table
            util::TableList $bintbl \
                    -hscroll 1
            pack $bintbl -after $itk_component(buttons) -fill both -expand 1
        }

        set hdu_bin_ $hdu
        set headings [$image_ hdu headings $hdu]
	busy {
	    set info [$image_ hdu get $hdu]
	}
        $bintbl config -title $name -headings $headings -info $info
    }

    
    # -- options  --

    # target SkyCatCtrl itcl class object
    itk_option define -image image Image {}

    # flag: if true, images are "subsampled" when shrinking, 
    # otherwise the pixels are averaged
    itk_option define -subsample subsample Subsample 1
    
    # X shared memory option
    itk_option define -usexshm useXshm UseXshm 1

    # X synchronisation option
    itk_option define -usexsync useXsync UseXsync 1

    # flag: if true, print diagnostic messages
    itk_option define -verbose verbose Verbose {0}

    # Default: show image extensions, bool
    itk_option define -show_images show_images Show_images {0}

    # optionally specify TopLevelWidget to display short help messages
    itk_option define -shorthelpwin shortHelpWin ShortHelpWin {}

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

    # max row and column
    protected variable max_row_ 0
    protected variable max_col_ 0

    # HDU number of last binary table
    protected variable hdu_bin_ -1
}

