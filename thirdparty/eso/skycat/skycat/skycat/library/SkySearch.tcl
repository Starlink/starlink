# E.S.O. - VLT project/ESO Archive
# @(#) $Id: SkySearch.tcl,v 1.35 1998/12/14 17:32:08 abrighto Exp $
#
# SkySearch.tcl - Widget for searching a catalog and plotting the results
#                 in the skycat image viewer. 
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 10 Dec 97   created


itk::usual SkySearch {}

# This class extends the AstroCat catalog widget browser class (see 
# AstroCat(n) to add support for plotting objects and displaying images.

itcl::class skycat::SkySearch {
    inherit cat::AstroCat


    # constructor

    constructor {args} {
	eval itk_initialize $args
    }


    # called after options have been evaluated

    protected method init {} {
	AstroCat::init

	# these are the supported plot symbols
	foreach i "circle square plus cross triangle diamond ellipse compass line arrow" {
	    set symbols_($i) 1
	}

	# add a menu item to the File menu to save the catalog as a FITS table
	if {$iscat_} {
	    set m [get_menu File]
	    insert_menuitem $m "Add to..." command "Save with image" \
		{Save the listed objects to the current FITS file as a binary table} \
		-command [code $this save_with_image]
	}

	# this unique canvas tag is used for all symbols (see also SkySearch.C)
	set tag_ $w_.cat
	set object_tag_ $tag_.objects
	set label_tag_ $tag_.labels

	# add bindings for symbols
	$canvas_ bind $object_tag_  <1> "[code $this select_symbol current 0]"
	$canvas_ bind $object_tag_  <Shift-1> "[code $this select_symbol current 1]"
	$canvas_ bind $object_tag_  <Control-1> "[code $this select_symbol current 1]"
	$canvas_ bind $object_tag_  <Any-Enter> "$canvas_ config -cursor tcross"
	$canvas_ bind $object_tag_  <Any-Leave> "$draw_ reset_cursor"

	# symbols can't be moved, but labels can (but not edited).
	# (See SkySearch.C:plot_symbol() for origin of tag name)
	$draw_ add_object_bindings $label_tag_ current
	$canvas_ bind $label_tag_ <ButtonRelease-1> "+$canvas_ focus {}"

	# add short help for canvas objects
	set msg "Catalog symbol: {bitmap b1} = select object"
	set w [winfo toplevel $canvas_]
	$canvas_ bind $object_tag_ <Enter> "+[code $w short_help $msg]"
	$canvas_ bind $object_tag_ <Leave> "+[code $w short_help {}]"
    }

    
    # save the current data as a FITS table in the current image file.
    
    public method save_with_image {} {
	busy {
	    $results_ save_with_image [$w_.cat entry get]
	}
    }


    # insert the id for the given object in the image near the object
    # and return a string containing status info. name identifies the
    # source catalog (short_name).

    public method label_object_in_image {id name} {
	if {"$canvas_" == ""} {
	    return
	}

	if {[llength [set box [$canvas_ bbox cat$id]]]} {
	    lassign $box x0 y0 x1 y1
	    make_label $name $id [expr ($x1+$x0)/2.0] [expr ($y1+$y0)/2.0] canvas $id white
	    return "labeled object '$id' in image"
	} else {
	    return "object '$id' is not visible"
	}
    }

    
    # remove any items in the query result list that have not been plotted
    # because they were not in the image (circular search/rectangualr image).

    public method filter_query_results {} {
	$w_.progress config -text "Filtering out off-image objects..."
	set new_info {}
	set n 0
	busy {
	    foreach row $info_ {
		set id [lindex $row [$w_.cat id_col]]
		if {[llength [$canvas_ find withtag cat$id]]} {
		    lappend new_info $row
		    incr n
		}
	    }
	    set t [$results_ total_rows]
	    if {$n != $t} {
		$results_ config \
		    -info [set info_ $new_info] \
		    -title "Search Results ($n*)"
		plot
		$w_.progress config -text "Removed [expr $t-$n] objects from the list."
	    } else {
		$w_.progress config -text "No change."
	    }
	}
    }


    # add a label to the image at the given coordinates (in the given units)
    # with the given text and color. The id arg should be a unique id for the 
    # label in the catalog and $name should be the short name of the catalog.
    # $units may be any of the units supported by the RTD {image canvas screen
    # "wcs $equinox" "deg $equinox"}
    
    public method make_label {name id x y units text color} {
	if {[catch {lassign [convert_coords $x $y $units canvas] x y} msg]} {
	    return
	}
	set tags [list objects $tag_ label$id $name]
	$canvas_ delete label$id
	set cid [$canvas_ create text $x $y \
		    -text $text \
		    -anchor sw \
		    -fill $color \
		    -font $itk_option(-canvasfont) \
		    -tags $tags]

	$draw_ add_object_bindings $cid
	ct_add_bindings $canvas_ $cid
    }

    
    # display the given image file

    public method display_image_file {filename} {
	$skycat_ config -file $filename
    }


    # retun the width of the image display canvas
    
    public method get_display_width {} {
	return [winfo width $canvas_]
    }

    
    # retun the height of the image display canvas
    
    public method get_display_height {} {
	return [winfo height $canvas_]
    }
    
    
    # return the name (file or object name) of the currently loaded image, 
    # or empty if no image is loaded.

    public method get_image_name {} {
	set name [$image_ cget -file]
	if {"$name" == ""} {
	    set name [$image_ object]
	}
	return $name
    }

    
    # generate a blank image that supports world coordinates for the purpose
    # of plotting catalog objects.
    # ra_deg, dec_deg and equinox give the center of the image (in deg), 
    # radius the radius in arcmin.
    # Returns "0" if all is OK.

    public method gen_wcs_image {ra_deg dec_deg equinox radius} {
	if {"$ra_deg" == "" || "$dec_deg" == ""} {
	    info_dialog "please specify values for RA and DEC." $w_
	    return
	}
	if {"$radius" == ""} {
	    info_dialog "please specify a radius value." $w_
	    return
	}

	if {"$equinox" == ""} {
	    set equinox 2000
	} elseif {[string match {[jJbB]} [string index $equinox 0]]} {
	    set equinox [string range $equinox 1 end]
	}

	$image_ clear \
	    -reuse 1 \
	    -ra $ra_deg \
	    -dec $dec_deg \
	    -equinox $equinox \
	    -radius $radius \
	    -width [get_display_width] \
	    -height [get_display_height]

	# enable the options panel, since we now have an image
	catch {[[$skycat_] component info] configure -state normal}

	return 0
    }


    # generate a blank image without WCS. radius is radius of the image in
    # pixels, 
    # Returns "0" if all is OK.

    public method gen_pix_image {radius} {
	if {"$radius" == ""} {
	    info_dialog "please specify a radius value." $w_
	    return
	}
	$image_ clear \
	    -reuse 1 \
	    -radius $radius \
	    -width [get_display_width] \
	    -height [get_display_height]
	
	# enable the options panel, since we now have an image
	catch {[[$skycat_] component info] configure -state normal}

	return 0
    }


    # This method is called when the user clicks on a graphic symbol for a star.
    # The user might be selecting this star, so call the RtdImage method to do that
    
    public method picked_wcs_object {x y units} {
	    if {[catch {
		lassign [convert_coords $x $y $units "wcs [$image_ wcsequinox]"] ra dec
		lassign [convert_coords $x $y $units image] ix iy
	    } msg]} {
		return
	    }
	    catch {$skycat_ picked_wcs_object $ix $iy $ra $dec}
    }

    
    # deselect any objects in the image

    public method deselect_objects {} {
	$canvas_ delete grip
    }
    

    # delete any graphic objects in the image belonging to this catalog

    public method delete_objects {} {
 	catch {$canvas_ delete $tag_}
    }
    
    
    # convert the given input coordinates in the given input units to the
    # given output units and return a list {x y} with the new values.
    # The units may be one of {canvas image wcs deg "wcs $equinox", "deg $equinox"}

    public method convert_coords {in_x in_y in_units out_units} {
	return [$image_ convert coords $in_x $in_y $in_units {} {} $out_units]
    }

    
    # add the dialog button frame
    # (redefined from parent class AstroCat to add Plot button)

    protected method add_dialog_buttons {} {
	AstroCat::add_dialog_buttons
	# only do this for catalogs, not for image servers...
	if {$iscat_} {
	    pack \
		[button $w_.plot \
		     -text "Plot" \
		     -command [code $this plot_again]] \
		[button $w_.filter \
		     -text "Filter" \
		     -command [code $this filter_query_results]] \
		-side left -expand 1 -pady 2m -in $w_.buttons -after $w_.search 

	    if {[llength [$w_.cat symbol]] == 0} {
		$w_.plot config -state disabled
		$w_.filter config -state disabled
	    }
	}
    }


    # add a short help window and set the help texts
    # (redefined from parent class AstroCat)
    
    protected method make_short_help {} {
	AstroCat::make_short_help
	add_short_help $w_.plot \
	    {{bitmap b1} = Plot the listed objects again in the image}
	add_short_help $w_.filter \
	    {{bitmap b1} = Filter out off-image objects from the listing (circular search/rectangular image...)}
    }


    # insert the Id for the object selected in the Table in the image
    # near the object.

    protected method label_selected_object {} {
	set id [lindex [lindex [$results_ get_selected] 0] [$w_.cat id_col]]
	if {"$id" == ""} {
	    return
	}
	set name [$w_.cat shortname $itk_option(-catalog)]
	$w_.progress config -text [label_object_in_image $id $name]
    }

    
    # add the search options panel
    # (redefined from parent class AstroCat to add image support)

    protected method add_search_options {} {
	# SkyQuery(n) widget (derived from AstroQuery(n)) for displaying
	# search options.
	itk_component add searchopts {
	    set searchopts_ [SkyQuery $w_.searchopts \
				 -relief groove \
				 -borderwidth 2 \
				 -debug $itk_option(-debug) \
				 -astrocat [code $w_.cat] \
				 -skycat $skycat_ \
				 -searchcommand [code $this search] \
				 -feedbackcommand [code $this set_feedback] \
				 -command [code $this query_done]]
	}
	pack $itk_component(searchopts) \
	    -side top -fill x
    }

    
    
    # add the table for displaying the query results
    # (redefined from parent class AstroCat to add image support)

    protected method add_result_table {} {
	# SkyQueryResult(n) widget to display the results of a catalog query.
	itk_component add results {
	    set results_ [SkyQueryResult $w_.results \
			      -astrocat [code $w_.cat] \
			      -skycat $skycat_ \
			      -title "Search Results" \
			      -hscroll 1 \
			      -height 12 \
			      -sortcommand [code $this set_sort_cols] \
			      -layoutcommand [code $this set_show_cols] \
			      -selectmode extended \
			      -exportselection 0]
	} {
	}
	pack $itk_component(results) -side top -fill both -expand 1
	bind $results_.listbox <ButtonRelease-1> [code $this select_result_row]
	$results_ set_options {MORE PREVIEW more preview} Show 0
	
	# for history catalog, double-click opens file
	if {"[$w_.cat longname]" == "$history_catalog_"} {
	    bind $results_.listbox <Double-ButtonPress-1> [code $this preview]
	} else {
	    bind $results_.listbox <Double-ButtonPress-1> [code $this label_selected_object]
	}
    }


    # set/reset widget states while busy 
    # (redefined from parent class AstroCat)

    public method set_state {state} {
	AstroCat::set_state $state
	if {$iscat_} {
	    if {[llength [$w_.cat symbol]] == 0} {
		set state disabled
	    }
	    $w_.plot config -state $state
	    $w_.filter config -state $state
	}
    }


    # re-plot the listed objects
    
    public method plot_again {} {
	busy {plot}
    }
        
       
    # plot the stars/objects found in the previous search in the image window.
    # The symbols to use are taken from the config file.

    public method plot {} {
	# can't plot with no coordinates
	if {![$w_.cat iswcs] && ![$w_.cat ispix]} {
	    return
	}

	# can't plot without symbol info
	if {"[$w_.cat symbol]" == ""} {
	    return
	}

	# if we have an image display, but no image is loaded, generate dummy image
	if {"[get_image_name]" == "" && [$image_ width] < 10} {
	    if {[gen_blank_image] != 0} {
		return
	    }
	}

	# if any objects are selected, deselect them first
	deselect_objects
	delete_objects
	$w_.progress config -text "Plotting objects..."
	update idletasks

	set equinox [$w_.searchopts get_equinox]
	
	# the plot method was reimplemented in C++ for better performance
	# See SkySearch.C for the implementation of the astrocat plot subcommand.
	if {[catch {$w_.cat imgplot $image_ $info_ $equinox $headings_} msg]} {
	    error_dialog $msg
	}
    }

    
    # Called when a row in the table is selected. Redefined from parent
    # clas to also select the plot symbol.

    protected method select_result_row {} {
	AstroCat::select_result_row

	# clear symbol selection
	deselect_symbol $w_.selected

	# select symbols matching selected rows
	foreach row [$results_ get_selected_with_rownum] {
	    lassign $row rownum row
	    set id [lindex $row [$w_.cat id_col]]
	    if {"$id" == ""} {
		continue
	    }
	    select_symbol cat$id 1 $rownum
	}
    }

    
    # Select a symbol, given the canvas id and optional row number 
    # in the table listing. If $toggle is 0, deselect all other symbols 
    # first, otherwise toggle the selection of the items given by $id.
    
    public method select_symbol {id toggle {rownum -1}} {
	set tag [lindex [$canvas_ gettags $id] 0]

	if {$rownum < 0} {
	    set rownum [get_table_row $id]
	    if {$rownum < 0} {
		return
	    }
	}
	
	if {$toggle} {
	    # toggle selection
	    if {[$draw_ item_has_tag $tag $w_.selected]} {
		deselect_symbol $tag
		$results_ deselect_row $rownum
		return
	    } 
	} else {
	    # clear selection
	    deselect_symbol $w_.selected
	}

	if {"$rownum" >= 0} {
	    $results_ select_row $rownum [expr !$toggle]
	    $results_ select_result_row
	}

	foreach i [$canvas_ find withtag $tag] {
	    set width [$canvas_ itemcget $i -width]
	    $canvas_ itemconfig $i -width [incr width 2]
	}
	$canvas_ addtag $w_.selected withtag $tag
	$canvas_ raise $tag $image_
    }


    # deselect the given symbol, given its canvas tag or id
    
    public method deselect_symbol {tag} {
	foreach i [$canvas_ find withtag $tag] {
	    set width [$canvas_ itemcget $i -width]
	    $canvas_ itemconfig $i -width [incr width -2]
	}
	$canvas_ dtag $tag $w_.selected
    }

   
    # Return the table row index corresponding the given symbol canvas id.
    # Note: The plot subcommand in SkySearch.C adds a canvas tag "row#$rownum"
    # that we can use here.
    # Also: cat$id is first tag in the tag list for each object.

    public method get_table_row {id} {
	set tags [$canvas_ gettags $id]
	# look for row# tag (but only if not sorted!)
	if {[llength [$w_.cat sortcols]] == 0} {
	    foreach tag $tags {
		if {[scan $tag "row#%d" rownum] == 1} {
		    return $rownum
		}
	    }
	}

	# search for $id in query results (slow way)
	set tag [lindex $tags 0]
	set rownum -1
	foreach row [$results_ get_contents] {
	    incr rownum
	    set id [lindex $row [$w_.cat id_col]]
	    if {"cat$id" == "$tag"} {
		return $rownum
	    }
	}
	# not found
	return -1
    }


    # This method is called when a region of the image has been selected
    # (From class SkyCat, via -regioncommand option when creating the image).
    # The arguments are the bounding box of the region in canvas coords.
    # Select any catalog symbols in the region.

    public method select_region {x0 y0 x1 y1} {
	# clear symbol selection first
	deselect_symbol $w_.selected
	$results_ clear_selection

	# make sure its is one of our objects
	foreach id [$canvas_ find enclosed $x0 $y0 $x1 $y1] {
	    if {[$draw_ item_has_tag $id $object_tag_]} {
		set rownum [get_table_row $id]
		if {[info exists got_it($rownum)]} {
		    continue
		}
		set got_it($rownum) 1
		
		# select the object in the image and table
		if {$rownum >= 0} {
		    select_symbol $id 1 $rownum
		}
	    }
	}
    }


    # generate a dummy blank image for the purpose of plotting catalog
    # objects on it.  Return 0 if OK, otherwise 1.
    
    public method gen_blank_image {} {
	if {[$w_.cat iswcs]} {
	    # using world coords
	    lassign [$w_.searchopts get_pos_radius] ra dec equinox radius
	    if {"$equinox" == ""} {
		set equinox 2000
	    }
	    if {"$ra" == "" || "$dec" == ""} {
		# can't create a blank image with no center
		# use coords from first row, if any
		set row [lindex $info_ 0]
		set ra [lindex $row [$w_.cat ra_col]]
		set dec [lindex $row [$w_.cat dec_col]]
		if {"$ra" == "" || "$dec" == ""} {
		    return 1
		}
	    }
	    if {[catch {lassign [$wcs_ hmstod $ra $dec] ra_deg dec_deg} msg]} {
		warning_dialog "$msg (ra = $ra, dec = $dec)"
		return 1
	    }

	    # generate dummy image
	    return [gen_wcs_image $ra_deg $dec_deg $equinox $radius]
	} else {
	    # generate dummy image
	    lassign [$w_.searchopts get_pos_radius] x y radius
	    return [gen_pix_image $radius]
	}
	
	return 0
    }


    # clear the table listing (done in base class) and remove any plot
    # symbols from the display.
    
    
    public method clear {} {
	AstroCat::clear

	# remove any catalog symbols (since table is also empty now)
	delete_objects
    }

    
    # Add the current image file to the history catalog under the given filename.
    # $skycat is the handle of the SkyCatCtrl itcl class object to use to extract image 
    # infomation to put in the catalog.

    public proc add_history {skycat filename} {
	set catalog $history_catalog_
	set image [$skycat get_image]
	
	# make sure at least an empty catalog exists
	if {! [file exists $catalog] || [file size $catalog] == 0} {
	    # If it doesn't exist yet, create an empty catalog file
	    if {[catch {set fd [::open $catalog w]} msg]} {
		error_dialog "can't create image history catalog: %msg"
		return
	    }
	    puts $fd "Skycat History Catalog v1.0"
	    puts $fd ""
	    puts $fd "ra_col: -1"
	    puts $fd "dec_col: -1"
	    puts $fd "x_col: -1"
	    puts $fd "y_col: -1"
	    puts $fd "show_cols: file ra dec object NAXIS NAXIS1 NAXIS2 NAXIS3"
	    puts $fd "sort_cols: timestamp"
	    puts $fd "sort_order: decreasing"
	    puts $fd ""
	    puts $fd [join $history_cols_ "\t"]
	    puts $fd "----"
	    ::close $fd
	    # get the catalog into the list of known catalogs
	    $astrocat_ open $catalog
	}

	if {"$filename" == "" || [string first /tmp $filename] == 0 \
		|| ! [file exists $filename]} {
	    # ignore temporary and non-existant files
	    return
	}
	
	# add an entry for the given image and filename
	set id [file tail $filename]
	lassign [$image wcscenter] ra dec equinox
	set object [$image fits get OBJECT] 
	set naxis [$image fits get NAXIS]
	set naxis1 [$image fits get NAXIS1]
	set naxis2 [$image fits get NAXIS2]
	set naxis3 [$image fits get NAXIS3]
	lassign [$image cut] lowcut highcut
	set colormap [$image cmap file]
	set itt [$image itt file]
	set colorscale [$image colorscale]
	set zoom [lindex [$image scale] 0]
	if {"$zoom" == ""} {
	    set zoom 1
	}
	set timestamp [clock seconds]

	# get full path name of file for preview URL
	if {"[string index $filename 0]" == "/"} {
	    set fullpath $filename
	} else {
	     set fullpath [pwd]/$filename
	}
	set preview file:$fullpath

	set data [list [list $id $ra $dec $object $naxis $naxis1 $naxis2 $naxis3\
			    $lowcut $highcut $colormap $itt $colorscale $zoom \
			    $timestamp $preview]]

	$astrocat_ open $catalog
	$astrocat_ save $catalog 1 $data $equinox

	# update history catalog window, if it is showing
	set w [cat::AstroCat::get_instance [file tail $catalog]]
	if {"$w" != "" && [winfo viewable $w]} {
	    $w search
	}
    }
    

    # Check if the given filename is in the history catalog, and if so, 
    # apply the cut levels and color settings for the file.
    # $skycat is the handle of a SkyCatCtrl itcl class object to use.

    public proc apply_history {skycat filename} {
	if {"$filename" == "" || [string first /tmp $filename] == 0 \
		|| ! [file exists $filename]} {
	    # ignore temporary and non-existant files
	    return
	}
	set catalog $history_catalog_
	set image [$skycat get_image]
	if {[catch {$astrocat_ open $catalog}]} {
	    # no catalog yet
	    return
	}
	set list [$astrocat_ query -id [file tail $filename]]
	if {[llength $list] == 0} {
	    # not in catalog
	    return
	}
	set row [lindex $list 0]
	eval lassign {$row} $history_cols_
	if {[catch {$image cut $lowcut $highcut 0}]} {
	    # must be something wrong with this entry, remove it
	    $astrocat_ remove $catalog
	    return
	}
	$image cmap file $colormap
	$image itt file $itt
	$image colorscale $colorscale
	# after 1000 [list $skycat scale $zoom $zoom]

	# update the main panel and color window
	$skycat component info updateValues
	$skycat update_color_window
    }

    
    # Add at most $n image history entries to the given menu $m.
    # w is the TopLevelWidget containing the menubar and $skycat is 
    # the handle to the SkyCatCtrl class to use to load an image.

    public proc add_history_menu_items {w skycat m n} {
	set catalog $history_catalog_
	if {[catch {$astrocat_ open $catalog}]} {
	    # no catalog yet
	    return
	}
	set list [$astrocat_ query -nrows $n -sort timestamp -sortorder decreasing]
	foreach row $list {
	    eval lassign {$row} $history_cols_
	    set filename [string range $PREVIEW 5 end]
	    $m add command \
		-label $file \
		-command [list after idle $skycat config -file $filename]
	}
    }


    # -- options --
    
    # Optional unique id, used in searching for already existing catalog widgets.
    itk_option define -id id Id "" {
	# in SkyCat.tcl, we passed the name of the SkyCatCtrl Itcl image widget as -id.
	set skycat_ $itk_option(-id)
	set canvas_ [$skycat_ get_canvas]
	set image_ [$skycat_ get_image]
	set draw_ [$skycat_ component draw]
    }

    # font used in canvas to mark objects
    itk_option define -canvasfont canvasFont CanvasFont -*-courier-medium-r-*-*-*-120-*-*-*-*-*-*


    # -- protected members --

    # SkyCatCtrl widget instance
    protected variable skycat_ {}

    # internal rtdimage image for main image
    protected variable image_ {}

    # canvas window containing main image
    protected variable canvas_ {}

    # CanvasDraw object for drawing on image
    protected variable draw_

    # array containing supported symbol names
    protected variable symbols_

    # canvas tag used to identify all symbols for this instance
    protected variable tag_

    # canvas tag used to identify all objects for this instance
    protected variable object_tag_

    # canvas tag used to identify all labels for this instance
    protected variable label_tag_

    # name of wcs object for converting between hh:mm:ss and double deg
    protected variable wcs_ ::skycat::.wcs

    # -- common class variables --

    # name of the history catalog
    global ::env
    protected common history_catalog_ $env(HOME)/.skycat/history

    # list of columns in the history catalog
    protected common history_cols_ \
	[list file ra dec object NAXIS NAXIS1 NAXIS2 NAXIS3 \
	     lowcut highcut colormap itt colorscale zoom timestamp PREVIEW]
}


# C++ wcs object used by procs below.
wcs ::skycat::.wcs

# convert a value in hh:mm:ss format to floating point format
# (can be used in plot symbol expressions)

proc hmstod {hms} {
    return [::skycat::.wcs hmstod $hms]
}


# convert a floating point format value to hh:mm:ss format
# (can be used in plot symbol expressions)

proc dtohms {d} {
    return [::skycat::.wcs dtohms $d]
}

