# E.S.O. - VLT project/ESO Archive
# @(#) $Id: SkyQueryResult.tcl,v 1.1.1.1 2002/04/04 20:11:54 brighton Exp $
#
# SkyQueryResult.tcl - Widget for viewing query results with skycat image support.
#
# See man page SkyQueryResult(n) for a complete description.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 5 Jan 98   created


# A SkyQueryResult widget is defined as a QueryResult (see cat package)
# with some added support for skycat image access, used for selecting objects
# to add to a local catalog.

itcl::class skycat::SkyQueryResult {
    inherit cat::QueryResult


    # constructor

    constructor {args} {
	eval itk_initialize $args
    }
   

    # pop up a dialog to enter the data for a new object for a local catalog
    # The command is evaluated after the users enters the new data.
    # (redefined from parent class to add image support)

    public method enter_new_object {{command ""}} {
	catch {delete object $w_.ef}
	EnterObject $w_.ef \
	    -title {Please enter the data for the object below:} \
	    -labels $headings_ \
	    -center 0 \
	    -image $skycat \
	    -command [code $this enter_object $command]
    }

    # pop up a window so that the user can edit the selected object(s)
    # The optional command is evaluated with no args if the object is
    # changed.
    # (redefined from parent class AstroCat to add image support)
    
    public method edit_selected_object {{command ""}} {
	catch {destroy $w_.ef}
	set values [lindex [get_selected] 0]

	if {[llength $values] == 0} {
	    error_dialog "No rows are selected" $w_
	    return;
	}

	EnterObject $w_.ef \
	    -title {Please enter the data for the object below:} \
	    -image $skycat \
	    -labels $headings_ \
	    -values $values \
	    -command [code $this enter_object $command]
    }


    # save the current data as a FITS table in the current image file.
    # The argument is the catalog config entry.
    
    public method save_with_image {entry} {
	set image [$skycat get_image]

	# make sure file exists
	set file [$image cget -file]
	set suffix [file extension $file]

	switch -exact -- "$suffix" {
	    ".gz" -
	    ".gzfits" -
	    ".gfits" -
	    ".Z" -
	    ".cfits" -
	    ".hfits" {
		error_dialog "Can't save catalog data to compressed image file."
		return
	    }
	}

	set headings [$image hdu listheadings]
	
	# get the short name of the catalog and use it as the table name
	set extname ""
	foreach i $entry {
	    lassign $i key value
	    if {"$key" == "short_name"} {
		set extname $value
		break
	    }
	}
	# build the name from the catalog name and the file base name
	set file [file tail [file rootname $file]]
	if {[string first "$file-" $extname] == 0} {
	    set extname [string range $extname [expr [string length $file]+1] end]
	}
	
	if {"$extname" == ""} {
	    set extname [input_dialog "Please enter a name for the FITS table"]
	}
	if {"$extname" == ""} {
	    return
	}
	
	# use all ASCII formats (use inherited size_ array)
	set tform {}
	if {$num_cols_ <= 1} {
	    error_dialog "No data to save"
	    return
	}
	if {! [info exists size_]} {
	    error_dialog "No column size info"
	    return
	}
	for {set i 1} {$i <= $num_cols_} {incr i} {
	    lappend tform "$size_($i)A"
	}
	
	# If there is aleady a table by this name in the file, delete it
	# and replace it with the new one.
	set hdu_list [$image hdu list]
	foreach hdu $hdu_list {
	    eval lassign [list $hdu] $headings
	    if {"$extname" == "$ExtName"} {
		if {[catch {$image hdu delete $HDU} msg]} {
		    error_dialog $msg
		}
	    }
	}

	# save the current HDU number and restore it before returning
	set saved_hdu [$image hdu]

	# create a new binary table
	if {[catch {
	    $image hdu create binary $extname $headings_ $tform $info_
	} msg]} {
	    error_dialog "error creating FITS table '$extname': $msg"
	    return
	}

	# create/update catalog config info to a special FITS table
	if {[catch {
	    save_config_info_to_fits_table $extname $entry
	} msg]} {
	    after idle [list error_dialog $msg]
	}

	# restore saved HDU
	set numHDUs [$image hdu count]
	if {$saved_hdu <= $numHDUs} {
	    $image hdu $saved_hdu
	} else {
	    # shouldn't happen, but if the HDU was deleted, use the new last one
	    $image hdu $numHDUs
	}

	# update/display the HDU window
	$skycat update_fits_hdus
    }


    # Save the given catalog config entry in a FITS table with the name
    # $catinfo. The hdu arg gives the HDU number of the $catinfo table,
    # or 0 if it does not exist.
    
    protected method save_config_info_to_fits_table {extname entry} {
	set image [$skycat get_image]

	# Look for an existing $catinfo table
	set headings [$image hdu listheadings]
	set hdu_list [$image hdu list]
	set hdu 0
	foreach row $hdu_list {
	    eval lassign [list $row] $headings
	    if {"$ExtName" == "$catinfo"} {
		set hdu $HDU
		break
	    }
	}

	# If the table exists, get the data and remove it, so that
	# we can recreate it, with possibly new columns or column
	# widths
	set rowNum 0
	if {$hdu} {
	    if {[catch {
		set headings [$image hdu headings $hdu]
		set info [$image hdu get $hdu]
		$image hdu delete $hdu
	    } msg]} {
		error_dialog $msg
		return
	    }
	    # scan the current info, allow for future additions to headings
	    foreach row $info {
		eval lassign [list $row] $headings
		foreach i $headings {
		    set ar($rowNum,$i) [set $i]
		}
		if {"$SHORT_NAME" == "$extname"} {
		    # replace this entry with the new one
		    continue
		}
		incr rowNum
	    }
	}

	# set headings for catalog config table
	set headings "SHORT_NAME ID_COL RA_COL DEC_COL X_COL Y_COL EQUINOX SYMBOL \
                      SEARCH_COLS SORT_COLS SORT_ORDER SHOW_COLS HELP COPYRIGHT"
	
	# initialize min column widths
	foreach i $headings {
	    set width($i) 1
	}

	# get values from config entry
	foreach i $entry {
	    lassign $i key value
	    if {"$key" == "symbol" || "$key" == "search_cols"} {
		# special treatment needed here (see CatalogInfo.tcl)
		set value [join $value " : "]
	    }
	    set ar($rowNum,[string toupper $key]) $value
	}
	set ar($rowNum,SHORT_NAME) $extname

	# build table data list and get max col widths for FITS formats
	set info {}
	set numRows [incr rowNum]
	for {set rowNum 0} {$rowNum < $numRows} {incr rowNum} {
	    set row {}
	    foreach i $headings {
		if {[info exists ar($rowNum,$i)]} {
		    lappend row $ar($rowNum,$i)
		    set width($i) [max $width($i) [string length $ar($rowNum,$i)]]
		} else {
		    lappend row {}
		}
	    }
	    lappend info $row
	}

	# build the tform argument
	set tform {}
	foreach i $headings {
	    lappend tform "$width($i)A"
	}

	# create a new binary table and insert the info
	if {[catch {
	    $image hdu create binary $catinfo $headings $tform $info
	} msg]} {
	    error_dialog "error creating FITS table '$catinfo': $msg"
	    return
	}
    }


    # -- public variables --

    # name of SkyCatCtrl itcl widget
    public variable skycat {}

    # name of the FITS table containing catalog config info
    public variable catinfo "CATINFO"
}


    

