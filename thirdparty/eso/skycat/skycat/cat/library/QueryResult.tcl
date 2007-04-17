# E.S.O. - VLT project/ESO Archive
# @(#) $Id: QueryResult.tcl,v 1.1.1.1 2006/01/12 16:36:15 abrighto Exp $
#
# QueryResult.tcl - Widget for viewing the results of a catalog query.
#
# See man page QueryResult(n) for a complete description.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 5 Jan 98   created


# A QueryResult widget frame is defined as a TableList (see TableList(n)
# in the tclutil package) with some methods added for catalog
# support. It is used to display the results of a catalog query. See
# also AstroQuery(n) for the query paramaters, AstroCat(n) for the main
# window or classes derived from these in the skycat package.
# These classes do not deal with images - only catalog data. See the
# derived classes in the skycat package for image support.

itcl::class cat::QueryResult {
    inherit util::TableList


    # constructor

    constructor {args} {
	eval itk_initialize $args
    }
   
    
    # pop up a dialog to sort the list

    public method sort_dialog {} {
	if {[llength $headings_] == 0} {
	    info_dialog "Please make a query first so that the column names are known" $w_
	    return
	}
	TableList::sort_dialog
    }

    
    # This method is called whenever a result row is selected.
    # If the row edit window exists, update it with the values for
    # the new row.

    public method select_result_row {} {
	if {[winfo exists $w_.ef]} {
	    $w_.ef configure -values [lindex [get_selected] 0]
	}
    }


    # pop up a dialog to select table columns to display

    public method select_columns {} {
	if {[llength $headings_] == 0} {
	    info_dialog "Please make a query first so that the column names are known" $w_
	    return
	}
	TableList::layout_dialog
    }


    # reset table dialogs if needed

    public method reset {} {
	foreach w "$w_.tblsort $w_.tblcfg" {
	    if {[winfo exists $w]} {
		$w reset
	    }
	}
	$astrocat showcols {}
	$astrocat sortcols {}
    }
   
  
    # save the current data to a local catalog
    
    public method save_as {} {
	set file [filename_dialog [pwd] * $w_]
	if {"$file" != ""} {
	    if {[file isfile $file]} {
		if {![confirm_dialog "File: `[file tail $file]' exists \
                                      - Do you want to overwrite it ?" $w_]} {
		    return
		}
		if {[file isdir $file]} {
		    error_dialog "File: `[file tail $file]' is a directory" $w_
		    return
		}
	    }
	    save_to_file $file $info_ $headings_
	}
    }


    # add the rows in the current listing to a local catalog file
    
    public method add_to {} {
	if {[llength $info_] == 0} {
	    error_dialog "There are no rows to save" $w_
	    return;
	}
	add_rows $info_ $headings_
    }

    
    # add the currently selected rows to a local catalog file

    public method add_selected {} {
	set info [get_selected]
	if {[llength $info] == 0} {
	    error_dialog "No rows are selected" $w_
	    return;
	}
	add_rows $info $headings_
    }

    
    # add the given info rows (result of a query) to a local catalog file with 
    # the given headings. The user selects the name of the catalog file.

    public method add_rows {info headings} {
	if {[llength $headings] == 0} {
	    error_dialog "There is no data to save" $w_
	    return;
	}
	set file [filename_dialog [pwd] * $w_]
	if {"$file" != ""} {
	    if {! [file isfile $file]} {
		if {[confirm_dialog "File: `[file tail $file]' does not exists \
                                    - Do you want to create it ?" $w_]} {
		    save_to_file $file $info $headings
		}
	    } else {
		if {[file isdir $file]} {
		    error_dialog "File: `[file tail $file]' is a directory" $w_
		    return
		}
		save_to_file $file $info $headings 1
	    }
	}
    }

    
    # remove the currently selected rows from a local catalog file

    public method remove_selected {} {
	set file [$astrocat longname]
	set info [get_selected]

	if {[llength $info] == 0} {
	    error_dialog "No rows are selected" $w_
	    return;
	}

	if {! [confirm_dialog "Remove selected objects?" $w_]} {
	    return
	}

	if {[catch {$astrocat remove $file $info $equinox $headings_} msg]} {
	    error_dialog $msg $w_
	    return
	}
    }

    
    # save the given info (the result of query) to the given catalog file,
    # using the given column headings.
    # If iflag is 1, insert rows in the existing file.

    public method save_to_file {file info headings {iflag 0}} {
	if {[catch {$astrocat save $file $iflag $info $equinox $headings} msg]} {
	    error_dialog "error saving rows to file: $msg" $w_
	    return 1
	}
	return 0
    }
    

    # Pop up a dialog to enter the data for a new object for a local catalog.
    # The command is evaluated after the users enters the new data.

    public method enter_new_object {{command ""}} {
	catch {delete object $w_.ef}
	EnterObject $w_.ef \
	    -title {Please enter the data for the object below:} \
	    -labels $headings_ \
	    -center 0 \
	    -command [code $this enter_object $command]
    }

    
    # check that the given row contains valid data for a catalog
    # and return 0 if OK

    public method check_row {data} {
	if {[catch {$astrocat checkrow $data} msg]} {
	    error_dialog $msg
	    return 1
	}
	return 0
    }

    
    # This method is called with the data for a new object to add to a local
    # catalog. The row is added to the local catalog file and the
    # command is evaluated. If a row with the given id already exists, it is
    # updated (after confirmation).
    
    public method enter_object {command info} {
	if {[check_row $info]} {
	    return
	}

	# see if this id already exists...
	set id [lindex $info [$astrocat id_col]]
	set row [lindex [$astrocat query -id $id] 0]
	set file [$astrocat longname]
	set append 1
	if {[llength $row]} {
	    if {"$row" == "$info"} {
		info_dialog "No changes were made to $id."
		return
	    }
	    # object with this id already exists
	    if {! [confirm_dialog "Update object $id ?" $w_]} {
		return
	    }
	    # replace with new data
	    if {[save_to_file $file [list $info] $headings_ $append] != 0} {
		return
	    }
	} else {
	    # must be a new object
	    if {! [confirm_dialog "Enter new object with id $id ?" $w_]} {
		return
	    }
	    save_to_file $file [list $info] $headings_ $append
	}

	# eval caller supplied command after change
	eval $command
    }

    
    # pop up a window so that the user can edit the selected object(s).
    # The optional command is evaluated with no args if the object is
    # changed.
    
    public method edit_selected_object {{command ""}} {
	catch {destroy $w_.ef}
	set values [lindex [get_selected] 0]

	if {[llength $values] == 0} {
	    error_dialog "No rows are selected" $w_
	    return;
	}

	EnterObject $w_.ef \
	    -title {Please enter the data for the object below:} \
	    -labels $headings_ \
	    -values $values \
	    -command [code $this enter_object $command]
    }

    
    # update the table sort and column display options from the
    # catalog entry

    public method update_options {} {
	# sort cols
	config \
	    -sort_cols [$astrocat sortcols] \
	    -sort_order [$astrocat sortorder]

	# show/hide cols
	set show_cols [$astrocat showcols]
	if {[llength $show_cols]} {
	    set_options $headings_ Show 0
	    set_options $show_cols Show 1
	    # $order should be a list of all columns (visible or not)
	    # in the order they should be displayed
	    set order $show_cols
	    foreach i $order {
		set a($i) 1
	    }
	    foreach i $headings_ {
		if {! [info exists a($i)]} {
		    lappend order $i
		}
	    }
	    config -order $order
	} 
    }


    # -- options --

    # astrocat (C++ based) object for accessing catalogs
    public variable astrocat

    # equinox of ra and dec columns in query result
    public variable equinox 2000
}
