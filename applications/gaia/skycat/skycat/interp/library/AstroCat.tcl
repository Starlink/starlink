# E.S.O. - VLT project/ESO Archive
# $Id 
#
# AstroCat.tcl - user interface class for viewing catalog info
#
# See man page AstroCat(n) for a complete description.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 14 Dec 95   created


itk::usual AstroCat {}


# create a class for the application

class cat::AstroCat {
    inherit util::TopLevelWidget


    # constructor

    constructor {args} {
	eval itk_initialize $args
    }

    
    # destructor - delete C++ based objects so that the temp image
    # files are deleted

    destructor {
	catch {$w_.cat delete}
	catch {$w_.wcs delete}
	catch {unset instances_($instance_idx_)}
	if {"$current_instance_" == "$w_"} {
	    set current_instance_ {}
	}
    }

    
    # called after options have been evaluated

    method init {} {
	# if running stand-alone, setup X defaults
	if {$itk_option(-standalone)} {
	    util::setXdefaults
	    cat::setXdefaults
	}

	# open the log file in ~/.skycat/
	set_logfile

	# create a pipe to get feedback during http transfers
	lassign [pipe] rfd_ wfd_
	fileevent $rfd_ readable [code $this feedback]
	
	# do window layout
	if {[layout_dialog]} {
	    destroy $w_
	    return
	}

	# enter widget name in instance array for later reference
	set_instance

	# add a short help window
	make_short_help

	# create an object for running interruptable batch queries
	Batch $w_.batch \
	    -command [code $this query_done] \
	    -debug $itk_option(-debug)

	# these are the supported plot symbols
	foreach i "circle square plus cross triangle diamond ellipse" {
	    set symbols_($i) 1
	}
	
	# create object for managing world coords
	wcs $w_.wcs
	
	# position from previous window, if pos.
	if {"$current_instance_" != ""} {
	    wm geometry $w_ +[winfo x $current_instance_]+[winfo y $current_instance_]
	}
	set current_instance_ $w_

	# set default values from image
	set_default_values

	# for local catalogs, start search automatically
	set name $itk_option(-catalog)
	if {"[$w_.cat servtype]" == "local"} {
	    wm title $w_ [file tail [$w_.cat longname $name]]
	    wm iconname $w_ [file tail [$w_.cat shortname $name]]
	    search
	}

	# check for TCS catalog
	if {[$w_.cat is_tcs] || \
		(! $itk_option(-tcs) \
		     && [file exists $name] \
		     && [is_tcs_catalog $name])} {
	    global ::$w_.tcs
	    set $w_.tcs 1
	    set_tcs_columns
	}

	set initialized_ 1

	# source user specific initialization file, if found
	check_plugin
    }

    
    # create the ~/.skycat dir if it does not already exists and keep a log
    # file there.

    method set_logfile {} {
	global ::env

	# open log file used to keep track of URLs (for debugging)
	set dir $env(HOME)/.skycat

	# XXX tmp: also remove old log files from previous version
	catch {exec rm -f ${dir}_logo ${dir}log $dir}

	if {! [file isdirectory $dir]} {
	    catch {mkdir $dir}
	}
	set logfile_name_ $dir/log
    }

    
    # keep an array of instances(name,image) to help locate the 
    # window for a catalog
    
    method set_instance {} {
	set name [$w_.cat longname $itk_option(-catalog)]
	set image $itk_option(-image)
	if {![winfo exists [info namespace tail $image]]} {
	    set image {}
	}
	set instance_idx_ "$name,$image"
	set instances_($instance_idx_) $w_ 
    }


    # add the menu bar

    method add_menubar {} {
	TopLevelWidget::add_menubar
	set m [add_menubutton File]
	set file_menu_ $m
	
	add_menuitem $m command "Open" \
	    {Open a local catalog in tab table format} \
	    -command [code AstroCat::local_catalog \
			  $w_.cat \
			  $itk_option(-image) \
			  $itk_option(-debug)]
	
	$m add separator

	add_menuitem $m command "Save as..." \
	    {Save listed objects to a local catalog file} \
	    -command [code $this save_as]

	add_menuitem $m command "Add to..." \
	    {Add listed objects to a local catalog file} \
	    -command [code $this add_to]

	add_menuitem $m command "Add selected..." \
	    {Add selected rows to a local catalog file} \
	    -command [code $this add_selected]

	$m add separator

	add_menuitem $m command "Print..." \
	    {Print the listing to a printer or file} \
	    -command [code $this print]

	add_menuitem $m command "Clear" \
	    {Clear the catalog listing} \
	    -command [code $this clear]

	$m add separator

	add_menuitem $m command "Close" \
	    {Close this window} \
	    -command [code $this close]
	
	if {$itk_option(-standalone)} {
	    add_menuitem $m command "Exit" \
		{Exit the application} \
		-command [code $this quit]
	}
	
	# Edit menu
	set m [add_menubutton Edit]
	set edit_menu_ $m

	add_menuitem $m command "Remove selected" \
	    {Remove selected rows from the local catalog} \
	    -command [code $this remove_selected] \
	    -state disabled

	add_menuitem $m command "Enter new object..." \
	    {Enter the data for a new object for the local catalog} \
	    -command [code $this enter_new_object] \
	    -state disabled

	add_menuitem $m command "Edit selected object..." \
	    {Edit the data for the selected object in the local catalog} \
	    -command [code $this edit_selected_object] \
	    -state disabled

	# Options menu
	set m [add_menubutton Options]
	set options_menu_ $m
	add_menuitem $m cascade "Set Name Server" \
	    {Select the name server used to resolve astronomical object names} \
	    -menu [set ns_menu [menu $itk_component(options).m.ns]]

	get_name_servers $ns_menu

	add_menuitem $m checkbutton "Use TCS Columns" \
	    {Display the standard columns used by the ESO TCS (Telescope Control Software)} \
	    -command [code $this set_tcs_columns] \
	    -variable $w_.tcs \
	    -onvalue 1 \
	    -offvalue 0

	$m add separator

	add_menuitem $m command "Set Sort Columns..." \
	    {Set options for sorting the query results} \
	    -command [code $this sort_dialog] \
	    -state disabled

	add_menuitem $m command "Hide/Show Columns..." \
	    {Set options for displaying columns of the query results} \
	    -command [code $this select_columns] \
	    -state disabled

	add_menuitem $m command "Set Search Columns..." \
	    {Set the list of columns to search by for this catalog} \
	    -command [code $this set_search_cols] \
	    -state disabled

	add_menuitem $m command "Set Plot Symbols..." \
	    {Set the symbol (color, size, etc.) to use to plot objects} \
	    -command [code $this set_plot_symbols] \
	    -state disabled

	# add a menu of catalogs ("Data-Servers")
	add_catalog_menu $this $w_.cat $itk_option(-image) $itk_option(-debug)
    }
    
    
    # enable or disable some menus
    
    method set_menu_states {} {
	$options_menu_ entryconfig "Set Sort Columns..." -state normal
	$options_menu_ entryconfig "Hide/Show Columns..." -state normal

	if {[$w_.cat iswcs] || [$w_.cat ispix]} {
	    set state normal
	} else {
	    set state disabled
	}
	$options_menu_ entryconfig "Set Plot Symbols..." -state $state
	$options_menu_ entryconfig "Set Name Server" -state $state
	$options_menu_ entryconfig "Use TCS Columns" -state $state

	# determine states for some menu items
	if {"[$w_.cat servtype]" == "local"} {
	    set state [set sstate normal]
	} else {
	    set state [set sstate disabled]
	    # allow user to edit search cols if URL contains "%cond"
	    if {[string first "%cond" [$w_.cat url]] >= 0} {
		set sstate normal
	    } 
	}
	$edit_menu_ entryconfig "Remove selected" -state $state
	$edit_menu_ entryconfig "Enter new object..." -state $state
	$edit_menu_ entryconfig "Edit selected object..." -state $state

	$options_menu_ entryconfig "Set Search Columns..." -state $sstate
    }
    

    # add a menubutton with catalog items to the given TopLevelWidget.
    #
    # w is an instance of TopLevelWidget.
    # astrocat is an astrocat object (C++ impl).
    # The image arg should be the Skycat image, if there is one, and the
    # debug flag is passed from the command line.
    #
    # (note: this is defined as a proc rather than a method so that it 
    # can be accessed from outside the class to add the Data-Servers menu)

    proc add_catalog_menu {w astrocat image debug} {
	set m [$w add_menubutton "Data-Servers"]
	
	# save this info so we can update the menu automatically later
	set catalog_menu_info_($w) \
	    [code AstroCat::add_catalog_menu $w $astrocat $image $debug]

	$w add_short_help [$w component menubar].data-servers \
	    "Data Servers menu: plot or display information from astronomical \
                   catalogs or image servers"

	$w add_menuitem $m cascade "Catalogs" \
	    {Select a catalog from the menu} \
	    -menu [menu $m.catalogs]
	fill_catalog_menu $w $m.catalogs $astrocat $image $debug catalog
	
	$w add_menuitem $m cascade "Image Servers" \
	    {Select an image server from the menu} \
	    -menu [menu $m.imagesvr]
	fill_catalog_menu $w $m.imagesvr $astrocat $image $debug imagesvr

	$w add_menuitem $m cascade "Archives" \
	    {Select an archive from the menu} \
	    -menu [menu $m.archive]
	fill_catalog_menu $w $m.archive $astrocat $image $debug archive
	
	$w add_menuitem $m cascade "Local Catalogs" \
	    {Select a local catalog from the menu} \
	    -menu [menu $m.local]
	fill_catalog_menu $w $m.local $astrocat $image $debug local shortname

	$m.local add separator

	$w add_menuitem $m.local command "Load from file..." \
	    {Open a local catalog file} \
	    -command [code AstroCat::local_catalog \
			  $astrocat $image $debug]

	$m add separator

	$w add_menuitem $m command "Browse Catalog Directories..."  \
	    "Browse the catalog directory hierarchy to view \
             catalogs or add them to the default list" \
	    -command [code AstroCat::catalog_directory $image $debug]

	$w add_menuitem $m command "Reload config file..."  \
	    "Reload the default catalog config file after it was edited by hand" \
	    -command [code AstroCat::reload_config_file $astrocat $w]
    }

    
    # reload the default catalog config file using the given astrocat object

    proc reload_config_file {astrocat w} {
	global ::env
	set config_file $env(HOME)/.skycat/skycat.cfg
	if {[file exists $config_file]} {
	    set env(CATLIB_CONFIG) "file:$config_file"
	}

	if {[catch {$astrocat reload} msg]} {
	    error_dialog $msg $w
	}
	# make sure these windows are updated
	catch {destroy $w_.symconf}
	catch {destroy $w_.searchconf}
	.catinf reinit_tree

	if {[info exists instances_]} {
	    foreach i [array names instances_] {
		$instances_($i) update_search_options
	    }
	}
	update_catalog_menus
    }


    # pop up a window to browse the catalog directories. The image
    # arg should be the Skycat image, if there is one, and the
    # debug flag is passed from the command line.

    proc catalog_directory {image debug} {
	utilReUseWidget cat::CatalogInfo .catinf \
	    -image $image \
	    -debug $debug \
	    -command [code AstroCat::update_catalog_menus]
    }

    
    # update all of the catalog menus in all instances to show 
    # the current catalog info. 

    proc update_catalog_menus {} {
	foreach i [array names catalog_menu_info_] {
	    if {[winfo exists [info namespace tail $i]]} {
		eval $catalog_menu_info_($i)
	    }
	}
    }
    

    # pop up a dialog to set the plot symbols to use for this catalog

    method set_plot_symbols {} {
	set columns $headings_
	if {[llength $columns] == 0} {
	    info_dialog "Please make a query first so that the column names are known" $w_
	    return
	}
	utilReUseWidget cat::SymbolConfig $w_.symconf \
	    -catalog $itk_option(-catalog) \
	    -astrocat [code $w_.cat] \
	    -columns $columns \
	    -command [code $this plot]
    }


    # pop up a dialog to set the search columns to use for this catalog

    method set_search_cols {} {
	set columns $headings_
	if {[llength $columns] == 0} {
	    info_dialog "Please make a query first so that the column names are known" $w_
	    return
	}
	utilReUseWidget cat::SearchConfig $w_.searchconf \
	    -catalog $itk_option(-catalog) \
	    -command [code $this update_search_options] \
	    -astrocat [code $w_.cat] \
	    -columns $columns
    }

    
    # update the search option entries after they have been edited

    method update_search_options {} {
	if {"$search_col_info_" != "[$w_.cat searchcols]"} {
	    add_search_options
	}
    }

    
    # close this window

    method close {} {
	if {$itk_option(-standalone)} {
	    wm iconify  $w_
	} else {
	    wm withdraw $w_
	}
    }

    
    # quit the application (for standalone version)

    method quit {} {
	if {$itk_option(-standalone)} {
	    clear
	    destroy $w_
	    exit
	} else {
	    wm withdraw $w_
	}
    }


    # pop up a dialog to sort the list

    method sort_dialog {} {
	set columns $headings_
	if {[llength $columns] == 0} {
	    info_dialog "Please make a query first so that the column names are known" $w_
	    return
	}
	$table_ sort_dialog
    }

    
    # called when the user has selected columns to sort the results by.
    # The first arg is the sort columns, the second arg is the order
    # (increasing, decreasing)
    
    method set_sort_cols {sort_cols sort_order} {
	global ::$w_.tcs
	if {"[$w_.cat sortcols]" != "$sort_cols" \
		|| "[$w_.cat sortorder]" != "$sort_order"} {
	    $w_.cat sortcols $sort_cols
	    $w_.cat sortorder $sort_order
	    $w_.cat is_tcs $itk_option(-catalog) [set $w_.tcs]
	    CatalogInfo::save {} [code $w_.cat] $w_ 0
	    $table_ config -sort_cols $sort_cols -sort_order $sort_order
	    search
	}
    }


    # pop up a dialog to select table columns to display

    method select_columns {} {
	set columns $headings_
	if {[llength $columns] == 0} {
	    info_dialog "Please make a query first so that the column names are known" $w_
	    return
	}
	$table_ layout_dialog
    }


    # called when the user has selected columns to show
    
    method set_show_cols {cols} {
	global ::$w_.tcs
	set show [$w_.cat showcols]
	if {"$show" == ""} {
	    set show $headings_
	}
	if {"$show" != "$cols"} {
	    $w_.cat showcols $cols
	    $w_.cat is_tcs $itk_option(-catalog) [set $w_.tcs]
	    CatalogInfo::save {} [code $w_.cat] $w_ 0
	}
    }


    # add the name server catalogs to the given menu

    method get_name_servers {m} {
	if {[catch {set list [$w_.cat info namesvr]} msg]} {
	    error_dialog $msg $w_
	    return
	}

	foreach namesvr_ $list {
	    $m add radiobutton \
		-label $namesvr_ \
		-command [code $this set_namesvr $namesvr_] \
		-variable $m
	}

	# set default name server
	global ::$m
	set $m $namesvr_
    }


    # toggle the TCS option (use the tcscat or the astrocat Tcl command)

    method set_tcs_columns {} {
	global ::$w_.tcs
	set is_tcs [set $w_.tcs]
	set itk_option(-tcs) $is_tcs
	catch {$w_.cat delete}

	if {$is_tcs} {
	    tcscat $w_.cat
	} else {
	    astrocat $w_.cat
	}
	$w_.cat feedback $wfd_
	if {[catch {$w_.cat open $itk_option(-catalog)} msg]} {
	    error_dialog $msg $w_
	}
	
	# if changing formats, reset table column info and entry
	# since column names will change making previous info invalid
	if {[$w_.cat is_tcs] != $is_tcs} {
	    $w_.cat is_tcs $itk_option(-catalog) $is_tcs
	    set reset_columns_ 1
	}

	if {[llength $info_]} {
	    search
	}
    }


    # reset table dialogs if needed

    method reset_table {} {
	foreach w "$w_.tblsort $w_.tblcfg" {
	    if {[winfo exists $w]} {
		$w reset
	    }
	}
	$table_ set_options {MORE PREVIEW more preview} Show 0
	$w_.cat showcols {}
	$w_.cat sortcols {}
	CatalogInfo::save {} [code $w_.cat] $w_ 0
    }

    
    # return true if the given file contains a TCS catalog

    proc is_tcs_catalog {filename} {
	if {[catch {set f [open $filename]}]} {
	    return 0
	}
	set title [gets $f]
	::close $f
	if {[string match "Tcs*" $title]} {
	    return 1
	}
	return 0
    }

    
    # clear the table listing
    
    method clear {} {
	catch {$table_ clear}

	# remove any catalog symbols (since table is also empty now)
	catch {$canvas_ delete [$w_.cat shortname $itk_option(-catalog)]}
    }

    
    # pop up a dialog to print the table listing
    
    method print {} {
	$table_ print_dialog
    }

       
    # open a local catalog file (tab table)
    # $astrocat is a astrocat object (C++ impl).
    # $image is the skycat image, if there is one.
    # $debug is a flag set from the command line arg.
    
    proc local_catalog {astrocat image debug} {
	set file [filename_dialog [pwd] *]
	if {"$file" != ""} {
	    if {[file isfile $file]} {
		if {[catch {$astrocat check $file} msg]} {
		    error_dialog $msg
		} else {
		    select_catalog $file local [is_tcs_catalog $file] $image $debug
		}
	    } else {
		error_dialog "There is no file named '$file'"
	    }
	}
    }

    
    # Fill up a menu of known data servers to choose from.
    # 
    # $w is the TopLevelWidget containing the menu $m.
    # $astrocat should be a valid "astrocat" object (C++ impl).
    # $image is the skycat image, if there is one.
    # $debug is a flag set from the command line arg.
    # The serv_type argument should be one of: "catalog", "archive".
    # An optional "cmd" argument may be set to longname or shortname
    # depending on the names you want in the menu.

    proc fill_catalog_menu {w m astrocat image debug serv_type {cmd longname}} {
	if {[catch {set catalog_list [lsort [$astrocat info $serv_type]]} msg]} {
	    error_dialog $msg
	    return
	}
	
	if {[llength $catalog_list]} {
	    foreach i $catalog_list {
		set name [$astrocat $cmd $i]
		set is_tcs [$astrocat is_tcs $i]
		$w add_menuitem $m command $name \
		    "Open $serv_type: \"$i\"" \
		    -command [code AstroCat::select_catalog $i $serv_type $is_tcs \
				  $image $debug]
	    }
	}
    }


    # save the current data to a local catalog
    
    method save_as {} {
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
    
    method add_to {} {
	if {[llength $info_] == 0} {
	    error_dialog "There are no rows to save" $w_
	    return;
	}
	add_rows $info_ $headings_
    }

    
    # add the currently selected rows to a local catalog file

    method add_selected {} {
	set info [$table_ get_selected]
	if {[llength $info] == 0} {
	    error_dialog "No rows are selected" $w_
	    return;
	}
	add_rows $info $headings_
    }

    
    # add the given info rows (result of a query) to a local catalog file with 
    # the given headings. The user selects the name of the catalog file.

    method add_rows {info headings} {
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

    method remove_selected {} {
	set info [$table_ get_selected]
	set file $itk_option(-catalog)

	if {[llength $info] == 0} {
	    error_dialog "No rows are selected" $w_
	    return;
	}

	if {! [confirm_dialog "Remove object with id [lindex [lindex $info 0] 0]" $w_]} {
	    return
	}

	if {[$w_.cat iswcs]} {
	    set equinox [$equinox_ get]
	} else {
	    set equinox 2000
	}
	if {[catch {$w_.cat remove $file $info $equinox $headings_} msg]} {
	    error_dialog $msg $w_
	    return
	}
	
	# update the display
	search
    }

    
    # save the given info (the result of query) to the given catalog file,
    # using the given column headings.
    # If iflag is 1, insert rows in the existing file.

    method save_to_file {file info headings {iflag 0}} {
	if {[$w_.cat iswcs]} {
	    set equinox [$equinox_ get]
	} else {
	    set equinox 2000
	}
	if {[catch {$w_.cat save $file $iflag $info $equinox $headings} msg]} {
	    error_dialog "error saving rows to file: $msg" $w_
	    return 1
	}
	return 0
    }
    

    # pop up a dialog to enter the data for a new object for a local catalog

    method enter_new_object {} {
	catch {destroy $w_.ef}
	EnterObject $w_.ef \
	    -title {Please enter the data for a new object below:} \
	    -image $itk_option(-image) \
	    -labels $headings_ \
	    -command [code $this enter_object]
    }

    
    # check that the given row contains valid data for a catalog
    # and return 0 if OK

    method check_row {data} {
	if {[catch {$w_.cat checkrow $data} msg]} {
	    error_dialog $msg
	    return 1
	}
	return 0
    }

    
    # this method is called with the data for a new object to add to a local
    # catalog
    
    method enter_object {info} {
	if {[check_row $info]} {
	    return
	}
	set id [lindex $info 0]
	
	if {! [confirm_dialog "Enter new object with id $id ?" $w_]} {
	    return
	}

	set file $itk_option(-catalog)
	save_to_file $file [list $info] $headings_ 1
    }

    
    # pop up a window so that the user can edit the selected object(s)
    
    method edit_selected_object {} {
	catch {destroy $w_.ef}
	set values [lindex [$table_ get_selected] 0]

	if {[llength $values] == 0} {
	    error_dialog "No rows are selected" $w_
	    return;
	}

	EnterObject $w_.ef \
	    -title {Please edit the data for the object below:} \
	    -image $itk_option(-image) \
	    -labels $headings_ \
	    -values $values \
	    -command [code $this replace_object $values]
    }

    
    # this method is called with the new data to replace the data
    # for the selected object/row in the local catalog
    
    method replace_object {old_data new_data} {
	if {[check_row $new_data]} {
	    return
	}
	
	if {"$old_data" == "$new_data"} {
	    info_dialog "No changes were made" $w_
	    return
	}

	set file $itk_option(-catalog)
	set id [lindex $new_data 0]

	if {! [confirm_dialog "Update object with id $id ?" $w_]} {
	    return
	}

	# remove old data
	if {[$w_.cat iswcs]} {
	    set equinox [$equinox_ get]
	} else {
	    set equinox 2000
	}
	if {[catch {$w_.cat remove $file [list $old_data] $equinox $headings_} msg]} {
	    error_dialog $msg $w_
	    return
	}

	# replace with new data
	if {[save_to_file $file [list $new_data] $headings_ 1] != 0} {
	    return
	}

	# change command to replace new object if changed again
	$w_.ef config -command [code $this replace_object $new_data]

	# update the display
	search
    }


    # open the catalog for this window

    method open_catalog {} {
	# create astrocat object (or tcscat if TCS catalog) and setup feedback
	global ::$w_.tcs
	if {[set $w_.tcs $itk_option(-tcs)]} {
	    tcscat $w_.cat
	} else {
	    astrocat $w_.cat
	}
	$w_.cat feedback $wfd_

	# normally -catalog should be specified when creating this widget
	# if not, choose a default...
	if {"$itk_option(-catalog)" == ""} {
	    if {[catch {set catalog_list [$w_.cat info $itk_option(-catalogtype)]} msg]} {
		error_dialog $msg $w_
		return
	    }
	    set itk_option(-catalog) [lindex $catalog_list [expr [llength $catalog_list]-1]]
	}

	# open the catalog
	set name $itk_option(-catalog)
	if {[catch {$w_.cat open $name} msg]} {
	    error_dialog $msg $w_
	    return
	}
	
	# if this is a local catalog, add it to the catalog menus
	if {"[$w_.cat servtype]" == "local"} {
	    update_catalog_menus
	    # add to catalog tree, if there is one
	    catch {.catinf insert_node $name}
	}
	
	# display catalog name in header and icon
	wm title $w_ [$w_.cat longname $name]
	wm iconname $w_ [$w_.cat shortname $name]
    }

    
    # set the table sort and column display options from the
    # catalog entry

    method set_table_options {} {
	# sort cols
	$table_ config \
	    -sort_cols [$w_.cat sortcols] \
	    -sort_order [$w_.cat sortorder]

	# show/hide cols
	set show_cols [$w_.cat showcols]
	if {[llength $show_cols]} {
	    $table_ set_options $headings_ Show 0
	    $table_ set_options $show_cols Show 1
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
	    $table_ config -order $order
	} 
    }

    
    # add (or update) the search options panel

    method add_search_options {} {
	if {! [winfo exists $w_.options]} {
	    pack [frame $w_.options -relief groove -borderwidth 2] \
		-side top -fill x

	    pack \
		[label $w_.options.head -text "Search Options"] \
		-side top -pady 2
	}

	if {[winfo exists $w_.options.f]} {
	    destroy $w_.options.f
	}

	pack [set f [frame $w_.options.f]] \
	    -side top -fill x
	
	blt::table $f
	set row 0

	if {[$w_.cat iswcs]} {
	    blt::table $f \
		[set name_ [LabelEntry $f.name \
				-text "Object Name:" \
				-command [code $this search] \
				-labelwidth $itk_option(-labelwidth) \
				-anchor $itk_option(-anchor) \
				-valuefont $itk_option(-valuefont) \
				-labelfont $itk_option(-labelfont)]] \
		$row,0 -fill x -pady 1m

	    # if we are using world coords, display ra, dec, equinox
	    blt::table $f \
		[set equinox_ \
		     [LabelEntry $f.equinox \
			  -text "Equinox:" \
			  -value "J2000" \
			  -autoselect 1 \
			  -command [code $this search] \
			  -labelwidth $itk_option(-labelwidth) \
			  -anchor $itk_option(-anchor) \
			  -valuefont $itk_option(-valuefont) \
			  -labelfont $itk_option(-labelfont)]] \
		$row,1 -fill x -pady 1m \
		[set ra_ \
		     [LabelEntry $f.ra \
			  -text "a:" \
			  -autoselect 1 \
			  -command [code $this search] \
			  -labelwidth $itk_option(-labelwidth) \
			  -anchor $itk_option(-anchor) \
			  -valuefont $itk_option(-valuefont) \
			  -labelfont $itk_option(-wcsfont)]] \
		[incr row],0 -fill x -pady 1m \
		[set dec_ \
		     [LabelEntry $f.dec \
			  -text "d:" \
			  -autoselect 1 \
			  -command [code $this search] \
			  -labelwidth $itk_option(-labelwidth) \
			  -anchor $itk_option(-anchor) \
			  -valuefont $itk_option(-valuefont) \
			  -labelfont $itk_option(-wcsfont)]] \
		$row,1 -fill x -pady 1m
	} elseif {[$w_.cat ispix]} {
	    # if we are using image coords, display x and y
	    blt::table $f \
		[set x_ \
		     [LabelEntry $f.x \
			  -text "X:" \
			  -autoselect 1 \
			  -command [code $this search] \
			  -labelwidth $itk_option(-labelwidth) \
			  -anchor $itk_option(-anchor) \
			  -valuefont $itk_option(-valuefont) \
			  -labelfont $itk_option(-labelfont)]] \
		[incr row],0 -fill x -pady 1m \
		[set y_ \
		     [LabelEntry $f.y \
			  -text "Y:" \
			  -autoselect 1 \
			  -command [code $this search] \
			  -labelwidth $itk_option(-labelwidth) \
			  -anchor $itk_option(-anchor) \
			  -valuefont $itk_option(-valuefont) \
			  -labelfont $itk_option(-labelfont)]] \
		$row,1 -fill x -pady 1m
	}

	if {[$w_.cat iswcs] || [$w_.cat ispix]} {
	    # radius from center position
	    blt::table $f \
		[set rad1_ [LabelEntry $f.rad1 \
				-text "Min Radius:" \
				-command [code $this search] \
				-labelwidth $itk_option(-labelwidth) \
				-anchor $itk_option(-anchor) \
				-valuefont $itk_option(-valuefont) \
				-labelfont $itk_option(-labelfont)]] \
		[incr row],0 -fill x -pady 1m \
		[set rad2_ [LabelEntry $f.rad2 \
				-text "Max Radius:" \
				-command [code $this search] \
				-labelwidth $itk_option(-labelwidth) \
				-anchor $itk_option(-anchor) \
				-valuefont $itk_option(-valuefont) \
				-labelfont $itk_option(-labelfont)]] \
		$row,1 -fill x -pady 1m
	}

	# display search columns from config entry
	set n 0
	set search_col_info_ [$w_.cat searchcols]
	foreach col_info $search_col_info_ {
	    if {[llength $col_info] != 3} {
		error_dialog \
		    "invalid search_cols entry in config file: '$col_info' ([llength $col_info])" $w_
		continue
	    }
	    lassign $col_info col minStr maxStr
	    lappend search_cols_ $col

	    blt::table $f \
		[set min_values_($col) [LabelEntry $f.min$n \
				-text "$minStr:" \
				-command [code $this search] \
				-labelwidth $itk_option(-labelwidth) \
				-anchor $itk_option(-anchor) \
				-valuefont $itk_option(-valuefont) \
				-labelfont $itk_option(-labelfont)]] \
		[incr row],0 -fill x -pady 1m \
		[set max_values_($col) [LabelEntry $f.max$n \
				-text "$maxStr:" \
				-command [code $this search] \
				-labelwidth $itk_option(-labelwidth) \
				-anchor $itk_option(-anchor) \
				-valuefont $itk_option(-valuefont) \
				-labelfont $itk_option(-labelfont)]] \
		$row,1 -fill x -pady 1m
	    incr n
	}

	blt::table $f \
	    [set maxnum_ [LabelEntry $f.maxnum \
		 -text "Max Objects:" \
		 -command [code $this search] \
		 -labelwidth $itk_option(-labelwidth) \
		 -anchor $itk_option(-anchor) \
		 -valuefont $itk_option(-valuefont) \
		 -labelfont $itk_option(-labelfont)]] \
	    [incr row],0 -fill x -pady 1m

	# if catalog does not use coordinates, disabled some buttons
	if {"$im_state_" == "normal" && ([$w_.cat iswcs] || [$w_.cat ispix])} {
	    set state normal
	} else {
	    set state disabled
	}

	frame $f.buttons
	pack \
	    [set setfromimg_ \
		 [button $f.buttons.setfromimg \
		      -text "Set From Image" \
		      -state $state \
		      -command [code $this set_from_image]]] \
	    [set selectarea_ \
		 [button $f.buttons.selectarea \
		      -text "Select Area..." \
		      -state $state \
		      -command [code $this select_area]]] \
	    -side left -padx 2m -pady 2m
	blt::table $f $f.buttons \
	    $row,1 -fill x -pady 1m

	if {"[$w_.cat copyright]" != ""} {
	    blt::table $f \
		[LabelValue $f.copyright \
		     -anchor w -relief flat \
		     -labelwidth 0 -valuewidth 0 \
		     -value [$w_.cat copyright] \
		     -valuefont $itk_option(-labelfont)] \
		[incr row],0 -anchor e -fill x -pady 1m -columnspan 2
	}

	blt::table configure $f C1 -padx 2m
	update idletasks
    }


    # add the table for displaying the query results

    method add_table {} {
	# table to display results as text
	itk_component add table {
	    set table_ [TableList $w_.table \
			    -title "Search Results" \
			    -hscroll 1 \
			    -height 12 \
			    -sortcommand [code $this set_sort_cols] \
			    -layoutcommand [code $this set_show_cols] \
			    -selectmode extended \
			    -exportselection 0]
	} {
	}
	pack $itk_component(table) -side top -fill both -expand 1

	bind $table_.listbox <ButtonRelease-1> [code $this select_table_row]
	bind $table_.listbox <Double-ButtonPress-1> [code $this label_selected_object]
	$table_ set_options {MORE PREVIEW more preview} Show 0
    }

    # add the dialog button frame

    method add_dialog_buttons {} {
	# dialog buttons
	pack [frame $w_.buttons -borderwidth 2 -relief raised] \
	    -side top -fill x
	pack \
	    [button $w_.search \
		 -text "Search" \
		 -command [code $this search]] \
	    [button $w_.plot \
		 -text "Plot" \
		 -state $im_state_ \
		 -command [code $this plot_again]] \
	    [button $w_.more \
		 -text "More Info" \
		 -state disabled \
		 -command [code $this more]] \
	    [button $w_.preview \
		 -text "Preview" \
		 -state $im_state_ \
		 -state disabled \
		 -command [code $this preview]] \
	    [button $w_.stop \
		 -text "Stop" \
		 -state disabled \
		 -command [code $this interrupt]] \
	    [button $w_.close \
		 -text "Close" \
		 -command [code $this close]] \
	    -side left -expand 1 -padx 8m -pady 2m -in $w_.buttons
    }


    # add a progress bar to display the progress of data transfers

    method add_progress_bar {} {
	# add a progress bar at the botton
	pack [ProgressBar $w_.progress] \
	    -side top -fill x
    }
    

    # do the dialog window layout
    
    method layout_dialog {} {
	open_catalog
	add_menubar
	add_search_options
	add_table
	add_dialog_buttons
	add_progress_bar

	return 0
    }


    # add a short help window and set the help texts
    
    method make_short_help {} {
	TopLevelWidget::make_short_help

	if {[$w_.cat iswcs]} {
	    add_short_help $name_ {object name: resolved via name server, if given}
	    add_short_help $equinox_ {World Coordinates equinox, default J2000}
	    add_short_help $ra_ {World Coordinates: right ascension (RA)}
	    add_short_help $dec_ {World Coordinates: declination (DEC)}
	    add_short_help $rad1_ {Radius: minimum radius in arcmin for circular search}
	    add_short_help $rad2_ {Radius: maximum radius in arcmin for circular search}
	} elseif {[$w_.cat ispix]} {
	    add_short_help $x_ {Image coordinates: X value}
	    add_short_help $y_ {Image coordinates: Y value}
	    add_short_help $rad1_ {Radius: minimum radius in pixels for circular search}
	    add_short_help $rad2_ {Radius: maximum radius in pixels for circular search}
	}

	add_short_help $maxnum_ {Max number of stars/objects to display}
	add_short_help $setfromimg_ {Set default values from the current image}
	add_short_help $selectarea_ {Select a circular area of the image for searching}

	foreach col $search_cols_ {
	    add_short_help $min_values_($col) \
		"If set, search for rows where $col is greater than this value"
	    add_short_help $max_values_($col) \
		"If set, search for rows where $col is less than this value"
	}

	add_short_help $table_ {Table: results of query: {bitmap b1} (double click) = label object}
	add_short_help $w_.search {{bitmap b1} = start catalog search}
	add_short_help $w_.plot {{bitmap b1} = Plot the listed objects again in the image}
	add_short_help $w_.preview {{bitmap b1} = view the preview image or plot data for the selected object}
	add_short_help $w_.more {{bitmap b1} = display more information for the selected object}
	add_short_help $w_.stop {{bitmap b1} = interrupt the current catalog operation}
	#add_short_help $w_.close {{bitmap b1} = close this window}

	add_short_help $w_.progress {Progress bar: displays status of work in progress}
    }

       
    # called when a catalog ($name) is selected from the menu.
    # serv_type is one of: catalog, archive, local, imagesvr,...
    # tcs_flag can be set to true, if you know it is a TCS catalog.
    # $image is the skycat image, if there is one.
    # $debug is a flag set from the command line arg.

    proc select_catalog {name serv_type tcs_flag image debug} {
	if {"$serv_type" == "imagesvr"} {
	    if {"$image" == ""} {
		error_dialog "Can't access $name without an image"
		return
	    }
	     AstroImage::new_imagesvr $name $image $debug
	} else {
	    new_catalog $name $image $debug $tcs_flag
	}
    }

    
    # set the name server to use to resolve object names
    
    method set_namesvr {name} {
	set namesvr_ $name
    }


    # interrupt the current search 

    method interrupt {} {
	$w_.batch interrupt
	catch {file delete $itk_option(-tmpfile)}
	$table_ config -title "Search Results"
	set_state normal
    }

    
    # set/reset widget states while busy 

    method set_state {state} {
	set state_ $state
	if {"$state" == "normal"} {
	    catch {blt::busy release $w_.options}
	    catch {focus -lastfor $w_.options}
	    $w_.search config -state normal
	    $w_.plot config -state normal
	    $w_.stop config -state disabled
	} else {
	    catch {focus .}
	    catch {blt::busy hold $w_.options}
	    $w_.search config -state disabled
	    $w_.plot config -state disabled
	    $w_.more config -state disabled
	    $w_.preview config -state disabled
	    $w_.stop config -state normal
	}
	update idletasks
	$w_.progress reset
    }

    
    # this method is called by the fileevent handler during the data transfer 
    # from the HTTP C++ class to give the user feedback about how much there is 
    # left to copy, etc...
    # Read a line from the feedback pipe, which should contain text to be displayed
    # in the progress widget.
    #
    # When we are in busy (disabled, waiting) mode, we also interpret some special
    # messages, such as: "total length: n bytes" to update the progress bar.

    method feedback {} {
	set text [gets $rfd_]
	if {"$state_" != "normal"} {
	    if {[scan $text {total length: %d bytes} n] == 1} {
		$w_.progress config -to $n
	    } elseif {[scan $text {read %d bytes} n] == 1} {
		$w_.progress config -value $n
	    } elseif {[scan $text {url: %s} url] == 1} {
	    	catch {
		    set fd [open $logfile_name_ a+]
		    puts $fd "[fmtclock [getclock]]\n$itk_option(-catalog)\n$url\n\n"
		    ::close $logfile_name_
		}
	    }
	}
	$w_.progress config -text $text
	update idletasks
    }


    # This method is called when the background query is done.
    # The arguments are the error status (0 is OK) and the result
    # of evaluating the Tcl commands. 
    #
    # The result arg contains a list of items:
    #   {info headings entry pos more}
    # Where:
    #   info is a list of rows (result of query)
    #   headings are the column headings 
    #   entry is the catalog config entry (including info from the header 
    #         in the query result), 
    #   pos is the query center position 
    #   more is a flag "more" indicating if there were more rows available.

    method query_done {status result} {
	if {$status} {
	    set_state normal
	    $table_ config -title "Search Results"
    	    # error messages starting with "***" are only displayed in progress win
	    if {"[string range $result 0 2]" != "***"} {
		error_dialog $result $w_
	    }
	    after 0 [list $w_.progress config -text $result]
	} else {
	    busy {
		set prev_headings $headings_
		lassign $result info_ headings_ entry pos more
		
		# update the catalog config entry with the entry data from the subprocess
		# (may have been modified by info in the query result header)
		$w_.cat entry update $entry

		# update table
		$table_ config -headings $headings_
		if {$reset_columns_} {
		    set reset_columns_ 0
		    reset_table
		}

		if {"$prev_headings" != "$headings_"} {
		    set_table_options
		    set_menu_states
		}

		$table_ config -info $info_

		if {$more} {
		    set more "+"
		} else {
		    set more ""
		}
		$table_ config -title "Search Results ([$table_ total_rows]$more)"

		if {[llength $pos] >= 2} {
		    if {[$w_.cat iswcs]} {
			$name_ config -value ""
			lassign $pos ra dec equinox
			$ra_ config -value $ra
			$dec_ config -value $dec
		    } elseif {[$w_.cat ispix]} {
			lassign $pos x y
			$x_ config -value $x
			$y_ config -value $y
		    }
		}

		# plot stars
		plot
	    }

	    set_state normal
	}
    }

    
    # start the catalog search based on the current search options
    # and display the results in the table.

    method search {args} {
	set cmd "$w_.cat query"

	if {[$w_.cat iswcs] || [$w_.cat ispix]} {
	    set equinox ""
	    if {[$w_.cat iswcs]} {
		set name [$name_ get]
		set x [$ra_ get]
		set y [$dec_ get]
		set equinox [$equinox_ get]
	    } elseif {[$w_.cat ispix]} {
		set name ""
		set x [$x_ get]
		set y [$y_ get]
	    }

	    set rad1 [$rad1_ get]
	    set rad2 [$rad2_ get]

	    if {"$equinox" != ""} {
		lappend cmd "-equinox" $equinox
	    }
	    if {"$name" != ""} {
		lappend cmd "-nameserver" $namesvr_ "-name" $name
	    } elseif {"$x" != "" && "$y" != ""} {
		lappend cmd "-pos" [list $x $y]
	    } else {
		#warning_dialog "Please specify either an object name or a position in WCS" $w_
	    }

	    if {"$rad1" != "" || "$rad2" != ""} {
		lappend cmd "-radius" "$rad1 $rad2"
	    }
	}

	set maxnum [$maxnum_ get]
	if {"$maxnum" != ""} {
	    lappend cmd "-nrows" $maxnum
	}
	
	if {"[set sort_cols [$w_.cat sortcols]]" != ""} {
	    lappend cmd "-sort" $sort_cols "-sortorder" [$w_.cat sortorder] 
	}

	# add optional search columns
	if {"$search_cols_" != ""} {
	    set minvalues {}
	    set maxvalues {}
	    set search_cols {}
	    foreach col $search_cols_ {
		set min [$min_values_($col) get]
		set max [$max_values_($col) get]
		if {"$min" == "" && "$max" != "" || "$max" == "" && "$min" != ""} {
		    error_dialog "Please specify min and max values for $col"
		    return
		}
		if {"$min" == ""} {
		    continue
		}
		lappend search_cols $col
		lappend minvalues $min
		lappend maxvalues $max
	    }
	    if {[llength $search_cols]} {
		lappend cmd -searchcols $search_cols -minvalues $minvalues -maxvalues $maxvalues
	    }
	}
	
	# start the query in the background
	$table_ config -title "Searching..."
	clear

	set_state disabled
	$w_.progress config -text "Attempting to contact catalog server..."
	$w_.progress look_busy

	$w_.batch bg_eval [code $this do_query $cmd]
    }

    
    # return the result of a catalog query. Since this may be run in a separate
    # process via fork, all of the necessary info is returned in the format:
    #
    # {results headings entry querypos more}
    #
    # where results are the query results (list of rows)
    #       headings are the result headings
    #       entry is the catalog config entry
    #       querypos is the center pos of the query
    #       more is a flag, 1 if more rows available

    method do_query {cmd} {
	return [list [eval $cmd] \
		    [$w_.cat headings] \
		    [$w_.cat entry get] \
		    [$w_.cat querypos] \
		    [$w_.cat more]]
    }
    
    
    # Set the default values for the form entries:
    # set the default position (RA, DEC) to the image center, 
    # set the min radius to 0 and the max radius to the distance 
    # from the center to the origin.

    method set_default_values {} {
	set_from_image
	$maxnum_ config -value 1000
    }


    # set the default values for position and radius from the image

    method set_from_image {} {
	if {"$image_" == ""} {
	    return
	}

	# set the center coords and default radius, if image is loaded
	if {[$w_.cat iswcs]} {
	    $name_ config -value ""
	    $ra_ config -value ""
	    $dec_ config -value ""
	    $equinox_ config -value 2000
	    $rad1_ config -value 0.0
	    $rad2_ config -value 10.0
	} elseif {[$w_.cat ispix]}  {
	    $x_ config -value ""
	    $y_ config -value ""
	    $rad1_ config -value 0.0
	    $rad2_ config -value 100.0
	}

	if {[$image_ isclear]} {
	    return
	}
	
	if {[$w_.cat iswcs]} {
	    # using world coords 
	    set center  [$image_ wcscenter]
	    if {[llength $center] >= 2} {
		lassign $center ra dec equinox
		$ra_ config -value $ra
		$dec_ config -value $dec
		$equinox_ config -value $equinox
		set r [format "%.2f" [$image_ wcsradius]]
		if {$r} {
		    $rad2_ config -value $r
		}
	    }
	} elseif {[$w_.cat ispix]}  {
	    # using image coords
	    set w [$image_ width]
	    set h [$image_ height]
	    set x [format "%.2f" [expr $w/2.]]
	    set y [format "%.2f" [expr $h/2.]]
	    set r [format "%.2f" [expr sqrt($x*$x+$y*$y)/2.]]
	    $x_ config -value $x
	    $y_ config -value $y
	    $rad2_ config -value $r
	}
    }


    # Ask the user to select an area of the image by dragging out a circle
    # and insert the resulting radius and center pos in the catalog window.

    method select_area {} {
	if {"$image_" == ""} {
	    return
	}
	
	if {[$image_ isclear]} {
	    error_dialog "No image is currently loaded"
	    return
	}

	# get canvas coords of selected area
	set list [$itk_option(-image) select_area]
	if {[llength $list] != 4} {
	    return
	}
	lassign $list x0 y0 x1 y1
	
	# get center and radius in canvas coords
	set x [expr ($x0+$x1)/2.]
	set y [expr ($y0+$y1)/2.]

	if {[$w_.cat iswcs]} {
	    # using world coords 
	    set equinox [$equinox_ get]
	    if {[catch {
		lassign [$image_ convert coords $x $y canvas {} {} "wcs $equinox"] ra dec
	    } msg]} {
		error_dialog "error converting canvas ($x, $y) to world coordinates: $msg" $w_
		return
	    }
	    $ra_ config -value $ra
	    $dec_ config -value $dec

	    set r [expr [$image_ wcsdist $x0 $y0 $x $y]/60.]
	    $rad1_ config -value 0.0
	    $rad2_ config -value [format "%.2f" $r]
	} elseif {[$w_.cat ispix]}  {
	    # using image coords
	    if {[catch {
		lassign [$image_ convert coords $x $y canvas {} {} image] xi yi
	    } msg]} {
		error_dialog "error converting canvas ($x, $y) to world coordinates: $msg" $w_
		return
	    }
	    $x_ config -value $xi
	    $y_ config -value $yi

	    if {[catch {
		lassign [$image_ convert coords $x0 $y0 canvas {} {} image] xi0 yi0
	    } msg]} {
		error_dialog "error converting canvas ($x0, $y0) to world coordinates: $msg" $w_
		return
	    }
	    if {[catch {
		lassign [$image_ convert coords $x1 $y1 canvas {} {} image] xi1 yi1
	    } msg]} {
		error_dialog "error converting canvas ($x1, $y1) to world coordinates: $msg" $w_
		return
	    }
	    set w [expr abs($xi1-$xi0)]
	    set h [expr abs($yi1-$yi0)]
	    set r [expr sqrt($w*$w+$h*$h)/2.]
	    $rad1_ config -value 0.0
	    $rad2_ config -value $r
	}
    }

    
    # insert the Id for the object selected in the Table in the canvas
    # near the object

    method label_selected_object {} {
	if {"$canvas_" == ""} {
	    return
	}

	set id [lindex [lindex [$table_ get_selected] 0] [$w_.cat id_col]]
	if {"$id" == ""} {
	    return
	}
	if {[llength [set box [$canvas_ bbox cat$id]]]} {
	    lassign $box x0 y0 x1 y1
	    make_label $id [expr ($x1+$x0)/2.0] [expr ($y1+$y0)/2.0] $id white
	    $w_.progress config -text "labeled object '$id' in image"
	} else {
	    $w_.progress config -text "object '$id' is not visible"
	}
    }

    
    # add a label to the canvas at the given canvas coords pos with the given text
    # and color. The id arg should be a unique id for the label.
    
    method make_label {id x y text color} {
	set tags [list objects label$id [$w_.cat shortname $itk_option(-catalog)]]
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


    # this method is called whenever a table row is selected
    # note the selected values and enable/disable some buttons

    method select_table_row {} {
	if {! [info exists col_(PREVIEW)] && ! [info exists col_(MORE)]} {
	    return
	}
	set row [lindex [$table_ get_selected] 0]
	if {[llength $row] == 0} {
	    return
	}
	set preview [get_col PREVIEW $row]
	set more [get_col MORE $row]
	set object_name_ [lindex $row 0]

	# note: HST returns P=http:....
	if {! [regsub {.*(http:.*)} $preview {\1} preview_url_]} {
	    set preview_url_ ""
	    $w_.preview config -state disabled
	} else {
	    $w_.preview config -state normal
	}

	# note: HST returns M=http:....
	if {! [regsub {.*(http:.*)} $more {\1} more_url_]} {
	    set more_url_ ""
	    $w_.more config -state disabled
	} else {
	    $w_.more config -state normal
	}   
    }


    # assuming there is a field called "preview" that is the URL of an
    # image, get the image or table data for the selected line over HTTP 
    # and display it
    # 
    # The preview data may be an image or other data, such as a tab table to
    # display as a graph (like the "cuts" (RtdImageSpectrum) graph).

    method preview {} {
	if {"$image_" == ""} {
	    return
	}

	if {[$table_ num_selected] == 0} {
	    warning_dialog "Please select an object from the list first" $w_
	    return
	}
	if {! [info exists col_(PREVIEW)]} {
	    warning_dialog "Previewing is not supported for this catalog" $w_
	    return
	}
	if {"$preview_url_" == ""} {
	    warning_dialog "No preview for this object" $w_
	    return
	}
	
	# XXX temp
	#if {"[exec uname -n]" == "piano"} {
	#    set preview_url_ "http://piano:8080/cgi-bin/getpreview"
	#}
	
	# do it in the background, since it may take a while
	set_state disabled
	$w_.progress config -text "Attempting to contact server..."
	$w_.progress look_busy

	$w_.batch config -command [code $this preview_done]
	$w_.batch bg_eval [code \
	    "$w_.cat open {$itk_option(-catalog)}; \
             [list $w_.cat getpreview -url $preview_url_ -tmpfile $itk_option(-tmpfile)]"]
    }
    

    # This method is called when we have received the preview data
    # The "status" argument is the status of the background http get operation (0 if ok).
    # The result is a list of {filename Content-type}, where filename contains the data
    # and Content-type indicates the type of the preview data. Decompression is already
    # taken care of, so we only expect to get a FITS image here or a tab table to plot.

    method preview_done {status result} {
	if {$status} {
	    error_dialog $result $w_
	} else {
	    if {[llength $result] != 2} {
		error_dialog "error getting preview data (result = $result)"
	    } else {
		lassign $result filename type
		if {"$type" == "image/x-fits"} {
		    # load the image and remove the temp file
		    $itk_option(-image) config -file $filename
		    catch {file delete $filename}
		} elseif {"$type" == "text/tab-separated-values" \
			      || "$type" == "text/x-starbase" \
			      || "$type" == "text/plain"} {
		    PreviewPlot $w_.pplot[incr count_] \
			-file $filename \
			-name $object_name_ \
			-shorthelpwin $this \
			-transient 1
		    catch {file delete $filename}
		} else {
		    error_dialog "unrecognized Content-type for preview data: $type"
		    catch {file delete $filename}
		}
	    }
	}
 	set_state normal
	
	# restore preview button state...
	select_table_row
    }

    
    # use the "more" URL to display more info about an object using netscape
    # or mosaic

    method more {} {
	if {[$table_ num_selected] == 0} {
	    warning_dialog "Please select an object from the list first" $w_
	    return
	}
	if {"$more_url_" == ""} {
	    warning_dialog "No more information is available for this object" $w_
	    return
	}
	# info_dialog "more url = $more_url_" $this
	
	# start netscape
	$w_.progress config -text "sending URL to netscape..."
	update idletasks
	$itk_option(-image) send_to_netscape $more_url_
    }


    # generate a dummy blank image for the purpose of plotting catalog
    # objects on it.  Return 0 if OK, otherwise 1.

    method gen_blank_image {} {
	if {[$w_.cat iswcs]} {
	    # using world coords
	    set ra [$ra_ get]
	    set dec [$dec_ get]
	    if {"$ra" == "" || "$dec" == ""} {
		# can't create a blank image with no center
		# use coords from first row, if any
		set row [lindex $info_ 0]
		set ra [lindex $row [$w_.cat ra_col]]
		set dec [lindex $row [$w_.cat dec_col]]
		if {"$ra" == "" || "$dec" == ""} {
		    return 1
		}
		$ra_ config -value $ra
		$dec_ config -value $dec
	    }
	    if {[catch {lassign [$w_.wcs hmstod $ra $dec] ra_deg dec_deg} msg]} {
		warning_dialog "$msg (ra = $ra, dec = $dec)"
		return 1
	    }

	    # generate dummy image
	    $image_ clear \
		-reuse 1 \
		-ra $ra_deg \
		-dec $dec_deg \
		-radius [$rad2_ get] \
		-width [get_display_width] \
		-height [get_display_height]
	} else {
	    # generate dummy image 
	    $image_ clear \
		-reuse 1 \
		-radius [$rad2_ get] \
		-width [get_display_width] \
		-height [get_display_height]
	}
	
	return 0
    }


    # retun the width of the image display canvas
    
    method get_display_width {} {
	return [winfo width $canvas_]
    }

    
    # retun the height of the image display canvas
    
    method get_display_height {} {
	return [winfo height $canvas_]
    }


    # plot the listed objects again (they may have been removed after an
    # image was loaded)
    
    method plot_again {} {
	busy {plot}
    }

        
    # plot the stars/objects found in the previous search in the image window.
    # The symbolsto use is taken from the config file.

    method plot {} {
	# can't plot with no coordinates
	if {![$w_.cat iswcs] && ![$w_.cat ispix]} {
	    return
	}

	# can't plot without symbol info
	if {"[$w_.cat symbol]" == ""} {
	    return
	}

	# can't plot with no image display either...
	if {"$image_" == ""} {
	    return
	}
	
	# if we have a display, but no image is loaded, generate dummy image
	if {"[$image_ cget -file]" == "" && "[$image_ object]" == ""} {
	    if {[gen_blank_image] != 0} {
		return
	    }

	    # enable the options panel, since we now have an image
	    catch {[[$itk_option(-image)] component info] configure -state normal}
	}

	# if any objects are selected, deselect them first
	$canvas_ delete grip
	update idletasks

	# note column indexes in array (use upper case to simplify search)
	catch {unset col_}
	set n -1
	foreach i $headings_ {
	    set col_([string toupper $i]) [incr n]
	}
		
	# for each symbol to plot...
	foreach col_info [$w_.cat symbol] {
	    if {[llength $col_info] < 3} {
		error_dialog \
		    "invalid symbol entry in config file: \
                    '[$w_.cat symbol]' length [llength [$w_.cat symbol]]" $w_
		return
	    }
	    lassign $col_info cols symbol expr
	    plot_objects $cols $symbol $expr
	}
    }

    
    # plot current objects on the image using the given plot info from the catalog config
    # file. cols should be a list of column names that may be used in $symbol
    # or $expr. $symbol may be a simple symbol name, such as "circle" or a list
    # such as {ellipse color ratio angle label condition}. The last 4 args may be
    # expressions using the column names given. ratio gives the ratio of x/y,
    # angle is the rotation angle, label is an optional text expr and condition
    # may evaluate to 1 or 0 to indicate whether or not the symbol should be displayed
    # for an object.
    # $expr should be an expression that evaluates to the size of the radius of the 
    # object in pixes, or a list {expr units} where  units is one of {image deg ...} 
    # as supported by rtd.

    method plot_objects {cols symbol expr} {
	# check column names
	foreach col [string toupper $cols] {
	    if {! [info exists col_($col)]} {
		error_dialog "invalid plot column: '$col'" $w_
		return
	    }
	}

	# parse the symbol info
	lassign $symbol sym color ratio angle label cond
	if {! [info exists symbols_($sym)]} {
	    error_dialog "invalid plot symbol: '$sym'" $w_
	    return
	}
	if {"$color" != ""} {
	    set fg [set bg $color]
	} else {
	    set bg black
	    set fg white
	}
	if {"$ratio" == ""} {
	    set ratio 1
	}
	if {"$angle" == ""} {
	    set angle 0
	}
	if {"$cond" == ""} {
	    set cond 1
	}

	# get expr for object radius and units
	lassign $expr size units
	if {"$units" == ""} {
	    set units image
	}

	# using world coords ?
	set iswcs [$w_.cat iswcs]
	if {$iswcs} {
	    set equinox [$equinox_ get]
	} else {
	    set equinox 2000
	}

	# tag for objects
	set tag [$w_.cat shortname $itk_option(-catalog)]

	set rownum -1
	# for each row ...
	foreach row $info_ {
	    incr rownum
	    set id [lindex $row [$w_.cat id_col]]
	    if {$iswcs} {
		# world coords
		set ra [lindex $row [$w_.cat ra_col]]
		set dec [lindex $row [$w_.cat dec_col]]
		if {[catch {
		    lassign [$image_ convert coords $ra $dec "wcs $equinox" {} {} canvas] \
			x y} msg]} {
		    #puts "error: line [expr $rownum+1]: converting world coordinates: $msg"
		    continue
		}
	    } elseif {[$w_.cat ispix]}  {
		# image coords
		set ix [lindex $row [$w_.cat x_col]]
		set iy [lindex $row [$w_.cat y_col]]
		if {[catch {
		    lassign [$image_ convert coords $ix $iy image {} {} canvas] \
			x y} msg]} {
		    #puts "error: line [expr $rownum+1]: converting image coordinates: $msg" $w_
		    continue
		}
	    }

	    # plot the obect for this row
	    set s [plot_row $row $id $x $y $cols $sym $bg $fg $ratio $angle $label \
		       $cond $size $units $tag]

	    if {"$s" != "ok"} {
		puts "error: plotting row [expr $rownum+1]" 
		return
	    }

	    if {$iswcs} {
		$canvas_ bind "cat$id" <1> "[code $this picked_wcs_object $ra $dec]"
	    }
	    $canvas_ bind "cat$id" <Any-Enter> "[code $table_ select_row $rownum]"
	}
    }

    
    # Plot the object for the given row. 
    #
    # id is a unique id for the row.
    #
    # $x and $y are the canvas coords of the center of the object to plot. 
    #
    # $cols is a list of column names for columns whose values are used in $expr. 
    #
    # $symbol is the plot symbol
    #
    # $bg and $fg are the background and foreground colors for drawing. If they are 
    # different, 2 lines are drawn, otherwise 1. 
    #
    # $ratio and $angle are optional arguments to the plot proc (for ellipse, plus). 
    #
    # $label is an expr to use to label the object.
    #
    # $cond should evaluate to a boolean expr. If true, plot the object, otherwise not.
    #
    # $size should be an expr and evaluate to the radius on the object in the 
    # given units, using the column names as tcl variables.
    #
    # $tag is a tag to add to the canvas object.
    #
    # This method returns the string "ok" if there were no errors.
    
    method plot_row {row id x y cols symbol bg fg ratio angle label cond size units tag} {
	# set local tcl variables for column values used
	foreach col $cols {
	    set $col [get_col [string toupper $col] $row]
	    if {"[set $col]" == ""} {
		return ok
	    }
	}

	# eval expr to get radius
	if {[catch {set bool [expr $cond]} msg]} {
	    error_dialog "error in plot symbol condition '$cond': $msg" $w_
	    return error
	}
	if {"$bool" != "1"} {
	    return ok
	}

	# eval expr to get radius
	if {[catch {set radius [expr $size]} msg]} {
	    error_dialog "error in plot symbol expression: '$size': $msg" $w_
	    return error
	}

	# convert radius to $units
	if {[catch {
	    lassign [$image_ convert dist $radius $radius $units {} {} canvas] \
		rx ry} msg]} {
	    error_dialog "error converting '$size' from '$units' to canvas coords: $msg"
	    return error
	}

	# ratio and angle may be expressions with column name variables
	foreach i {ratio angle} {
	    if {[catch {set $i [expr [set $i]]} msg]} {
		error_dialog "error in $i expression: '[set $i]': $msg" $w_
		return error
	    }
	}
	
	# label may also contain col name vars, but may not be numeric
	if {[catch {set label [subst $label]} msg]} {
	    error_dialog "error in label '$label': $msg" $w_
	    return error
	}

	# draw the symbol
	$canvas_ delete cat$id
	set tags [list objects cat$id $tag]
	eval [list draw_$symbol $canvas_ $x $y $rx $ry $bg $fg $tags $ratio $angle]
	
	# draw the label
	if {"$label" != ""} {
	    make_label $id $x $y $label $fg
	}

	return ok
    }


    # This method is called when the user clicks on a graphic symbol for a star.
    # The user might be selecting this star, so call the RtdImage method to do that
    
    method picked_wcs_object {ra dec} {
	if {"$ra" != "" && "$dec" != ""} {
	    if {[catch {$image_ convert coords $ra $dec "wcs [$image_ wcsequinox]" \
			    x y image} msg]} {
		puts $msg
	    }
	    catch {$itk_option(-image) picked_wcs_object $x $y $ra $dec}
	}
    }

    
    # return the value for the given column name in the given row

    method get_col {name row} {
	if {[info exists col_($name)]} {
	    return [lindex $row $col_($name)]
	} 
    }


    # member proc (like a static member function) to create an instance of this
    # class for the named catalog (or reuse the existing one for the catalog)
    #
    # The args are: 
    #
    # name  -    long name of catalog from config file, or {} for default catalog
    #            (first catalog in config file)
    # image -    name of skycat itcl image widget, or empty if running standalone
    # debug -    flag: if true, run queries in foreground
    # tcs_flag - may be set to true if the catalog is TCS format
    # type -     if name is empty and type is "archive", the default catalog will
    #            be an archive. Otherwise type should be "catalog".

    proc new_catalog {name image debug {tcs_flag 0} {type "catalog"}} {
	set i "$name,$image"
	if {[info exists instances_($i)] && [winfo exists $instances_($i)]} {
	    utilRaiseWindow $instances_($i)
	    return
	}
	set instances_($i) \
	    [AstroCat .ac[incr n_instances_] \
		 -image $image \
		 -debug $debug \
		 -catalog $name \
		 -catalogtype $type \
		 -tcs $tcs_flag \
		 -transient 0 \
		 -center 0]
    }
    
    
    # This is the same as new_catalog, except the default for name is the
    # first archive type catalog in the config file

    proc new_archive {name image debug {tcs_flag 0}} {
	new_catalog $name $image $debug $tcs_flag "archive"
    }

    
    # return a list of the instances of this class

    proc instances {} {
	set list {}
	if {[info exists instances_]} {
	    foreach i [array names instances_] {
		if {[winfo exists $instances_($i)]} {
		    lappend list $instances_($i)
		}
	    }
	}
	return $list
    }
    

    # return a list of top level windows that this class might create

    proc list_windows {} {
	set list [AstroCat::instances]
	# $w.tblsort may be created by the TableList class
	foreach w $list {
	    lappend list $w.tblsort $w.preview
	}
	lappend list .catinf
	return $list
    }

   
    # -- options --

    # name of catalog
    itk_option define -catalog catalog Catalog "" {
	# make sure we use full path name for local catalogs
	set f $itk_option(-catalog)
	if {[file exists $f] && "[string index $f 0]" != "/"} {
	    set itk_option(-catalog) [pwd]/$f
	}
    }

    # type of catalog (catalog or archive)
    itk_option define -catalogtype catalogType CatalogType "catalog"

    # image handle (itcl class)
    itk_option define -image image Image {} {
	if {"$itk_option(-image)" != ""} {
	    set canvas_ [$itk_option(-image) get_canvas]
	    set image_ [$itk_option(-image) get_image]
	    set draw_ [$itk_option(-image) component draw]
	    set im_state_ normal
	}
    }

    # GUI fonts used
    itk_option define -labelfont labelFont LabelFont -Adobe-helvetica-bold-r-normal--12*
    itk_option define -valuefont valueFont ValueFont -adobe-courier-medium-r-*-*-*-120-*-*-*-*-*-*
    itk_option define -wcsfont wcsFont WcsFont -*-symbol-*-*-*-*-14-*-*-*-*-*-*-*

    # font used in canvas to mark objects
    itk_option define -canvasfont canvasFont CanvasFont -*-courier-medium-r-*-*-*-120-*-*-*-*-*-*

    # set the width for  displaying labels
    itk_option define -labelwidth labelWidth LabelWidth 15

    # set the anchor for labels
    itk_option define -anchor anchor Anchor e

    # flag: if true, run queries in foreground for better debugging
    itk_option define -debug debug Debug 0

    # temp image file to use for preview
    itk_option define -tmpfile tmpfile Tmpfile {} {
	if {"$itk_option(-tmpfile)" == ""} {
	    set itk_option(-tmpfile) "/tmp/preview[pid].[incr count_]"
	}
    }

    # if -tcs is 1, use fixed format TCS catalog listings
    itk_option define -tcs tcs Tcs {0} 

    
    # -- protected members --

    #  canvas window containing main image
    protected variable canvas_ {}

    # internal rtdimage widget for main image
    protected variable image_ {}

    # CanvasDraw object for drawing on image
    protected variable draw_

    # state for image related operations
    protected variable im_state_ {disabled}

    # table and headings
    protected variable table_ {}
    protected variable headings_ {}

    # table contents
    protected variable info_ {}

    # array(uppercase col name) of col index from catalog headings
    protected variable col_

    # array containing supported symbol names
    protected variable symbols_

    # saved value of catalog search_cols_ entry, for comparison
    protected variable search_col_info_ {}

    # preview and more-info URLs for selected object
    protected variable preview_url_ {}
    protected variable more_url_ {}

    # option fields
    protected variable name_
    protected variable equinox_
    protected variable ra_
    protected variable dec_
    protected variable x_
    protected variable y_
    protected variable rad1_
    protected variable rad2_
    protected variable maxnum_
    protected variable setfromimg_
    protected variable selectarea_

    # list of columns to search by
    protected variable search_cols_ {}

    # array(colName) of widget name for search column min and max fields
    protected variable min_values_
    protected variable max_values_
    
    # copyright field
    protected variable copyright_

    # pipe to write/read feedback during image transfer
    protected variable rfd_
    protected variable wfd_

    # name server to use to resolve object names
    protected variable namesvr_ {}

    # current state: normal, disabled (i.e.: waiting)
    protected variable state_ {normal}

    # log file handle (used to log URLs)
    protected variable logfile_name_ 

    # flag: set at end of constructor
    protected variable initialized_ 0
    
    # index of this object in the instances_ array
    protected variable instance_idx_ {}

    # name of file menu widet
    protected variable file_menu_

    # names of menu widets
    protected variable edit_menu_
    protected variable options_menu_

    # flag: set when the column headings are changed between TCS and normal
    protected variable reset_columns_ {0}

    # currently selected object (Id field in row)
    protected variable object_name_ {}

    # -- common variables (common to all instances of this class) --
    
    # array mapping catalog name to widget/class name
    common instances_

    # instance count
    common n_instances_ 0

    # current instance name
    common current_instance_ {}

    # count used for filename generation
    common count_ 0

    # array(TopLevelWidget instance) of command used to update the
    # Data-Servers menu. This is used to make it posible to update
    # all instances of this menu in various top level windows.
    common catalog_menu_info_
}
