# E.S.O. - VLT project/ESO Archive
# @(#) $Id: AstroCat.tcl,v 1.1.1.1 2006/01/12 16:36:21 abrighto Exp $
#
# AstroCat.tcl - user interface class for viewing catalog info
#
# See man page AstroCat(n) for a complete description.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 14 Dec 95   created
# P.W.Draper 12 Dec 97   added methods to get equinox and table name
#            11 May 00   stop immediate delete of images, need backing 
#                        store for my catalogue handling commands.
#            03 Mar 08   Remove check in new_catalog, opened file is
#                        never closed (and can leave a temporary file).


itk::usual AstroCat {}

# The AstroCat widget class defines a top level widget for searching and
# displaying astronomical catalog data. It contains a menubar with items
# for loading, editing, and saving catalog data and a table displaying
# rows and columns of catalog data. This class does not know anything
# about images or plotting objects in images, however these features may
# be added in a derived class (see the SkySearch(n) in the skycat
# package for an example).
#
# You can also run this class as a standalone application with the
# command "astrocat". The application options are the same as the class
# options.

itcl::class cat::AstroCat {
    inherit util::TopLevelWidget


    # constructor

    constructor {args} {
	eval itk_initialize $args
    }

    
    # destructor - delete C++ based objects so that the temp 
    # files are deleted

    destructor {
	# delete temp files on close
	catch {
	    if {"[$w_.cat servtype]" == "local"} {
		if {"[string range $itk_option(-catalog) 0 4]" == "/tmp/"} {
		    file delete $itk_option(-catalog)
		}
	    }
	}
        catch {
           if { $tempimage_ != {} } { 
              file delete $tempimage_
           }
        }
	catch {$w_.cat delete}
	catch {close $rfd_}
	catch {close $wfd_}
	catch {unset instances_($instance_idx_)}
	if {"$current_instance_" == "$w_"} {
	    set current_instance_ {}
	}
    }

    
    # called after options have been evaluated

    protected method init {} {
	# if running stand-alone, setup X defaults
	if {$itk_option(-standalone)} {
	    util::setXdefaults
	    cat::setXdefaults
	}

	# set the file used to keep track of URLs (for debugging)
	set_logfile

	# do window layout
	layout_dialog

	# enter widget name in instance array for later reference
	set_instance

	# add a short help window
	make_short_help

	# create an object for running interruptable batch queries
	Batch $w_.batch \
	    -command [code $this preview_done] \
	    -debug $itk_option(-debug)

	# position from previous window, if pos.
	if {"$current_instance_" != ""} {
	    wm geometry $w_ +[winfo x $current_instance_]+[winfo y $current_instance_]
	}
	set current_instance_ $w_

	# for local catalogs, start search automatically
	set name $itk_option(-catalog)
	if {"[$w_.cat servtype]" == "local"} {
	    wm title $w_ "[file tail [$w_.cat longname $name $itk_option(-catalogdir)]] ($itk_option(-number))"
	    wm iconname $w_ "[file tail [$w_.cat shortname $name $itk_option(-catalogdir)]]"
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
    }

    
    # create the ~/.skycat dir if it does not already exists and keep a log
    # file there.

    protected method set_logfile {} {
	global ::env

	# open log file used to keep track of URLs (for debugging)
	set dir $env(HOME)/.skycat

	if {! [file isdirectory $dir]} {
	    catch {mkdir $dir}
	}
	set logfile_name_ $dir/log
    }

    
    # open or close a pipe to get feedback during HTTP transfers.
    # (To save limited fds, we close the feedback pipe after each HTTP op).
    # The arg should be "on" to turn feedback on, or "off" to turn it off.

    protected method set_feedback {onoff} {
	# Process any pending file events.
	# Note: this is important: if we don't process the events before
	# closing the feedback file, a crash may result.
	update 

	if {$itk_option(-debug)} {
	    # if -debug was given, the feedback bit wont work, since the
	    # query is done in the foreground and trying to read the feedback
	    # from the pipe may cause the application to hang...
	    return
	}
	if {"$onoff" == "on"} {
	    lassign [pipe] rfd_ wfd_
	    fileevent $rfd_ readable [code $this feedback]
	    $w_.cat feedback $wfd_
	} elseif {[info exists rfd_]} {
	    ::close $rfd_
	    ::close $wfd_
	    unset rfd_ wfd_
	    $w_.cat feedback {}
	}
    }

    
    # keep an array of instances(name,id) to help locate the 
    # window for a catalog
    
    protected method set_instance {} {
	set name [$w_.cat longname $itk_option(-catalog) $itk_option(-catalogdir)]
	set id $itk_option(-id)
	set dirPath $itk_option(-catalogdir)
	set instance_idx_ "$name,$id,$dirPath"
	set instances_($instance_idx_) $w_ 
    }


    # add the menu bar

    protected method add_menubar {} {
	TopLevelWidget::add_menubar

	set m [add_menubutton File "Display File menu"]
	set file_menu_ $m
	
	if {$iscat_} {
	    add_menuitem $m command "Open" \
		{Open a local catalog in tab table format} \
		-command [code cat::AstroCat::local_catalog \
			      $itk_option(-id) \
			      [info class] \
			      $itk_option(-debug) $w_] \
		-accelerator "Control-o"
	    
	    $m add separator

	    add_menuitem $m command "Save as..." \
		{Save listed objects to a local catalog file} \
		-command [code $this save_as] \
		-accelerator "Control-s"

	    add_menuitem $m command "Add to..." \
		{Add listed objects to a local catalog file} \
		-command [code $this add_to] \
		-accelerator "Control-a"

	    add_menuitem $m command "Add selected..." \
		{Add selected rows to a local catalog file} \
		-command [code $this add_selected] \
		-accelerator "Control-A"

	    $m add separator

	    add_menuitem $m command "Print..." \
		{Print the listing to a printer or file} \
		-command [code $this print] \
		-accelerator "Control-p"

	    add_menuitem $m command "Clear" \
		{Clear the catalog listing} \
		-command [code $this clear]

	    $m add separator
	}

	add_menuitem $m command "Close" \
	    {Close this window} \
	    -command [code $this close]
	
	if {$itk_option(-standalone)} {
	    add_menuitem $m command "Exit" \
		{Exit the application} \
		-command [code $this quit] \
		-accelerator "Control-q"
	}
	
	if {$iscat_} {
	    # Edit menu
	    set m [add_menubutton Edit "Display Edit menu"]
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
	}

	# Options menu
	set m [add_menubutton Options "Display Options menu"]
	set options_menu_ $m
	add_menuitem $m cascade "Set Name Server" \
	    {Select the name server used to resolve astronomical object names} \
	    -menu [set ns_menu_ [menu $itk_component(options).m.ns]]

	if {$iscat_} {
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

	    $m add separator
	}
	add_menuitem $m command "Proxies..."  \
	    "Define an HTTP proxy server for use with a firewall." \
	    -command [code cat::AstroCat::proxies]
	check_proxies

	# add a menu of catalogs ("Data-Servers")
	add_catalog_menu $w_ $itk_option(-id) [info class] $itk_option(-debug)
    }
    
    
    # enable or disable some menus
    
    protected method set_menu_states {} {
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
    # w is the caller's toplevel window (an instance of TopLevelWidget).
    #
    # id is an optional unique id to be associated with a new catalog widget.
    #
    # classname is the name of the AstroCat subclass to use to create new
    # catalog widgets (defaults to "AstroCat").
    #
    # debug is a flag is passed from the command line. If true, querries
    # are run in the foreground, to make debugging easier.
    #
    # note: this is defined as a proc rather than a method so that it 
    # can be accessed from outside the class to add the Data-Servers menu

    public proc add_catalog_menu {w {id ""} {classname AstroCat} {debug 0}} {
	# save this info so we can update the menu automatically later
	set catalog_menu_info_($w) \
	    [code cat::AstroCat::add_catalog_menu $w $id $classname $debug]

	# add the menu to the caller's menubar
	set m [$w add_menubutton "Data-Servers" "Display Data Servers menu"]
	
	$w add_menuitem $m cascade "Catalogs" \
	    {Select a catalog from the menu} \
	    -menu [menu $m.catalogs]
	fill_catalog_menu $w $id $classname $m.catalogs $debug catalog
	
	$w add_menuitem $m cascade "Image Servers" \
	    {Select an image server from the menu} \
	    -menu [menu $m.imagesvr]
	fill_catalog_menu $w $id $classname $m.imagesvr $debug imagesvr

	$w add_menuitem $m cascade "Archives" \
	    {Select an archive from the menu} \
	    -menu [menu $m.archive]
	fill_catalog_menu $w $id $classname $m.archive $debug archive
	
	$w add_menuitem $m cascade "Local Catalogs" \
	    {Select a local catalog from the menu} \
	    -menu [menu $m.local]
	fill_catalog_menu $w $id $classname $m.local $debug local shortname

	$m.local add separator

	$w add_menuitem $m.local command "Load from file..." \
	    {Open a local catalog file} \
	    -command [code cat::AstroCat::local_catalog $id $classname $debug $w] \
		-accelerator "Control-O"

	$m add separator

	$w add_menuitem $m command "Browse Catalog Directories..."  \
	    "Browse the catalog directory hierarchy to view \
             catalogs or add them to the default list" \
	    -command [code cat::AstroCat::catalog_directory $id $classname $debug $w] \
	    -accelerator "Control-b"

	$w add_menuitem $m command "Reload config file..."  \
	    "Reload the default catalog config file after it was edited by hand" \
	    -command [code cat::AstroCat::reload_config_file $w] \
		-accelerator "Control-R"
	
	# if there is a local catalog called "history", add it to the menu also
	if {"[$astrocat_ servtype history]" == "local"} {
	    $m add separator
	    $w add_menuitem $m command "History..."  \
		"Open an automatically generated catalog of previously viewed images" \
		-command [code cat::AstroCat::select_catalog history local $id $classname 0 $debug $w] \
		-accelerator "Control-h"
	}
    }

    
    # reload the default catalog config file using the given astrocat object

    public proc reload_config_file {w} {
	global ::env
	set config_file $env(HOME)/.skycat/skycat.cfg
	if {[file exists $config_file]} {
	    set env(CATLIB_CONFIG) "file:$config_file"
	}

	if {[catch {$astrocat_ reload} msg]} {
	    error_dialog $msg $w
	}
	# make sure these windows are updated
	catch {destroy $w_.symconf}
	catch {destroy $w_.searchconf}
	if {[winfo exists .catinf]} {
	    .catinf reinit_tree
	}
	
	if {[info exists instances_]} {
	    foreach i [array names instances_] {
		$instances_($i) update_search_options
	    }
	}
	update_catalog_menus
    }


    # Check for a file ~/.skycat/proxies, once each session, and
    # use it to initialize environment variables for a proxy server
    # (see also tclutil/util/src/HTTP.C).

    public proc check_proxies {} {
	global ::env
	# only do it once
	if {$checked_proxies_} {
	    return 
	}
	cat::ProxyDialog::check_proxies $env(HOME)/.skycat/proxies
	set checked_proxies_ 1
    }

    # Pop up a dialog to set or change the HTTP proxy server.

    public proc proxies {} {
	global ::env
	utilReUseWidget ProxyDialog .proxy \
	    -configfile $env(HOME)/.skycat/proxies
    }


    # pop up a window to browse the catalog directories. 
    #
    # id is an optional unique id to be associated with a new catalog widget.
    #
    # classname is the name of the AstroCat subclass to use to create new
    # catalog widgets (defaults to "AstroCat").
    #
    # debug is a flag is passed from the command line. If true, querries
    # are run in the foreground, to make debugging easier.
    #
    # w should be the caller's top level window, if specified.
    #

    proc catalog_directory {{id ""} {classname AstroCat} {debug 0} {w ""}} {
	utilReUseWidget cat::CatalogInfo .catinf \
	    -id $id \
	    -classname $classname \
	    -debug $debug \
	    -callerw $w \
	    -command [code cat::AstroCat::update_catalog_menus]
	
	# put the window clone number in the title
	if {[winfo exists $w]} {
	    catch {wm title .catinf "Catalog Directory ([$w cget -number])"}
	}
    }

    
    # update all of the catalog menus in all instances to show 
    # the current catalog info. 

    public proc update_catalog_menus {} {
	foreach i [array names catalog_menu_info_] {
	    if {[winfo exists [utilNamespaceTail $i]]} {
		eval $catalog_menu_info_($i)
	    }
	}
    }
    

    # pop up a dialog to set the plot symbols to use for this catalog

    public method set_plot_symbols {} {
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

   public  method set_search_cols {} {
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

    public method update_search_options {} {
	$searchopts_ update_search_options
    }


    # close this window

    public method close {} {
	if {$itk_option(-standalone)} {
	    wm iconify  $w_
	} else {
	    wm withdraw $w_
	}
    }

    
    # pop up a dialog to sort the list

    public method sort_dialog {} {
	$results_ sort_dialog
    }

    
    # called when the user has selected columns to sort the results by.
    # The first arg is the sort columns, the second arg is the order
    # (increasing, decreasing)
    
    public method set_sort_cols {sort_cols sort_order} {
	global ::$w_.tcs
	if {"[$w_.cat sortcols]" != "$sort_cols" \
		|| "[$w_.cat sortorder]" != "$sort_order"} {
	    $w_.cat sortcols $sort_cols
	    $w_.cat sortorder $sort_order
	    $w_.cat is_tcs $itk_option(-catalog) [set $w_.tcs]
	    cat::CatalogInfo::save {} $w_ 0
	    $results_ config -sort_cols $sort_cols -sort_order $sort_order
	    search
	}
    }


    # pop up a dialog to select table columns to display

    public method select_columns {} {
	$results_ select_columns
    }


    # called when the user has selected columns to show
    
    public method set_show_cols {cols} {
	global ::$w_.tcs
	set show [$w_.cat showcols]
	if {"$show" == ""} {
	    set show [$results_ get_headings]
	}
	if {"$show" != "$cols"} {
	    $w_.cat showcols $cols
	    $w_.cat is_tcs $itk_option(-catalog) [set $w_.tcs]
	    cat::CatalogInfo::save {} $w_ 0
	}
    }


    # add the name server catalogs to the name server menu

    protected method get_name_servers {} {
	set m $ns_menu_
	if {[catch {set list [$w_.cat info namesvr]} msg]} {
	    error_dialog $msg $w_
	    return
	}

	foreach namesvr $list {
	    $m add radiobutton \
		-label $namesvr \
		-command [code $this set_namesvr $namesvr] \
		-variable $m
	}

	if {![info exists namesvr]} {
	    error_dialog "No default name server found for astronomical objects"
	    return
	}

	# set default name server
	global ::$m
	set $m $namesvr
	set_namesvr $namesvr
    }


    # toggle the TCS option (use the tcscat or the astrocat Tcl command)

    public method set_tcs_columns {} {
	global ::$w_.tcs
	set is_tcs [set $w_.tcs]
	set itk_option(-tcs) $is_tcs
	catch {$w_.cat delete}

	if {$is_tcs} {
	    tcscat $w_.cat
	} else {
	    astrocat $w_.cat
	}
	if {[catch {$w_.cat open $itk_option(-catalog) $itk_option(-catalogdir)} msg]} {
	    error_dialog $msg $w_
	    return
	}
	
	set iscat_ 1

	# if changing formats, reset table column info and entry
	# since column names will change making previous info invalid
	if {[$w_.cat is_tcs] != $is_tcs} {
	    $w_.cat is_tcs $itk_option(-catalog) $is_tcs
	    set reset_columns_ 1
	}

	if {[llength $headings_]} {
	    search
	}
    }


    # reset table dialogs if needed

    public method reset_table {} {
	$results_ reset
	$results_ set_options {MORE PREVIEW more preview} Show 0
	$w_.cat showcols {}
	$w_.cat sortcols {}
	cat::CatalogInfo::save {} $w_ 0
    }

    
    # return true if the given file contains a TCS catalog

    public proc is_tcs_catalog {filename} {
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
    
    public method clear {} {
	catch {$results_ clear}
    }

    
    # pop up a dialog to print the table listing
    
    public method print {} {
	$results_ print_dialog
    }

       
    # Ask the user for the name of a local catalog file and then
    # open a window for the catalog.
    #
    # id is an optional unique id to be associated with a new catalog widget.
    #
    # classname is the name of the AstroCat subclass to use to create new
    # catalog widgets (defaults to "AstroCat").
    #
    # $debug is a flag set from the command line arg.
    #
    # $w should be the top level window of the caller, if specified.
    
    public proc local_catalog {{id ""} {classname AstroCat} {debug 0} {w ""}} {
	set file [filename_dialog [pwd] *]
	if {"$file" != ""} {
	    if {[file isfile $file]} {
		if {[catch {$astrocat_ check $file} msg]} {
		    error_dialog $msg
		} else {
		    select_catalog $file local $id $classname \
			[is_tcs_catalog $file] $debug $w
		}
	    } else {
		error_dialog "There is no file named '$file'"
	    }
	}
    }


    # Fill up a menu of known data servers to choose from.
    # 
    # w is the caller's top level widget (containing menubar)
    #
    # id is an optional unique id to be associated with a new catalog widget.
    #
    # classname is the name of the AstroCat subclass to use to create new
    # catalog widgets (defaults to "AstroCat").
    #
    # $debug is a flag set from the command line arg.
    #
    # The serv_type argument should be one of: "catalog", "archive".
    #
    # An optional "cmd" argument may be set to longname or shortname
    # depending on the names you want in the menu (for local catalogs,
    # specify shortname, to avoid getting the complete path name in
    # the menu.

    public proc fill_catalog_menu {w id classname m debug serv_type {cmd longname}} {
	if {[catch {set catalog_list [lsort [$astrocat_ info $serv_type]]} msg]} {
	    error_dialog $msg $w
	    return
	}
	
	if {[llength $catalog_list]} {
	    foreach i $catalog_list {
		set name [$astrocat_ $cmd $i]
		set is_tcs [$astrocat_ is_tcs $i]
		$w add_menuitem $m command $name \
		    "Open $serv_type: \"$i\"" \
		    -command [code cat::AstroCat::select_catalog $i $serv_type $id \
				  $classname $is_tcs $debug $w]
	    }
	}
    }

    
    # Open a window for the given catalog, or report an error if the catalog
    # does not exist. 
    # $name is the name of the catalog to open,
    # $classname is the class to use to open it,
    # $debug is an optional debugging flag,
    # $w should be set to the top level window of the caller (optional).

    public proc open_catalog_window {name {id ""} {classname AstroCat} {debug 0} {w ""}} {
	if {[catch {$astrocat_ open $name} msg]} {
	    error_dialog $msg
	}
	cat::AstroCat::select_catalog $name catalog $id $classname 0 $debug $w
    }


    # save the current data to a local catalog
    
    public method save_as {} {
	$results_ save_as
    }


    # add the rows in the current listing to a local catalog file
    
    public method add_to {} {
	$results_ add_to
    }

    
    # add the currently selected rows to a local catalog file

    public method add_selected {} {
	$results_ add_selected
    }

    
    # remove the currently selected rows from a local catalog file

    public method remove_selected {} {
	$results_ remove_selected

	# update the display
	clear
	search
    }

    
    # pop up a dialog to enter the data for a new object for a local catalog

    public method enter_new_object {} {
	$results_ enter_new_object [code $this search]
    }

    
    # pop up a window so that the user can edit the selected object(s)
    
    public method edit_selected_object {} {
	$results_ edit_selected_object [code $this search]
    }
    

    # open the catalog for this window

    public method open_catalog {} {
	# create astrocat object (or tcscat if TCS catalog) 
	global ::$w_.tcs
	if {[set $w_.tcs $itk_option(-tcs)]} {
	    tcscat $w_.cat
	} else {
	    astrocat $w_.cat
	}

	# normally -catalog should be specified when creating this widget
	# if not, choose a default...
	if {"$itk_option(-catalog)" == ""} {
	    return
#	    if {[catch {set catalog_list [$w_.cat info $itk_option(-catalogtype)]} msg]} {
#		error_dialog $msg $w_
#		return
#	    }
#	    set itk_option(-catalog) [lindex $catalog_list [expr [llength $catalog_list]-1]]
	}

	# open the catalog
	set name $itk_option(-catalog)
	if {[catch {$w_.cat open $name $itk_option(-catalogdir)} msg]} {
	    error_dialog $msg $w_
	    return -code error
	}

	# set iscat_ to true if the catalog is not an image server
	set iscat_ 1
	if {"[$w_.cat servtype]" == "imagesvr"} {
	    set iscat_ 0
	} 

	# if this is a local catalog, add it to the catalog menus and tree
	# XXX should check if local catalog was already known
	if {"[$w_.cat servtype]" == "local"} {
	    update_catalog_menus

	    # add to catalog tree, if there is one
	    catch {.catinf insert_node $name}
	    
	    # add an entry to the config file for this catalog
	    cat::CatalogInfo::save "" $w_ 0
	}
	
	# display catalog name in header and icon
	wm title $w_ "[$w_.cat longname $name $itk_option(-catalogdir)] ($itk_option(-number))"
	wm iconname $w_ "[$w_.cat shortname $name $itk_option(-catalogdir)]"
    }

    
    # add the search options panel

    protected method add_search_options {} {
	# AstroQuery(n) widget for displaying catalog search options.
	itk_component add searchopts {
	    set searchopts_ [AstroQuery $w_.searchopts \
				 -relief groove \
				 -borderwidth 2 \
				 -debug $itk_option(-debug) \
				 -astrocat [code $w_.cat] \
				 -searchcommand [code $this search] \
				 -feedbackcommand [code $this set_feedback] \
				 -command [code $this query_done]]
	}
	pack $itk_component(searchopts) \
	    -side top -fill x
    }

    
    # add the table for displaying the query results

    protected method add_result_table {} {
	# QueryResult(n) widget to display catalog query results
	itk_component add results {
	    set results_ [QueryResult $w_.results \
			      -astrocat [code $w_.cat] \
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
    }

    # add the dialog button frame

    protected method add_dialog_buttons {} {
	# dialog buttons
	pack [frame $w_.buttons -borderwidth 2 -relief raised] \
	    -side top -fill x

	if {$iscat_} {
	    set search_label "Search"
	} else {
	    set search_label "Get Image"
	}
	
	pack \
	    [button $w_.search \
		 -text $search_label \
		 -command [code $this search]] \
	    -side left -expand 1 -pady 2m -in $w_.buttons

	if {$iscat_} {
	    pack \
		[button $w_.more \
		     -text "More Info" \
		     -state disabled \
		     -command [code $this more]] \
		[button $w_.preview \
		     -text "Preview" \
		     -state disabled \
		     -command [code $this preview]] \
		-side left -expand 1 -pady 2m -in $w_.buttons
	}

	pack \
	    [button $w_.stop \
		 -text "Stop" \
		 -state disabled \
		 -command [code $this interrupt]] \
	    [button $w_.close \
		 -text "Close" \
		 -command [code $this close]] \
	    -side left -expand 1 -pady 2m -in $w_.buttons

	# add a help button, if there is a help URL
	if {"[$w_.cat help]" != ""} {
	    pack \
		[button $w_.help \
		     -text "Help" \
		     -command [code $this help]] \
		-side left -expand 1 -pady 2m -in $w_.buttons -before $w_.close 
	}
    }


    # disable the search button, if the arg is 1, otherwise enable it

    public method disable_search {{arg 1}} {
	if {$arg} {
	    set search_state_ disabled
	} else {
	     set search_state_ normal
	}
    }

    
    # if the catalog has a "help" URL, try to display it in netscape using the
    # netscape remote interface.

    public method help {} {
	set url [$w_.cat help]
	if {"$url" != ""} {
	    send_to_netscape $url
	} else {
	    info_dialog "Sorry, no help is available for this catalog"
	}
    }


    # add a progress bar to display the progress of data transfers

    protected method add_progress_bar {} {
	# add a progress bar at the botton
	pack [ProgressBar $w_.progress] \
	    -side top -fill x
    }
    

    # do the dialog window layout
    
    protected method layout_dialog {} {
	open_catalog
	add_menubar
	add_search_options
	get_name_servers
	if {$iscat_} {
	    add_result_table
	}
	add_dialog_buttons
	add_progress_bar
    }


    # add a short help window and set the help texts
    
    protected method make_short_help {} {
	TopLevelWidget::make_short_help

	add_short_help $results_ \
	    {Query results: {bitmap b1} = select object, \
	       double click {bitmap b1} = label object, \
	                    {bitmap dragb2} = scroll list}

	if {$iscat_} {
	    add_short_help $w_.search \
		{{bitmap b1} = start catalog search}

	    add_short_help $w_.preview \
		{{bitmap b1} = view the preview image or plot data for the selected object}

	    add_short_help $w_.more \
		{{bitmap b1} = display more information for the selected object}

	} else {
	    add_short_help $w_.search \
		{{bitmap b1} = fetch the image from the image server}
	}

	add_short_help $w_.stop \
	    {{bitmap b1} = interrupt the current catalog operation}

	if {[winfo exists $w_.help]} {
	    add_short_help $w_.help \
		{{bitmap b1} = Display help information for catalog in netscape}
	}

	add_short_help $w_.close {{bitmap b1} = close this window}

	add_short_help $w_.progress {Progress bar: displays status of work in progress}
    }

       
    # This proc is called when the named catalog selected from the menu.
    #
    # serv_type is the type of the catalog: one of: catalog, archive, 
    # local, imagesvr, etc...
    #
    # id is an optional unique id to be associated with a new catalog widget.
    #
    # classname is the name of the AstroCat subclass to use to create new
    # catalog widgets (defaults to "AstroCat").
    #
    # tcs_flag can be set to true, if you know it is a TCS catalog.
    #
    # $debug is a flag set from the command line arg.
    #
    # $w should be the top level window of the caller (optional).

    public proc select_catalog {name serv_type id classname {tcs_flag 0} {debug 0} {w ""}} {
	new_catalog $name $id $classname $debug $tcs_flag "catalog" $w
    }

    
    # set the name server to use to resolve object names
    
    public method set_namesvr {name} {
	$searchopts_ config -namesvr $name
	
    }


    # interrupt the current search 

    public method interrupt {} {
	$w_.batch interrupt
	$searchopts_ interrupt
	set_feedback off
	catch {$results_ config -title "Search Results"}
	set_state normal
    }

    
    # set/reset widget states while busy 

    public method set_state {state} {
	set state_ $state
	if {"$state" == "normal"} {
	    catch {blt::busy release $w_.options}
	    catch {focus -lastfor $w_.options}
	    $w_.search config -state $search_state_
	    $w_.stop config -state disabled
	} else {
	    catch {focus .}
	    catch {blt::busy hold $w_.options}
	    $w_.search config -state disabled
	    catch {
		$w_.more config -state disabled
		$w_.preview config -state disabled
	    }
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

    public method feedback {} {
	set text [gets $rfd_]
	if {"$state_" != "normal"} {
	    if {[scan $text {total length: %d bytes} n] == 1} {
		$w_.progress config -to $n
	    } elseif {[scan $text {read %d bytes} n] == 1} {
		$w_.progress config -value $n
	    } elseif {[scan $text {url: %s} url] == 1} {
	    	catch {
		    set fd [open $logfile_name_ a+]
		    puts $fd "[clock format [clock seconds]]\n$itk_option(-catalog)\n$url\n\n"
		    ::close $fd
		} 
	    }
	}
	$w_.progress config -text $text
	update idletasks
    }


    # This method could be used by a plugin to set the contents of the listing
    # without doing a query.
    # The arguments are the table headings (list of strings) and the table data
    # (list of rows, where each row is a list of values). The "more" flag is
    # optional and indicates that there would be more data.

    public method set_info {headings info {more 0}} {
	query_done "" $headings $info $more
    }
    


    # This method is called when the background query is done.
    #
    # Args:
    #
    #   errmsg - If this is not empty, it contains an error message and
    #   the following args should be ignored. If there were no
    #   errors, this arg is an empty string.
    #
    #   headings  - are the column headings 
    #
    #   info - is a list of rows (result of query)
    #
    #   more - is a flag set to 1 if there were more rows available that were
    #   not returned due to the maxrows limit set.

    public method query_done {errmsg headings info more} {
	if {"$errmsg" != ""} {
	    # check if we need user/passwd info. errmsg should have the format:
	    # "Authorization Required for <realm> at <host>"
	    if {[regsub {Authorization Required} $errmsg {Enter username} msg]} {
		lassign [passwd_dialog $msg] username passwd
		if {"$username" != "" && "$passwd" != ""} {
		    lassign $errmsg {} {} {} realm {} host
		    $w_.cat authorize $username $passwd $realm $host
		    $searchopts_ search
		}
	    } else {
		set_state normal
		catch {$results_ config -title "Search Results"}
		# error messages starting with "***" are only displayed in progress win
		if {"[string range $errmsg 0 2]" != "***"} {
		    error_dialog $errmsg $w_
		}
		after 0 [list $w_.progress config -text $errmsg]
	    }
	} else {
	    if {! $iscat_} {
		# for image servers, the info is the image file name
		set filename $info
		# load the image and remove the temp file
		display_image_file $filename
                if { $tempimage_ != {} } { 
                   catch {file delete $tempimage_}
                }
                set tempimage_ $filename
	    } else {
		busy {
		    set prev_headings $headings_
		    set headings_ $headings
		    set info_ $info
		    
		    # update table
		    $results_ config -headings $headings_
		    if {$reset_columns_} {
			set reset_columns_ 0
			reset_table
		    }

		    if {"$prev_headings" != "$headings_"} {
			$results_ update_options
			set_menu_states
		    }

		    $results_ config -info $info
		    
		    # need to know the equinox of the results, if using world coords
		    if {[$w_.cat iswcs]} {
			lassign [$w_.searchopts get_pos_radius] {} {} equinox
			$results_ config -equinox $equinox
		    } 

		    if {$more} {
			set more "+"
		    } else {
			set more ""
		    }
		    $results_ config -title "Search Results ([$results_ total_rows]$more)"

		    # note column indexes in array (use upper case to simplify search)
		    catch {unset col_}
		    set n -1
		    foreach i $headings_ {
			set col_($i) [incr n]
		    }
		    
		    # plot stars
		    #puts "plot time: [expr [lindex [time plot] 0]/1000000.]"
		    plot
		}
	    }

	    set_state normal
	}
    }

    
    # start the catalog search based on the current search options
    # and display the results in the table.

    public method search {args} {
	# start the query in the background
	catch {
	    $results_ config -title "Searching..."
	    $results_ clear
	}

	set_state disabled
	set servtype [$w_.cat servtype]

	if {"$servtype" == "local"} {
	    $w_.progress config -text "Searching catalog..."
	} elseif {"$servtype" == "imagesvr"} {
	    $w_.progress config -text "Attempting to contact image server..."
	} else {
	    $w_.progress config -text "Attempting to contact catalog server..."
	}
	$w_.progress look_busy

	$searchopts_ search
    }

 
    # Get the requested image from the image server based on the given arguments
    # for the world coord position or name and the given width and height

    public method getimage_from_args {ra dec name equinox width height} {
	$searchopts_ set_pos_width_height [list $ra $dec $equinox $width $height $name]
	search
    }


    # If the given name is the name of a local catalog, check that the file exists,
    # and if not, ask the user for a new path name or remove it from the menu.
    
    public proc check_local_catalog {name {id ""} {classname AstroCat} {debug 0} 
				     {tcs_flag 0} {type "catalog"} {w ""} {dirPath ""}} {
	if {"[$astrocat_ servtype $name $dirPath]" != "local"} {
	    return 0
	}
	set file [$astrocat_ url $name]
	if {[file isfile $file]} {
	    return 0
	}

	set msg "The catalog file [file tail $file] does not exists. \
                     Do you want to specify a new name?"
	switch [choice_dialog $msg {{Specify a new name} Cancel {Remove from menu}}] {
	    {Specify a new name} {
		set file [filename_dialog]
		if {[file isfile $file]} {
		    $astrocat_ entry remove $name
		    new_catalog $file $id $classname $debug $tcs_flag $type $w $dirPath
		    if {[winfo exists .catinf]} {
			.catinf reinit_tree
		    }
		}
	    }
	    Cancel {
		return 1
	    }
	    {Remove from menu} {
		$astrocat_ entry remove $name
		if {[winfo exists .catinf]} {
		    .catinf reinit_tree
		}
	    }
	}

	# update config file and menus
	cat::CatalogInfo::save "" $w 0
	update_catalog_menus
	return 1
    }

    
    # set the values for the position and radius entries from the given
    # list, which should be in the format {ra dec equinox radius} if
    # we are using wcs, otherwise {x y radius}.

    public method set_pos_radius {list} {
	$searchopts_ set_pos_radius $list
    }


    # set the values for the position, width and height entries from 
    # the given list, which should be in the format 
    # {ra dec equinox width height (in arcmin)} for wcs, or {x y width height},
    # for pixel coordinates. 

    public method set_pos_width_height {list} {
	$searchopts_ set_pos_width_height $list
    }


    # This method is called whenever a result row is selected.
    # Note the selected values and enable/disable some buttons

    public method select_result_row {} {
	$w_.results select_result_row

	if {! [info exists col_(PREVIEW)] && ! [info exists col_(MORE)]} {
	    return
	}
	set row [lindex [$results_ get_selected] 0]
	if {[llength $row] == 0} {
	    return
	}
	set preview [get_col PREVIEW $row]
	set more [get_col MORE $row]
	set object_name_ [lindex $row 0]

	# note: HST returns P=http:....
	if {! [regsub {P=(.*)} $preview {\1} preview_url_]} {
	    set preview_url_ $preview
	}
	if {"$preview_url_" == ""} {
	    $w_.preview config -state disabled
	} else {
	    $w_.preview config -state normal
	}

	# note: HST returns M=http:....
	if {! [regsub {M=(.*)} $more {\1} more_url_]} {
	    set more_url_ $more
	}
	if {"$more_url_" == ""} {
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

    public method preview {} {
	if {[$results_ num_selected] == 0} {
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
	
	# if it is a file URL, we can just get the file
	if {[string first file: $preview_url_] == 0} {
	    set filename [string range $preview_url_ 5 end]
	    preview_done 0 [list $filename image/x-fits]
	} else {
	    # HTTP: do it in the background, since it may take a while
	    set_state disabled
	    $w_.progress config -text "Attempting to contact server..."
	    $w_.progress look_busy

	    # set up the feedback pipe
	    set_feedback on

	    $w_.batch bg_eval [code $this get_preview]
	}
    }

    
    # get the preview image given by $preview_url_ into a temp file

    protected method get_preview {} {
	# get the preview and call preview_done
	$w_.cat getpreview -url $preview_url_
    }
    

    # This method is called when we have received the preview data
    # The "status" argument is the status of the background http get operation (0 if ok).
    # The result is a list of {filename Content-type}, where filename contains the data
    # and Content-type indicates the type of the preview data. Decompression is already
    # taken care of, so we only expect to get a FITS image here or a tab table to plot.
    #
    # If we get an authorization error, we ask the user to type in a username and
    # password and retry the operation with the new info. This is needed for some
    # HTTP server sites that have special restrictions on data access (especially
    # image access).

    protected method preview_done {status result} {
	set_feedback off

	if {$status} {
	    # check if we need user/passwd info. errmsg should have the format:
	    # "Authorization Required for <realm> at <host>"
	    if {[regsub {Authorization Required} $result {Enter username} msg]} {
		lassign [passwd_dialog $msg] username passwd
		if {"$username" != "" && "$passwd" != ""} {
		    lassign $result {} {} {} realm {} host
		    $w_.cat authorize $username $passwd $realm $host
		    preview
		}
	    } else {
		error_dialog $result $w_
	    }
	} else {
	    if {[llength $result] != 2} {
		error_dialog "error getting preview data (result = $result)"
	    } else {
		lassign $result filename type
		if {"$type" == "image/x-fits"} {
		    # load the image and remove the temp file
		    display_image_file $filename
		} elseif {"$type" == "text/tab-separated-values" \
			      || "$type" == "text/x-starbase" \
			      || "$type" == "text/plain" \
			      || "$type" == ""} {
		    PreviewPlot $w_.pplot[incr count_] \
			-file $filename \
			-name $object_name_ \
			-shorthelpwin $this \
			-transient 1
		} else {
		    error_dialog "unrecognized Content-type for preview data: $type"
		}
		if {[string first file: $preview_url_] != 0} {
		    # if the image was retrieved via http, remove it
		    catch {file delete $filename}
		}
	    }
	}
 	set_state normal
	
	# restore preview button state...
	select_result_row
    }

    
    # send a URL to be displayed by netscape

    public proc send_to_netscape {url} {
	if {[catch {exec netscape -remote "openURL($url)"} msg1]} {
	    if {! [string match $msg1 "netscape: warning"]} {
		if {[catch {exec netscape $url &} msg2]} {
		    warning_dialog "couldn't start netscape: $msg1"
		} else {
		    info_dialog "Starting netscape..."
		}
	    }
	} else {
	    info_dialog "Sending help URL to netscape..."
	}
    }
    
    
    # use the "more" URL to display more info about an object using netscape
    # or mosaic

    public method more {} {
	if {[$results_ num_selected] == 0} {
	    warning_dialog "Please select an object from the list first" $w_
	    return
	}
	if {"$more_url_" == ""} {
	    warning_dialog "No more information is available for this object" $w_
	    return
	}
	# start netscape
	$w_.progress config -text "sending URL to netscape..."
	update idletasks
	send_to_netscape $more_url_
    }


    
    # return the value for the given column name in the given row

    public method get_col {name row} {
	if {[info exists col_($name)]} {
	    return [lindex $row $col_($name)]
	} 
    }


    # This member proc is used to open a window for the named catalog, 
    # or reuse the existing one for the catalog, if it is already open.
    #
    # name is the long name of catalog from config file
    #
    # id is an optional unique id to be associated with a new catalog widget.
    #
    # classname is the name of the AstroCat subclass to use to create new
    # catalog widgets (defaults to "AstroCat").
    #
    # debug is a flag: if true, run queries in foreground
    #
    # tcs_flag may be set to true if the catalog is TCS format
    #
    # type is the catalog serv_type (here it is enough to specify "catalog" or
    # "imagesvr".
    #
    # w should be the top level window of the caller, if specified.
    #
    # dirPath may be the name of the catalog directory (or a tcl list forming
    # the path to it). The default is the root catalog directory.

    public proc new_catalog {name {id ""} {classname AstroCat} {debug 0} {tcs_flag 0} 
			     {type "catalog"} {w ""} {dirPath ""}} {

	# if it is a local catalog, make sure it exists still, or get a new name
	if {[check_local_catalog $name $id $classname $debug $tcs_flag $type $w $dirPath] != 0} {
	    return
	}

	set i "$name,$id,$dirPath"
	if {[info exists instances_($i)] && [winfo exists $instances_($i)]} {
	    utilRaiseWindow $instances_($i)
	    if {"[$instances_($i).cat servtype]" == "local"} {
		# for local catalogs, search automatically when opened
		$instances_($i) search
	    }
	    return
	}

	# If $w was specified, put the window under that top level window
	# so we get the window numbers right (for cloning, see TopLevelWidget).
	if {[winfo exists $w]} {
	    set instname $w.ac[incr n_instances_]
	} else {
	    set instname .ac[incr n_instances_]
	}
	set instances_($i) \
	    [$classname $instname \
		 -id $id \
		 -debug $debug \
		 -catalog $name \
		 -catalogtype $type \
		 -catalogdir $dirPath \
		 -tcs $tcs_flag \
		 -transient 0 \
		 -center 0]
    }
    
    
    # Return a Tcl list of instances of this class. By default
    # (and for backward compatibility) only catalog windows are
    # included - not image servers. If "choice" is "all", then
    # all instances are returned. If choice is imagesvr, only 
    # image server windows are returned.

    public proc instances {{choice catalogs}} {
	set list {}
	if {[info exists instances_]} {
	    foreach i [array names instances_] {
		if {[winfo exists $instances_($i)]} {
		    if {[$instances_($i) iscat]} {
			# instance of a catalog window
			if {"$choice" == "catalogs" || "$choice" == "all"} {
			    lappend list $instances_($i)
			}
		    } else {
			# instance of an image server window
			if {"$choice" == "imagesvr"} {
			    lappend list $instances_($i) 
			}
		    }
		}
	    }
	}
	return $list
    }


    # Return the widget instance for the given catalog, if found,
    # otherwise an empty string

    public proc get_instance {catalog} {
	foreach w [cat::AstroCat::instances all] {
	    if {"[$w cget -catalog]" == "$catalog"} {
		return $w
	    }
	}
    }

    
    # Return 1 if this window is for a searchable catalog, 
    # and 0 otherwise (in which case it must be an image server window).
    
    public method iscat {} {
	return $iscat_
    }


    # ---------------------------------------------------------------
    # The following methods deal with images and plotting of objects
    # in an image and are meant to be defined in a derived class.
    # See the skycat/skycat/interp/library/SkySearch.tcl for an example.
    # ---------------------------------------------------------------

    
    # plot the stars/objects found in the previous search in a (image) window.
    # The symbols to use are taken from the config file.
    # (To be defined in a subclass)

    public method plot {} {
    }
    

    # display the given image file (To be defined in a subclass)

    public method display_image_file {filename} {
    }


    #  return the equinox

    public method get_equinox {} {
	return [component searchopts get_equinox]
    }


    #  return the name of the TableList

    public method get_table {} {
	if {[info exists itk_component(results)]} {
	    return $itk_component(results)
	}
	error "This catalog is an image server"
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

    # name of catalog directory, or a tcl list forming the path to it (empty means root)
    itk_option define -catalogdir catalogDir CatalogDir ""

    # Optional unique id, used in searching for already existing catalog widgets.
    itk_option define -id id Id ""

    # list of catalog column headings (from results of most recent query)
    protected variable headings_ {}

    # result from most recent query (list of rows)
    protected variable info_ {}

    # font to use for labels
    itk_option define -labelfont labelFont LabelFont -Adobe-helvetica-bold-r-normal--12*

    # font to use for values
    itk_option define -valuefont valueFont ValueFont -Adobe-helvetica-medium-r-normal--12*

    # font to use for ra,dec labels
    itk_option define -wcsfont wcsFont WcsFont -*-symbol-*-*-*-*-14-*-*-*-*-*-*-*

    # set the width for  displaying labels
    itk_option define -labelwidth labelWidth LabelWidth 12

    # set the anchor for labels
    itk_option define -anchor anchor Anchor e

    # flag: if true, run queries in foreground for better debugging
    itk_option define -debug debug Debug 0

    # if -tcs is 1, use fixed format TCS catalog listings
    itk_option define -tcs tcs Tcs {0}

    # AstroQuery widget used to manage search options
    protected variable searchopts_ {}

    # QueryResult widget used to display search results
    protected variable results_ {}

    # array(uppercase col name) of col index from catalog headings
    protected variable col_

    # preview URLs for selected object
    protected variable preview_url_ {}

    # more-info URLs for selected object
    protected variable more_url_ {}

    # pipe to read feedback during HTTP transfer
    protected variable rfd_

    # pipe to write feedback during HTTP transfer
    protected variable wfd_

    # current state: normal, disabled (i.e.: waiting)
    protected variable state_ {normal}

    # log file handle (used to log URLs)
    protected variable logfile_name_ 

    # flag: set at end of constructor
    protected variable initialized_ 0
    
    # index of this object in the instances_ array
    protected variable instance_idx_ {}

    # name of File menu widget
    protected variable file_menu_

    # name of Edit menu widget
    protected variable edit_menu_

    # name of Options menu widget
    protected variable options_menu_

    # name of Name Server menu widget
    protected variable ns_menu_

    # flag: set when the column headings are changed between TCS and normal
    protected variable reset_columns_ {0}

    # currently selected object (Id field in row)
    protected variable object_name_ {}

    # count used for filename generation
    protected variable count_ 0

    # flag: true if catalog is not an image server
    protected variable iscat_ 1

    # flag: true if searching is allowed
    protected variable search_state_ normal

    # name of temporary image file, deleted when new image is
    # obtained, or when object destroyed.
    protected variable tempimage_ {}

    # -- common variables (common to all instances of this class) --
    
    # C++ astrocat object used by static member procs
    protected common astrocat_ [astrocat ::cat::.astrocat]
    
    # array mapping catalog name to widget/class name
    protected common instances_

    # instance count
    protected common n_instances_ 0

    # current instance name
    protected common current_instance_ {}

    # array(TopLevelWidget instance) of command used to update the
    # Data-Servers menu. This is used to make it posible to update
    # all instances of this menu in various top level windows.
    protected common catalog_menu_info_

    # flag: set to 1 after we checked for a proxy server
    protected common checked_proxies_ 0
}
