# E.S.O. - VLT project/ESO Archive
# "@(#) $Id: CatalogInfo.tcl,v 1.2 2006/01/20 23:36:27 abrighto Exp $"
#
# CatalogInfo.tcl - widget for browsing through a hierarchical list of catalogs
#
# See man page CatalogInfo(n) for a complete description.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 14 Oct 97   created


itk::usual CatalogInfo {}


# The CatalogInfo class defines a user interface for browsing through a 
# hierarchical list of catalogs. The list of catalogs is defined in catalog 
# config files at various hosts on the network or in local catalog config files.
# This widget allows you to choose, from various catalog servers on the net,
# which catalogs to keep in a personal default catalog list.

itcl::class cat::CatalogInfo {
    inherit util::TopLevelWidget


    # constructor

    constructor {args} {
	eval itk_initialize $args
    }

    
    # destructor - delete C++ based objects so that the temp image
    # files are deleted

    destructor {
	catch {destroy $tree_}
    }

    
    # called after options have been evaluated

    protected method init {} {
	global ::cat_library

	# if running stand-alone, setup X defaults
	if {$itk_option(-standalone)} {
	    cat::setXdefaults
	}

	wm title $w_ "Catalog Directory ($itk_option(-number))"
	wm iconname $w_ "Catalog Directory"

	# set the strings to display for server types
	set serv_types_(local) "local catalog"
	set serv_types_(directory) "directory"
	set serv_types_(catalog) "catalog"
	set serv_types_(archive) "archive"
	set serv_types_(imagesvr) "image server"
	set serv_types_(namesvr) "name server"

	set images_(local) [image create photo -file $cat_library/local.xpm]
	set images_(directory) [image create photo -file $cat_library/nsfolder.xpm]
	set images_(open_directory) [image create photo -file $cat_library/nsopenfold.xpm]
	set images_(catalog) [image create photo -file $cat_library/catalog.xpm]
	set images_(archive) [image create photo -file $cat_library/archive.xpm]
	set images_(imagesvr) [image create photo -file $cat_library/imagesvr.xpm]
	set images_(namesvr) [image create photo -file $cat_library/namesvr.xpm]	

	# do window layout
	if {[layout_dialog]} {
	    destroy $w_
	    return
	}

	# add a short help window
	make_short_help

	set initialized_ 1
    }

    
    # add the menu bar

    protected method add_menubar {} {
	TopLevelWidget::add_menubar
	set m [add_menubutton File "Display File menu"]
	set file_menu_ $m
	
	add_menuitem $m command "Load config file..." \
	    {Load a local catalog configuration file and insert it in the catalog list.} \
	    -command [code $this load_config_file]
	
	add_menuitem $m command "Save to config file..." \
	    {Save catalog configuration information to a file.} \
	    -command [code cat::CatalogInfo::save_as $w_]

	$m add separator

	add_menuitem $m command "Close" \
	    {Apply changes and close this window} \
	    -command [code $this close]
	
	cat::AstroCat::add_catalog_menu $w_ $itk_option(-id) \
	    $itk_option(-classname) $itk_option(-debug)
    }

    
    # Apply the current changes, save to default file, call $command

    public method apply {{ask 0}} {
	if {$save_needed_} {
	    if {!$ask || [confirm_dialog \
			      "Do you want to save your changes in the default catalog \
                               config file (~/.skycat/skycat.cfg)?" $w_]} {
		save {} $w_ $ask
	    }
	    if {"$command" != ""} {
		eval $command
	    }
	}
    }

    
    # apply changes and close the window 

    public method close {{ask 0}} {
	# apply $ask
	quit
    }

    
    # pop up a dialog to save the current info to a catalog config file
    # Note: this is defined as a proc so that it can be accessed by other
    # widgets without this widget being created first. $w should be the main 
    # window of the calling widget.
    
    public proc save_as {w} {
	set file [filename_dialog [pwd] * $w]
	if {"$file" != ""} {
	    save $file $w
	}
    }

   
    # save current config info to the given file, or if file is an empty
    # string, to the default config file.

    public proc save {file w {ask 1}} {
	global ::env
	set default_file $env(HOME)/.skycat/skycat.cfg
	if {"$file" == ""} {
	    set file $default_file
	}

	set update 0
	if {"$file" == "$default_file"} {
	    incr update
	} elseif {$ask && [file isfile $file]} {
	    if {![confirm_dialog "File: `[file tail $file]' exists \
                                      - Do you want to overwrite it ?" $w]} {
		return
	    }
	}

	if {[file isdir $file]} {
	    error_dialog "File: `[file tail $file]' is a directory" $w
	    return
	}
	
	# get the default catalog list
	if {[catch {set catalog_list [lsort [$astrocat_ info {}]]} msg]} {
	    error_dialog $msg $w
	    return
	}

	# use a temp file in case of errors
	if {[catch {set fd [open $file.tmp w]} msg]} {
	    error_dialog $msg $w
	    return
	}
	
	puts $fd "# Catalog config file"
	puts $fd "# This file was automatically generated by Skycat on [clock format [clock seconds]]"
	puts $fd ""

	foreach name $catalog_list {
	    if {[catch {set entry [$astrocat_ entry get $name]} msg]} {
		error_dialog $msg $w
		::close $fd
		catch {file delete $file.tmp}
		return
	    }
	    puts $fd ""
	    foreach line $entry {
		lassign $line key value
		if {"$value" != ""} {
		    if {"$key" == "symbol" || "$key" == "search_cols"} {
			puts $fd [format "%-15s %s" $key: [join $value " : "]]
		    } else {
			puts $fd [format "%-15s %s" $key: $value]
		    }
		}
	    }
	}
	::close $fd

	# backup $file, then rename $file.tmp to $file
	if {[file exists $file]} {
	    if {[catch {file rename -force $file $file.BAK} msg]} {
		error_dialog $msg $w
		catch {file delete $file.tmp}
		return
	    }
	}
	if {[catch {file rename -force $file.tmp $file} msg]} {
	    error_dialog $msg $w
	    catch {file delete $file.tmp}
	    return
	}

	set save_needed_ 0

	# tell widgets to update the catalog menus
	if {$update} {
	    cat::AstroCat::update_catalog_menus
	}
    }


    # do the dialog window layout
    
    protected method layout_dialog {} {
	add_menubar
	add_tree_frame
	add_dialog_buttons
	
	init_tree

	return 0
    }


    # add a frame for displaying a tree of catalog servers and catalogs
    # inside the given frame and side

    protected method add_tree_frame {} {
	set tf [frame $w_.tf]
	set tree_ $tf.tree
	set vs [scrollbar $w_.vs -orient vertical -command "$tree_ yview"]
	set hs [scrollbar $w_.hs -orient horizontal -command "$tree_ xview"]

	set rootdir_ [$astrocat_ root]
	
	treeview $tree_ \
	    -separator $sep_ \
	    -autocreate no \
	    -allowduplicates no \
	    -hideroot yes \
	    -selectcommand [code $this select_catalog] \
	    -yscrollcommand "$vs set" \
	    -xscrollcommand "$hs set"


	$tree_ column configure treeView -text Name
	$tree_ column insert end type -text Type -justify left

	$tree_ sort auto yes

	table $tf \
	    0,0 $tree_ -fill both \
	    0,1 $vs -fill y \
	    1,0 $hs -fill x
	table configure $tf c1 r1 r2 -resize none
	pack $tf -side top -fill both -expand 1 
    }


    # initialize the tree of catalogs

    protected method init_tree {} {
	# add the root of the tree
	set id [add_node {} $rootdir_]
	open_catalog_directory $rootdir_ $id
    }

    
    # Add a node to the catalog tree. "path" is the path to the parent 
    # of the new node in the format "a${sep_}b${sep_}c" where a is the 
    # root and c is immediate parent of the new node. "name" is the name 
    # of the new node and the text to display.
    
    protected method add_node {path name} {
	if {[string first $sep_ $name] != -1} {
	    puts "invalid catalog '$name', contains invalid char: '$sep_'"
	    return
	}

	if {"$path" == ""} {
	    set newpath $name
	} else {
	    set newpath "$path${sep_}$name"
	}
	set text $name
	
	set dirPath [split $path $sep_]

	# get server type and display string for it
	if {[catch {set serv_type [$astrocat_ servtype $name $dirPath]} msg]} {
	    error_dialog $msg $w_
	    return
	}
	if {[info exists serv_types_($serv_type)]} {
	    set type $serv_types_($serv_type)
	} else {
	    set type $serv_type
	}

	# set icon to display
        if {[catch {set image $images_($serv_type)}]} {
	    set image $images_(local)
	}
	if {"$serv_type" == "local"} {
	    set text [file tail $name]
	}

	# add the node and server type
	set node [$tree_ insert end $newpath]
	$tree_ entry configure $node -data "type $serv_type"

	$tree_ entry configure $node \
	    -icons "$image $image" \
	    -activeicons "$image $image"
	
	$tree_ bind $node <Double-ButtonPress-1> [code $this node_action $node $path $name $serv_type]

	return $node
    }


    # called for double click on a tree node 

    protected method node_action {id path name serv_type} {
	if {"$serv_type" == "directory"} {
	    open_catalog_directory $path${sep_}$name $id
	} else {
	    open_catalog
	}
    }

    
    # called when a tree node is opened 

    protected method open_catalog_directory {path id} {
	# get name and type of selected catalog
	if {"$path" == "$rootdir_"} {
	    set name $rootdir_
	    set dirPath ""
	} else {
	    set list [split $path $sep_]
	    set name [lindex $list end]
	    set dirPath [lrange $list 0 [expr [llength $list]-2]]
	}

	# catalog directory - add list of catalogs, or show existing list
	set children [$tree_ entry children $id]
	if {[llength $children] == 0} {
	    # get and display list of catalogs under this node
	    busy {
		set dirPath [split $path $sep_]
		set status [catch {set catalog_list [lsort [$astrocat_ info {} $dirPath]]} msg]
	    }
	    if {$status} {
		error_dialog $msg $w_
		return
	    }
	    foreach i $catalog_list {
		add_node $path $i
	    }
	    $tree_ open $id
	    
	    # change the directory icon
	    $tree_ entry configure $id \
		-opencommand [code $this opened_catalog_directory $id] \
		-closecommand [code $this closed_catalog_directory $id]
	    opened_catalog_directory $id
	}

	# scroll so that the selected directory is at the top of the window
	#$tree_ yview $id
    }

    # Called when a catalog directory node is opened

    protected method opened_catalog_directory {id} {
	set image $images_(open_directory)
	$tree_ entry configure $id \
	    -icons "$image $image" \
	    -activeicons "$image $image"
    }

    # Called when a catalog directory node is closed

    protected method closed_catalog_directory {id} {
	set image $images_(directory)
	$tree_ entry configure $id \
	    -icons "$image $image" \
	    -activeicons "$image $image"
    }

    # add the dialog button frame

    protected method add_dialog_buttons {} {
	# dialog buttons
	pack [frame $w_.buttons -borderwidth 2 -relief raised] \
	    -side top -fill x
	pack \
	    [button $w_.add \
		 -text "Add" \
		 -state disabled \
		 -command [code $this add_catalog]] \
	    [button $w_.remove \
		 -text "Remove" \
		 -state disabled \
		 -command [code $this remove_catalog]] \
	    [button $w_.open \
		 -text "Open" \
		 -state disabled \
		 -command [code $this open_catalog]] \
	    [button $w_.close \
		 -text "Close" \
		 -command [code $this close]] \
	    -side left -expand 1 -padx 8m -pady 2m -in $w_.buttons
    }


    # called when a catalog is selected in the catalog list.

    protected method select_catalog {} {
	set id [$tree_ curselection]
	if {$id == ""} {
	    return
	}
	set path [$tree_ get -full $id] 

	set catalog_id_ $id
	set catalog_path_ [split $path $sep_]
	set catalog_dir_ [lrange $catalog_path_ 0 [expr [llength $catalog_path_]-2]]
	set catalog_ [lindex $catalog_path_ end]

	if {[catch {set serv_type [$astrocat_ servtype $catalog_ $catalog_dir_]} msg]} {
	    error_dialog $msg $w_
	    return
	}

	if {"$serv_type" == "directory"} {
	    $w_.open config -state disabled
	} else {
	    $w_.open config -state normal
	}
	
	if {"$rootdir_${sep_}$catalog_" == "$path"} {
	    $w_.add config -state disabled
	    $w_.remove config -state normal
	} else {
	    $w_.add config -state normal
	    $w_.remove config -state disabled
	}
    }


    # open a window to search the selected catalog

    protected method open_catalog {args} {
	if {[catch {set serv_type [$astrocat_ servtype $catalog_ $catalog_dir_]} msg]} {
	    error_dialog $msg $w_
	    return
	}
	
	cat::AstroCat::new_catalog \
	    $catalog_ \
	    $itk_option(-id) \
	    $itk_option(-classname) \
	    $itk_option(-debug) \
	    0 \
	    $serv_type \
	    $itk_option(-callerw) \
	    $catalog_dir_
    } 


    # add a short help window and set the help texts
    
    protected method make_short_help {} {
	TopLevelWidget::make_short_help

	add_short_help $tree_ \
	    "Catalog list: {bitmap b1} = open and display catalog server contents, \
             double-click to open a catalog window"

	add_short_help $w_.add {Add the selected catalog to the default list}
	add_short_help $w_.remove {Remove the selected catalog from the default list}
	add_short_help $w_.open {Open the selected catalog for searching}
	add_short_help $w_.close {Close: close this window}
    }

    
    # add the selected catalog to the catalog list

    protected method add_catalog {} {
	if {[catch {set e [$astrocat_ entry get $catalog_ $catalog_dir_]} msg]} {
	    error_dialog $msg $w_
	    return
	}
	
	if {[catch {set shortName [$astrocat_ shortname $catalog_ $catalog_dir_]} msg]} {
	    error_dialog $msg $w_
	    return
	}
	
	# get the current default catalog list
	if {[catch {set catalog_list [lsort [$astrocat_ info {}]]} msg]} {
	    error_dialog $msg $w_
	    return
	}

	# check for duplicates
	foreach i $catalog_list {
	    if {"$i" == "$catalog_"} {
		if {! [confirm_dialog \
			   "\"$catalog_\" is already in the default catalog list. \
                            Do you want to replace it?"]} {
		    return
		}
		# update the entry definition
		if {[catch {$astrocat_ entry set $e $catalog_ $rootdir_} msg]} {
		    error_dialog $msg $w_
		    return
		}
		incr save_needed_
		return
	    } else {
		if {[catch {set s [$astrocat_ shortname $i]} msg]} {
		    error_dialog $msg $w_
		    return
		}
		if {"$s" == "$shortName"} {
		    error_dialog "short name '$s' for '$catalog_' is not unique" $w_
		    return
		}
	    }
	}

	# add the catalog to the default list
	if {[catch {$astrocat_ entry add $e} msg]} {
	    error_dialog $msg $w_
	    return
	}
	
	# add the new entry to the tree (in sort order)
	insert_node $catalog_

	incr save_needed_
	apply
    }


    # insert the given catalog in the tree under the root, if it is
    # not already there.

    public method insert_node {name} {
	catch {add_node $rootdir_ $name}
    }


    # remove the selected catalog from the catalog list

    protected method remove_catalog {} {
	# if this is a directory...
	if {[catch {set serv_type [$astrocat_ servtype $catalog_ $catalog_dir_]} msg]} {
	    error_dialog $msg $w_
	    return
	}

	if {[catch {$astrocat_ entry remove $catalog_} msg]} {
	    error_dialog $msg $w_
	    return
	}

	$tree_ delete $catalog_id_

	$w_.add config -state disabled
	$w_.remove config -state disabled
	$w_.open config -state disabled

	incr save_needed_
	apply
    }
    

    # Pop up a dialog to open and load a catalog config file

    protected method load_config_file {} {
	set file [filename_dialog [pwd] * $w_]
	if {"$file" != ""} {
	    if {[file isfile $file]} {
		if {[file isdir $file]} {
		    error_dialog "File: `[file tail $file]' is a directory" $w_
		    return
		}
	    }
	    set name [input_dialog \
			  "Please enter a descriptive name for the new catalog directory:" \
			  $w_]
	    if {"$name" == ""} {
		set name [file rootname [file tail $file]]
	    }
	    if {[catch {$astrocat_ load $file $name} msg]} {
		error_dialog $msg $w_
	    }
	    insert_node $name
	    incr save_needed_
	    apply
	}
    }

   
    # -- options --

    # command to eval when done
    public variable command {}

    # debugging flag
    itk_option define -debug debug Debug 0

    # Optional unique id, used in searching for already existing catalog widgets.
    itk_option define -id id Id ""

    # Optional: name of caller's top level widget.
    itk_option define -callerw callerw Callerw ""

    # name of a subclass of AstroCat to use to create new catalog widgets
    itk_option define -classname classname Classname ""

    # font to use for labels
    itk_option define -labelfont labelFont LabelFont -Adobe-helvetica-bold-r-normal-*-12*

    # font to use for values
    itk_option define -valuefont valueFont ValueFont -adobe-courier-medium-r-*-*-*-120-*-*-*-*-*-*

    # set the width for  displaying labels
    itk_option define -labelwidth labelWidth LabelWidth 10

    # set the anchor for labels
    itk_option define -anchor anchor Anchor e

    # -- protected variables --

    # tree widget
    protected variable tree_ 

    # array(serv_type) of server type text to display
    protected variable serv_types_

    # array(serv_type) of image to display in tree node
    protected variable images_

    # name of the root catalog directory
    protected variable rootdir_

    # name of currently selected catalog
    protected variable catalog_

    # id of currently selected catalog
    protected variable catalog_id_

    # full path name of currently selected catalog
    protected variable catalog_path_

    # currently selected catalog directory (as a list of catalog directories in the path)
    protected variable catalog_dir_

    # set to true if the catalog configuration has been edited and needs to be saved
    protected variable save_needed_ 0

    # separator char for tree widget (used in catalog path names)
    protected variable sep_ "^"

    # C++ astrocat object used by static member procs
    common astrocat_ [astrocat ::cat::.cataloginfo]
}
