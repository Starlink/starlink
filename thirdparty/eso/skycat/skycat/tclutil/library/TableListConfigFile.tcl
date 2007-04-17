# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: TableListConfigFile.tcl,v 1.1.1.1 2006/01/12 16:40:45 abrighto Exp $"
#
# TableListConfigFile.tcl - Class for managing config info for TableList widget
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created
# pbiereic        04/11/03   Workaround bug in tcl 8.4.3 (SourceForge Request ID 835020)

itk::usual TableListConfigFile {}

# TableListConfigFile is an itcl class (no windows) for managing the config
# file for the TableList widget.
# See also TableListConfig(n) for the user interface to this class.

itcl::class util::TableListConfigFile {
    # dummy, just to catch Destroy event...
    inherit util::FrameWidget
    

    # constructor: load the config file if it exists, or set the default
    # values

    constructor {args} {
	eval itk_initialize $args
	set_defaults
    }


    # save the current information to the given file for later 
    # loading (with "load" below)

    public method save {file} {
	if {[catch {set fd [open $file w]} msg]} {
	    warning_dialog "couldn't save layout information in file \"$file\": $msg" $w_
	    return
	}
	
	# output the header and list of column headings 
	puts $fd $magic_
	puts $fd $headings_
	puts $fd [list $sort_by_ $sort_order_ $sort_type_]

	# make a list with all the headings and options in column order
	set list {}
	foreach i $order_ {
	    lappend list [list $i \
		    $options_($i,Show) \
		    $options_($i,Align) \
		    $options_($i,Wildcard) \
		    $options_($i,Separator) \
		    $options_($i,Precision)]
	}
	puts $fd $list
	close $fd
    }

    
    # load the user config file (created by save) if one exists,
    # otherwise set the default values. 

    public method load {file} {
	global ::env

	if {![file exists $file]} {
	    error "file $file does not exist"
	    return
	}

	# read the file
	set fd [open $file]

	if {"[gets $fd]" != "$magic_"} {
	    set_defaults
	} else {
	    set saved_headings_ [gets $fd]
	    if {"$headings_" != "" && "$headings_" != "$saved_headings_"} {
		set_defaults
	    } else {
		# sorting options
		lassign [gets $fd] sort_by_ sort_order_ sort_type_
		# XXX needed for bug in tcl 8.4.3
		set bug "$sort_by_ $sort_order_ $sort_type_"
		
		# headings and column options
		set order_ {}
		foreach i [gets $fd] {
		    lassign $i name show align wildcard sep prec
		    lappend order_ $name
		    set options_($name,Align) $align
		    set options_($name,Show) $show
		    set options_($name,Wildcard) $wildcard
		    set options_($name,Separator) $sep
		    set options_($name,Precision) $prec
		}
	    }
	}
	close $fd
    }

    
    # set the option value for the given heading name
    # 
    # Options: 
    #  Show (bool)         - display or don't display the column
    #  Align (Left,Right)  - for left or right justify
    #  Separator           - set the separator string (goes after col)
    #  Wildcard            - only show rows where wildcard matches
    #  Precision           - number of places after the decimal for floating point columns

    public method set_option {name option value} {
	set options_($name,$option) $value
    }

    
    # return the value of the given option for the given heading name
    # (see comments above for set_option...)

    public method get_option {name option} {
	return $options_($name,$option)
    }


    # read option values from the named array

    public method set_options {ar} {
	upvar $ar opts
	catch {unset options_}
	foreach i [array names opts] {
	    set options_($i) $opts($i)
	}
    }

    
    # write the current option values to the named array

    public method get_options {ar} {
	upvar $ar opts
	catch {unset opts}
	foreach i [array names options_] {
	    set opts($i) $options_($i)
	}
    }


    # set the default values in case there is no config
    # file or it was out of date
    # 
    # If force is 1, set even values that are already set.

    public method set_defaults {{force 0}} {
	set order_ [set saved_headings_ $headings_]
	foreach name $headings_ {
	    if {$force || ![info exists options_($name,Align)]} {
		set options_($name,Align) Left
	    }
	    if {$force || ![info exists options_($name,Show)]} {
		set options_($name,Show) 1
	    }
	    if {$force || ![info exists options_($name,Wildcard)]} {
		set options_($name,Wildcard) $match_any_
	    }
	    if {$force || ![info exists options_($name,Separator)]} {
		set options_($name,Separator) {}
	    }
	    if {$force || ![info exists options_($name,Precision)]} {
		set options_($name,Precision) 0
	    }
	}
    }


    # return a list of column headings in the order they are displayed
    
    public method get_order {} {
	return $order_
    }

    
    # -- options --

    # column headings from table
    itk_option define -headings headings Headings {} {
	set headings_ $itk_option(-headings)
	if {"$headings_" != "$saved_headings_"} {
	    set_defaults
	}
    }

    # list of headings, in the order they (and the columns)
    # should be displayed
    itk_option define -order order Order {} {
	set order_ $itk_option(-order)
    }

    # -- sort options --

    # list of col index: sort table based on given columns 
    # (empty means don't sort)
    itk_option define -sort_by sort_by Sort_by {} {
	set sort_by_ $itk_option(-sort_by)
    }

    # set direction of sort: may be one of (increasing, decreasing) 
    itk_option define -sort_order sort_order Sort_order {increasing} {
 	set sort_order_ $itk_option(-sort_order)
    }

    # set type of sort key: may be one of (ascii, integer, real) 
    itk_option define -sort_type sort_type Sort_type {ascii} {
  	set sort_type_ $itk_option(-sort_type)
    }

    # -- match options --

    # flag: if true, use regular exprs for matching, otherwise use wildcards
    itk_option define -use_regexp use_regexp Use_regexp {0} {
	if {$itk_option(-use_regexp)} {
	    set match_any_ ".*"
	} else {
	    set match_any_ "*"
	}
    }


    # -- protected members --

    # array(heading,opt) of option values
    protected variable options_

    # magic string for config files to keep track of versions
    protected variable magic_ {-- table list config file v1.4 --}

    # previous column headings - read from config_file
    protected variable saved_headings_ {}

    # set to 1 after initialization
    protected variable initialized_ 0

    # string used to match any string
    protected variable match_any_ {*}

    # table column headings
    protected variable headings_ {}

    # sort order
    protected variable order_ {}

    # sort keys
    protected variable sort_by_ {}

    # direction of sort: may be one of (increasing, decreasing) 
    protected variable sort_order_ {increasing}

    # set type of sort key: may be one of (ascii, integer, real) 
    protected variable sort_type_ {ascii}
}

