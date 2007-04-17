# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: TableListPrint.tcl,v 1.1.1.1 2006/01/12 16:40:37 abrighto Exp $"
#
# TableListPrint.tcl - Print dialog box for printing the contents of a TableList
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created


itk::usual TableListPrint {}

# TableListPrint is a print dialog box for printing the contents of a TableList.

itcl::class util::TableListPrint {
    inherit util::PrintDialog

    # create a new TableListPrint 

    constructor {args} {
	eval itk_initialize $args
    }


    # this method is called after all options have been evaluated

    protected method init {} {
	util::PrintDialog::init
	
	config -suffix ""

	if {$itk_option(-choose)} {
	    make_choice_frame
	}
    }

    
    # add a frame to let the user choose which columns to print

    protected method make_choice_frame {} {
	# print options frame
	itk_component add options {
	    frame $w_.options -borderwidth 3 -relief groove
	}
	pack $itk_component(options) \
	    -side top -fill x -expand 1 -in $itk_component(config)

	# get and display a list of visible headings
	set headings_ [$itk_option(-table) cget -headings]
	set showing {}
	foreach i $headings_ {
	    if {[$itk_option(-table) get_option $i Show]} {
		lappend showing $i
	    }
	}

	# LabelCheck(n) widget displaying table columns to print
	itk_component add cols {
	    util::LabelCheck $itk_component(options).cols \
		-text "Table Columns to print:" \
		-orient vertical \
		-cols 3 \
		-choice $headings_ \
		-value $showing
	}
	pack $itk_component(cols) -side top -fill x
    }

    
    # This method is redefined here from the parent class to
    # print the contents of the TableList with specified columns
    
    public method print {fd} {
	if {$itk_option(-choose)} {
	    foreach i $headings_ {
		set save($i) [$itk_option(-table) get_option $i Show]
	    }
	    $itk_option(-table) set_options $headings_ Show 0
	    foreach i [$itk_component(cols) get] {
		$itk_option(-table) set_option $i Show 1
	    }

	    $itk_option(-table) calculate_format
	    $itk_option(-table) print $fd

	    foreach i $headings_ {
		$itk_option(-table) set_option $i Show $save($i)
	    }
	    $itk_option(-table) calculate_format
	} else {
	    $itk_option(-table) print $fd
	}
    }

    

    # -- options -- 

    # handle of the TableList class that created this dialog
    # (used to access the current values there...)
    itk_option define -table table Table {}

    # flag: if true, add a widget for choosing the columns to print
    itk_option define -choose choose Choose 1


    # -- Protected Variable variables --
    
    # table column headings
    protected variable headings_
}


