# E.S.O. - VLT project/ESO Archive
# "@(#) $Id: Chooser.tcl,v 1.1.1.1 2006/01/12 16:40:56 abrighto Exp $"
#
# Chooser.tcl - A simple widget for selecting items based on filenames
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created

itk::usual Chooser {}

# Chooser is a simple itcl widget for selecting items based on files
# with a given suffix in a given directory.

itcl::class util::Chooser {
    inherit util::FrameWidget

    #  create a new  Chooser widget

    constructor {args} {
	itk_option add hull.borderwidth hull.relief

	# ListboxWidget(n) for displaying choice
	itk_component add list {
	    util::ListboxWidget $w_.list \
		-exportselection 0 \
		-borderwidth 2 \
		-relief sunken
	} {
	    keep -title -vscroll -hscroll -width \
		-background -foreground -font -height
	}

	pack $itk_component(list) -fill both -expand 1
	set listbox_ [$itk_component(list) component listbox]
	bind $listbox_ <ButtonRelease-1> "+[code $this choose]"

	eval itk_initialize $args
    }

    
    # set new values

    protected method choose {} {
	if {"$itk_option(-command)" != ""} {
	    set sel [lindex [$itk_component(list) get_selected] 0]
	    eval "$itk_option(-command) $itk_option(-dir)/$sel.$itk_option(-suffix)"
	}
    }

    
    # change the selection to the given file
    
    public method set_choice {file} {
	set n 0
	foreach i $itk_option(-files) {
	    set name [file rootname [file tail $i]]
	    if {"$name" == "$file"} {
		$itk_component(list) select_row $n
		break
	    }
	    incr n
	}
	choose
    }


    # -- options --

    # tcl command to evaluate with selected file name
    itk_option define -command command Command {} 

    # directory for files
    itk_option define -dir dir Dir {.} 

    # suffix for files
    itk_option define -suffix suffix Suffix {} 

    # list of files (complete pathnames, usually output of [glob $dir/*.$suffix]
    itk_option define -files files Files {} {
	set n 0
	foreach i $itk_option(-files) {
	    set name [file rootname [file tail $i]]
	    $itk_component(list) append $name
	    if {"$name" == "$itk_option(-default)"} {
		$itk_component(list) select_row $n
	    }
	    incr n
	}
    }

    # default selection
    itk_option define -default default Default {} 

   
    # -- protected vars --
    
    # internal listbox widget
    protected variable listbox_
}
