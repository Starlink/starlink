# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: ListDialog.tcl,v 1.7 1998/10/28 17:46:39 abrighto Exp $"
#
# ListDialog.tcl - Dialog to display a listbox window and some text
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created

itk::usual ListDialog {}

# A ListDialog is a dialog to display a listbox window and some text.

itcl::class util::ListDialog {
    inherit util::DialogWidget

    # create the dialog

    constructor {args} {
	# listbox widget below the message
	itk_component add list {
	    util::ListboxWidget $itk_component(ext).list
	} {
	    rename -width -listboxwidth listboxWidth ListboxWidth
	    rename -height -listboxheight listboxHeight ListboxHeight
	    rename -font -listboxfont listboxFont ListboxFont
	}
	pack $itk_component(list) -side top -fill both -expand 1
	
	set listbox_ [$itk_component(list) component listbox]
	bind $listbox_ <Double-ButtonPress-1> [code $this set_result]
	
	eval itk_initialize $args
    }


    # this method may be redefined in a derived class to change the value
    # that is returned from activate (default is the number of the button selected)

    protected method set_result {} {
	global ::$variable_
	return [set $variable_ [lindex [$itk_component(list) get_selected] 0]]
    }

   
    # -- options --

    # list of items to choose from
    itk_option define -choice choice Choice {} {
	$itk_component(list) set_contents $itk_option(-choice) 
    }

    protected variable listbox_
}

