# E.S.O. - VLT project/ESO Archive
# @(#) $Id: EnterObject.tcl,v 1.1.1.1 2002/04/04 20:11:47 brighton Exp $
#
# EnterObject.tcl - Widget for entering data to add to a local catalog.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 26 Jun 96   created


itk::usual EnterObject {}

# EnterObject is a dialog widget for entering object data to add to a 
# local catalog.

itcl::class cat::EnterObject {
    inherit util::EntryForm


    # constructor

    constructor {args} {
	eval itk_initialize $args
    }


    # called after options have been evaluated

    protected method init {} {
	EntryForm::init
	wm title $w_ "Enter Object ($itk_option(-number))"

	if {"$itk_option(-image)" != ""} {
	    $itk_component(buttons) append \
		"Pick object..." \
		[code $this pick_object]
	}
    }

    
    # this method is called to pop up a dialog to allow the user to pick 
    # an object in the image

    public method pick_object {} {
	if {"$itk_option(-image)" != ""} {
	    if {[catch {set list [$itk_option(-image) pick_dialog [code $this picked_object]]} msg]} {
		error_dialog $msg
		return
	    }
	}
    }
    
    
    # this is called when an object has been selected with the pick dialog

    public method picked_object {list} {
	lassign $list x y ra dec equinox fwhmX fwhmY symetry object background
	set_entry ra $ra
	set_entry dec $dec
    }

    
    # -- options --

    # optional image handle (itcl class RtdImage or derived class)
    itk_option define -image image Image {}
    
    # -- protected vars --
}



    
