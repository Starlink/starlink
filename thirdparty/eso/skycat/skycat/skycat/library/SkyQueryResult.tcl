# E.S.O. - VLT project/ESO Archive
# @(#) $Id: SkyQueryResult.tcl,v 1.6 1998/10/28 17:44:48 abrighto Exp $
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


    # -- public variables --

    # name of SkyCat itcl widget
    public variable skycat {}

}


    

