# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: LabelValue.tcl,v 1.3 2005/02/02 01:43:02 brighton Exp $"
#
# LabelValue.tcl - Widget displaying a label and a selectable value
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created
# Peter W. Draper 19 Dec 05  Switch to "readonly" state, which better
#                            matches the original aim to be selectable
#                            but not editable.


itk::usual LabelValue {}

# This widget displays a label and a value (also a label(n)) and
# implements convenient methods for accessing and modifying the label
# and the value.

itcl::class util::LabelValue {
    inherit util::LabelEntry

    # constructor: create a new LabelValue widget

    constructor {args} {
	eval itk_initialize $args

        # entry widget that can be selected, but not edited, has same
        # background colour as normal form
        $itk_component(entry) config -state readonly -readonlybackground {}
    }
}
