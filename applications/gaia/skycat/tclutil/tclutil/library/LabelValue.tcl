# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: LabelValue.tcl,v 1.4 1998/10/28 17:46:39 abrighto Exp $"
#
# LabelValue.tcl - Widget displaying a label and a selectable value
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created



itk::usual LabelValue {}

# This widget displays a label and a value (also a label(n)) and
# implements convenient methods for accessing and modifying the label
# and the value.

itcl::class util::LabelValue {
    inherit util::LabelEntry

    # constructor: create a new LabelValue widget

    constructor {args} {
	#itk_option remove LabelEntry::state LabelWidget::state
	eval itk_initialize $args
	$itk_component(entry) config -state disabled
    }
}
