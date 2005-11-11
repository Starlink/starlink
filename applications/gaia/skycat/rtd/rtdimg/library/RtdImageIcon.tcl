#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: RtdImageIcon.tcl,v 1.2 2005/02/02 01:43:03 brighton Exp $"
#
# RtdImageIcon.tcl - itcl widget to display current image in icon window
# 
# See man page RtdImageIcon(n) for a complete description.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  08 Jun 96  Created

itk::usual RtdImageIcon {}

# RtdImageIcon is an itcl widget used to display the current image in the 
# icon window.

itcl::class rtd::RtdImageIcon {
    inherit util::TopLevelWidget


    #  constructor: create a new instance of this class

    constructor {args} {
	eval itk_initialize $args

	# RtdImage widget
	itk_component add image {
	    rtd::RtdImage $w_.image \
		  -name "IconImage" \
		  -scrollbars 0 \
		  -graphics 0 \
		  -drag_scroll 0 \
		  -displaymode 0 \
		  -fitwidth $itk_option(-width) \
		  -fitheight $itk_option(-height) \
		  -subsample $itk_option(-subsample) \
		  -verbose $itk_option(-verbose) \
                  -usexsync $itk_option(-usexsync) \
		  -usexshm $itk_option(-usexshm) \
		  -show_object_menu 0
	}

	pack $itk_component(image) -side right -anchor n

	[$itk_option(-image) get_image] view add [$itk_component(image) get_image]  0
	bind $this <Map> [code $itk_component(image) center]
    }
  
    
    # -- public vars --
    
    # target RtdImage (itcl widget)
    itk_option define -image image Image {}

    # dimensions of pan frame
    itk_option define -width width Width 48
    itk_option define -height height Height 48

    # flag: if true, pan image is "subsampled" when shrinking, 
    # otherwise the pixels are averaged
    itk_option define -subsample subsample Subsample 1
   
    # X shared memory option
    itk_option define -usexshm useXshm UseXshm 1

    # X synchronisation option
    itk_option define -usexsync useXsync UseXsync 1

    # flag: if true, print diagnostic messages
    itk_option define -verbose verbose Verbose {0}
}
