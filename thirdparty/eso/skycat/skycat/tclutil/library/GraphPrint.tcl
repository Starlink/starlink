# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: GraphPrint.tcl,v 1.5 1998/10/28 17:46:37 abrighto Exp $"
#
# GraphPrint.tcl - Print dialog box for printing the contents of a graph
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created

itk::usual GraphPrint {}

# This widget defines a print dialog box for printing the contents of a graph.

itcl::class util::GraphPrint {
    # similar to canvas...
    inherit util::CanvasPrint


    # create a new instance of this class

    constructor {args} {
	eval itk_initialize $args
    }

     
    # print the contents of the graph as postscript to the given file descriptor
    
    public method print {fd} {
	global ::$w_.color ::$w_.rotate ::$w_.colormap ::$w_.fit_to_page

	# try to fit on a page
	set width [lindex [$itk_option(-graph) config -width] 4]
	set height [lindex [$itk_option(-graph) config -height] 4]

        $itk_option(-graph) postscript configure \
		-colormode [set $w_.color] \
		-paperwidth [$w_.pagesize.width get] \
		-paperheight [$w_.pagesize.height get] \
		-landscape [set $w_.rotate] \
		-padx 0 \
		-pady 0 \
		-center 1 \
		-maxpec [expr ![set $w_.fit_to_page]]

	set cmd [list $itk_option(-graph) postscript output]
	puts $fd [eval $cmd]
    }
    
    # use PrintDialog::ok to get the fd (CanvasPrint::ok returns filename)

    public method ok {args} {
	PrintDialog::ok $args
    }

    # Public data

    # graph to print
    itk_option define -graph graph Graph {}

}
