# GraphPrint.tcl - Print Dialog Box for printing the contents of a graph
#
# usage: GraphPrint
#
# Copyright (C) 1994 Allan Brighton (abrighto@eso)
#
# Peter W. Draper 16/9/97: Modified to correct postscript options to blt2.1.
#
# "@(#) $Id$"

itk::usual GraphPrint {}

class util::GraphPrint {
    # similar to canvas...
    inherit util::CanvasPrint


    # create a new instance of this class

    constructor {args} {
	eval itk_initialize $args
    }

     
    # print the contents of the graph as postscript to the given file descriptor
    
    method print {fd} {
	global ::$w_.color ::$w_.rotate ::$w_.colormap

	# try to fit on a page
	set width [lindex [$itk_option(-graph) config -width] 4]
	set height [lindex [$itk_option(-graph) config -height] 4]
	
	set cmd [list $itk_option(-graph) postscript output \
		     -colormode [set $w_.color] \
		     -center 0 \
		     -paperwidth 8.5i \
		     -paperheight 11i \
		     -landscape [set $w_.rotate]]

	puts $fd [eval $cmd]
    }
    

    # Public data

    # graph to print
    itk_option define -graph graph Graph {}

}


