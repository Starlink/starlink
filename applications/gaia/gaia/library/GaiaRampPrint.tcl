#+
#  Name:
#     GaiaRampPrint

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Extends RtdImagePrint to add print the colour ramp, not the main
#     image.

#  Description:
#     This class redefines the image processed to the colour ramp and
#     adds graphics around the edges to show the different levels. The
#     output is always encapsulated and no option to add the footer is
#     provided.

#  Invocations:
#
#        GaiaRampPrint object_name [configuration options]
#
#     This creates an instance of a GaiaRampPrint object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Configuration options:
#     See itk_define statements below.

#  Methods:
#     See method declarations below.

#  Inheritance:
#     rtd::RtdImagePrint

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     26-JUN-1999 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaRampPrint {

   #  Inheritances:
   #  -------------
   inherit rtd::RtdImagePrint

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {
       eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Override set_background to really set the background to the
   #  requested colour (rather than always white). Add a large
   #  surround to remove edge problems.
   method set_background {} {
       lassign [$canvas_ bbox all] xlow ylow xhigh yhigh
       set xlow [expr round($xlow-10)]
       set ylow [expr round($ylow-10)]
       set xhigh [expr round($xhigh+10)]
       set yhigh [expr round($yhigh+10)]
       $canvas_ create rectangle $xlow $ylow $xhigh $yhigh \
	   -fill [$itk_option(-maincanvas) cget -background] \
	   -tags ${this}_back
       $canvas_ lower ${this}_back all
   }

   #  Add the border and numeric labels to the ramp. This is done by adding
   #  a pseudo AST WCS and using the "grid" command.
   method add_border {} {
       $image_ configure -ast_tag "${this}_border"
       $image_ colorramp setwcs $itk_option(-mainimage)
       $image_ plotgrid "edge(1)=top
                         labelup(1)=0
                         labelling=exterior
                         numlab(1)=1
                         numlab(2)=0
                         numlabgap(1)=1.0
                         size(numlab)=1.0
                         width(border)=0.005
                         width(axes)=0.005
                         width(ticks)=0.005
                         majticklen=0.0
                         minticklen=0.0
                         mintick(2)=5
                         border=1
                         grid=0
                         drawtitle=0
                         drawaxes=0
                         textlab=0
                         tickall=0
                         labelunits(1)=0
                         colour(border)=1
                         colour(numlab)=3
                         colour(axes)=1
                         colour(ticks)=0
                         font(numlab)=3"
   }

   #  Print the contents of the canvas to the open filedescriptor
   protected method print {fd} {
       global ::$w_.color ::$w_.rotate $w_.colormap

       set cmd [list $canvas_ postscript \
		    -colormode [set $w_.color] \
		    -rotate [set $w_.rotate]]

       #  Get the offsets that correct for the apparent shift of the
       #  image when zoomed.
       set xoff [expr int([$canvas_ canvasx 0])]
       set yoff [expr int([$canvas_ canvasy 0])]
       set xoff [max $xoff 0]
       set yoff [max $yoff 0]

       #  Add the label border.
       add_border

       #  Use whole printing surface.
       lassign [$canvas_ bbox all] x0 y0 x1 y1

       #  Set the background (use a filled rectangle to simulate this).
       set_background

       #  Set the width, height and corner.
       lappend cmd \
	   -width [expr $x1-$x0+1] \
	   -height [expr $y1-$y0+1] \
	   -x $x0 \
	   -y $y0

       if {"[set $w_.color]" == "mono"} {
	   # you can add to this array, see canvas(n) man page
	   set $w_.colormap(grey) "0.0 0.0 0.0 setrgbcolor"
	   lappend cmd -colormap $w_.colormap
       }

       #  Shift all canvas items so that they align to the image
       #  with its origin of 0,0.
       $canvas_ move all [expr -1.0*$xoff] [expr -1.0*$yoff]
       $canvas_ move $image_ $xoff $yoff
       $canvas_ move print $xoff $yoff

       #  Write postscript into file stream.
       puts $fd [eval $cmd]

       #  Shift everything back to original position.
       $canvas_ move all $xoff $yoff
       $canvas_ move $image_ [expr -1.0*$xoff] [expr -1.0*$yoff]

       #  Remove background.
       remove_background

       #  Remove border.
       $canvas_ delete ${this}_border
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Main rtdimage (displaying image).
   itk_option define -mainimage mainimage MainImage {}

   #  And its canvas.
   itk_option define -maincanvas maincanvas MainCanvas {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
