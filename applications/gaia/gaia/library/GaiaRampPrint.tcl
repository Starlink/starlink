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

#  Copyright:
#     Copyright (C) 1999-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     26-JUN-1999 (PWD):
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

   #  Init method. Called after construction is complete.
   public method init {} {
      rtd::RtdImagePrint::init

      wm title $w_ "Print ramp to postscript ($itk_option(-number))"

      #  Remove elements that are not required here.
      pack forget $w_.whole.yes
      blt::blttable forget $w_.pagesize.footer

      #  Add menu to select the font and its size.
      itk_component add lsize {
         util::LabelEntryScale $w_.lsize \
            -text "Label size:" \
            -value 1.0 \
            -labelwidth 10 \
            -valuewidth 5 \
            -from 0.25 \
            -to 4.0 \
            -increment 0.25 \
            -resolution 0.25 \
            -show_arrows 1 \
            -anchor w \
            -command [code $this set_label_size_]
      }
      set lsize 1.0
      set parent [winfo parent $w_]
      $parent add_short_help $itk_component(lsize) \
         {Size of labels added around ramp}
      pack $itk_component(lsize) -side top -fill x -ipadx 1m -ipady 1m

      #  Create a menu that allows the visual selection of one of the
      #  known fonts. Use a LabelMenu widget to host the choice.
      itk_component add lfont {
         util::LabelMenu $w_.lfont \
            -text "Label font:" \
            -relief raised \
            -labelwidth 10 \
            -valuewidth 5
      }
      pack $itk_component(lfont) -side top -fill x -ipadx 1m -ipady 1m
      $parent add_short_help $itk_component(lfont) \
         {Font for the labels added around ramp}

      catch {
         #  Now add all the fonts.
         foreach {index xname desc} $fontmap_ {
            $itk_component(lfont) add \
               -command [code $this set_label_font_ $index] \
               -label $desc \
               -value $xname \
               -font $xname
         }
         set lfont_ 0
      }

      #  Colours, border and font.
      itk_component add lcolour {
         util::LabelMenu $w_.lcolour \
            -text "Label color:" \
            -relief raised \
            -labelwidth 10
      }
      $parent add_short_help $itk_component(lcolour) \
         {Colour for labels added around ramp}
      pack $itk_component(lcolour) -side top -fill x -ipadx 1m -ipady 1m

      #  Now add all the colours.
      foreach {index xname} $colourmap_ {
         $itk_component(lcolour) add \
            -command [code $this set_label_colour_ $index] \
            -label {    } \
            -value $xname \
            -background $xname
      }
      set lcolour_ 0

      itk_component add bcolour {
         util::LabelMenu $w_.bcolour \
            -text "Border color:" \
            -relief raised \
            -labelwidth 10
      }
      $parent add_short_help $itk_component(bcolour) \
         {Colour of border added around the ramp}
      pack $itk_component(bcolour) -side top -fill x -ipadx 1m -ipady 1m

      #  Now add all the colours.
      foreach {index xname} $colourmap_ {
         $itk_component(bcolour) add \
            -command [code $this set_border_colour_ $index] \
            -label {    } \
            -value $xname \
            -background $xname
      }
      set bcolour_ 0
   }

   #  Set the fonts, scales and colours.
   protected method set_label_size_ {size} {
      set lsize_ $size
   }
   protected method set_label_font_ {index} {
      set lfont_ $index
   }
   protected method set_label_colour_ {index} {
      set lcolour_ $index
   }
   protected method set_border_colour_ {index} {
      set bcolour_ $index
   }

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
   #  a pseudo AST WCS and using the "grid" command. Just catch any failures
   #  as these are not fatal, you still get a ramp, just no intensity values.
   method add_border {} {
      catch {
         $image_ configure -ast_tag "${this}_border"
         $image_ colorramp setwcs $itk_option(-mainimage)
         $image_ plotgrid "font(numlab)=$lfont_
                        size(numlab)=$lsize_
                        colour(border)=$bcolour_
                        colour(numlab)=$lcolour_
                        grid=0 border=1 drawtitle=0 drawaxes=0
                        textlab=0 tickall=0 numlab(1)=1 numlab(2)=0
                        edge(1)=top labelup(1)=0 labelling=exterior
                        numlabgap(1)=1.0 width(border)=0.005
                        labelunits(1)=0 majticklen=0.0 minticklen=0.0"
      }
   }

   #  Print the contents of the canvas to the open file descriptor
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

   #  Names of the fonts that we will use and their AST indices.
   #  A text string to very briefly describe the font is also set.
   protected variable fontmap_ $::gaia::astfontmap

   #  Names of the possible colours and their AST index equivalents.
   protected variable colourmap_ {
      0 "#fff" 1 "#000" 2 "#f00" 3 "#0f0" 4 "#00f" 5 "#0ff" 6 "#f0f"
      7 "#ff0" 8 "#f80" 9 "#8f0" 10 "#0f8" 11 "#08f" 12 "#80f"
      13 "#f08" 14 "#512751275127" 15 "#a8b4a8b4a8b4"}

   #  Label font, size and colour.
   protected variable lfont_ 0
   protected variable lsize_ 1.0
   protected variable lcolour_ 0

   #  Border colour.
   protected variable bcolour_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
