#+
#  Name:
#     GaiaImagePanel.tcl

#  Purpose:
#     Defines a class for creating a control panel for the GAIA main window.

#  Type of Module:
#     [incr Tk] class

#  Description:
#     This is class extends the RtdImagePanel class to add the extras
#     facilities required for the GAIA interface. This consists of the 
#     extras needed to add autocuts and colour/intensity table controls.

#  Invocation:
#     GaiaImagePanel name [configuration options]

#  Authors:
#     PDRAPER: Peter Draper (STARLINK)
#     {enter_new_authors_here}

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Inherits:
#     Methods and configuration options of SkyCat (and Rtd).

#  History:
#     26-SEP-1997 (PDRAPER):
#        Original version
#     3-FEB-1999 (PDRAPER):
#        Added quick selections for color and itt tables.
#     25-MAR-1998 (ALLAN):
#        Changed ($f) to ([file tail $f]) in object entry, to save space
#     2-MAR-1999 (PDRAPER):
#        Converted for skycat 2.3.2.
#     {enter_changes_here}

#-

itk::usual GaiaImagePanel {}

itcl::class gaia::GaiaImagePanel {
   inherit rtd::RtdImagePanel

   #  Constructor.
   constructor {args} {

      #  Remove fonts as we want to override these.
      itk_option remove rtd::RtdImagePanel::labelfont
      itk_option remove rtd::RtdImagePanel::valuefont
      eval itk_initialize $args
      
      #  OK to proceed now as all set up (including image_ variable?).
      set make_now_ 1
      make_layout
   }

   #  Override the make_layout method so the panel has percentiles
   #  cuts and colour table controls added.
   method make_layout {} {
      if { ! $make_now_ } { 
         #  Hack to stop base constructor from calling this method
         #  prematurely. 
         return 
      }
      blt::table $w_
      add_short_help $w_ {Image information area}

      #  Add a series of buttons to set the cut levels.
      if {$itk_option(-showcut) } {
         itk_component add aframe {
            frame $w_.aframe -borderwidth 2 -relief flat
         }
         set lwidth [expr $itk_option(-labelwidth)+6]
         itk_component add autocut {
            LabelCommandMenu $itk_component(aframe).autocut \
               -text "Auto Cut:" \
               -labelwidth $lwidth \
               -labelfont $itk_option(-labelfont) \
               -valuefont $itk_option(-valuefont) \
               -anchor e
         }
         foreach value {100 99.5 99 98 95 90 80 70 60 50} {
            $itk_component(autocut) add \
               -label "$value%" \
               -command [code $this set_percent_level $value] \
         }

         #  Select a colour table.
         itk_component add autocolor {
            LabelCommandMenu $itk_component(aframe).autocolor \
               -text "Color Map:" \
               -labelwidth $lwidth \
               -labelfont $itk_option(-labelfont) \
               -valuefont $itk_option(-valuefont) \
               -anchor e
         }
         foreach {name value} \
            {default real greyscale ramp color bgyrw heat heat pastel pastel} {
            $itk_component(autocolor) add \
               -label "$name" \
               -command [code $this set_colormap_ $value] \
         }

         #  Select an ITT table.
         itk_component add autoitt {
            LabelCommandMenu $itk_component(aframe).autoitt \
               -text "Intensity Map:" \
               -labelwidth $lwidth \
               -labelfont $itk_option(-labelfont) \
               -valuefont $itk_option(-valuefont) \
               -anchor e
         }
         foreach {name value} \
            {default ramp {hist equalize} equa log log 
            {negative ramp} neg wrapped lasritt} {
            $itk_component(autoitt) add \
               -label "$name" \
               -command [code $this set_ittmap_ $value] \
         }
         pack $itk_component(autocut) -expand 1 -ipadx 1m -ipady 1m
         pack $itk_component(autocolor) -expand 1 -ipadx 1m -ipady 1m
         pack $itk_component(autoitt) -expand 1 -ipadx 1m -ipady 1m
         blt::table $w_ \
            $itk_component(aframe) 3,2 -rowspan 3 -anchor w
      }

      #  The RtdImage code sets this array for us to speed up the panel
      #  update by using the -textvariable option
      set var $image_
      global ::$var

      #  Display object name
      if {$itk_option(-showobject)} {
         itk_component add object {
            util::LabelValue $w_.object \
               -text "Object:" \
               -valuefont $itk_option(-valuefont) \
               -labelfont $itk_option(-labelfont) \
               -anchor e \
               -relief groove \
               -labelwidth $itk_option(-labelwidth)
         }
         blt::table $w_ \
            $itk_component(object)  0,0 -columnspan 3 -fill x -anchor w
         add_short_help $itk_component(object) {Filename or Object name (filename )}
      }

      # X and Y
      if {$itk_option(-showxy)} {
         itk_component add x {
            util::LabelValue $w_.x \
               -text "X:" \
               -textvariable ${var}(X) \
               -labelfont $itk_option(-labelfont) \
               -valuefont $itk_option(-valuefont) \
               -labelwidth $itk_option(-labelwidth) \
               -valuewidth $itk_option(-valuewidth) \
               -relief groove \
               -anchor e
         }
         itk_component add y {
            util::LabelValue $w_.y \
               -text "Y:" \
               -textvariable ${var}(Y) \
               -labelfont $itk_option(-labelfont) \
               -valuefont $itk_option(-valuefont) \
               -labelwidth $itk_option(-labelwidth) \
               -valuewidth $itk_option(-valuewidth) \
               -relief groove \
               -anchor e
         }
         
         itk_component add value {
            util::LabelValue $w_.value \
               -text "Value:" \
               -textvariable ${var}(VALUE) \
               -labelfont $itk_option(-labelfont) \
               -valuefont $itk_option(-valuefont) \
               -labelwidth 6 \
               -valuewidth $itk_option(-valuewidth) \
               -relief groove \
               -anchor e
         }
         blt::table $w_ \
            $itk_component(x)       1,0 -fill x -anchor w \
            $itk_component(y)       1,1 -fill x -anchor w \
            $itk_component(value)   1,2 -fill x -anchor w
         
         #  Workaround for bug in itcl2.0.
         $itk_component(x) config -textvariable ${var}(X)
         $itk_component(y) config -textvariable ${var}(Y)
         $itk_component(value) config -textvariable ${var}(VALUE)

         add_short_help $itk_component(x) {X image coordinates at mouse pointer}
         add_short_help $itk_component(y) {Y image coordinates at mouse pointer}
         add_short_help $itk_component(value) {Raw image value at X,Y coordinates}
      }
      
      #  Ra and dec.
      if {$itk_option(-showwcs)} {
         itk_component add ra {
            util::LabelValue $w_.ra \
               -text "a:" \
               -textvariable ${var}(RA) \
               -labelfont $itk_option(-wcsfont) \
               -valuefont $itk_option(-valuefont) \
               -labelwidth $itk_option(-labelwidth) \
               -valuewidth $itk_option(-valuewidth) \
               -relief groove \
               -anchor e
         }
         itk_component add dec {
            util::LabelValue $w_.dec \
               -text "d:" \
               -textvariable ${var}(DEC) \
               -labelfont $itk_option(-wcsfont) \
               -valuefont $itk_option(-valuefont) \
               -labelwidth $itk_option(-labelwidth) \
               -valuewidth $itk_option(-valuewidth) \
               -relief groove \
               -anchor e
         }
         itk_component add equinox {
            util::LabelValue $w_.equinox \
               -text "Equinox:" \
               -textvariable ${var}(EQUINOX) \
               -labelfont $itk_option(-labelfont) \
               -valuefont $itk_option(-valuefont) \
               -labelwidth 6 \
               -valuewidth $itk_option(-valuewidth) \
               -relief groove \
               -anchor e
         }
         blt::table $w_ \
            $itk_component(ra)      2,0 -fill x -anchor w \
            $itk_component(dec)     2,1 -fill x -anchor w \
            $itk_component(equinox) 2,2 -fill x -anchor w
         
         #  Workaround for bug in itcl2.0.
         $itk_component(ra) config -textvariable ${var}(RA)
         $itk_component(dec) config -textvariable ${var}(DEC)
         $itk_component(equinox) config -textvariable ${var}(EQUINOX)
         
         add_short_help $itk_component(ra)  {World Coordinates RA value}
         add_short_help $itk_component(dec)  {World Coordinates DEC value}
         add_short_help $itk_component(equinox) {World Coordinates equinox (default: J2000)}
      }

      #  Min and max values.
      if {$itk_option(-showminmax)} {
         itk_component add min {
            util::LabelValue $w_.min \
               -text "Min:" \
               -labelfont $itk_option(-labelfont) \
               -valuefont $itk_option(-valuefont) \
               -labelwidth $itk_option(-labelwidth) \
               -valuewidth $itk_option(-valuewidth) \
               -relief groove \
               -anchor e
         }
         itk_component add max {
            util::LabelValue $w_.max \
               -text "Max:" \
               -labelfont $itk_option(-labelfont) \
               -valuefont $itk_option(-valuefont) \
               -labelwidth $itk_option(-labelwidth) \
               -valuewidth $itk_option(-valuewidth) \
               -relief groove \
               -anchor e
         }
         itk_component add bitpix {
            util::LabelValue $w_.bitpix \
               -text "Bitpix:" \
               -labelfont $itk_option(-labelfont) \
               -valuefont $itk_option(-valuefont) \
               -labelwidth 6 \
               -valuewidth $itk_option(-valuewidth) \
               -relief groove \
               -anchor e
         }
         blt::table $w_ \
            $itk_component(min)     3,0 -fill x -anchor w \
            $itk_component(max)     3,1 -fill x -anchor w
#            $itk_component(bitpix)  3,2 -fill x -anchor w

         add_short_help $itk_component(min) {Estimated minimum raw image value}
         add_short_help $itk_component(max) {Estimated maximum raw image value}
#         add_short_help $itk_component(bitpix) {Raw image FITS data type code}
      }
      
      # cut level controls
      if {$itk_option(-showcut)} {
         itk_component add low {
            util::LabelEntry $w_.low \
               -text "Low:" \
               -command [code $this set_cut_levels] \
               -labelfont $itk_option(-labelfont) \
               -valuefont $itk_option(-valuefont) \
               -labelwidth $itk_option(-labelwidth) \
               -valuewidth $itk_option(-valuewidth) \
               -anchor e \
               -relief sunken \
               -validate real
         } {
            keep -state
         }
         itk_component add high {
            util::LabelEntry $w_.high \
               -text "High:" \
               -command [code $this set_cut_levels] \
               -labelfont $itk_option(-labelfont) \
               -valuefont $itk_option(-valuefont) \
               -labelwidth $itk_option(-labelwidth) \
               -valuewidth $itk_option(-valuewidth) \
               -anchor e \
               -relief sunken \
               -validate real
         }  {
            keep -state
         }
         blt::table $w_ \
            $itk_component(low)     4,0 -fill x -anchor w \
            $itk_component(high)    4,1 -fill x -anchor w
         
         add_short_help $itk_component(low) {Image low cut value, type return after editing value}
         add_short_help $itk_component(high) {Image high cut value, type return after editing value}
         add_short_help $itk_component(autocut) \
            {Quick settings for image cuts, colormaps and intensity transfer tables}
      }
      
      #  Image transformation controls.
      if {$itk_option(-showtrans)} {
         itk_component add trans {
            rtd::RtdImageTrans $w_.trans \
               -labelfont $itk_option(-labelfont) \
               -valuefont $itk_option(-valuefont) \
               -labelwidth [max $itk_option(-labelwidth) 5] \
               -relief flat \
               -min_scale $itk_option(-min_scale) \
               -max_scale $itk_option(-max_scale) \
               -image $itk_option(-image)
         } {
            keep -state
         }
         blt::table $w_ \
            $itk_component(trans)   5,0 -fill x -anchor w -columnspan 2
      }
      blt::table configure $w_ c2 -padx 1m
   }

   #  Set the cut levels using a given percentage cut...
   method set_percent_level {percent} {
      busy {$image_ autocut -percent $percent}
      lassign [$image_ cut] low high
      $itk_component(low) config -value $low
      $itk_component(high) config -value $high
      catch {[$itk_option(-image) component cut] update_graph}
   }

   #  Set the colormap
   protected method set_colormap_ {map} {
      global gaia_library
      $image_ cmap file $gaia_library/colormaps/$map.lasc
   }

   #  Set the ITT map.
   protected method set_ittmap_ {map} {
      global gaia_library
      $image_ itt file $gaia_library/colormaps/$map.iasc
   }

   #  Update the display with the current image values (overriden for
   #  float panel changes). Note fix to stop floating point compares.
   method updateValues {} {
      if {$itk_option(-showobject)} {
         set s [$image_ object]
         set f [file tail [$image_ cget -file]]
         if { "z$s" == "z" } {
            $itk_component(object) config -value "$f"
         } else {
            $itk_component(object) config -value "$s (file:$f)"
         }
      }
      
      if {$itk_option(-showminmax)} {
         $itk_component(min) config -value [$image_ min]
         $itk_component(max) config -value [$image_ max]
         $itk_component(bitpix) config -value [$image_ bitpix]
      }
      
      if {$itk_option(-showcut)} {
         #  avoid conflict with user typing
         lassign [$image_ cut] low high
         set f [focus]
         if {"$f" != "[$itk_component(low) component entry]"} {
            $itk_component(low) config -value $low
         }
         if {"$f" != "[$itk_component(high) component entry]"} {
            $itk_component(high) config -value $high
         }
      }
      update_cut_window
      
      if {$itk_option(-showtrans)} {
         $itk_component(trans) update_trans
      }
   }


   #  Override the set the cut levels method to use GaiaImageCut
   #  instead of RtdImageCut
   method cut_level_dialog {} {
      if {[$image_ isclear]} {
         warning_dialog "No image is currently loaded" $w_
         return
      }
      utilReUseWidget gaia::GaiaImageCut $w_.cut \
         -image $itk_option(-image) \
         -shorthelpwin $itk_option(-shorthelpwin) \
         -transient 1 \
         -command [code $this updateValues]
   }


   #   Define the fonts as the RtdImagePanel ones are not available on
   #   all Linux systems.
   itk_option define -labelfont labelFont LabelFont -adobe-helvetica-bold-r-normal-*-12*
   itk_option define -valuefont valueFont ValueFont -adobe-helvetica-medium-r-normal-*-12*

   #   Protected variable.
   protected variable make_now_ 0

}
