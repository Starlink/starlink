#+
#  Name:
#     GaiaImagePanel.tcl

#  Purpose:
#     Defines a class for creating a control panel for the GAIA main window.

#  Type of Module:
#     [incr Tk] class

#  Description:
#     This is class extends the RtdImagePanel class to add the extras
#     facilities required for the GAIA interface. This mainly consists
#     of the ability to make the control panel float (i.e. appear in 
#     a separate top-level window). We also defined the automatic cuts
#     to use a set of percentiles instead of one median filter.

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
#     25-MAR-1998 (ALLAN):
#        Changed ($f) to ([file tail $f]) in object entry, to save space
#     {enter_changes_here}

#-

itk::usual GaiaImagePanel {}

itcl::class gaia::GaiaImagePanel {
   inherit rtd::RtdImagePanel

   #  Constructor.
   constructor {args} {

      eval itk_initialize $args
      
      #  OK to proceed now as all set up (including image_ variable?).
      set make_now_ 1
      make_layout
   }

   # Override the make_layout method so the panel can be made to float
   method make_layout {} {
      if { ! $make_now_ } { 
         #  Hack to stop base constructor from calling this method
         #  prematurely. 
         return 
      }
      blt::table $w_
      add_short_help $w_ {Image information area}

      #  Make a frame at the lower right of the panel that optionally
      #  holds the "Auto Set" button, ...
      itk_component add lrframe {
         frame $w_.lrframe
      }

      #  Add a series of buttons to set the cut levels.
      if {$itk_option(-showcut) } {
         itk_component add autocut {
            frame $w_.autocut
         }
         itk_component add autolabel {
            label $itk_component(autocut).label -text "Auto Cut:" \
               -font $itk_option(-labelfont)
         }
         pack $itk_component(autolabel) -side left
         itk_component add autotablef {
            frame $itk_component(autocut).table
         }
         itk_component add a90 {
            button $itk_component(autotablef).a90 -text "90%" -width 1 \
               -command [code $this set_percent_level 90] \
               -font $itk_option(-valuefont)
         }
         itk_component add a95 {
            button $itk_component(autotablef).a95 -text "95%" -width 1 \
               -command [code $this set_percent_level 95] \
               -font $itk_option(-valuefont)
         }
         itk_component add a98 {
            button $itk_component(autotablef).a98 -text "98%" -width 1 \
               -command [code $this set_percent_level 98] \
               -font $itk_option(-valuefont)
         }
         itk_component add a99 {
            button $itk_component(autotablef).a99 -text "99%" -width 1 \
               -command [code $this set_percent_level 99] \
               -font $itk_option(-valuefont)
         }
         blt::table $itk_component(autotablef) \
            $itk_component(a90)   0,0 \
            $itk_component(a95)   0,1 \
            $itk_component(a98)   1,0 \
            $itk_component(a99)   1,1
         
         pack $itk_component(autotablef)
         pack $itk_component(autocut) \
            -anchor ne -padx 1m -pady 1m -fill x -in $itk_component(lrframe)
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
            $itk_component(max)     3,1 -fill x -anchor w \
            $itk_component(bitpix)  3,2 -fill x -anchor w
         
         add_short_help $itk_component(min) {Estimated minimum raw image value}
         add_short_help $itk_component(max) {Estimated maximum raw image value}
         add_short_help $itk_component(bitpix) {Raw image FITS data type code}
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
            $itk_component(high)    4,1 -fill x -anchor w \
            $itk_component(lrframe) 4,2 -fill x -anchor w -rowspan 2
         
         add_short_help $itk_component(low) {Image low cut value, type return after editing value}
         add_short_help $itk_component(high) {Image high cut value, type return after editing value}
         add_short_help $itk_component(autocut) {Set the image cut levels using a percentile cut}
      }
      
      #  Image transformation controls.
      if {$itk_option(-showtrans)} {
         itk_component add trans {
            rtd::RtdImageTrans $w_.trans \
               -labelfont $itk_option(-labelfont) \
               -valuefont $itk_option(-valuefont) \
               -labelwidth [max $itk_option(-labelwidth) 5] \
               -relief flat \
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

   #  Update the display with the current image values (overriden for
   #  float panel changes).
   method updateValues {} {
      if {$itk_option(-showobject)} {
         set s [$image_ object]
	  set f [file tail [$image_ cget -file]]
         if {"$s" == ""} {
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
      utilReUseWidget RtdImageCut $w_.cut \
         -image $itk_option(-image) \
         -shorthelpwin $itk_option(-shorthelpwin) \
         -transient 1 \
         -command [code $this updateValues]
   }


   #   Protected variable.
   protected variable make_now_ 0

}
