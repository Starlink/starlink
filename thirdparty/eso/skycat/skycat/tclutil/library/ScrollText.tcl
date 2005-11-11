# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: ScrollText.tcl,v 1.2 2005/02/02 01:43:02 brighton Exp $"
#
# ScrollText.tcl - Itcl class for displaying a text window with scrollbars.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  18 Mar 98  Copied from GAIA (Peter Draper, Starlink)
#                            and changed namespace to util::
#                            Changed comment format for use with itcldoc.


itk::usual ScrollText {}

# This class defines methods and configuration options for
# creating a text widget with scrollbars. The scrolltext widget
# has scrollbars along the right and bottom.

itcl::class util::ScrollText {
                       
   #  Inheritances:
   inherit FrameWidget

   #  Constructor.
   constructor {args} {

      #  Do initialisations.
      eval itk_initialize $args

      #  Tk text widget.
      itk_component add Text {
         text $w_.text
      } {
         keep -width -height -exportselection -font
      }

      #  Tk label for title.
      itk_component add Label {
         label $w_.label -text "$itk_option(-label)"
      }

      #  vertical scrollbar
      itk_component add Scrollright {
         scrollbar $w_.scrollright \
            -orient vertical \
            -command "$itk_component(Text) yview"
      }
      $itk_component(Text) configure \
         -yscrollcommand "$itk_component(Scrollright) set"

      # outer frame
      itk_component add Frame {
         frame $w_.frame -borderwidth 0
      }
      set rwidth [winfo reqwidth $itk_component(Scrollright)]
      
      # corner frame
      itk_component add Corner {
         frame $itk_component(Frame).corner -width $rwidth
      }
      
      # horizontal scrollbar
      itk_component add Scrollbottom {
         scrollbar $itk_component(Frame).scrollbottom)\
            -orient horizontal \
            -command "$itk_component(Text) xview"
      }
      $itk_component(Text) configure \
         -xscrollcommand "$itk_component(Scrollbottom) set"

      #  Now pack everything into place.
      pack $itk_component(Label) -side top -fill x
      pack $itk_component(Frame) -side bottom -fill x
      pack $itk_component(Corner) -side right -fill y
      pack $itk_component(Scrollbottom) -side bottom -fill x
      pack $itk_component(Scrollright) -side right  -fill y
      pack $itk_component(Text) -expand true -fill both
   }

   # Inserts a line of text with the given index. "index" can
   # be 0 or end which inserts at the beginning and at the end.

   public method insert { index args } {
      eval $itk_component(Text) insert $index $args
   }
      
   # Clears a range of items from the text widget. If first is "all"
   # then all lines are deleted. If only first is given then this
   # clears a single line. "last" may be set as end.

   public method clear { args } {
      if { [lindex $args 0 ] != "all" } {
         eval $itk_component(Text) delete $args
      } else {
         $itk_component(Text) delete 0.0 end
      }
   }

   # Gets the item with the given indices from the text widget.

   public method get { index } {
      set contents ""
      if { $index != "all" } {
         set contents [$itk_component(Text) get $index]
      } else {
         set size [$itk_component(Text) size]
         for { set i 0 } { $i < $size } { incr i } {
            lappend contents [$itk_component(Text) get $i]
         }
      }
      return $contents
   }

   #  Configuration options:
   #  ----------------------

   # Adds a label over at top of the text widget.
   itk_option define -label scrolltextlabel ScrolltextLabel {} {
      if { [info exists itk_component(Label)] } { 
         $itk_component(Label) configure -text "$itk_option(-label)"
      }
   }
}
