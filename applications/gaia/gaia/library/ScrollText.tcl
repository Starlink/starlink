#+
#  Name:
#     ScrollText

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#    Defines a class of "text widget with scrollbars".

#  Description:
#    This class description defines methods and configurations for
#    creating a text widget with scrollbars. The scrolltext widget
#    has scrollbars along the right and bottom.

#  Invocations:
#
#        ScrollText window [-option value]...
#
#     This command create an instance of a scrolltext and returns a
#     command "window" for manipulating it via the methods and
#     configuration options described below. Configuration options may
#     be appended to the command.
#
#        window configure -configuration_options value
#
#     Applies any of the configuration options (after the widget
#     instance has been created).
#
#        window method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#
#        -exportselect boolean
#
#     This option configues the text widget so that the selection is the X11
#     selection or not. If a selection is to be made in more than one
#     place then this will require setting to false.
#
#        -height  value
#
#     The height of the text widget. If no qualifiers are given to the value
#     then this will be in characters (see Tk_GetPixels).
#
#        -width value
#
#     The width of the text widget. If no qualifiers are given to the value
#     then this will be in characters (see Tk_GetPixels).
#
#         -label "text"
#
#     Adds a label over at top of the text widget.

#  Methods:
#     constructor [-option value]...
#        This method is invoked automatically by the class command and
#	 creates the scrolltext widget with a default configuration,
#	 except when overridden by command line options.
#     destructor
#        Destroys the scrolltext, invoked by the "delete" method.
#     insert index text
#        Inserts a line of text with the given index. "index" can
#	 be 0 or end which inserts at the beginning and at the end.
#     clear first [last]
#        Clears a range of items from the text widget. If first is "all"
#	 then all lines are deleted. If only first is given then this
#	 clears a single line. "last" may be set as end.
#     get index1 [index2]
#        Gets the item with the given indices from the text widget.

#  Inheritance:
#     FrameWidget

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     14-MAR-1995 (PDRAPER):
#     	 Original version.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4. Commented out ::rename in destructor, no
#        longer needed.
#     11-AUG-1995 (PDRAPER):
#        Added option for horizontal scrollbar (Tk 4 enhancement).
#     15-NOV-1996 (PDRAPER):
#        Now ScrollText in GAIA and converted to itcl2.0.
#     {enter_further_changes_here}

#-

itk::usual ScrollText {}

itcl::class gaia::ScrollText {
                       
   #  Inheritances:
   inherit FrameWidget

   #  Constructor.
   #  ------------
   constructor {args} {

      #  Do initialisations.
      eval itk_initialize $args

      #  Create text widget.
      itk_component add Text {
         text $w_.text
      } {
         keep -width -height -exportselection -font -foreground -background
      }

      #  Create the label.
      itk_component add Label {
         label $w_.label -text "$itk_option(-label)"
      }

      #  Create the scrollbars.
      itk_component add Scrollright {
         scrollbar $w_.scrollright \
            -orient vertical \
            -command "$itk_component(Text) yview"
      }
      $itk_component(Text) configure \
         -yscrollcommand "$itk_component(Scrollright) set"

      itk_component add Frame {
         frame $w_.frame -borderwidth 0
      }
      set rwidth [winfo reqwidth $itk_component(Scrollright)]
      itk_component add Corner {
         frame $itk_component(Frame).corner -width $rwidth
      }
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

   #  Methods.
   #  --------
   
   #  Insert line of text method.
   method insert { index args } {
      eval $itk_component(Text) insert $index $args
   }
      
   #  Clear range of lines of text method.
   method clear { args } {
      if { [lindex $args 0 ] != "all" } {
         eval $itk_component(Text) delete $args
      } else {
         $itk_component(Text) delete 0.0 end
      }
   }

   #  Get information back from the text widget.
   method get { index } {
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

   #  Set text of label.
   itk_option define -label scrolltextlabel ScrolltextLabel {} {
      if { [info exists itk_component(Label)] } { 
         $itk_component(Label) configure -text "$itk_option(-label)"
      }
   }
}
