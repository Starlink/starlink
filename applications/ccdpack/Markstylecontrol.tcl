   class Marktypecontrol {
#+
#  Name:
#     Marktypecontrol

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Purpose:
#     Control widget for selecting the appearance of marker plot items.

#  Description:
#     This control provides an interface from which the user can select
#     a canvas item to use for marking points on a GWM widget.  It 
#     is also used by other objects to draw the markers.  In this way,
#     the client objects need have no knowledge of the shape of 
#     the markers.

#  Public Methods:
#
#     draw canvas cx cy taglist
#        Draws a marker of the current type on the canvas window given
#        by the canvas argument at coordinates (cx, cy).
#        All the elements of the list taglist are added as tags of
#        the canvas item thus created.
#

#  Public Variables (Configuration Options):
#
#     value
#        The value which indicates the type of the marker.  This is an
#        opaque value, and other than reading and writing it, client code
#        cannot do anything useful with it.
#
#     Marktypecontrol also inherits all the public variables of the
#        Control widget.

#-

#  Inheritance:
      inherit Control


########################################################################
#  Constructor.
########################################################################
      constructor { args } {

#  Construct the control.
         itk_component add control {
            frame [ childsite ].control
         }
         itk_component add marktype {
            frame $itk_component(control).marktype \
               -relief raised \
               -borderwidth 3
         }
         itk_component add shower {
            canvas $itk_component(marktype).shower \
               -height 26 \
               -width 40 
         }

#  Pack the control.
         pack $itk_component(shower)
         pack $itk_component(marktype)
         pack $itk_component(control)

#  Construct the dialog which selects the marker shape.
         set attmenus {Colour Size Thickness Shape}
         set option(Colour) {Red Blue Green Yellow White Black}
         set option(Size) {3 5 7 9 11 15}
         set option(Thickness) {1 2 3 4 5}
         set option(Shape) {Plus Cross Circle Square Diamond}
         set attchecks {Showindex}
         set checklabels(Showindex) "Show Index"
         set lws ""
         itk_component add markerdialog {
            iwidgets::dialog $itk_interior.markdialog \
               -modality application \
               -master [ winfo toplevel $itk_interior ]
         } {
            usual
            ignore -modality
         }
         set dialog $itk_component(markerdialog)
         $dialog hide Help
         $dialog buttonconfigure Apply \
            -command [ code "$this configure -value \[ $this getvalue \]" ]
         $dialog buttonconfigure OK \
            -command [ code "$this configure -value \[ $this getvalue \]; \
                             $dialog deactivate 1" ]
         set menubox [ $dialog childsite ]

#  Add an option menu for each of the menu-like attributes.
         foreach att $attmenus {
            itk_component add $att {
               iwidgets::optionmenu $menubox.x$att \
                  -labeltext $att: \
                  -command [ code $this newmarker ]
            }
            eval $itk_component($att) insert end $option($att)
            pack $itk_component($att) -side top -anchor w -fill x
            lappend lws $itk_component($att)
         }

#  Add a labelled checkbutton each of the checkbutton-like attributes.
         foreach att $attchecks {
            itk_component add $att-lw {
               iwidgets::labeledwidget $menubox.x$att \
                  -labeltext $checklabels($att):
            }
            itk_component add $att {
               checkbutton [ $itk_component($att-lw) childsite ].cb \
                  -variable [ scope checkvars($att) ] \
                  -command [ code $this newmarker ]
            }
            pack $itk_component($att)
            lappend lws $itk_component($att-lw)
            pack $itk_component($att-lw) -side top -anchor w -fill none
         }
         eval iwidgets::Labeledwidget::alignlabels $lws

#  Add the marker preview canvas.
         itk_component add scratchholder {
            frame $menubox.sh -relief groove -borderwidth 2
         }
         itk_component add scratchcanvas {
            canvas $itk_component(scratchholder).canv \
               -height 40 \
               -width 40
         }
         pack $itk_component(scratchholder) -side top -anchor c
         pack $itk_component(scratchcanvas)
         set but $itk_component(marktype)
         set canv $itk_component(shower)
         configure -balloonstr "Marker type"
         configure -state normal
         eval itk_initialize $args
         newmarker
      }


########################################################################
#  Public methods.
########################################################################

#-----------------------------------------------------------------------
      public method draw { canvas cx cy {tags ""} {label ""} } {
#-----------------------------------------------------------------------
         set x [ expr round( $cx ) + 0.5 ]
         set y [ expr round( $cy ) - 0.5 ]
         foreach copair $mcopairs {
            lappend coords [ expr [ lindex $copair 0 ] + $x ] \
                           [ expr [ lindex $copair 1 ] + $y ]
         }
         set id [ eval $canvas create $mitem $coords $mconfig ]
         $canvas itemconfigure $id -tags $tags
         if { $label != "" && [ getatt Showindex ] } {
            set ss [ getatt Size ]
            $canvas create text \
               [ expr round( $x ) + $ss * 0.8 ] [ expr round( $y ) ] \
               -tags $tags \
               -anchor w \
               -font "helvetica [ expr -2 * ( $ss - 1 ) ]" \
               -fill [ getatt Colour ] \
               -text $label
         }
      }


#-----------------------------------------------------------------------
      public method getvalue {} {
#-----------------------------------------------------------------------
         set vals {}
         foreach att $attmenus {
            lappend vals "$att=[ getatt $att ]"
         }
         foreach att $attchecks {
            lappend vals "$att=$checkvars($att)"
         }
         return [ join $vals "," ]
      }


#-----------------------------------------------------------------------
      public method getatt { att } {
#-----------------------------------------------------------------------
         if { [ lsearch $attmenus $att ] > -1 } {
            return [ $itk_component($att) get ]
         } elseif { [ lsearch $attchecks $att ] > -1 } {
            return $checkvars($att)
         } else {
            error "No such attribute $att."
         }
      }


#-----------------------------------------------------------------------
      public method setatt { att val } {
#-----------------------------------------------------------------------
         if { [ lsearch $attmenus $att ] > -1 } {
            set ival [ max [ lsearch $option($att) $val ] 0 ]
            $itk_component($att) select $ival
         } elseif { [ lsearch $attchecks $att ] > -1 } {
            set checkvars($att) $val
         } else {
            error "No such attribute $att."
         }
      }



########################################################################
#  Public variables.
########################################################################

#-----------------------------------------------------------------------
      public variable state { normal } {
#-----------------------------------------------------------------------
         if { $state == "normal" } {
            bind $canv <ButtonPress-1> [ code $this dodialog ]
         } elseif { $state == "disabled" } {
            bind $canv <ButtonPress-1> {}
         }
      }
 

#-----------------------------------------------------------------------
      public variable value "" {
#-----------------------------------------------------------------------
         foreach att [ concat $attmenus $attchecks ] {
            if { [ regexp -nocase "$att=(\[^,\]*)" $value dummy val ] } {
               setatt $att $val
            }
         }
         if { $value != [ getvalue ] } { 
            configure -value [ getvalue ]
         }
         newmarker
         $canv delete sampler
         set cx [ expr [ $canv cget -width ] / 2 - [ getatt Size ] ]
         set cy [ expr [ $canv cget -height ] / 2 ]
         draw $canv $cx $cy sampler 1
      }


########################################################################
#  Private methods.
########################################################################

#-----------------------------------------------------------------------
      private method dodialog { } {
#-----------------------------------------------------------------------
         set val $value
         $dialog buttonconfigure Cancel -command \
            [ code "$this configure -value $val; $dialog deactivate 0" ]
         $dialog center $itk_component(control)
         $dialog activate
      }


#-----------------------------------------------------------------------
      private method newmarker { } {
#-----------------------------------------------------------------------
#  Set configuration options for the marker just selected in the dialog.
         set mconfig "-width [ getatt Thickness ]"
         set s [ expr int( ( [ getatt Size ] - 1 ) / 2 ) ]
         switch [ getatt Shape ] {
            Plus {
               set mitem line
               set mcopairs [ list "0 0" "0 -$s" "0 0" "0 $s" \
                                   "0 0" "-$s 0" "0 0" "$s 0" "0 0" ]
               append mconfig " -joinstyle bevel -fill [ getatt Colour ]"
            }
            Cross {
               set mitem line
               set mcopairs [ list "0 0" "-$s -$s" "0 0" "-$s $s" \
                                   "0 0" "$s $s" "0 0" "$s -$s" "0 0" ]
               append mconfig " -fill [ getatt Colour ]"
            }
            Circle {
               set mitem oval
               set mcopairs [ list "-$s -$s" "$s $s" ]
               append mconfig " -outline [ getatt Colour ]"
            }
            Square {
               set mitem polygon
               set mcopairs [ list "-$s -$s" "-$s $s" "$s $s" "$s -$s" ]
               append mconfig " -outline [ getatt Colour ] -fill {}"
            }
            Diamond {
               set mitem polygon
               set mcopairs [ list "0 -$s" "$s 0" "0 $s" "-$s 0" ]
               append mconfig " -outline [ getatt Colour ] -fill {}"
            }
         }

#  Draw the marker on the example canvas.
         foreach c "$itk_component(scratchcanvas)" {
            $c delete sampler
            set cx [ expr [ $c cget -width ] / 2 - [ getatt Size ] ]
            set cy [ expr [ $c cget -height ] / 2 ]
            draw $c $cx $cy sampler 1
         }
      }



########################################################################
#  Private variables.
########################################################################

      private variable attchecks         ;# List of attribute checkbox names
      private variable attmenus          ;# List of attribute menu names
      private variable but               ;# Path name button-like window 
      private variable canv              ;# Path name of canvas show window
      private variable checkvars         ;# Value of checkbox attributes
      private variable dialog            ;# Path name of marker dialog box
      private variable mitem             ;# Marker canvas item type
      private variable mconfig           ;# Marker configuration string
      private variable mcopairs          ;# Marker base coordinate list
      private variable option            ;# Possible values for marker types

   }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Marktypecontrol {
      keep -background -cursor -foreground 
   }


########################################################################
#  Constructor alias
########################################################################

   proc marktypecontrol { pathname args } {
      uplevel Marktypecontrol $pathname $args
   }
   

# $Id$
