   class Markstylecontrol {
#+
#  Name:
#     Markstylecontrol

#  Purpose:
#     Control widget for selecting the appearance of marker plot items.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Description:
#     This control provides an interface from which the user can select
#     a canvas item to use for marking points on a GWM widget.  It
#     is also used by other objects to draw the markers.  In this way,
#     the client objects need have no knowledge of the shape of
#     the markers.

#  Public Methods:
#     draw canvas cx cy taglist label
#        Draws a marker in the current style on the canvas window given
#        by the canvas argument at coordinates (cx, cy).
#        All the elements of the list taglist are added as tags of
#        the canvas item thus created.  The label is text printed only
#        if the 'showindex' of the 'value' configuration variable is
#        true.

#  Public Variables (Configuration Options):
#     value
#        The value which indicates the style of the marker.  This has
#        the form of a comma-separated list of attribute=value
#        strings.  The available attributes are:
#           - colour     -- Colour of the marker.
#           - size       -- Approximate height of the marker in pixels.
#           - thickness  -- Approximate thickness of lines in pixels.
#           - shape      -- One of Plus, Cross, Circle, Square, Diamond
#           - showindex  -- 1 to show index numbers, 0 not to do so.
#
#        When modifying this value using the configure method, not all
#        of the values have to be supplied; any that are supplied will
#        supplant the old values, and any that are not will remain the
#        same.
#
#     Markstylecontrol also inherits all the public variables of the
#        Control widget.

#  Copyright:
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

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
         itk_component add markstyle {
            frame $itk_component(control).markstyle \
               -relief raised \
               -borderwidth 2
         }
         itk_component add shower {
            canvas $itk_component(markstyle).shower \
               -height 24 \
               -width 50
         } {
            usual
            ignore -background
         }

#  Pack the control.
         pack $itk_component(shower) -fill both -expand 1
         pack $itk_component(markstyle) -fill both -expand 1
         pack $itk_component(control) -fill both -expand 1

#  Construct the dialog which selects the marker shape.
         set attmenus {colour size thickness shape}
         set option(colour) {red blue darkgreen black green yellow white}
         set option(size) {2 4 6 8 10 12 14 16 18 20 22}
         set option(thickness) {1 2 3 4 5 6}
         set option(shape) {plus cross circle square diamond}
         set attchecks {showindex}
         set checklabels(showindex) "Show Index"
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
                  -labeltext [ string totitle $att ]: \
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
                  -command [ code $this newmarker ] \
                  -indicatoron 1
            }
            pack $itk_component($att) -expand 0 -fill none
            lappend lws $itk_component($att-lw)
            pack $itk_component($att-lw) -side top -anchor w -fill none
         }
         eval iwidgets::Labeledwidget::alignlabels $lws

#  Set defaults.
         setatt colour red
         setatt size 12
         setatt thickness 2
         setatt shape plus
         setatt showindex 0

#  Add the marker preview canvas.
         itk_component add scratchholder {
            frame $menubox.sh -relief groove -borderwidth 2
         }
         itk_component add scratchcanvas {
            canvas $itk_component(scratchholder).canv \
               -height 24 \
               -width 50
         }
         pack $itk_component(scratchholder) -side top -anchor c
         pack $itk_component(scratchcanvas)
         set but $itk_component(markstyle)
         set canv $itk_component(shower)
         configure -balloonstr "Marker style"
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
         set x [ expr round( $cx ) + 1 ]
         set y [ expr round( $cy ) - 1 ]

         foreach copair $mcopairs {
            lappend coords [ expr [ lindex $copair 0 ] + $x ] \
                           [ expr [ lindex $copair 1 ] + $y ]
         }
         set id [ eval $canvas create $mitem $coords $mconfig ]
         $canvas itemconfigure $id -tags $tags
         if { $label != "" && [ getatt showindex ] } {
            set ss [ getatt size ]
            $canvas create text \
               [ expr round( $x ) + $ss * 0.8 ] [ expr round( $y ) ] \
               -tags $tags \
               -anchor w \
               -font "helvetica [ expr -2 * round( $ss * 0.75 ) ]" \
               -fill [ getatt colour ] \
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
         set att [ string tolower $att ]
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
         set att [ string tolower $att ]
         if { [ lsearch $attmenus $att ] > -1 } {
            if { [ set ival [ lsearch $option($att) $val ] ] < 0 } {
               set option($att) [ concat $val $option($att) ]
               set c [ $itk_component($att) cget -command ]
               $itk_component($att) configure -command ""
               $itk_component($att) insert 0 $val
               $itk_component($att) configure -command $c
               set ival 0
            }
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
            bind $canv <Enter> [ code $canv configure -background \
                                         [ $this cget -activebackground ] ]
            bind $canv <Leave> [ code $canv configure -background \
                                         [ $this cget -background ] ]
         } elseif { $state == "disabled" } {
            bind $canv <ButtonPress-1> {}
            bind $canv <Enter> {}
            bind $canv <Leave> {}
            $canv configure -background [ $this cget -background ]
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
         set cx [ expr [ $canv cget -width ] / 2 - [ getatt size ] / 2 ]
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
         set s [ expr int( ( [ getatt size ] + 1 ) / 2 ) ]
         set t [ getatt thickness ]
         switch [ getatt shape ] {
            plus {
               set mitem line
               set mcopairs [ list "0 0" "0 -$s" "0 0" "0 $s" \
                                   "0 0" "-$s 0" "0 0" "$s 0" "0 0" ]
               if { [ expr $t / 2 * 2 ] != $t } {
                  incr t
               }
               set mconfig "-joinstyle bevel -fill [ getatt colour ] -width $t"
            }
            cross {
               set mitem line
               set mcopairs [ list "0 0" "-$s -$s" "0 0" "-$s $s" \
                                   "0 0" "$s $s" "0 0" "$s -$s" "0 0" ]
               if { [ expr $t / 2 * 2 ] == $t } {
                  incr t
               }
               set mconfig "-fill [ getatt colour ] -width $t"
            }
            circle {
               set mitem oval
               set mcopairs [ list "-$s -$s" "$s $s" ]
               set mconfig "-outline [ getatt colour ] -width $t"
            }
            square {
               set mitem polygon
               set mcopairs [ list "-$s -$s" "-$s $s" "$s $s" "$s -$s" ]
               set mconfig "-outline [ getatt colour ] -fill {} -width $t"
            }
            diamond {
               set mitem polygon
               set mcopairs [ list "0 -$s" "$s 0" "0 $s" "-$s 0" ]
               set mconfig "-outline [ getatt colour ] -fill {} -width $t"
            }
            default {
               error "No such shape: [ getatt shape ]"
            }
         }

#  Draw the marker on the example canvas.
         foreach c "$itk_component(scratchcanvas)" {
            $c delete sampler
            set cx [ expr [ $c cget -width ] / 2 - [ getatt size ] / 2 ]
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
      private variable option            ;# Possible values for marker styles

   }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Markstylecontrol {
      keep -background -cursor -foreground
   }
   option add *Markstylecontrol.selectColor #b03060 widgetDefault


########################################################################
#  Constructor alias
########################################################################

   proc markstylecontrol { pathname args } {
      uplevel Markstylecontrol $pathname $args
   }


# $Id$
