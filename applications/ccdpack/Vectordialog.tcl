   class Vectordialog {
#+
#  Name:
#     Vectordialog

#  Purpose:
#     Get a vector of values by user dialog.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Description:
#     This class currently posts a dialog box consisting of a number of
#     scale widgets.  The ranges of these can be controlled individually.

#  Configuration Options:
#     The following options take a list which must have num elements.
#     Each one is taken in turn to apply to the corresponding control.
#        -from list
#           Start of scale.
#        -label list
#           Label used for scale.
#        -to list
#           End of scale.

#  Constructor:
#     vectordialog number ?-option value ...?
#        The obligatory constructor argument num gives the number of
#        values in the vector, which is the same as the number of scale
#        widgets which will appear in the dialog box.

#  Public Methods:
#     activate
#        The widget begins its life inactive.  When the activate method
#        is called, the window will display itself.  If the -modality
#        configuration option of the widget is 'application' or 'global',
#        then this method will block until either the Cancel or OK button
#        is pushed.  The return value of this will then be the value
#        of the vector.
#     getvec
#        Get the value of the vector as a list of number elements.
#     setvec list
#        Set the value of the vector.  List must number elements.

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

#  Inheritance.
      inherit iwidgets::Dialog

########################################################################
#  Constructor.
########################################################################

      constructor { elements args } {

         set number $elements
         for { set i 0 } { $i < $elements } { incr i } {
            set c $itk_interior.control$i
            lappend controls $c
            itk_component add scale$i {
               scale $c -orient horizontal
            }
            pack $c -side top -fill x
            lappend from 0
            lappend to 100
            lappend label ""
         }

         hide Help

         eval itk_initialize $args
      }


########################################################################
#  Public methods.
########################################################################

#-----------------------------------------------------------------------
      public method activate { args } {
#-----------------------------------------------------------------------
         set initial [ getvec ]
         $this buttonconfigure Cancel \
            -command [ code "$this setvec { $initial } ; $this deactivate" ]
         return [ chain ]
      }


#-----------------------------------------------------------------------
      public method deactivate { args } {
#-----------------------------------------------------------------------
         set list [ getvec ]
         return [ eval chain $list ]
      }


#-----------------------------------------------------------------------
      public method getvec { } {
#-----------------------------------------------------------------------
         foreach c $controls {
            lappend result [ $c get ]
         }
         return $result
      }


#-----------------------------------------------------------------------
      public method setvec { vec } {
#-----------------------------------------------------------------------
         checknum $vec
         foreach c $controls {
            $c set [ shift vec ]
         }
      }


########################################################################
#  Public variables (configuration options).
########################################################################

#-----------------------------------------------------------------------
      public variable from "" {
#-----------------------------------------------------------------------
         set vec $from
         checknum $vec
         foreach c $controls { $c configure -from [ shift vec ] }
      }


#-----------------------------------------------------------------------
      public variable to "" {
#-----------------------------------------------------------------------
         set vec $to
         checknum $vec
         foreach c $controls { $c configure -to [ shift vec ] }
      }


#-----------------------------------------------------------------------
      public variable label "" {
#-----------------------------------------------------------------------
         set vec $label
         checknum $vec
         foreach c $controls { $c configure -label [ shift vec ] }
      }


########################################################################
#  Private methods.
########################################################################

#-----------------------------------------------------------------------
      private method shift { listvar } {
#-----------------------------------------------------------------------
         upvar $listvar list
         if { [ llength $list ] < 1 } {
            error "Supplied list too short (programming error)"
         }
         set result [ lindex $list 0 ]
         set list [ lrange $list 1 end ]
         return $result
      }


#-----------------------------------------------------------------------
      private method checknum { list } {
#-----------------------------------------------------------------------
         if { [ llength $list ] != $number } {
            error "List has wrong number of elements (programming error)"
         }
      }


########################################################################
#  Private variables.
########################################################################

      private variable controls {}
      private variable initial {}
      private variable number 0

   }

########################################################################
#  Widget resource management
########################################################################

   itk::usual Vectordialog {
      keep -background -cursor -foreground
   }


########################################################################
#  Constructor alias
########################################################################

   proc vectordialog { pathname args } {
      uplevel Vectordialog $pathname $args
   }


# $Id$
