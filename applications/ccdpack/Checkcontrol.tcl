   class Checkcontrol {
#+
#  Name:
#     Checkcontrol

#  Purpose:
#     Viewer control containing a checkbutton.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Description:
#     This class provides a very simple control which just contains a
#     checkbutton.  It inherits from the Control class, and so inherits
#     any appropriate behaviour from there.

#  Public Methods:
#     Checkcontrol inherits all the public methods of the Control widget.

#  Public Variables (Configuration Options):
#     label
#        Text with which to label the checkbutton.
#
#     value
#        A boolean giving the selected status of the checkbutton.
#
#     Checkcontrol also inherits all the appropriate public variables
#     of the Control widget.

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

         itk_component add control {
            frame [ childsite ].control
         }
         itk_component add checkbutton {
            checkbutton $itk_component(control).cb \
               -indicatoron 1 \
               -pady 0 \
               -variable [ scope checkval ] \
               -command [ code "$this setvalue" ]
         }
         pack $itk_component(checkbutton) -side left -fill both -expand 1
         pack $itk_component(control) -side left -fill both -expand 1
         configure -balloonstr {Toggle me}
         eval itk_initialize $args
      }


########################################################################
#  Public methods.
########################################################################

#-----------------------------------------------------------------------
      private method setvalue {} {
#-----------------------------------------------------------------------
         configure -value $checkval
      }


########################################################################
#  Public variables.
########################################################################

#-----------------------------------------------------------------------
      public variable value 0 {
#-----------------------------------------------------------------------
         set checkval $value
      }


#-----------------------------------------------------------------------
      public variable label {} {
#-----------------------------------------------------------------------
         $itk_component(checkbutton) configure -text $label
      }


#-----------------------------------------------------------------------
      public variable state { normal } {
#-----------------------------------------------------------------------
         if { $state == "normal" } {
            $itk_component(checkbutton) configure -state normal
         } elseif { $state == "disabled" } {
            $itk_component(checkbutton) configure -state disabled
         }
      }



########################################################################
#  Private variables.                                                  #
########################################################################

      private variable checkval    ;# Value of the checkbutton variable

  }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Checkcontrol {
      keep -background -cursor -foreground
   }
   option add *Checkcontrol.selectColor #b03060 widgetDefault


########################################################################
#  Constructor alias
########################################################################

   proc checkcontrol { pathname args } {
      uplevel Checkcontrol $pathname $args
   }


# $Id$
