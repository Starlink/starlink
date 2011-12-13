   itcl::class Helpcontrol {
#+
#  Name:
#     Helpcontrol

#  Purpose:
#     Viewer control which activates a help window.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Description:
#     This class provides a simple control which pops up a text window
#     when activated which displays some help text.

#  Public Methods:
#     Helpcontrol inherits all the public methods of the Control widget.

#  Public Variables (Configuration Options):
#     helptext
#        Text to display to the user when the control is activated.
#
#     redirect
#        If set to a non-empty string, this should be the name of another
#        Helpcontrol widget.  In this case, invoking the Help button
#        associated with this widget will have the same effect as
#        invoking the button associated with the named widget.
#
#     Helpcontrol also inherits all the appropriate public variables
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

         set leader [ winfo toplevel $itk_interior ]
         itk_component add window {
            iwidgets::dialogshell $itk_interior.window \
               -modality none \
               -master $leader
         }
         set helpwin $itk_component(window)
         $helpwin add dismiss \
             -text "Dismiss" \
             -command [ code $helpwin deactivate ]
         $helpwin default dismiss
         wm group $helpwin $leader
         itk_component add usertext {
            label [ $helpwin childsite ].usertext \
               -justify left \
               -font {Helvetica -12 normal}
         } {
            usual
            rename -text -helptext helpText Text
            ignore -font
         }
         pack $itk_component(usertext)
         itk_component add control {
            frame [ childsite ].control
         }
         itk_component add button {
            button $itk_component(control).button \
               -relief raised \
               -command [ code $this dohelp ] \
               -text Help
         } {
            usual
            ignore -text
         }
         pack $itk_component(button) -fill y -expand 1
         pack $itk_component(control) -fill both -expand 1
         configure -balloonstr {Display help text}
         eval itk_initialize $args
      }


########################################################################
#  Private methods.
########################################################################

#-----------------------------------------------------------------------
      private method dohelp {} {
#-----------------------------------------------------------------------
         if { [ winfo exists $redirect ] } {
            $redirect dohelp
         } else {
            if { [ wm positionfrom $helpwin ] == "" } {
               $helpwin center
               wm positionfrom $helpwin program
            }
            $helpwin activate
            wm resizable $helpwin 0 0
         }
      }


########################################################################
#  Public variables.
########################################################################


#-----------------------------------------------------------------------
      public variable redirect {} {
#-----------------------------------------------------------------------
      }


#-----------------------------------------------------------------------
      public variable state { normal } {
#-----------------------------------------------------------------------
         if { $state == "normal" } {
            $itk_component(button) configure -state normal
         } elseif { $state == "disabled" } {
            $itk_component(button) configure -state disabled
            $helpwin deactivate
         }
      }



########################################################################
#  Private variables.                                                  #
########################################################################
      private variable helpwin
      private variable leader
  }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Helpcontrol {
      keep -background -cursor -foreground
   }


########################################################################
#  Constructor alias
########################################################################

   proc helpcontrol { pathname args } {
      uplevel Helpcontrol $pathname $args
   }


# $Id$
