#+
#  Name:
#     GaiaPolOptionsDialog

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     A dialog box for making multiple selctions from a list of choices

#  Description:
#     This class produces a dialog box containing a single column of
#     checkbuttons containing supplied values and labels.
#
#  Invocations:
#
#        GaiaPolOptionsDialog object_name [configuration options]
#
#     This creates an instance of a GaiaPolOptionsDialog object. The returned value
#     is the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#     See the "itk_option define" declarations below.

#  Inheritance:
#     ::util::DialogWidget

#  Copyright:
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
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
#     DSB: David S. Berry  (STARLINK)
#     {enter_new_authors_here}

#  History:
#     27-NOV-2000 (DSB):
#        Original version.
#     {enter_further_changes_here}

#-

#.
itk::usual GaiaPolOptionsDialog {}

itcl::class gaia::GaiaPolOptionsDialog {

#  Inheritances:
#  -------------
   inherit util::DialogWidget

#  Constructor:
#  ------------
   constructor {args} {
      eval itk_initialize $args

#  Instructions.
      itk_component add info {
         label $w_.info -text "$itk_option(-text)"
      }
      pack $itk_component(info) -side top -anchor nw -fill both \
                                -expand 1 -padx 2m -pady 1m -ipady 1m

#  Now add a frame containing the checkbuttons.
      addButtons

#  Indicate the widget has been initialized.
      set initialized_ 1
   }

#  Destructor:
#  -----------

#  Public Methods:
#  ===============

#  Select a specified set of the displayed options.
#  ------------------------------------------------
   public method setOptions {sel} {
      set i 0
      foreach item $itk_option(-options) {
         if { [lsearch -exact $sel $item] != -1 } {
            set values_($this,item$i) 1
         } else {
            set values_($this,item$i) 0
         }
         incr i
      }
   }

#  Check all options.
#  ------------------
   public method allOptions {} {
      set i 0
      foreach item $itk_option(-options) {
         set values_($this,item$i) 1
         incr i
      }
   }

#  Protected Methods:
#  =================

#  Add the checkbutons. The -options option gives a list of strings used
#  both as option labels and option values.
#  --------------------------------------------------------------------
   protected method addButtons {} {

#  Delete any existing buttons by destroying their containing Frame.
      if { [winfo exists $w_.buts] } {
         destroy $w_.buts
      }

#  Create a new frame for the check buttons.
      itk_component add buts {
            frame $w_.buts -relief groove -bd 2
      }
      pack $itk_component(buts) -side top -anchor n -fill both \
                                -expand 1 -padx 2m -pady 3m -ipady 2m

#  Get the width for the label length (default to the longest label length).
      set lwidth $itk_option(-labelwidth)
      if { $lwidth == "" } {
         if { $itk_option(-options) != "" } {
            set lwidth  0
            foreach item $itk_option(-options) {
               set len [string length $item]
               if { $len > $lwidth } {
                  set lwidth $len
               }
            }
            incr lwidth
         } else {
            set lwidth 13
         }
      }

#  Add the CheckButtons.
      set i 0
      foreach item $itk_option(-options) {
         itk_component add item$i {
            gaia::StarLabelCheck $itk_component(buts).item$i \
                                  -text "$item:" \
                                  -onvalue 1 \
                                  -offvalue 0 \
                                  -labelwidth $lwidth \
                                  -variable [scope values_($this,item$i)]

            }
         pack $itk_component(item$i) -side top -fill both -anchor n -expand 1 -padx 3m
         incr i
      }
   }

#  Called after options have been evaluated
#  ---------------------------------------
   protected method init {} {
      util::DialogWidget::init
      destroy $itk_component(top)
   }

#  This method is redefined here to change the value that is returned
#  from activate to be a two element list; the first element gives the
#  index of the button that was pressed, and the second elemtn is a list of
#  the selected options.
#  ------------------------------------------------------------------
   protected method set_result {} {
      global ::$variable_
      set but [set $variable_]

      set opts ""
      set i 0
      foreach item $itk_option(-options) {
         if { $values_($this,item$i) } {
            lappend opts $item
         }
         incr i
      }
      return [list $but $opts]
    }

#  Configuration options:
#  ----------------------

#  The options.
    itk_option define -options options Options {} {
       if { $initialized_ } {
          addButtons
       }
    }

#  The width of the check button labels.
    itk_option define -labelwidth labelwidth LabelWidth {} {
       if { $initialized_ } {
          addButtons
       }
    }

#  Protected variables:
#  ====================
   protected {
      variable initialized_ 0
   }

#  Common (i.e. static) data members:
#  ==================================

#  Array for passing around at global level. Indexed by ($this,param).
   common values_

}
