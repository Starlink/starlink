#+
#  Name:
#     GaiaImageCut.tcl

#  Purpose:
#     Defines a class for controlling the cut levels of an image.

#  Type of Module:
#     [incr Tk] class

#  Description:
#     This class extends the RtdImageCut class to add any abilities
#     required by the GAIA interface. The main change is to extend
#     way that very small and large ranges are dealt with.

#  Invocation:
#     GaiaImageCut name [configuration options]

#  Authors:
#     PWD: Peter Draper (STARLINK)
#     {enter_new_authors_here}

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils
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

#  Inherits:
#     Methods and configuration options of SkyCat (and Rtd).

#  History:
#     26-SEP-1997 (PWD):
#        Original version
#     {enter_changes_here}

#-

itk::usual GaiaImageCut {}

itcl::class gaia::GaiaImageCut {
   inherit rtd::RtdImageCut

   #  Constructor: create a new instance of this class.
   constructor {args} {
      eval rtd::RtdImageCut::constructor $args
   } {
      eval itk_initialize $args
   }

   #  Override the make the control panel to make changes in the way
   #  that the slider widget's limits and resolutions are set.
   method make_controls {} {
      rtd::RtdImageCut::make_controls
      set from [$image_ min]
      set to [$image_ max]
      set increment_ [expr min(50,($to-$from)/1000.0)]
      set resolution_ [expr min(1.0,$increment_/10.0)]
      lassign [$image_ cut] low_ high_
      update_graph
   }

   #  Override make buttons to change some text.
   method make_buttons {} {
      rtd::RtdImageCut::make_buttons
      $itk_component(median) configure -text "Median Filter"
   }

   #  Set entry values of the lowcut scale widget and update scale
   #  widgets. Override to increment by resolution.
   method setb_lowcut {setlow_ value} {
        lassign [get_cuts] low high
        if {$value >= $high} {
            set value [expr $high - $resolution_]
        }
        if {$setlow_} {
            set low_ $value
        }
        entry_value lowcut $value
        update_cut $value $high
    }

   #  Set entry values of the highcut scale widget and update scale widgets.
   #  Override to increment by resolution.
   method setb_highcut {sethigh_ value} {
      lassign [get_cuts] low high
      if {$value <= $low} {
         set value [expr $low + $resolution_]
      }
      if {$sethigh_} {
         set high_ $value
      }
      entry_value highcut $value
      update_cut $low $value
   }

   #  Update min, max values of the lowcut scale widget.
   #  Override to increment by resolution.
   method update_lowcut {low high} {
      global ::$w_.lowcut
      set from $low_
      set to [expr $high + $resolution_]
      if {$from > $low} {
         set from $low
      }
      if {$to < $low} {
         set to $low
      }

      #  Only change, if necessary, otherwise events can feed into
      #  update_highcut and back again.
      set oldfrom [$itk_component(lowcut) cget -from]
      set oldto [$itk_component(lowcut) cget -to]
      if { $oldfrom != $from && $oldto != $to } {
         $itk_component(lowcut) config -from $from -to $to
      }
      set $w_.lowcut $low
   }

   #  Update min, max values of the highcut scale widget. Override to
   #  increment by resolution.
   method update_highcut {low high} {
      global ::$w_.highcut
      set from [expr $low + $resolution_]
      set to $high_
      if {$from > $high} {
         set from $high
      }
      if {$to < $high} {
         set to $high
      }
      if {$from == $to} {
         set from [expr $from - $resolution_]
      }
      set oldfrom [$itk_component(lowcut) cget -from]
      set oldto [$itk_component(lowcut) cget -to]
      if { $oldfrom != $from && $oldto != $to } {
         $itk_component(highcut) config -from $from -to $to
      }
      set $w_.highcut $high
   }

   #  Protected variables.
   protected variable increment_ 1.0
   protected variable resolution_ 1.0
}
