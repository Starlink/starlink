   class Zoomcontrol {
#+
#  Name:
#     Zoomcontrol

#  Purpose:
#     Control widget for selecting a zoom level.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Description:
#     This control provides an interface from which the user can select
#     a zoom level for image display.

#  Public Methods:
#     zoominc factor inc
#        This method is provided for doing arithmetic on zoomfactors;
#        the factor argument is a possible contents of the "value"
#        public variable, a zoom factor, and the inc argument is an
#        integer indicating how to change it; +1 means zoom in one level,
#        -1 means zoom out one level, etc.  Exactly what a level means
#        is not defined by this interface, but the result returned by
#        this method will give another possible value of the "value"
#        public variable.  This method respects the max and min limits.

#  Public Variables (Configuration Options):
#     max
#        The maximum value that the value variable may take.
#
#     min
#        The mininum value that the value variable may take.
#
#     value
#        The zoom factor, which is notionally the number of screen pixels
#        used for one NDF grid pixel.
#
#     Zoomcontrol also inherits all the public variables of the Control
#     widget.

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
         itk_component add zoom {
            iwidgets::optionmenu $itk_component(control).zoom \
               -width 70 \
               -command [ code $this setvalue ]
         }
         itk_component add zoomin {
            button $itk_component(control).zoomin \
               -text "+" \
               -command [ code $this zoomer +1 ]
         }
         itk_component add zoomout {
            button $itk_component(control).zoomout \
               -text "-" \
               -command [ code $this zoomer -1 ]
         }
         # itk_component add zoomnum {
         #    label $itk_component(control).zoomnum \
         #          -width 3
         # }
         pack $itk_component(zoomout) \
              $itk_component(zoom) \
              $itk_component(zoomin) \
              -side left -fill x -expand 1
         pack $itk_component(control) -fill both -expand 1
         set omenu $itk_component(zoom)
         configure -balloonstr {Image magnification}
         eval itk_initialize $args
      }


########################################################################
#  Public methods.
########################################################################

#-----------------------------------------------------------------------
      public method zoominc { factor inc } {
#-----------------------------------------------------------------------
         set lev [ expr [ factor2level $factor ] + $inc ]
         if { $lev > $maxlevel } {
            set lev $maxlevel
         } elseif { $lev < $minlevel } {
            set lev $minlevel
         }
         return [ level2factor $lev ]
      }


########################################################################
#  Public variables.
########################################################################

#-----------------------------------------------------------------------
      public variable max { 8 } {
#-----------------------------------------------------------------------
         set maxlevel [ factor2level $max ]
         menuconfig
      }


#-----------------------------------------------------------------------
      public variable min { 0.05 } {
#-----------------------------------------------------------------------
         set minlevel [ factor2level $min ]
         menuconfig
      }


#-----------------------------------------------------------------------
      public variable state { normal } {
#-----------------------------------------------------------------------
         if { $state == "normal" } {
            $itk_component(zoomin) configure -state "normal"
            $itk_component(zoomout) configure -state "normal"
            $itk_component(zoom) configure -state "normal"
         } elseif { $state == "disabled" } {
            $itk_component(zoomin) configure -state "disabled"
            $itk_component(zoomout) configure -state "disabled"
            $itk_component(zoom) configure -state "disabled"
         }
      }


#-----------------------------------------------------------------------
      public variable value { 1 } {
#-----------------------------------------------------------------------
         if { $value < $min } {
            configure -value $min
         } elseif { $value > $max } {
            configure -value $max
         }
         set level [ factor2level $value ]
         $omenu select [ expr $level + $minlevel ]
      }


########################################################################
#  Private procedures.
########################################################################

#-----------------------------------------------------------------------
      private proc level2factor { level } {
#-----------------------------------------------------------------------
         return [ lindex $factors $level ]
      }


#-----------------------------------------------------------------------
      private proc factor2level { factor } {
#-----------------------------------------------------------------------
         catch {
            if { $factor > $max } {
               set factor $max
            } elseif { $factor < $min } {
               set factor $min
            }
         }
         for { set lev 0 } { $lev < [ llength $factors ] } { incr lev } {
            if { $factor <= [ expr [ lindex $factors $lev ] + 0.0001 ] } {
               return $lev
            }
         }
         return [ expr [ llength $factors ] - 1 ]
      }


#-----------------------------------------------------------------------
      private proc level2text { level } {
#-----------------------------------------------------------------------
         if { $level < 0 } {
            return [ lindex $factorreps 0 ]
         } elseif { $level > [ llength $factorreps ] } {
            return [ lindex $factorreps end ]
         }
         return [ lindex $factorreps $level ]
      }


#-----------------------------------------------------------------------
      private proc text2factor { text } {
#-----------------------------------------------------------------------
         return [ expr ${text}.0 ]
      }


      common factorreps
      common factors
      foreach fac { 1/20 1/16 1/12 1/8 1/6 1/4 1/3 1/2 1 2 3 4 6 8 12 16 20 } {
         lappend factorreps $fac
         lappend factors [ expr ${fac}.0 ]
      }



########################################################################
#  Private methods.
########################################################################

#-----------------------------------------------------------------------
      private method zoomer { inc } {
#-----------------------------------------------------------------------
         configure -value [ zoominc $value $inc ]
      }


#-----------------------------------------------------------------------
      private method menuconfig { } {
#-----------------------------------------------------------------------
#  Clear out the current contents of the menu.
         $omenu delete 0 end

#  Fill up the menu with all the valid values of the zoom factor between
#  the min and max values.
         for { set lev $minlevel } { $lev <= $maxlevel } { incr lev } {
            set tex [ level2text $lev ]
            $omenu insert end $tex
         }
      }


#-----------------------------------------------------------------------
      private method setvalue { } {
#-----------------------------------------------------------------------
         set factor [ text2factor [ $omenu get ] ]
         configure -value $factor
      }


########################################################################
#  Private variables.
########################################################################

      private variable maxlevel [ expr [ llength $factorreps ] - 1 ]
                                       # Maximum zoom level
      private variable omenu          ;# Path name of the optionmenu widget
      private variable minlevel 0     ;# Minimum zoom level
      private variable level          ;# The zoom level

   }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Zoomcontrol {
      keep -background -cursor -foreground
   }


########################################################################
#  Constructor alias
########################################################################

   proc zoomcontrol { pathname args } {
      uplevel Zoomcontrol $pathname $args
   }


# $Id$
