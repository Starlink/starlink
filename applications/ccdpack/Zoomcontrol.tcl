   class Zoomcontrol {
#+
#  Name:
#     Zoomcontrol

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Purpose:
#     Control widget for selecting a zoom level.

#  Description:
#     This control provides an interface from which the user can select
#     a zoom level for image display.

#  Public Methods:
#
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
#
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
         itk_component add zoomnum {
            label $itk_component(control).zoomnum \
                  -width 3
         }
         pack $itk_component(zoomin) \
              $itk_component(zoomnum) \
              $itk_component(zoomout) \
              -side left
         pack $itk_component(control)
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
      }


#-----------------------------------------------------------------------
      public variable min { 0.05 } {
#-----------------------------------------------------------------------
         set minlevel [ factor2level $min ]
      }


#-----------------------------------------------------------------------
      public variable state { normal } {
#-----------------------------------------------------------------------
         if { $state == "normal" } {
            $itk_component(zoomin) configure -state "normal"
            $itk_component(zoomout) configure -state "normal"
         } elseif { $state == "disabled" } {
            $itk_component(zoomin) configure -state "disabled"
            $itk_component(zoomout) configure -state "disabled"
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
         $itk_component(zoomnum) configure -text [ level2text $level ]
      }


########################################################################
#  Private procedures.
########################################################################

#-----------------------------------------------------------------------
      private proc level2factor { level } {
#-----------------------------------------------------------------------
         if { $level >= 0 } {
            return [ expr 1 + $level ]
         } else {
            return [ expr 1.0 / ( 1 - $level ) ]
         }
      }


#-----------------------------------------------------------------------
      private proc level2text { level } {
#-----------------------------------------------------------------------
         if { $level >= 0 } {
            return [ expr round( 1 + $level ) ]
         } else {
            return "1/[ expr round( 1 - $level ) ]"
         }
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
         if { $factor >= 1 } {
            return [ expr round( $factor - 1 ) ]
         } else {
            return [ expr 1 - round( 1.0 / $factor ) ]
         }
      }



########################################################################
#  Private methods.
########################################################################
      private method zoomer { inc } {
         configure -value [ zoominc $value $inc ]
      }


########################################################################
#  Private variables.
########################################################################

      private variable maxlevel  ;# Maximum zoom level
      private variable minlevel  ;# Minimum zoom level
      private variable level     ;# The zoom level

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
