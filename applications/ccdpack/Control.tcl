   class Control {
#+
#  Name:
#     Control

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Purpose:
#     Base class for viewer controls.

#  Description:
#     This class provides a base from which specific controls can inherit.  
#     Such controls will have a compact representation (e.g. a button) 
#     which can be placed on a control bar, and will allow the user to
#     modify a given value or set of values.  It may need to pop up
#     additional windows for user interaction if a button-sized area
#     is not big enough for it.

#  Public Methods:
#

#  Public Procedures:
#
#     max num ...
#        Gives the maximum of all the (numeric) arguments supplied.
#        Just here because it's useful.
#
#     mim num ...
#        Gives the minimum of all the (numeric) arguments supplied.
#        Just here because it's useful.
#

#  Public Variables (Configuration Options):
#
#     childsite
#        The pathname of the childsite widget, in which the specific 
#        control should be put.
#
#     command
#        This gives a command which will be executed whenever the value
#        of the control changes.  The value of value will be appended
#        to the command string before it is executed.
#
#     hidden
#        This variable has a boolean value which indicates whether it is
#        visible or not.  By default it is not hidden, but if this is
#        configured true, it will effectively be invisible.
#
#     label
#        The label displayed to the user to indicate what the widget is
#        here for.  If it is a button, this will be the label on the
#        button, but for other widget kinds it may be presented in
#        other ways.
#
#     state
#        This variable may have one of the values "normal" or "disabled".
#        If "disabled" no user interaction will be permitted and the value
#        in the valuevar variable will not change.
#
#     valuevar
#        Gives the name of a variable which will behave as a public 
#        variable.  If the widget is configured with -valuevar name,
#        then the -name option may be used in cget or configure method
#        calls.
#
#        The coding of this is rather ugly and fragile.  This option 
#        should only be used if the control being used to compose a 
#        Mega-Widget.  You'd think there was some clean way round
#        this using Itk's complicated option handling facilities, but
#        it doesn't seem to work properly.
#        
#     value
#        The value of the widget.

#-

#  Inheritance:
      inherit iwidgets::Labeledwidget


########################################################################
#  Constructor.
########################################################################
      constructor { args } {
         itk_component add controlchildsite {
            frame $itk_interior.childsite
         }
         pack $itk_component(controlchildsite)
         eval itk_initialize $args
      }


########################################################################
#  Public methods.
########################################################################

#-----------------------------------------------------------------------
      public method childsite { } {
#-----------------------------------------------------------------------
         return $itk_component(controlchildsite)
      }

########################################################################
#  Public procedures.
########################################################################

#-----------------------------------------------------------------------
      public proc max { num args } {
#-----------------------------------------------------------------------
         foreach x $args {
            if { $x > $num } { set num $x }
         }
         return $num
      }


#-----------------------------------------------------------------------
      public proc min { num args } {
#-----------------------------------------------------------------------
         foreach x $args {
            if { $x < $num } { set num $x }
         }
         return $num
      }


########################################################################
#  Public variables.
########################################################################

#-----------------------------------------------------------------------
      public variable command {} {
#-----------------------------------------------------------------------
      }


#-----------------------------------------------------------------------
      public variable hidden {0} {
#-----------------------------------------------------------------------
         if { $hidden && \
             ! [ catch { pack info $itk_component(controlchildsite) } ] } {
            pack forget $itk_component(controlchildsite)
         } elseif { ! $hidden && \
                    [ catch { pack info $itk_component(controlchildsite) } ] } {
            pack $itk_component(controlchildsite)
         }
      }


#-----------------------------------------------------------------------
      public variable label {} {
#-----------------------------------------------------------------------
      }


#-----------------------------------------------------------------------
      public variable value {} {
#-----------------------------------------------------------------------
         if { $valuecmd != "" } {
            if { [ catch { eval $valuecmd "{$value}" } ] } {
               # eval $valuecmd "{$value}"
               # error "Warning: Control widgets should specify initial value"
            }
         }
         if { $command != "" } {
            eval $command "{$value}"
         }
      }


#-----------------------------------------------------------------------
      public variable state {} {
#-----------------------------------------------------------------------
         if { $state == "normal" } {
         } elseif { $state == "disabled" } {
         } else {
            error "Unknown control state"
         }
      }


#-----------------------------------------------------------------------
      public variable valuevar {} {
#-----------------------------------------------------------------------
         if { $valuevar != "" } {

#  Find the level at which we are being called from - the level which 
#  contains this variable as a public variable.
            set level 0
            for { set l [ info level ] } { $l > 0 } { incr l -1 } {
               if { ! [ catch { uplevel #$l set $valuevar } ] } {
                  set level $l
                  break
               }
            }
            if { $level <= 0 } {
               error "Cannot find variable $valuevar in stack"
            }

            set valuecmd [ uplevel #$level code \$this configure -$valuevar ]
            upvar #$level $valuevar var
            set var $value
            set valuevarname [ uplevel #$level scope $valuevar ]
            trace variable var w [ code $this valuevarchange ]
         } else {
            set valuecmd ""
         }
      }


########################################################################
#  Private methods.                                                    #
########################################################################
      private method valuevarchange { name1 name2 op } {
         configure -value [ set $valuevarname ]
      }


########################################################################
#  Private variables.                                                  #
########################################################################
      private variable valuecmd ""      ;# Command to execute on value change
      private variable valuevarname     ;# Scoped name of value variable

  }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Control {
      keep -background -cursor -foreground
   }


########################################################################
#  Constructor alias
########################################################################

   proc control { pathname args } {
      uplevel Control $pathname $args
   }
   

# $Id$
