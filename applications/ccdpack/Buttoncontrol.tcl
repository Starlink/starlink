   class Buttoncontrol {
#+
#  Name:
#     Buttoncontrol

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Purpose:
#     Viewer control containing a button.

#  Description:
#     This class provides a very simple control which just contains a 
#     button.  It just provides the -cmd and -text configuration
#     options, along with the possiblity to specify a confirmation
#     dialog, but it inherits from the Control class, and so inherits
#     any appropriate behaviour from there.

#  Public Methods:
#
#     Buttoncontrol inherits all the public methods of the Control widget.

#  Public Variables (Configuration Options):
#
#     text
#        Text with which to label the button.
#
#     cmd
#        A command string to execute when the button is pressed.  Note
#        this is not called -command becuase of a clash with the 
#        -command option inherited from the Control class.
#
#     confirmcmd
#        The expression given by this variable will be evaluated when
#        the button is pressed.  Only if it evaluates to a true value
#        will the 'cmd' command be executed.  This will typically be
#        set to the activate method of a confirmation dialog box.
#
#     Buttoncontrol also inherits all the appropriate public variables
#     of the Control widget.

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
         itk_component add button {
            button $itk_component(control).button \
               -relief raised
         }
         pack $itk_component(button) -fill y -expand 1
         pack $itk_component(control) -fill both -expand 1
         configure -balloonstr {Press me}
         eval itk_initialize $args
      }


########################################################################
#  Public methods.
########################################################################


########################################################################
#  Public variables.
########################################################################

#-----------------------------------------------------------------------
      public variable cmd {} {
#-----------------------------------------------------------------------
         $itk_component(button) configure -command \
            "if { $confirmcmd } { $cmd }"
      }


#-----------------------------------------------------------------------
      public variable command {} {
#-----------------------------------------------------------------------
      }


#-----------------------------------------------------------------------
      public variable text {} {
#-----------------------------------------------------------------------
         $itk_component(button) configure -text $text
      }


#-----------------------------------------------------------------------
      public variable confirmcmd {1} {
#-----------------------------------------------------------------------
         configure -cmd $cmd
      }


#-----------------------------------------------------------------------
      public variable state { normal } {
#-----------------------------------------------------------------------
         if { $state == "normal" } {
            $itk_component(button) configure -state normal
         } elseif { $state == "disabled" } {
            $itk_component(button) configure -state disabled
         }
      }



########################################################################
#  Private variables.                                                  #
########################################################################

  }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Buttoncontrol {
      keep -background -cursor -foreground
   }


########################################################################
#  Constructor alias
########################################################################

   proc buttoncontrol { pathname args } {
      uplevel Buttoncontrol $pathname $args
   }
   

# $Id$
