   class Waiter {
#+
#  Name:
#     Waiter
 
#  Type of Module:
#     [incr Tk] Mega-Widget.
 
#  Purpose:
#     Provide a window to display indicating the user has to wait.
 
#  Description.
#     This class provides a mega-widget which will be posted indicating
#     to the user that something is going on and he/she will have to
#     wait.  The window can contain a text message, and is centred over
#     its parent.  It also performs a grab so that no other windows in
#     the application will receive an event, and sets the curesor over
#     all application windows to a suitable busy form, until it is
#     destroyed.
#
#     This widget has no methods other than the constructor and destructor.

#  Public Methods:

#  Public Variables (Configuration Options):
#
#     text = string
#        Text to appear in the message box.
#
#     busycursor = string
#        The name in usual Tk format of a cursor which will be used for
#        all the windows made inactive by the grab as long as this
#        window exists.

#  Authors:
#     MBT: Mark Taylor (Starlink)

#  History:
#     2-NOV-2000 (MBT):
#        Original version.
 
#-

#  Inheritance.
      inherit itk::Toplevel

########################################################################
#  Constructor.
########################################################################
      constructor { args } {

#  Construct the toplevel window.
         set waitwin $itk_interior
         wm withdraw $waitwin
         set master [ winfo parent $itk_interior ]
         $waitwin configure -title "Busy"

#  Pack it with content.
         itk_component add messframe {
            frame $itk_interior.messframe
         }
         pack $itk_component(messframe) -side left -fill both \
                                        -expand 1 -ipadx 15
         itk_component add animframe {
            frame $itk_interior.animframe
         }
         pack $itk_component(animframe) -side right -fill both
         itk_component add message {
            label $itk_component(messframe).message -anchor center
         } {
            usual
            keep -text
         }
         pack $itk_component(message) -fill both -expand 1
         itk_component add animation {
            label $itk_component(animframe).animation -bitmap @$Tickerbitmap(1)
         }
         pack $itk_component(animation) -fill both -expand 1

#  Configure requested options.
         eval itk_initialize $args

#  Position the window 
         update idletasks
         set xpos [ expr [ winfo rootx $master ] \
                       + [ winfo width $master ] / 2 \
                       - [ winfo reqwidth $waitwin ] / 2 ]
         set ypos [ expr [ winfo rooty $master ] \
                       + [ winfo height $master ] / 2 \
                       - [ winfo reqheight $waitwin ] / 2 ]
         wm geometry $waitwin +$xpos+$ypos

#  Begin the animation.
         CCDAnimateBitmap $itk_component(animation) [ scope Tickerbitmap ] 8

#  Save the current cursors for all application windows.
         savecursors $master

#  Set the cursor busy for this window and all application windows.
         $master configure -cursor $busycursor
         foreach win "$master" {
            $win configure -cursor $busycursor
         }

#  Arrange that the window is never eclipsed by its master.
         wm transient $waitwin $master
         bind $waitwin <Visibility> [ code raise $waitwin $master ]

#  Map the window and grab pointer events.
         wm deiconify $waitwin
         grab $waitwin
      }


########################################################################
#  Destructor.
########################################################################
      destructor {

#  Stop the animation.
         CCDAnimateBitmap $itk_component(animation) stop

#  Restore the cursors of the affected windows.
         restorecursors

#  Release the grab.
         grab release $waitwin

#  Destroy the toplevel window.
         destroy $waitwin
      }


########################################################################
#  Public variables (configuration options).
########################################################################
      public variable busycursor { watch } {
      }




########################################################################
#  Private methods.
########################################################################

#-----------------------------------------------------------------------
      private method restorecursors { } {
#-----------------------------------------------------------------------
         foreach element $cursors {
            set win [ lindex $element 0 ]
            set curs [ lindex $element 1 ]
            if { [ winfo exists $win ] } {
               $win configure -cursor $curs
            }
         }
      }


#-----------------------------------------------------------------------
      private method savecursors { win } {
#-----------------------------------------------------------------------
         if { ! [ catch { $win cget -cursor } curs ] } {
            lappend cursors [ list $win $curs ]
         }
         foreach child [ winfo children $win ] {
            savecursors $child
         }
      }


########################################################################
#  Private variables.
########################################################################

      private variable cursors         ;# Array holding orig cursor for windows
      private variable master          ;# Master window (probably the parent)
      private variable waitwin         ;# Toplevel waiter window
      private common Tickerbitmap      ;# Bitmaps for the animation
      private common CCDdir $env(CCDPACK_DIR)

      foreach i { 1 2 3 4 5 6 7 8 } {
         set Tickerbitmap($i) "$CCDdir/c$i.xbm"
      }

   }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Waiter {
      keep -background -foreground -font
   }
   option add *Waiter.busyCursor watch widgetDefault


########################################################################
#  Constructor alias
########################################################################

   proc waiter { window args } {
      uplevel Waiter $window $args
   }



# $Id$
