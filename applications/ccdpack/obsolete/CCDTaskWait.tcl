   proc CCDTaskWait { task Wtop } {
#+
#  Name:
#     CCDTaskWait

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Waits for a task to terminate.

#  Description:
#     This procedure waits for the currently running task to
#     terminate. This is indicated by output to the TASK($task,return)
#     global variable. While waiting for a task to complete the window
#     named in the argument in put into a wait state (stopping further
#     input to it).

#  Notes:
#     If you only want to wait then use the "wait" method of the 
#     CCDRunTask procedure. Using this routine exposes the possibility 
#     of the task completing before the tkwait is reached.

#  Arguments:
#     task = string (read)
#        Name of the task we're waiting on (i.e. ccdpack).
#     Wtop = window (read)
#        Name of top-level window to control locking. This and all the
#        other top-level windows will be locked.  This must be a
#        Ccd_toplevel meta-widget or "". If "" then locking will
#        not be used.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     24-MAR-1995 (PDRAPER):
#     	 Original version.
#     2-AUG-1995 (PDRAPER):
#        Converted for use with tcl ADAM.
#     11-AUG-1995 (PDRAPER):
#        Added optional locking.
#     {enter_changes_here}

#-
      global TASK
#.

#  Make all subsidiary windows busy.
      set waiton [winfo exists $Wtop]
      if { $waiton } { $Wtop busy hold 1 }

#  Create information form that task is running.
      set Top [Ccd_toplevel .taskwait -title {Information...}]
      wm withdraw $Top
      set Frame1 [frame $Top.frame1]
      set Frame2 [frame $Top.frame2]
      set Bitmap [label $Frame1.bitmap -bitmap hourglass]
      set Message [label $Frame2.message -anchor center \
                      -text "  Processing data please wait..  "]
      pack $Frame1 -side left -fill x
      pack $Frame2 -side left -fill both -expand true -ipadx 15
      pack $Bitmap $Message -side left -fill x

#  Make sure that this can be seen and position it prominently.
      update idletasks
      if { [winfo exists $TASK(window)] } {
         set W $TASK(window)
         set x [expr [winfo rootx $W] + [winfo reqwidth $W]/2 \
                   -[winfo reqwidth $Top]/2]
         set y [expr [winfo rooty $W] + [winfo reqheight $W]/2 \
                   -[winfo reqheight $Top]/2]
      } elseif { $waiton } {
         set x [expr [winfo rootx $Wtop] + [winfo reqwidth $Wtop]/2 \
                   -[winfo reqwidth $Top]/2]
         set y [expr [winfo rooty $Wtop] + [winfo reqheight $Wtop]/2 \
                   -[winfo reqheight $Top]/2]
      } else {
         set x [expr [winfo screenwidth $Top]/2 - [winfo reqwidth $Top]/2]
         set y [expr [winfo screenheight $Top]/2 - [winfo reqheight $Top]/2]
      }

      wm geometry $Top +$x+$y
      wm deiconify $Top

#  Wait for task to complete and then destroy "Information..." window.
      tkwait variable TASK($task,return)
      $Top kill $Top

#  Make all subsidiary windows active again (if they were on entry).
      if { $waiton } { $Wtop busy forget 1 }

#  End of procedure.
   }
# $Id$
