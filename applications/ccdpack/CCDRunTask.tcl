   proc CCDRunTask {app arguments wait args} {
#+
#  Name:
#     CCDRunApp

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Runs an application.

#  Description:
#     This routine runs an application of an ADAM task loaded using
#     the adamtask Tcl extensions. It monitors the output (that is
#     also optionally displayed in a separate window) for errors and
#     makes a report if any occur. If required this routine will wait
#     until the applications has completed (using the tkwait call).
#
#     If waiting is used and no display of the application output is
#     required then three different ways of passing the time may be
#     used.
#
#       1) If the application is a fully functional CCDPACK one then a
#          progress bar with a message can be used. This will show
#          the amount of progress if processing one than one file.
#       2) An informational message about the need for patience can be
#          displayed together with with an animation (this is a clock
#          ticking, a better graphic & idea is needed).
#       3) Just wait locking up the application. Use this for quick
#          programs.

#  Arguments:
#     app = string (read)
#        The name of the application to be run. This should be registered
#        in the routine CCDAppRegister.
#     arguments = string (read)
#        The arguments to be used with the application (as would be
#        supplied on the command line).
#     wait = integer (read)
#        Whether to wait for the application to complete or not. If 0
#        then no waiting occurs, otherwise waiting will happen with
#        one of three options. If this value is set to 1 then an
#        clock-like animation is used (together with the description
#        passed in the next argument) while waiting. If this value is
#        2 then the application should be of the type that issues
#        messages like
#           (Number 1 of 12)
#        in its output. If this case a bar will be draw and
#        incremented in line with the amount of processing performed.
#        Other CCDPACK applications will also show a little
#        information. If this value is 3 then we just wait.
#     args = window (read)
#        The description to use in the window while waiting. This
#        arguments are not required if wait is 0 or 3.

#  Global variables (of note):
#     CCDdir = string (read)
#        The CCDPACK binary directory.
#     CCDseetasks = boolean (read)
#        Whether the output from the tasks should be displayed or not.
#    TASK = array (read and write)
#        The element.
#
#          TASK($app,return)
#
#        Is only written to when the application exits, so can be used
#        to wait, but care needs to be taken with the timing since an
#        application may well complete quickly (i.e. before you get a
#        chance to tkwait). It is better to use the internal wait
#        method of this routine.
#
#        The elements.
#
#           TASK($app,error)
#           TASK($app,output)
#
#        may contain the actual textual output from the application
#        and any error messages.
#
#        The element
#
#           TASK($app,progress)
#
#        is used to indicate the fraction of the process that is completed.

#  Notes:
#     The wait bar mechanisms are CCDPACK specific and should be
#     generalised for other use.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     14-MAR-1995 (PDRAPER):
#     	 Tidied up and added header.
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     20-JUL-1995 (PDRAPER):
#        Changed to use ADAM Tcl commands.
#     11-AUG-1995 (PDRAPER):
#        Changed to monitor task output into a single window. Previously
#        each command used a separate window.
#     24-AUG-1995 (PDRAPER):
#        Folded CCDTaskWait code into this section. Having this as a
#        separate call was occasionally a timing problem (TASK return was
#        updated before tkwait invoked!).
#     30-AUG-1995 (PDRAPER):
#        Added animations.
#     27-SEP-1995 (PDRAPER):
#        Added wait option 3.
#     11-OCT-1995 (PDRAPER):
#        Changed to use new task & application controls.
#     {enter_further_changes_here}

#-

#  Global variables.
      global CCDdir
      global CCDseetasks
      global TASK
      global MONOLITH
      global Bitmaps
#.

#------------------------------------------------------------------------------
#   Initialisations.
#------------------------------------------------------------------------------
#  If the monitor variable isn't set set it to false.
      if { ! [ info exists CCDseetasks ] } { set CCDseetasks 0 }

#  Check that the application is available.
      if { ! [CCDTaskStart $app] } {
         CCDIssueError "The application \"$app\" is not available in
this interface (probably programming error)."
         return
      }

#  Set the names of the appropriate bitmaps.
      set Bitmaps(1)    "$CCDdir/c1.xbm"
      set Bitmaps(2)    "$CCDdir/c2.xbm"
      set Bitmaps(3)    "$CCDdir/c3.xbm"
      set Bitmaps(4)    "$CCDdir/c4.xbm"
      set Bitmaps(5)    "$CCDdir/c5.xbm"
      set Bitmaps(6)    "$CCDdir/c6.xbm"
      set Bitmaps(7)    "$CCDdir/c7.xbm"
      set Bitmaps(8)    "$CCDdir/c8.xbm"

#  Set the names of the variable top-level widgets.
      set Btop [lindex $args 0]
      set Otop ".taskoutput"
      set Itop ".taskwait"

#------------------------------------------------------------------------------
#  Create a top-level widget for looking at the task output if needed, and/or
#  use a simple informational message about the delay.
#------------------------------------------------------------------------------
      if { $CCDseetasks } {

#  Check if output window already exists, if not create one.
         if { ! [winfo exists .taskoutput] } {

#  Top level widget.
            Ccd_toplevel $Otop -title "Output from applications"

#  Menubar.
            set Menubar [Ccd_helpmenubar $Otop.menubar -standard 1]

#  Scrolled text widget for the output.
            set Output [Ccd_scrolltext $Otop.output]

#  Choice bar to get rid of the top-level.
            set Choice [Ccd_choice $Otop.choice -standard 0]

#  Add an option to switch off future task output.
            $Menubar addcheckbutton Options \
               {Monitor output from applications} -variable CCDseetasks

#  Add option to remove the window altogether.
   	    $Choice addbutton {Remove from screen} "$Otop kill $Otop"

#  File items to cancel window and exit interface.
            $Menubar addcommand File {Close Window} \
               "$Choice invoke {Remove from screen}"
            $Menubar addcommand File {Exit} CCDExit

#  Packing.
            pack $Menubar -fill x
            pack $Choice -side bottom -fill x
            pack $Output -fill both -expand true
         }
      }
      if { $wait == 1 || $wait == 2 } {

#  Use an information window about status.
         Ccd_toplevel $Itop -title {Information...}
         wm withdraw $Itop
         set Frame1 [frame $Itop.frame1]
         set Frame2 [frame $Itop.frame2]
         set descript [lindex $args 1]
         if { $descript == "" } {set descript " Processing -- please wait. "}
         set Message [label $Frame2.message -text "$descript" -anchor center]
         if { $wait == 1 } {
            set Animate [label $Frame1.animate -bitmap @$Bitmaps(1)]
            pack $Frame1 -side right -fill both
            pack $Frame2 -side left -fill both -expand true -ipadx 15
         } else {
            set Animate [scale $Frame1.animate -orient horizontal \
                            -from 0 -to 100 \
                            -sliderlength 0 -state disabled -showvalue 0 \
                            -takefocus 0 -variable TASK($app,progress)]
            pack $Frame1 -side bottom -fill both -expand true
            pack $Frame2 -side top -fill both -expand true -ipadx 15
         }
         pack $Animate -fill both -expand true
         pack $Message -fill both -expand true

#  Make sure that this can be seen and position it prominently.
         update idletasks
         if { $CCDseetasks } {
            set x [expr [winfo rootx $Otop] + [winfo reqwidth $Otop]/2 \
                      -[winfo reqwidth $Itop]/2]
            set y [expr [winfo rooty $Otop] + [winfo reqheight $Otop]/2 \
                      -[winfo reqheight $Itop]/2]
         } else {
            set x [expr [winfo rootx $Btop] + [winfo reqwidth $Btop]/2 \
                      -[winfo reqwidth $Itop]/2]
            set y [expr [winfo rooty $Btop] + [winfo reqheight $Btop]/2 \
                      -[winfo reqheight $Itop]/2]
         }
         wm geometry $Itop +$x+$y
         wm deiconify $Itop

#  And animate it with the appropriate function.
         if { $wait == 1 } {
            CCDAnimateBitmap $Frame1.animate Bitmaps 8
         }
      }

#  If we're waiting then make all windows show a busy cursor until the
#  application completes.
      if { $wait != 0 } {
         $Btop busy hold 1
      }

#------------------------------------------------------------------------------
#  Now run the application.
#------------------------------------------------------------------------------
      set TASK($app,error) ""
      set TASK($app,output) ""
      set TASK(window) .taskoutput.output
      set TASK($app,progress) 0
      if { $wait == 2 } { set seeprogress 1 } else { set seeprogress 0 }

#  And run the task.
      set task $MONOLITH($TASK($app,monolith),taskname)
      $task obey $app "$arguments" \
         -endmsg   "global TASK; set TASK($app,return) completed" \
         -inform   "CCDMonitorTask $TASK(window) $seeprogress $app %V" \
         -paramreq "$task paramreply %R !!"

#  And wait if required (this must be quick enough!).
      if { $wait != 0 } {
         tkwait variable TASK($app,return)

#  Release the busy cursor.
         $Btop busy forget 1

#  And destroy the informational window.
         if { $wait == 1 } {
            CCDAnimateBitmap $Itop.frame1.animate stop
            $Itop kill $Itop
         } elseif { $wait == 2 } {
            $Itop kill $Itop
         }
      }

}
# $Id$
