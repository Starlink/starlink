   proc CCDVariableWait { variable Top focus bit } {
#+
#  Name:
#     CCDVariableWait

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Waits for a global variable to be set.

#  Description:
#     This procedure waits for the named global variable to be
#     modified.  Meanwhile it sets the grab (for all events) to a named
#     window ($Top) so that no other interaction may proceed, and
#     defines a widget to receive the focus (this is also named as the
#     default focus widget, so that further dialogs resulting from this
#     level have a sensible focus on return). When the named variable is
#     modified the current grab (if any) and focus defaults are restored.

#  Arguments:
#     variable = string (read)
#        The name of a global variable which needs to be modified before
#	 proceeding.
#     Top = window (read)
#        The name of a top-level window which will probably modify the
#        variable and which needs to have all events grabbed for it.
#     focus = window (read)
#        The name of the window to receive focus, part of $Top (see also bit).
#     bit = string (read)
#        If focus window is a mega-widget the focus is set using a
#	 "$focus focus $bit" command. If this is {} then the ordinary
#	 focus command is used.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     4-MAY-1994 (PDRAPER):
#     	 Original version.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4.
#     {enter_changes_here}

#-

#  Global variables:
      global $variable

#.


#  First set the focus. Remember the current focus which will be
#  restored on exit.
      set old_focus [ focus ]
      if { $bit != {} } {
         $focus focus $bit
      } else {
	 focus $focus
      }

#  Now set the grab
      set old_grab [ grab current ]
      grab set $Top

#  Make all other windows use the "busy" cursor.
      $Top busy hold 0

#  Make sure we exit cleanly if the user wants the window where this
#  is all happening destroyed.
      set oldproto [wm protocol $Top WM_DELETE_WINDOW]
      wm protocol $Top WM_DELETE_WINDOW "global gotfilename;set gotfilename 0"
      set oldbind [bind $Top <Destroy>]
      bind $Top <Destroy> "+ global gotfilename;set gotfilename 0"

#  Wait for variable to be modified.
      tkwait variable $variable

#  Now restore old behaviour. This occasionally fails when the routine
#  is misused (and windows lower in the heirarchy are destroyed), so
#  catch any errors and ignore.
      catch {
               focus $old_focus
               grab release $topwin
               grab set $old_grab
               bind $Top <Destroy> "$oldbind"
               wm protocol $Top WM_DELETE_WINDOW "$oldproto"
            }

#  End of procedure.
   }
# $Id$
