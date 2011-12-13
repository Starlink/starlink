#+
#  Name:
#     raisehack

#  Purpose:
#     Work around a window manager bug which makes the raise command slow.

#  Language:
#     TCL

#  Invocation:
#     source raisehack.tcl

#  Description:
#     Several window managers have a bug in their ICCCM compliance
#     (or something) which causes them to wait for a timeout when
#     performing a raise operation under certain circumstances.
#     For instance, fvwm2 on Linux will wait two seconds every time it
#     tries to raise an initially unobscured toplevel window (sometimes).
#     This script redefines the raise command in such a way that this
#     no longer happens.  It does it by making sure it only actually
#     tries to raise a window if it is already partially obscured;
#     this operation does not encounter the timeout.

#  Copyright:
#     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
#     MBT: Mark Taylor (STARLINK)
#     EB: Eric Boudaillier (ProXad - France)
#     {enter_new_authors_here}

#  History:
#     10-NOV-2000 (MBT):
#        Original version, following EB's advice from comp.lang.tcl.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Ensure that we have not executed this script before.
      if { [ info procs raise_orig ] != "" } return

#  Save a reference to the original raise command.
      rename raise raise_orig

#  Set a binding to record the visibility state of all windows.
#  The <Map> binding assumes that when a window is mapped it comes up
#  fully visible.  This seems reasonable, but perhaps there are window
#  managers which do not guarantee this?
      bind all <Visibility> { set ::visibilityState(%W) %s }
      bind all <Destroy> { catch { unset ::visibilityState(%W) } }
      bind all <Map> { if !%o \
                          { set ::visibilityState(%W) VisibilityUnobscured } }

#  Redefine the raise command
      proc raise { window { above "" } } {
         if { $above == "" } {
            if { ! [ info exists ::visibilityState($window) ] || \
                 ! [ string equal $::visibilityState($window) \
                                  "VisibilityUnobscured" ] } {
               raise_orig $window
            }
         } else {
            raise_orig $window $above
         }
      }

# $Id$
