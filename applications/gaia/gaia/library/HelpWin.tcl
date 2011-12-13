# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id$"
#
# HelpWin.tcl - Itcl class for displaying a text window with a help text.
#
#  Copyright:
#     Copyright (C) 1998-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.
#
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
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  18 Mar 98  Copied from GAIA (Peter Draper, Starlink)
#                            and renamed to util::HelpWin.
#                            Modified comment format a little for use
#                            with the itcldoc script to generate
#                            man pages.
# Peter W. Draper 01 May 01  Reclaimed to use a hypertext help system.

# This is a simple class that displays the contents of a named html
# file. The file should be set as a configuration option when an
# instance is created. This class shares a window amongst all its
# instances, this means that it overwrites any previous contents.

itcl::class gaia::HelpWin {

   # Constructor: Note that all HelpWin objects share a single toplevel window.
   constructor {args} {

      #  Evaluate any options.
      eval configure $args

      #  Preset helpdir to GAIA default.
      if { $helpdir == {} } {
         global gaia_help
         if { [info exists gaia_help] } {
            set helpdir $gaia_help
         }
      }

      #  We have an interest in health of widget.
      incr reference_
   }

   # Destructor: deletes shared window if there are no more references.
   destructor {
      incr reference_ -1
      if { $reference_ == 0 } {
         destroy_help
      }
   }

   # This method causes the object to take control of the text
   # widget and display the help text.
   public method display {} {
      if { ![winfo exists $hyperhelp_] } {
         create_
      } else {
         wm deiconify $hyperhelp_
         raise $hyperhelp_
      }
      $hyperhelp_ showtopic $file

   }

   # Remove the help window.
   public method remove_help {} {
      if { [winfo exists $hyperhelp_] } {
         wm withdraw $hyperhelp_
      }
   }

   # Destroy the window.
   public method destroy_help {} {
      delete object $this
   }

   # Create the help window.
   private method create_ {} {
      if { ![winfo exists $hyperhelp_] } {
         set hyperhelp_ [::gaia::GaiaHyperHelp .\#auto \
                            -topics {{Home index}} \
                            -helpdir $helpdir \
                            -withdraw 1]
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of the directory that contains all help files.
   public variable helpdir {}

   #  Name of the topic that should be displayed
   public variable file {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

   # Name of the widget used to display the help.
   common hyperhelp_ {}

   # Reference count of help objects that have an interest in the
   # help widget.
   common reference_ 0

#  End of class definition.
}
