# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id$"
#
# HelpWin.tcl - Itcl class for displaying a text window with a help text.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  18 Mar 98  Copied from GAIA (Peter Draper, Starlink)
#                            and renamed to util::HelpWin.             
#                            Modified comment format a little for use 
#                            with the itcldoc script to generate
#                            man pages.

# This is a simple class that displays the contents of a named
# file in a text widget. The file should be set as a configuration
# option when an instance is created. This class shares a text
# window amongst all its instances, this means that it overwrites
# any previous contents.

itcl::class util::HelpWin {

   # Constructor: Note that all HelpWin objects share a single toplevel window.

   constructor {args} {

      #  Evaluate any options.
      eval configure $args

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
      }
      $hyperhelp_ showtopic $file ;#$topic
      $hyperhelp_ activate
   }

   # Remove the help window.
   public method remove_help {} {
      if { [winfo exists $hyperhelp_] } {
         wm withdraw $hyperhelp_
      }
   }

   # Create the help window.
   private method create_ {} {
      if { ![winfo exists $hyperhelp_] } {
         set hyperhelp_ [iwidgets::hyperhelp .\#auto \
                            -title "On-line Help" \
                            -modality none \
                            -topics {index} \
                            -helpdir $helpdir\
                            -wrap word \
                            -width 700 \
                            -height 500 \
                            -fontname helvetica]
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of the directory that contains all help files.
   public variable helpdir {} {
      if { $helpdir == {} } {
         global env
         set helpdir $env(GAIA_HELP)
      }
   }

   #  Name of the topic that should be displayed
   public variable file {}

   #  Protected variables: (available to instance)
   #  --------------------

   # Names of available fonts.
   protected variable fonts_ {
      -*-courier-medium-r-*-*-*-120-*-*-*-*-*-*
      -*-courier-medium-r-*-*-*-140-*-*-*-*-*-*
      -*-courier-medium-r-*-*-*-180-*-*-*-*-*-*
   }

   #  Common variables: (shared by all instances)
   #  -----------------

   # Name of the widget used to display the help.
   private common hyperhelp_ {}

   # Name of the top-level widget the contains the help and control
   # buttons .
   private common Top_ {}

   # Reference count of help objects that have an interest in the
   # help widget.
   private common reference_ 0

#  End of class definition.
}

