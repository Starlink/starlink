# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: HelpWin.tcl,v 1.4 1998/10/28 17:46:38 abrighto Exp $"
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
      if { [file readable $file] } {
         if { ![winfo exists $Top_] } {
            create_
         } else {
            wm deiconify $Top_
            raise $Top_
         }
         $ScrollText_ clear all
         set fd [open $file r]
         $ScrollText_ insert 0.0 [read $fd]
      }
   }

   # Remove the help window.

   public method remove_help {} {
      if { [winfo exists $Top_] } {
         wm withdraw $Top_
      }
   }


   # Create the help window.

   private method create_ {} {
      if { ![winfo exists $Top_] } {
         set Top_ [TopLevelWidget .\#auto]

         #  Set the top-level window title.
         wm title $Top_ {Help}

         #  Add the font control menu.
         $Top_ add_menubar
         set m [$Top_ add_menubutton Font]
         foreach font $fonts_ {
            $m add command -label abc -font $font \
               -command [code $this set_font_ "$font"]
         }

         #  Add the text widget.
         set ScrollText_ [ScrollText $Top_.text]
         set_font_ "[lindex $fonts_ 0]"

         #  Create the control button.
         set Button [button $Top_.dismiss \
                        -text {Dismiss} \
                        -command [code $this remove_help]]
         pack $ScrollText_ -side top -fill both -expand true
         pack $Button -side bottom
      }
   }

   # Set the font of the help text.

   private method set_font_ {font} {
      if { [winfo exists $ScrollText_] } { 
         $ScrollText_ configure -font "$font"
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   # The name of the file whose contents are to be displayed
   # in the help text widget.
   public variable file {name} {}

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

   # Name of the text widget used to display the help.
   private common ScrollText_ {}

   # Name of the top-level widget the contains the help and control
   # buttons .
   private common Top_ {}

   # Reference count of help objects that have an interest in the
   # help widget.
   private common reference_ 0

#  End of class definition.
}

