#+
#  Name:
#     StarHelp

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class of object for displaying help in a text window.

#  Description:
#     This is a simple class that displays the contents of a named
#     file in a text widget. The file should be set as a configuration
#     option when an instance is created. This class shares a text
#     window amongst all its instances, this means that it overwrites
#     any previous contents.

#  Invocations:
#
#        StarHelp object_name [configuration options]
#
#     This creates an instance of a StarHelp object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Configuration options:
#
#        -file file_name
#
#     The name of the file whose contents are to be displayed
#     in the help text widget.

#  Methods:
#
#        display
#
#     This method causes the object to take control of the text
#     widget and display the help text.

#  Inheritance:
#     Nothing

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     15-NOV-1996 (PDRAPER):
#        Original version.
#     21-JAN-1998 (PDRAPER):
#        Added controls for fonts.
#     {enter_further_changes_here}

#-

#.

class gaia::StarHelp {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval configure $args

      #  We have an interest in health of widget.
      incr reference_
   }

   #  Destructor:
   #  -----------
   destructor {
      incr reference_ -1
      if { $reference_ == 0 } {
         destroy_help
      }
   }

   #  Methods:
   #  --------
   method display {} {
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

   #  Remove the help window.
   method remove_help {} {
      if { [winfo exists $Top_] } {
         wm withdraw $Top_
      }
   }


   #  Create the help window.
   method create_ {} {
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

   #  Set the font of the help text.
   method set_font_ {font} {
      if { [winfo exists $ScrollText_] } { 
         $ScrollText_ configure -font "$font"
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------
   public variable file {name} {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Names of available fonts.
   protected variable fonts_ {
      -*-courier-medium-r-*-*-*-120-*-*-*-*-*-*
      -*-courier-medium-r-*-*-*-140-*-*-*-*-*-*
      -*-courier-medium-r-*-*-*-180-*-*-*-*-*-*
   }

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Name of the text widget used to display the help.
   common ScrollText_ {}

   #  Name of the top-level widget the contains the help and control
   #  buttons .
   common Top_ {}

   #  Reference count of help objects that have an interest in the
   #  help widget.
   common reference_ 0

#  End of class definition.
}
