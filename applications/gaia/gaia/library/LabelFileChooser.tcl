#+
#  Name:
#     LabelFileChooser

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class for choosing a named file.

#  Description:
#     This class defines a label entry field with an associated
#     button. The button, if pressed, displays a FileChooser for
#     selecting a name from existing files. The FileChooser is
#     re-used to keep the context.

#  Invocations:
#
#        LabelFileChooser object_name [configuration options]
#
#     This creates an instance of a LabelFileChooser object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#     See itk_option define declarations below.

#  Methods:
#     See descriptions with method declarations below

#  Inheritance:
#     LabelEntry

#  Copyright:
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

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

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     28-NOV-1998 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual LabelFileChooser {}

itcl::class gaia::LabelFileChooser {

   #  Inheritances:
   #  -------------
   inherit util::LabelEntry

   #  Constructor:
   #  ------------
   constructor {args} {

      itk_component add eframe {
         frame $w_.eframe
      }

      #  Add a button to the widget.
      itk_component add chooser {
         button $itk_component(eframe).chooser -text "Choose file..." \
            -command [code $this choose_file_]
      } {
         keep -relief -borderwidth
         rename -font -labelfont labelFont LabelFont
         rename -relief -buttonrelief buttonRelief ButtonRelief
      }
      pack $itk_component(chooser) -side right
      pack $itk_component(eframe) -side right -padx 1m

      #  Now handle unprocessed configurations.
      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor  {
      #  Close the file chooser...
      if { $file_chooser_ != {} && [winfo exists $file_chooser_] } {
         destroy $file_chooser_
      }
   }

   #  Methods:
   #  --------

   #  Choose an existing file and enter it into the entry field.
   protected method choose_file_ {} {
      if { $file_chooser_ == {} } {
         if { $itk_option(-filter_types) == {} } {
            set file_chooser_ \
               [util::FileSelect .\#auto -title $itk_option(-chooser_title)]
         } else {
            set file_chooser_ [util::FileSelect .\#auto \
                                  -title $itk_option(-chooser_title) \
                                  -filter_types $itk_option(-filter_types) \
                                  -button_4 "Browse" \
                                  -cmd_4 [code $this browse_file_] ]
         }
      }
      if { [$file_chooser_ activate] } {
         configure -value [$file_chooser_ get]
         if { "$itk_option(-command)" != "" } {
            command_proc_ $itk_option(-command)
         }
      }
   }

   #  Browse the content of the file selected in the dialog.
   #  Use to look for HDUs.
   protected method browse_file_ {} {

      #  Release the file selection window.
      set file [$file_chooser_ get]
      if { [::file exists $file] && [::file isfile $file] } {
         wm withdraw $file_chooser_
         utilReUseWidget gaia::GaiaHduBrowser $w_.browser \
            -file $file \
            -transient 1 \
            -open_cmd [code $this browsed_open_] \
            -cancel_cmd [code $this choose_file]
      } else {
         #  Ignore, no such file.
         warning_dialog "Not a disk filename ($file)" $w_
         choose_file
      }
   }

   #  Handle an open request from the file browser. Just same as
   #  accept from the file open dialog.
   protected method browsed_open_ {type name {naxes 0}} {
      configure -value $name
      if { "$itk_option(-command)" != "" } {
         command_proc_ $itk_option(-command)
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Filter types (for images, if used).
   itk_option define -filter_types filter_types Filter_Types {} {}

   #  Title for fileselection window.
   itk_option define -chooser_title chooser_title Chooser_Title \
      "Select Detection Image"

   #  Protected variables: (available to instance)
   #  --------------------

   #  The file chooser.
   protected variable file_chooser_ {}

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
