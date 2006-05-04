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
#     selecting a name from existing files.

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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

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

      #  Add a button to the widget.
      itk_component add chooser {
         button $w_.chooser -text "Choose file..." \
            -command [code $this choose_file_]
      } {
         keep -relief -borderwidth
         rename -font -labelfont labelFont LabelFont
         rename -relief -buttonrelief buttonRelief ButtonRelief
      }
      pack $itk_component(chooser) -side right -padx 1m -ipadx 1m

      #  Now handle unprocessed configurations.
      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Choose an existing file and entry it into the entry field.
   protected method choose_file_ {} {
      if { $itk_option(-filter_types) == {} } { 
         set w [FileSelect .\#auto -title "Select Detection Image"]
      } else {
         set w [FileSelect .\#auto \
                   -title "Select Detection Image"\
                   -filter_types $itk_option(-filter_types)]

      }
      if {[$w activate]} {
         configure -value [$w get]
         if { "$itk_option(-command)" != "" } {
            command_proc_ $itk_option(-command)
         }
      }
      destroy $w
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Filter types (for images, if used).
   itk_option define -filter_types filter_types Filter_Types {} {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}

