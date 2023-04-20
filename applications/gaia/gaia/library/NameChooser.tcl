#+
#  Name:
#     NameChoose

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Displays a list of names and descriptions for selection.

#  Description:
#     This class defines a top-level widget for displaying a list of
#     names and some associated descriptions. The user can then
#     select from amongst the names and choose a suitable one. This
#     name is then returned as the value of the given textvariable.

#  Invocations:
#
#        NameChooser object_name [configuration options]
#
#     This creates an instance of a NameChooser object. The return is
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
#        -textvariable global_variable
#
#     Name of a variable to receive the selected name.

#  Methods:

#  Inheritance:
#     TopLevelWidget

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
#     12-DEC-1997 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual NameChooser {}

itcl::class gaia::NameChooser {

    #  Inheritances:
    #  -------------
    inherit util::TopLevelWidget

    #  Constructor.
    constructor {args} {

	#  Add an entry window for the current selection.
	itk_component add entry {
	    util::LabelEntry $w_.entry -text "Selection:" \
		    -valuewidth 0
	} {
	    keep -textvariable
	}

	#  Add a scrolling listbox to show the contents.
	itk_component add scrollbox {
	    gaia::Scrollbox $w_.scrollbox -singleselect 1
	} {
	    keep -width
	}
	set listbox [$itk_component(scrollbox) listname]

	#  Evaluate all options.
	eval itk_initialize $args

	#  Ok can now set the scrollbox title.
	$itk_component(scrollbox) configure -label $itk_option(-title)

	#  Set the bindings of the listbox to show the selected name
	#  or select and invoke the OK command.
	bind $listbox <1> [code $this select $listbox %y 0]
	bind $listbox <Double-1> [code $this select $listbox %y 1]

	#  Add a button bar to OK or cancel selection.
	itk_component add action {
	    frame $w_.action
	}
	itk_component add ok {
	    button $itk_component(action).ok \
		    -text {OK} \
		    -command [code $this set_ok_]
	}
	itk_component add cancel {
	    button $itk_component(action).cancel \
		    -text {Cancel} \
		    -command [code $this set_cancel_]
	}

	#  Pack frames.
	pack $itk_component(scrollbox) -side top -fill both -expand 1
	pack $itk_component(entry) -side top -fill x -expand 1 -pady 3 -padx 3
	pack $itk_component(action) -side top -fill x
	pack $itk_component(ok) -side left -expand 1 -pady 3 -padx 3
	pack $itk_component(cancel) -side left -expand 1 -pady 3 -padx 3
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Set the contents of the entry field and optional accept this as
   #  the final result.
   public method select {listbox y accept} {
       #  Get the index of the y value.
       set index [$listbox nearest $y]
       if { [info exists names_($index)] } {
	   $itk_component(entry) configure -value $names_($index)
       }
       if { $accept } {
	   set_ok_
       }
   }

   #  Insert a name and description into the listbox.
   public method insert {name description} {
       $itk_component(scrollbox) insert $end_ "$name -- $description"
       set names_($end_) $name
       incr end_
   }

   #  Accept the name in the entry field. Just close the window.
   protected method set_ok_ {} {
       delete object $this
   }

   #  Do not accept the entry value. This is reset to {} before exit.
   protected method set_cancel_ {} {
       $itk_component(entry) configure -value {}
       delete object $this
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Protected variables: (available to instance)
   #  --------------------

   #  Number of names inserted into list.
   protected variable end_ 0

   #  The names of the entries indexed by there position in the list.
   protected variable names_

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
