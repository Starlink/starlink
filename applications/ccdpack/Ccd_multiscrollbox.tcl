#+
#  Name:
#     Ccd::multiscrollbox

#  Purpose:
#     Defines a class of many scrollboxes.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tcl] class

#  Description:
#     This class description defines methods and configurations for
#     creating and controlling multiple sets of scrollboxes. It provides
#     methods for controlling as a whole the set of listboxes (providing
#     insertions, deletions etc. for all the listboxes at once) or
#     individually. It also allows re-stacking in the vertical and
#     horizontal directions.

#  Configuration Options:
#        -scrollbarplaces "place1 place2"
#
#     This option configures the placing of the scrollbars. These may
#     be any combination of orthogonal pairs of "left, right" and "top,
#     bottom" or any one or even none of these. The default is "right
#     bottom". All listboxes are re-configured.
#
#        -singleselect boolean
#
#     This option configures the scrollboxes bindings so that only one
#     item may be selected in a single listbox at any time. The default
#     is false (=0).
#
#        -exportselect boolean
#
#     This option configures the scrollboxes so that the selection is
#     the X11 selection or not. If a selection is to be made in more
#     than one scrollbox then this needs to be false (0 default).
#
#        -nboxes
#
#     The number of scrollboxes that are required in the multitem.
#     The default number is 2. This may be changed at any time.
#
#        -stack horizontal|vertical
#
#     The stacking order of the scrollboxes. Must be either horizontal
#     or vertical. Horizontal is the default.
#
#         -seealltext boolean
#
#     If true then the boxes are resized in an attempt to display all
#     the text. Default is true.

#  Inheritance:
#     This class inherits Ccd::scrollbox and its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Invocations:
#        Ccd::multiscrollbox window [-option value]...
#
#     This command create an instance of a multiscrollbox and returns a
#     command "window" for manipulating it via the methods and
#     configuration options described below. Configuration options may
#     be appended to the command.
#
#        window configure -configuration_options value
#
#     Applies any of the configuration options (after the widget
#     instance has been created).
#
#        window method arguments
#
#     Performs the given method on this widget.

#  Methods:
#     constructor [-option value]...
#        This method is invoked automatically by the class command and
#        creates the multiscrollbox widget with a default configuration,
#        except when overridden by command line options.
#     destructor
#        Destroys the multiscrollbox, invoked by the "delete" method.
#     configure [-option value]...
#        Activates the configuration options. If no configuration value
#        is given then the current value of any known option is returned
#        in a form similar (but not identical to) the Tk widget command.
#     insert listno index text
#        Insert a text string into a scrollboxes at position index.
#        "index" can be 0 or end which inserts at the beginning	and at
#        the end. If "listno" is "all" then the text is inserted into
#        all the listboxes.
#     clear listno first [last]
#        Clears a range of items from the a scrollbox. If listno is
#        "all" then all scrollboxes are cleared. If first is "all"
#        then all lines are deleted. If only first is given then this
#        clears a single line. "last" may be set as end.
#     get listno index
#        Gets the item with the given indices from the scrollbox with
#        number listno . If "listno" is "all" then the items with
#        index are extracted from all lists. If index is "all" then
#        all the items from a scrollbox are returned.
#     size listno
#        Returns the size of the listbox "listno". If listno is "all"
#        then all the listbox sizes are returned.
#     bind listno args
#        Binds the event and procedure in args to the given list. This
#        uses the "bind list" method of the scrollbox. "listno" may be
#        "all" in which case the binding is applied to all the listboxes
#        in the scrollboxes.
#     wconfig listno option widget value
#        Invokes the Ccd::base wconfig method for the named widget.
#        "listno" may be "all" in which case all widgets of that name
#        in the scrollboxes are configured. Widget is the name of the
#        basic widget (one of list, scrolltop, scrollright, scrollleft,
#        scrollbottom for the constituents of the scrollbox, see
#        Ccd::scrollbox).
#     number
#        Returns the current number of scrollboxes being used.
#     select listno args
#        Sets the selection in the given listbox. The args are the an
#        option valid for a listbox and its qualifiers. If listno is
#        "all" then all listboxes are selected (requires exportselect
#        to be false - 0).
#     scrollbarnames place
#        Returns the names of the constituent scrollbars (of the
#        scrollboxes) which are at the given places. This function
#        should only be used by sub-classes and allows the
#        re-configuration (of configuration change commands for instance)
#        of non-trivial options.
#     listnames
#        Returns the names of the constituent listboxes. This function
#        should only be used by sub-classes and allows the
#        re-configuration (of the x and y scrollcommands for instance)
#        of non-trivial options.
#     scrollbarplaces listno places
#        Allows the re-configuration of the scrollbars of any scrollbox
#        or all if listno is "all". (Not to be confused with the
#        configuration option, which configures all scrollboxes)
#     curselection listno
#        Returns a list of the indices of any items selected in listbox
#        with the given listno.
#     label listno text
#        Set the label of the listbox listno to text.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995, 2000-2001 Central Laboratory of the Research
#     Councils. Copyright (C) 2006 Particle Physics & Astronomy
#     Research Council. All Rights Reserved.

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
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     19-MAR-1994 (PDRAPER):
#        Original version.
#     24-MAR-1994 (PDRAPER):
#        Final methods.
#     5-MAY-1994 (PDRAPER):
#        Removed complex commands from publics into internal methods so
#        that classes can inherit their capabilities.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4. Commented out ::rename in destructor, no
#        longer needed.
#     12-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     3-JUL-2001 (MBT):
#        Fixed a bug from Tcl8 upgrade.
#     27-JAN-2006 (PDRAPER):
#        Updated for itcl::class syntax.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

   itcl::class Ccd::multiscrollbox {

#  Inheritances:
      inherit Ccd::base
#.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construction creates a instance of the Ccd::multiscrollbox class and
#  configures it with the default and command-line options.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { args } {

#  Configure the required boxes
         eval configure $args
         configure -nboxes $nboxes
         configure -stack  $stack
         configure -scrollbarplaces $scrollbarplaces
         configure -singleselect $singleselect
         configure -exportselect $exportselect
      }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Destructor.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Note need to delete scrollboxes as these are not removed by Tk and
#  as of itcl2.0, are no longer available in the namespace for Ccd::base.
      destructor {
         for { set i $haveboxes; incr i } { $i <= $nboxes } { incr i } {
            $Lists($i) delete
         }
      }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#  Method for inserting elements into the listboxes.
      method insert { listno index text } {
         if { $listno != "all" } {
            if { $listno <= $haveboxes } {
               $Lists($listno) insert $index $text

	       if { $seealltext } {

#  Get the length of the text and reset the preferred width of the
#  listbox, if this is now larger.
		  set curwidth [$Lists($listno) cget -width]
		  set newwidth [string length $text]
		  if { $newwidth > $curwidth } {
		     $Lists($listno) configure -width $newwidth
		  }
	       }
            } else {
               error "No multiscrollbox with index \"$listno\" "
            }
         } else {
            for { set i 1 } { $i <= $haveboxes } { incr i } { \
               $Lists($i) insert $index $text
            }
         }
      }

#  Clear listbox of text method.
      method clear { listno args } {
         if { $listno != "all" } {
            if { $listno <= $haveboxes } {
               eval $Lists($listno) clear $args
            } else {
               error "No multiscrollbox with index \"$listno\" "
            }
         } else {
            for { set i 1 } { $i <= $haveboxes } { incr i } { \
               eval $Lists($i) clear $args
            }
         }
      }

#  Get the contents of a listbox.
      method get { listno index } {
         if { $listno != "all" } {
            if { $listno <= $haveboxes } {
               return [$Lists($listno) get $index]
            } else {
               error "No multiscrollbox with index \"$listno\" "
            }
         } else {
            for { set i 1 } { $i <= $haveboxes } { incr i } { \
               lappend getlist [$Lists($i) get $index]
            }
            return $getlist
         }
      }

#  Return the range of lines with current selection.
      method curselection { listno } {
         if { $listno <= $haveboxes } {
            return [$Lists($listno) curselection]
         } else {
            error "No multiscrollbox with index \"$listno\" "
         }
      }

#  Get the size of a listbox.
      method size { listno }  {
         if { $listno != "all" } {
            if { $listno <= $haveboxes } {
               return [$Lists($listno) size]
            } else {
               error "No multiscrollbox with index \"$listno\" "
            }
         } else {
            set indices ""
            for { set i 1 } { $i <= $haveboxes } { incr i } { \
               lappend indices [$Lists($i) size]
            }
            return [$indices]
         }
      }

#  Bind a listbox to an event.
      method bind { listno args } {
         if { $listno != "all" } {
            if { $listno <= $haveboxes } {
               eval $Lists($listno) bind list $args
            } else {
               error "No multiscrollbox with index \"$listno\" "
            }
         } else {
            for { set i 1 } { $i <= $haveboxes } { incr i } { \
               eval $Lists($i) bind list $args
            }
         }
      }

#  Return names of scrollboxes
      method scrollnames {} {

#  For each scrollbox name ask for the listbox name.
         for { set i 1 } { $i <= $haveboxes } { incr i } {
            lappend scrollboxes $Lists($i)
         }
         if { [ info exists scrollboxes ] } {
            return "$scrollboxes"
         } else {
            return {}
         }
      }

#  Return names of listboxes.
      method listnames {} {

#  For each scrollbox name ask for the listbox name.
         for { set i 1 } { $i <= $haveboxes } { incr i } {
            lappend listnames [$Lists($i) listname]
         }
         if { [ info exists listnames ] } {
            return "$listnames"
         } else {
            return {}
         }
      }

#  Return names of scrollbars
      method scrollbarnames { places } {

#  For each scrollbox name ask for scrollbars name(s)
         for { set i 1 } { $i <= $haveboxes } { incr i } {
            set newnames [$Lists($i) scrollbarnames $places]
            if { $newnames != {} } {
               lappend barnames $newnames
            }
         }
         if { [ info exists barnames ] } {
            return "$barnames"
         } else {
            return {}
         }
      }

#  Set the selection in the given window(s).
      method select { listno args } {
         if { $listno != "all" } {
            if { $listno <= $haveboxes } {
               eval $Lists($listno) select $args
            } else {
               error "No multiscrollbox with index \"$listno\" "
            }
         } else {
            if { ! $exportselect } {
               for { set i 1 } { $i <= $haveboxes } { incr i } {
                  eval $Lists($i) select $args
                }
            } else {
               error "Cannot select \"all\" when the selection is to be exported"
	    }
         }
      }

#  Allow sub-widgets to be configured using their own options.
      method wconfig { listno option widget value } {
         if { $listno == "all" } {
            for { set i 1 } { $i <= $haveboxes } { incr i } {
               $Lists($i) wconfig $option $widget $value
   	     }
   	 } else {
            if { $listno <= $haveboxes } {
               $Lists($listno) wconfig $option $widget $value
   	    } else {
               error "No multiscrollbox with index \"$listno\""
            }
         }
      }

#  Return the number of scrollboxes being used.
      method number {} {
         return $haveboxes
      }

#  Re-configure the scrollbars of the scrollboxes
      method scrollbarplaces { listno places } {
         if { $listno == "all" } {
            for { set i 1 } { $i <= $haveboxes } { incr i } {
               $Lists($i) configure -scrollbarplaces $places
   	     }
   	 } else {
            if { $listno <= $haveboxes } {
               $Lists($listno) configure -scrollbarplaces $places
   	    } else {
               error "No multiscrollbox with index \"$listno\""
            }
         }
      }

#  Set label of a scrollbox.
      method label { listno text } {
         if { $listno <= $haveboxes } {
            $Lists($listno) configure -label $text
         } else {
            error "No multiscrollbox with index \"$listno\""
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Public variables:
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Number of boxes in multiscroll widget.
      public variable nboxes 2 {
         if { $exists } {
            if { $haveboxes < $nboxes } {
               for { set i $haveboxes; incr i } { $i <= $nboxes } { incr i } {
                  CCDCcdWidget Lists($i) lists($i) Ccd::scrollbox $oldthis.list$i
                  set widgetnames($Oldthis:list$i) $Lists($i)
               }
            } else {

#  Need to lose some boxes?
               if { $haveboxes > $nboxes } {
                  for { set i $haveboxes } { $i > $nboxes } { incr i -1 } {
                     $Lists($i) delete
                  }
               }
            }
            set haveboxes $nboxes

#  Repack boxes if one packing has already been performed (during contruction).
            if { $packedboxes } { configure -stack $stack }
         }
      }

#  Scrollbar placements. This applies globally.
      public variable scrollbarplaces "right bottom" {
         if { $exists } {
            scrollbarplaces all $scrollbarplaces
         }
      }

#  Stacking public.
      public variable stack horizontal {
         if { $exists } {
            switch $stack {
               horizontal { set side left }
               vertical   { set side top }
               default {
                  error "Unknown option \"$stack\": should be vertical or horizontal"
               }
            }
            for { set i 1 } { $i <= $haveboxes } { incr i } {
               if { $packedboxes } { pack forget $lists($i) }
               pack $lists($i) -side $side -expand true -fill both
	    }
	 }
         set packedboxes 1
      }

#  Can more than one entry be selected at a time from the listboxes?
      public variable singleselect 0 {
         if { $exists } {
            for { set i 1 } { $i <= $haveboxes } { incr i } {
               $Lists($i) configure -singleselect $singleselect
            }
         }
      }

#  Is the selection exportable? If not may have more than one selection
#  (one for each instance), otherwise the selection is the X11 one.
      public variable exportselect 1 {
         if { $exists } {
            for { set i 1 } { $i <= $haveboxes } { incr i } {
               $Lists($i) configure -exportselect $exportselect
            }
         }
      }

#  Are the boxes to be reconfigured to fit the text?
      public variable seealltext 1 {}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Common and protected variables.  Common are visible to all instances
#  of this class, protected to just this instance (both are available
#  anywhere in the scope of this class and in derived classes).
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#     "haveboxes"    Is the number of scrollboxes currently in existence.
#     "packedboxes"  Shows if the scrollboxes have been packed or not. If they
#                    have a call to change the number of scrollboxes also
#                    restacks them.
      protected variable haveboxes 0
      protected variable packedboxes 0

#  Names of widgets.
      protected variable Lists
      protected variable lists

#  End of class definition.
   }
# $Id$
