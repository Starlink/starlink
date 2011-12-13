#+
#  Name:
#     Ccd::multitem

#  Purpose:
#     Defines an class which controls a multiscrollbox, under the
#     assumption that the boxes contain related data fields.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tcl] class

#  Description:
#     This class implements control of a series of scrollboxes which
#     have related information entries. This means that they will have
#     the same number of entries, hence should be controlled singly in
#     the vertical direction. This is implemented by having a single
#     scrollbar if the scrollboxes are stack horizontally, or a series
#     of scrollbars (all controlled together) if stacked vertically).
#     The insertion, retrival and clearing mechanisms are setup to work
#     across the whole list of scrollboxes.

#  Configuration Options:
#        -scrollbarplaces "place1 place2"
#
#     This option configures the placing of the scrollbars. These may
#     be any combination of orthogonal pairs of "left, right" and "top,
#     bottom" or any one or even none of these. The default is "right
#     bottom". The scrollbars are shown one to each listbox along the
#     horizontal directions and one only (at the left or right of the
#     multitem) for controlling the vertical scroll. The vertical scroll
#     of all listboxes are tied.
#
#        -exportselect boolean
#
#     This option configues the scrollboxes so that the selection is
#     the X11 selection or not. If a selection is to be made in more
#     than one scrollbox then this must be set to false (= 0).
#
#        -nboxes
#
#     The number of scrollboxes that are required in the multitem
#     widget. The default number is 2. This may be changed at any time,
#     but if it is changed then all the boxes are cleared (unlike for
#     multiscrollboxes)

#  Inheritance:
#     This class inherits Ccd::multiscrollbox and its methods and
#     configuration options, which are not directly occluded by those
#     specified here.

#  Invocations:
#        Ccd::multitem window [-option value]...
#
#     This command create an instance of a "multiscrollbox" and returns
#     a command "window" for manipulating it via the methods and
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
#        creates the a multiitem widget with a default configuration,
#        except when overridden by command line options.
#     destructor
#        Destroys the multitem widget, invoked by the "delete" method.
#     configure [-option value]...
#        Activates the configuration options. If no configuration value
#        is given then the current value of any known option is returned
#        in a form similar (but not identical to) the Tk widget command.
#     insert index item1 item2 ...
#        Insert a list of strings into the scrollboxes at position
#        index. "index" can be 0 or end which inserts at the beginning
#        and at the end. There should be as many items as listboxes.
#        A single list with sufficient elements may be used instead of a
#        separate list.
#     clear first [last]
#        Clears a range of items from the scrollboxes.  If first is
#        "all" then all lines are deleted. If only first is given then
#        this clears a single line. "last" may be set as end.
#     get index
#        Gets the items with the given index from the scrollboxes. If
#        index is "all" then all the items from a scrollbox are
#        returned. The return is a list.
#     size
#        Returns the size of the scrollboxes.
#     bind args
#        Binds the event and procedure in args to all the listboxes. This
#        uses the "bind list" method of the scrollbox.
#     curselection
#        Returns a list of the indices of any items selected from the
#        listboxes.
#     _moveThoughList, _startScanThroughList, _endScanThoughList and
#     _managelists
#        These internal methods manage the scrolling and scanning of
#        the listboxes, in ways similar to the default bindings for
#        Listboxes.

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
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4.
#     19-MAY-1995 (PDRAPER):
#        Now uses Tk4 listbox bindings rather than re-creating
#        own. Listbox control happens only via one routine now (_managelists)
#        that makes sure that listboxes show the same part and have
#        the same selection. Many of the old internal routines for
#        Motif scrolling etc. are now redundant and have been removed.
#     21-JUL-1995 (PDRAPER):
#        Fixed bindtags to use top-level ancestor rather than ., dot
#        was picking up binding from adamtask (and destroying everything).
#     12-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     3-JUL-2001 (MBT):
#        Fixed a bug from Tcl8 upgrade.
#     27-JAN-2006 (PDRAPER):
#        Updated to itcl::class syntax.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

   itcl::class Ccd::multitem {

#  Inheritances:
      inherit Ccd::multiscrollbox
#.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construction creates a instance of the Ccd::multiscrollbox class and
#  configures it with the default and command-line options.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { args } {

#  Create a multiscrollbox with the desired number of elements. The
#  scrollplaces for the scrollboxes can only be top or bottom for this
#  construct. We will manage the last scrollbar (which can be left or
#  right) at this level.
         eval configure $args
         configure -Ccd::multiscrollbox::nboxes $nboxes
         configure -scrollbarplaces $scrollbarplaces
         configure -exportselect $exportselect
         configure -singleselect $singleselect
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Method to manage the scroll of listboxes so that they scroll together
#  and are followed by the scrollbar too.
      method _managelists { Window Scrollbars Listboxes type direction args } {
         switch $type {
            listbox {
               if { $args != "" } {
                  foreach oneof $Scrollbars {
                     eval $oneof set $args
                  }
               }

#  Create list of windows without the providing the instructions.
               set selfat [ lsearch -exact $Listboxes $Window ]
               set noself [ lreplace $Listboxes $selfat $selfat ]

#  Make all other lists have the same position.
               set index [ $Window index @0,0 ]
               switch $direction {
                  x {
                     foreach oneof $noself {
                        eval $oneof xview $index
                     }
                  }
                  y {
                     foreach oneof $noself {
                        eval $oneof yview $index
                     }
                  }
                  default {
                     error "Unknown direction \"$direction\" for adjusting listbox"
                  }
               }

#  Now make sure selections are the same.
               set selfsel [ $Window curselection ]
               if { $selfsel != "" } {
                  foreach oneof $noself {
                     $oneof selection clear 0 end
                     foreach line $selfsel {
                        $oneof selection set $line
                     }
                  }
               } else {
                  foreach oneof $noself {
                     $oneof selection clear 0 end
                  }
               }
            }

#  Change in scrollbar state (no selection changes).
            scrollbar {
               switch $direction {
                  x {
                     foreach oneof $Listboxes {
                        eval $oneof xview $args
                     }
                  }
                  y {
                     foreach oneof $Listboxes {
                        eval $oneof yview $args
                     }
                  }
                  default {
                     error "Unknown direction \"$direction\" for adjusting listbox"
                  }
               }
            }
            default {
               error "Unknown widget type \"$type\" should be one of listbox or scrollbar"
            }
         }
      }

#  Insert a set of data values into the scrollboxes.
      method insert { index args } {

#  Check that the number of elements in the args list is equal to the
#  number of scrollboxes. If only one element is present try to expand
#  this as a list.
         set nargs [ llength $args ]
         if { $nargs == "1" && $nboxes != "1" } {
            set args [split $args]
         }
         if { [ llength $args ] == $nboxes } {
            set i 1
            foreach item $args {
               Ccd::multiscrollbox::insert $i $index $item
               incr i
            }
         } else {
            error "Need \"$nboxes\" elements for insertion into listboxes"
         }
      }

#  Get a set of data values from the scrollboxes.
      method get { index } {
         return [Ccd::multiscrollbox::get all $index]
      }

#  Get the indices of any items selected in the listboxes. These should
#  be the same across all listboxes so use number 1 for reference.
      method curselection {} {
         return [Ccd::multiscrollbox::curselection 1]
      }

#  Select a set of items. Just uses "all" of multiscrollbox method.
      method select { args } {
         eval Ccd::multiscrollbox::select all $args
      }

#  Clear a set of data values.
      method clear { args } {
         eval Ccd::multiscrollbox::clear all $args
      }

#  Bind all the listboxes to the same event.
      method bind { args } {
         eval Ccd::multiscrollbox::bind all $args
      }

#  Size of the scrollboxes (all the same).
      method size {} {
         return [Ccd::multiscrollbox::size 1]
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Set the Scrollbarplaces. Must keep these correct for this object, as
#  we do not use the usual Ccd::multiscrollbox methods and need to
#  completely manage these at this level.
      public variable scrollbarplaces "bottom right" {
         if { $exists } {

#  Decide which places we can accomodate.
            set hand {}
            switch -regexp $scrollbarplaces {
               right  { set hand right; set index [ number ]}
               left   { set hand left;  set index 1   }
            }
            set side {}
            switch -regexp $scrollbarplaces {
               top    { set side top }
               bottom { set side bottom }
            }

#  Set the required scrollbars. The first or last listbox may have a
#  vertical scrollbar to control all listboxes. If the stacking is
#  vertical then need scrollbars all round.
            if { $stack == "horizontal" } {
               if { $side != {} } { Ccd::multiscrollbox::scrollbarplaces all $side }
               if { $side != {} || $hand != {} } {
                  Ccd::multiscrollbox::scrollbarplaces $index "$hand $side"
               }
            } else {
               if { $side != {} || $hand != {} } {
                  Ccd::multiscrollbox::scrollbarplaces all "$hand $side"
               }
            }

#  Need names of all the actual listboxes and scrollbar which will
#  manage the scroll between them.
            if { $hand != {} } {
               set Listnames [ listnames ]
               set Scrollbar [ scrollbarnames $hand ]

#  Set up the management of the scrollbar to control all listboxes in
#  the multiscrollbox.
               foreach Box $Listnames {
                  $Box configure -yscrollcommand \
      "$Oldthis _managelists $Box [list $Scrollbar] [list $Listnames] listbox y"
               }
               foreach Bar $Scrollbar {
                  $Bar configure -command \
      "$Oldthis _managelists $Scrollbar [list $Scrollbar] [list $Listnames] scrollbar y"
               }

#  Now add enhanced bindings as well.
               ::bind <1>  "$Oldthis _managelists %W [list $Scrollbar] \
                                [list $Listnames] listbox y"
               ::bind <B1-Motion> "$Oldthis _managelists %W [list $Scrollbar] \
                                       [list $Listnames] listbox y"
               ::bind <Shift-1>   "$Oldthis _managelists %W [list $Scrollbar] \
                                       [list $Listnames] listbox y"
               ::bind <Shift-B1-Motion> "$Oldthis _managelists %W [list $Scrollbar] \
                                             [list $Listnames] listbox y"
               ::bind <Any-Up> "$Oldthis _managelists %W [list $Scrollbar] \
                                    [list $Listnames] listbox y"
               ::bind <Any-Down> "$Oldthis _managelists %W [list $Scrollbar] \
                                      [list $Listnames] listbox y"
#  Clear selections.
               ::bind <3>       "$Oldthis select clear 0 end"

#  Need to reorder the bindtags to make Class bindings work before
#  these ones (Class bindings do scroll & selections, need to happen
#  before we update). Note use top-level of this object not ".".
               set topanc [winfo toplevel $oldthis]
               foreach Box $Listnames {
                  set box [CCDPathOf $Box]
                  bindtags $box "Listbox $box $topanc all"
               }
            }
         }
      }

#  Create a series of new boxes. Old boxes must be cleared as item lists
#  should always have the same number of elements.
      public variable nboxes 2 {
         if { $exists } {
            clear 0 end
            configure -Ccd::multiscrollbox::nboxes $nboxes
            configure -scrollbarplaces $scrollbarplaces

#  Listboxes do not export selection as we want to have a selection
#  which runs through all.
            configure -exportselect $exportselect
            configure -singleselect $singleselect
         }
      }

#  Exportselection needs a default of false for this class.
      public variable exportselect 0 {
         if { $exists } {
            configure -Ccd::multiscrollbox::exportselect $exportselect
         }
      }

#  Set label of a scrollbox.
      method label { listno text } {
         if { $listno <= $haveboxes } {
            Ccd::multiscrollbox::label $listno $text

#  Now need to restablish the scrollbar commands (these are destroyed by a
#  repack).
	    ::$Oldthis configure -scrollbarplaces $scrollbarplaces
         } else {
            error "No multiscrollbox with index \"$listno\""
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Common and protected variables.  Common are visible to all instances
#  of this class, protected to just this instance (both are available
#  anywhere in the scope of this class and in derived classes).
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#  End of class definition.
   }
# $Id$
