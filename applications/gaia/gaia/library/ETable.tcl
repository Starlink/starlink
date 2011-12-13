#+
#  Name:
#     ETable

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a table widget with editable cells.

#  Description:
#     This file describes the class of table widget that allows the
#     insertion and control of values on a row, column or cellular
#     basis. Each cell in the table can be editted if desired.
#
#     The table can be controlled in a pseudo multi listbox fashion in
#     which the selection control is row orientated.
#
#     Each element of the table is an entry widget and can be editted
#     and scrolled etc. using the normal bindings. The viewable
#     portion of table can be smaller than the actual extent allowing
#     a much larger table to be used.

#  Invocations:
#
#        ETable window [-option value]...
#
#     This command creates an instance of a "table" and returns a
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

#  Configuration options:
#
#        -rows number
#
#     Sets the numer of rows to be displayed. This value is changed if
#     insertion requires more values. Default is 0.
#
#        -columns number
#
#     Sets the numer of columns to be displayed. This value is changed if
#     insertion requires more values. Default is 1.
#
#        -width value
#
#     Sets the width of the displayed region (of the canvas), in
#     display pixels. Default is not to set.
#
#        -height value
#
#     Sets the height of the displayed region (of the canvas), in
#     display pixels. Default is not to set.
#
#        -singleselect boolean
#
#     Whether or not the selection should only be of a single row at a
#     time or not (work like browse and extended modes of
#     listbox). This is true by default.
#
#        -scrollbarplaces (right|left) (top|bottom) none none
#
#     Sets where the scrollbars are to be placed. The default is
#     "right bottom". "none none" means no scrollbars.
#
#        -padvalue value
#
#     Sets the value used to pad cells with no value when inserting
#     into whole rows or columns. If set this relaxes the requirement
#     that sufficient values are supplied when inserting into a row or
#     column. Default is unset.
#
#         -modified boolean
#
#     This option is set to 1 (true) after a keypress in any of the
#     entries occurs (these keypresses are not any of the recognised
#     ones for moving the selection etc.). This doesn't actually
#     guarantee that an entry has changed, but indicates it might
#     have.
#
#        -action command
#
#     Command to execute when <Return> is pressed in one of the cells.

#  Methods:
#     constructor [-option value]...
#        This method is invoked automatically by the class command and
#        creates the "ETable" widget with a default configuration,
#        except when overridden by command line options.
#     destructor
#        Destroys the "ETable" instance, invoked by the "delete" method.
#     configure [-option value]...
#        Activates the configuration options. If no configuration value
#        is given then the current value of any known option is returned
#        in a form similar (but not identical to) the Tk widget command.
#     setlabel column label
#        Sets the label for the given column.
#     insertrow row values
#        Inserts a list of values into a row. The number of columns
#        will extend to allow all the values to appear if required.
#     insert row values
#        Shortname for insertrow method.
#     insertrow row values
#        Inserts a list of values into a column. The number of rows
#        will extend to allow all the values to appear if required.
#     insertcell row column value
#        Insert a value into a cell. The cell will be created if it
#        does not exist already (as will any cells necessary to pad
#        out to this extent).
#     getrow row
#        Returns a list of all the values in a row.
#     get row
#        Shortname for getrow method.
#     getcol column
#        Returns a list of all the values in a column.
#     getcell row column
#        Returns the value of a cell.
#     colstate column [state]
#        Sets or returns the state of the entries in a column. The
#        state is one of "normal", "disabled" or "mixed". "disabled"
#        indicates that the column cannot be editted. "mixed'
#        indicates that the column consists of "disabled" and "normal"
#        cells.
#     rowstate column [state]
#        Sets or returns the state of the entries in a row. The
#        state is one of "normal", "disabled" or "mixed". "disabled"
#        indicates that the row cannot be editted. "mixed' indicates
#        that the row consists of "disabled" and "normal" cells.
#     cellstate column [state]
#        Sets or returns the state of the entrie at a given cell. The
#        state is one of "normal" or "disabled". "disabled"  indicates
#        that the column cannot be editted.
#     size
#        Returns the number of rows in table.
#     rownum
#        Returns the number of rows in table.
#     colnum
#        Returns the number of columns in table.
#     clearrows first [last]
#        Removes the cells in the rows first through last. If last is
#        not given then it defaults to first. Note that the rows are
#        reshuffled after the range is removed, so the index numbers
#        will change (they always run from 0 to the number of visible
#        rows).
#     clear first [last]
#        Shortname for clearrows.
#     clearcols first [last]
#        Removes the cells in the columns first through last. If last
#        is not given then it defaults to first. Note that the columns
#        are reshuffled after the range is removed, so the index
#        numbers will change (they always run from 0 to the number of
#        visible columns).
#     clearcell row column
#        Clears the value of a cell. Note the cell still exists after
#        this operation.
#     selection option args
#        Controls the apparent selection. These are very similar to
#        the listbox selection and are row oriented. "args" can be
#        "anchor", "clear", "set" or "includes".
#     index value
#        Returns the index given by value. Understands "active",
#        "anchor" and "end"
#     activate row column
#        Sets the active cell, i.e. the one with the focus.
#     see row
#        Ensures that the given row is visible.
#     curselection
#        Returns a list of the indices of the currently selected
#        values.
#
#  Internal methods (should not be used outside of this class defintion).
#     Control of region of canvas to scroll
#        _scrollregion {bounds}
#     Support for selection control.
#        _select { first last }
#        _deselect { first last }
#        _beginselect {row col}
#        _motion {row col}
#        _beginextend {row col}
#        _begintoggle {row COL}
#        _autoscan {w row col wx wy rx ry}
#        _updown {row col amount}
#        _extendupdown {row col amount}
#        _cancelrepeat {}
#     Index translation for rows.
#        _index { arguments element default args }
#     Change size of displayed elements (when canvas is reconfigured
#     in size.)
#        _reconfigure {newwidth}

#  Notes:
#     Efficiency: changing the number of rows and columns using the
#     "insert" methods can be quite slow (this is because of a
#     "update" that is necessary to get the new size of the canvas to
#     define the scrollable region) it is much better to set the size
#     of the table as a configuration option (with -rows and
#     -columns), before inserting new entries.
#
#    This probably isn't the best implementation for very large tables.

#  Inheritance:
#     This class inherits "FrameWidget" its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Copyright:
#     Copyright (C) 1995-2005 Central Laboratory of the Research Councils.
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
#     1-JUN-1995 (PWD):
#        Original version.
#     10-JUN-1996 (PWD):
#        Converted from CCDPACK Ccd_table to ETable for use in RTD.
#     5-JUL-1996 (PWD):
#        Converted to [incr Tk] widget.
#     {enter_changes_here}

#-

itk::usual ETable {}

itcl::class gaia::ETable {

   #  Inheritances:
   inherit util::FrameWidget

   #  Constructor: creates a instance of the class and configures it with
   #  the default and command-line options.
   constructor {args} {

      #  Create the canvas widget to hold the frames. Stop it from taking
      #  the focus and confine the scrollable region.
      itk_component add canvas {
         canvas $w_.canvas -takefocus 0 -confine 1
      }

      #  Initialise the minimum width of a column on the canvas.
      entry $w_.e
      frame $w_.f
      set ewidth [expr [winfo reqwidth $w_.e] + [$w_.e cget -borderwidth] ]
      set columnwidth [expr $ewidth + [$w_.f cget -borderwidth] ]
      set mincolumnwidth $columnwidth

      #  Remember height and remove the unwanted widgets.
      set eheight [expr [winfo reqheight $w_.e] + [$w_.e cget -borderwidth] ]
      destroy $w_.e
      destroy $w_.f

      #  Evaluate args.
      eval itk_initialize $args

      #  Set state(afterId) to {}. This is used in bindings for controlling
      #  selection. Also set other states that require initialisation
      set state(afterId) {}
      set state(prev) 0
      set state(selection) {}
      set state(x) 0
      set state(y) 0
      set state(0) 0

      #  Finally set binding to control resizing of the entry widget widths.
      ::bind $itk_component(canvas) <Configure> [code $this _reconfigure %w]

   }

   #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   #  Methods.
   #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   #  Set column label.
   method setlabel {col label} {
      if { [info exists itk_component(${col}label)] } {
         $itk_component(${col}label) configure -text $label
      }
   }

   #  Insert row of data method.
   method insertrow { row args } { insert $row $args }
   method insert { row args } {

      #  Check that the number of elements in the args list is equal to the
      #  number of columns. If only one element is present try to expand
      #  this as a list. If this fails then apply the pad value unless this
      #  is not set.
      set nargs [ llength $args ]
      if { $nargs == "1" && $itk_option(-columns) != "1" } {
         set args [split $args]
      }
      if { [ llength $args ] >= $itk_option(-columns) } {

         #  Expand $row in case has "end", only want to do this once.
         set minrows [_index $row 0 0 append]
         set i 0
         foreach item $args {
            insertcell $minrows $i $item
            incr i
         }
      } elseif { [info exists itk_option(-padvalue)] } {
         set minrows [_index $row 0 0 append]
         set i 0
         foreach item $args {
            insertcell $minrows $i $item
            incr i
         }
         for {} { $i < $itk_option(-columns) } { incr i } {
            insertcell $minrows $i $itk_option(-padvalue)
         }
      } else {
         error "Need \"$itk_option(-columns)\" elements for insertion into row"
      }
   }

   #  Insert column of data method.
   method insertcolumn { col args } {

      #  Check that the number of elements in the args list is equal to the
      #  number of rows. If only one element is present try to expand
      #  this as a list.
      set nargs [ llength $args ]
      if { $nargs == "1" && $itk_option(-rows) != "1" } {
         set args [split $args]
      }
      if { [ llength $args ] >= $itk_option(-rows) } {
         set i 0
         foreach item $args {
            insertcell $i $col $item
            incr i
         }
      } elseif { [info exists itk_option(-padvalue)] } {
         set i 0
         foreach item $args {
            insertcell $i $col $item
            incr i
         }
         for {} { $i < $itk_option(-rows) } { incr i } {
            insertcell $i $col $itk_option(-padvalue)
         }
      } else {
         error "Need \"$itk_option(-rows)\" elements for insertion into column"
      }
   }

   #  Insert data into cell method. Clears the current contents and makes
   #  sure that we can see the end of the new insertion if flushright is set.
   method insertcell { row col value } {
      set row [_index $row 0 0 append]
      if { $row >= $itk_option(-rows) } {
         configure -rows [expr $row +1]
      }
      if { $col >= $itk_option(-columns) } {
         configure -columns [expr $col +1]
      }
      $itk_component(${col}${row}) delete 0 end
      $itk_component(${col}${row}) insert 0 $value
      if { $itk_option(-flushright) } {
         $itk_component(${col}${row}) xview end
      }
   }

   #  Get values from a row.
   method getrow { row } { get $row }
   method get { row } {
      set row [_index $row 0 0 ]
      set values ""
      for { set i 0 } { $i < $itk_option(-columns) } { incr i } {
         lappend values [$itk_component(${i}${row}) get]
      }
      return $values
   }

   #  Get values from a column.
   method getcol { col } {
      set values ""
      for { set i 0 } { $i < $itk_option(-rows) } { incr i } {
         lappend values [$itk_component(${col}${i}) get]
      }
      return $values
   }

   #  Get value from a cell.
   method getcell { row col } {
      set row [_index $row 0 0]
      return [$itk_component(${col}${row}) get]
   }

   #  Enable insertion state for methods for rows, columns and cells.
   #  If no value is given then the current state is returned. This may
   #  be mixed if a mixture of enabled and disabled is found.
   method colstate { col istate } {
      if { $istate == "normal" || $istate == "disabled" } {
         if { $istate != "" } {
            for { set i 0 } { $i < $itk_option(-rows) } { incr i } {
               $itk_component(${col}${i}) configure -state $istate
            }
         } else {
            set istate ""
            for { set i 0 } { $i < $itk_option(-rows) } { incr i } {
               set cellstate [$itk_component(${col}${i}) cget -state]
               if { $i == 0 } {
                  set istate $cellstate
               } else {
                  if { $cellstate != $istate } {
                     set istate "mixed"
                     break
                  }
               }
            }
            return $istate
         }
      } else {
         error "Unknown state \"$istate\" must one of \"normal\" or \"disabled\""
      }
   }
   method rowstate { row istate } {
      set row [_index $row 0 0]
      if { $istate == "normal" || $istate == "disabled" } {
         if { $istate != "" } {
            for { set i 0 } { $i < $itk_option(-columns) } { incr i } {
               $itk_component(${i}${row}) configure -state $istate
            }
         } else {
            set istate ""
            for { set i 0 } { $i < $itk_option(-columns) } { incr i } {
               set cellstate [$itk_component(${i}${row}) cget -state]
               if { $i == 0 } {
                  set istate $cellstate
               } else {
                  if { $cellstate != $istate } {
                     set istate "mixed"
                     break
                  }
               }
            }
            return $istate
         }
      } else {
         error "Unknown state \"$istate\" must one of \"normal\" or \"disabled\""
      }
   }
   method cellstate { row col istate } {
      set row [_index $row 0 0]
      if { $istate == "normal" || $istate == "disabled" } {
         if { $istate != "" } {
            $itk_component(${col}${row}) configure -state $istate
         } else {
            return [$itk_component(${col}${row}) cget -state]
         }
      } else {
         error "Unknown state \"$istate\" must one of \"normal\" or \"disabled\""
      }
   }

   #  Get the number of rows.
   method size {}  {
      return $itk_option(-rows)
   }

   #  Get the number of rows.
   method rownum {}  {
      return $itk_option(-rows)
   }
   #  Get the number of rows.
   method colnum {}  {
      return $itk_option(-columns)
   }

   #  Clear a range of rows. Note this actually deletes the entry widgets
   #  in these rows and is fundamentally different from clearing a cell.
   method clearrows { first args } { clear $first $args}
   method clear { first args } {
      set first [_index $first 0 0]
      if { $args == "" } {
         set last $first
      } else {
         set last [_index $args 0 0]
         if { $first > $last } {
            set temp $first
            set first $last
            set last $temp
         }
      }
      set last [expr $last +1]
      if { $last > $itk_option(-rows) } { set last $itk_option(-rows) }

      #  Remove any selection from these rows.
      _deselect $first $last

      #  Shuffle data so that rows are moved, then reconfigure to remove the
      #  extra rows.
      set k $first
      for { set i $last } { $i < $itk_option(-rows) } { incr i } {
         for { set j 0 } { $j < $itk_option(-columns) } { incr j } {
            set Entryi $itk_component(${j}${i})
            set Entryk $itk_component(${j}${i})
            $Entryk delete 0 end
            $Entryk insert 0 [$Entryi get]
            $Entryi delete 0 end
         }
         incr k
      }
      configure -rows [expr $itk_option(-rows) - $last + $first]
   }

   #  Clear a range of columns. Note this actually deletes the entry widgets
   #  in these columns and is fundamentally different from clearing a cell.
   method clearcols { first args } {
      if { $args == "" } {
         set last $first
      } else {
         set last [lindex $args 0]
         if { $first > $last } {
            set temp $first
            set first $last
            set last $temp
         }
      }
      set last [expr $last +1]
      if { $last > $itk_option(-columns) } { set last $itk_option(-columns) }

      #  Shuffle data so that columns are moved, then reconfigure to remove the
      #  extra ones.
      set k $first
      for { set i $last } { $i < $itk_option(-columns) } { incr i } {
         for { set j 0 } { $j < $itk_option(-rows) } { incr j } {
            set Entryi $itk_component(${i}${j})
            set Entryk $itk_component(${i}${j})
            $Entryk delete 0 end
            $Entryk insert 0 [$Entryi get]
            $Entryi delete 0 end
         }
         incr k
      }
      configure -columns [expr $itk_option(-columns) - $last + $first]
   }

   #  Clear a cell.
   method clearcell { row col } {
      if [info exists itk_component(${col}${row})] {
         $itk_component(${col}${row}) clear 0 end
      }
   }

   #  Selection control method.
   method selection {option args} {
      switch -- $option {
         anchor {
            set anchor [_index $args 0 0]
         }
         clear {
            set first [_index $args 0 0]
            set last [_index $args 1 $first]
            _deselect $first $last
         }
         set {
            set first [_index $args 0 0]
            set last [_index $args 1 $first]
            _select $first $last
         }
         includes {
            set row [_index $args 0 0]
            return $state($row)
         }
         default {
            error \
               {Unknown selection type, must be one of "anchor", "clear", "set" or "includes".}
         }
      }
   }

   #  Method to return row number given an index.
   method index value {
      set newvalue [_index $value 0 -1]
      if { $newvalue == -1 } {
         return ""
      } else {
         return $newvalue
      }
   }

   #  Method to set active row.
   method activate {row col} {
      set active [_index $row 0 0]
      ::focus $itk_component(${col}${active})
   }

   #  Method to make sure can "see" a particular row.
   method see value {
      set newvalue [_index $value 0 -1]
      if { $newvalue == -1 } {
         return
      } elseif {$newvalue == 0} {
         $itk_component(canvas) yview moveto 0
      } else {
         set y [winfo y $itk_component(0${newvalue})]
         set s [$itk_component(canvas) cget -scrollregion]
         set h [lindex $s 3]
         set viewfrac [expr double($y)/double($h)]
         set current [$itk_component(canvas) yview]
         set low [lindex $current 0]
         set high [lindex $current 1]
         if { $viewfrac < $low } {
            $itk_component(canvas) yview moveto $viewfrac
         } elseif {$viewfrac > $high } {
            set top [expr $y - $itk_option(-height) + $eheight]
            set viewfrac [expr double($top)/double($h)]
            $itk_component(canvas) yview moveto $viewfrac
         }
      }
   }

   #  Method curselection; returns current selection as a list of row numbers.
   method curselection {} {
      set values ""
      for { set i 0 } { $i < $itk_option(-rows) } { incr i } {
         if { $state($i) } {
            lappend values $i
         }
      }
      return $values
   }

   #  Set the scrollregion of canvas to used width and height. If the
   #  displayed width and height of the canvas haven't been set then set
   #  them to the used region so that all is displayed. Note scrollregion
   #  is the actual size of the canvas.
   private method _scrollregion {bounds} {
      if { $bounds == "" } {
         update idletasks
         set bounds [eval $itk_component(canvas) bbox $tags]
         if { $itk_option(-width) == "" } {
            set width [expr [lindex $bounds 2] -[lindex $bounds 0]]
            incr width
            configure -width $width
         }
         if { $itk_option(-height) == "" } {
            set height [expr [lindex $bounds 3] -[lindex $bounds 1]]
            incr height
            configure -height $height
         }
      }
      $itk_component(canvas) configure -scrollregion "$bounds"
   }


   #  Method (internal) to convert index into row value.
   private method _index { arguments element default args } {
      set value [lindex $arguments $element]
      switch -- $value {
         active {
            return $active
         }
         anchor {
            return $anchor
         }
         end {
            if { $itk_option(-rows) > 0 } {
               if { $args == "append" } {
                  return $itk_option(-rows)
               } else {
                  return [expr $itk_option(-rows) -1]
               }
            } else {
               return 0
            }
         }
         "" {
            return $default
         }
         default {
            return $value
         }
      }
   }

   # Methods (internal) to control the "highlighting" of the selected
   # parts. This is pseudo and just changes the background colour.
   private method _select { first last } {
      if { $first > $last } {
         set top $first
         set bot $last
      } else {
         set top $last
         set bot $first
      }
      for { set j $bot } { $j <= $top } { incr j } {
         if { ! $state($j) } {
            for { set i 0 } { $i < $itk_option(-columns) } { incr i } {
               $itk_component(${i}${j}) configure \
                  -background $itk_option(-selectBackground)
            }
            set state($j) 1
         }
      }
   }

   private method _deselect { first last } {
      if { $first > $last } {
         set top $first
         set bot $last
      } else {
         set top $last
         set bot $first
      }
      for { set j $bot } { $j <= $top } { incr j } {
         if { [info exists state($j)] } {
            if { $state($j) } {
               for { set i 0 } { $i < $itk_option(-columns) } { incr i } {
                  $itk_component(${i}${j}) configure \
                     -background $itk_option(-background)
               }
               set state($j) 0
            }
         }
      }
   }

   #  Methods (internal) for controlling the selection bindings. These
   #  are based on the listbox class procedures.
   private method _beginselect {row col x y} {
      selection clear 0 end
      selection set $row
      selection anchor $row
      set state(selection) {}
      set state(prev) $row
      set state(y) $y
      set state(x) $x
   }

   private method _motion {row col} {
      if {$row == $state(prev)} {
         return
      }
      if { $itk_option(-singleselect) } {
         selection clear 0 end
         selection set $row
      } else {
         set i $state(prev)
         selection clear $i $row
         selection set $anchor $row
         while { $i < $row } {
            if {[lsearch $state(selection) $i] >= 0} {
               selection set $i
            }
            incr i
         }
         while { $i > $row } {
            if {[lsearch $state(selection) $i] >= 0} {
               selection set $i
            }
            incr i -1
         }
      }
      set state(prev) $row
   }

   private method _beginextend {row col} {
      if { ( ! $itk_option(-singleselect) ) && [selection includes anchor]} {
         _motion $row $col
      }
   }

   private method _begintoggle {row col} {
      if { ! $itk_option(-singleselect) } {
         set state(selection) [curselection]
         set state(prev) $row
         selection anchor $row
         if [selection includes $row] {
            selection clear $row
         } else {
            selection set $row
         }
      }
   }

   private method _scanpos {rx ry} {
      set state(x) $rx
      set state(y) $ry
   }

   private method _autoscan {row col} {
      set xlow [winfo rootx $itk_component(canvas)]
      set ylow [winfo rooty $itk_component(canvas)]
      set xhigh [expr $xlow + $itk_option(-width)]
      set yhigh [expr $ylow + $itk_option(-height)]
      set state(y) [winfo pointery $itk_component(${col}${row})]
      set state(x) [winfo pointerx  $itk_component(${col}${row})]
      if { $state(y) > $yhigh } {
         $itk_component(canvas) yview scroll 1 units
      } elseif { $state(y) < $ylow } {
         $itk_component(canvas) yview scroll -1 units
      }
      if { $state(x) > $xhigh } {
         $itk_component(canvas) xview scroll 1 units
      } elseif { $state(x) < $xlow } {
         $itk_component(canvas) xview scroll -1 units
      }

      set curwin [winfo containing $state(x) $state(y)]
      set row ""
      if { $curwin != "" } {
         scan $curwin "$itk_component(canvas).frame%d.entry%d" coldum row
      } else {
         set cy [winfo y $itk_component(canvas)]
         if { $state(y) < $cy } {
            set row 0
         } else {
            set row $itk_option(-rows); incr row -1
         }
      }
      if { $row == "" } {
         set row $state(prev)
      }
      _motion $row $col
      set state(afterId) [after 25 [code $this _autoscan $row $col]]
   }

   private method _updown {row col amount} {
      set newrow [expr $state(prev) + $amount]
      if { $newrow > -1 && $newrow < $itk_option(-rows) } {
         see $newrow
         selection clear 0 end
         if { ! $itk_option(-singleselect) } {
            selection anchor $newrow
         }
         selection set $newrow
         set state(prev) $newrow
         set state(selection) {}
      }
   }

   private method _extendupdown {row col amount} {
      if { ! $itk_option(-singleselect) } {
         set newrow [expr $state(prev) + $amount]
         if { $newrow > -1 && $newrow < $itk_option(-rows) } {
            see $newrow
            _motion $newrow $col
         }
      }
   }

   private method _cancelrepeat { x y } {
      after cancel $state(afterId)
      set state(afterId) {}
      set state(x) $x
      set state(y) $y
   }

   #  Control reconfiguration of columns on the canvas. The places of any
   #  columns on the canvas are changed if the new canvas width makes a
   #  column greater or smaller in size, but not smaller than mincolumnwidth.
   private method _reconfigure {newwidth} {
      set smallest [expr $newwidth/$itk_option(-columns)]
      if { $smallest > $mincolumnwidth } {
         set newcolumnwidth $smallest
      } else {
         set newcolumnwidth $mincolumnwidth
      }
      if { $newcolumnwidth != $columnwidth } {
         set columnwidth $newcolumnwidth

         #  And make changes.
         set xoff 0
         for { set j 0 } { $j < $itk_option(-columns) } { incr j } {
            $itk_component(canvas) coords c$j $xoff 0
            $itk_component(canvas) itemconfigure c$j -width $columnwidth
            set xoff [expr $xoff + $columnwidth]
         }

         #  And adjust the positions of the text. Need update to make sure we
         #  get the real end not some intermediary position.
         if { $itk_option(-flushright) } {
            update
            for { set j 0 } { $j < $itk_option(-columns) } { incr j } {
               for { set i 0 } { $i < $itk_option(-rows) } { incr i } {
                  $itk_component(${j}${i}) xview end
               }
            }
         }
      }
      set itk_option(-height) [winfo height $itk_component(canvas)]
      set itk_option(-width)  [winfo width $itk_component(canvas)]
   }

   #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   #  Configuration options:
   #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   #  Number of rows of table that actually exist (note column must be
   #  created first, hence the check for havrows).
   itk_option define -rows rows Rows 0 {
      if { [info exists itk_option(-rows) ] &&
           [info exists havrows(0)] } {
         for { set j 0 } { $j < $itk_option(-columns) } { incr j } {
            if { $havrows($j) < $itk_option(-rows) } {
               for { set i $havrows($j) } { $i < $itk_option(-rows) } { incr i } {
                  itk_component add ${j}${i} {
                     entry $itk_component(canvas).frame$j.entry$i \
                        -background $itk_option(-background) \
                        -highlightbackground $itk_option(-background)
                  }
                  set ent $itk_component(${j}${i})

                  #  Set bindings for controlling selection.
                  ::bind $ent <1>         [code $this _beginselect $i $j %X %Y]
                  ::bind $ent <B1-Leave>  [code $this _autoscan $i $j]
                  ::bind $ent <ButtonRelease-1> [code $this _cancelrepeat %X %Y]
                  ::bind $ent <Shift-1>   [code $this _beginextend $i $j]
                  ::bind $ent <Control-1> [code $this _begintoggle $i $j]
                  ::bind $ent <B1-Enter>  [code $this _cancelrepeat %X %Y]
                  ::bind $ent <Up>        [code $this _updown $i $j -1]
                  ::bind $ent <Down>      [code $this _updown $i $j 1]
                  ::bind $ent <Shift-Up>  [code $this _extendupdown $i $j -1]
                  ::bind $ent <Shift-Down> [code $this _extendupdown $i $j 1]

                  #  And watch for potential modifications.
                  ::bind $ent <KeyPress>   [code $this configure -modified 1]

                  #  Set the action command if defined already.
                  if { $itk_option(-action) != {} } {
                     ::bind $ent <Return> [code $itk_option(-action)]
                  }

                  #  Pack entry widget into frame.
                  pack $ent -fill x
                  set state($i) 0
               }
               set havrows($j) $itk_option(-rows)
            } elseif { $havrows($j) > $itk_option(-rows) } {

               #  Need to delete some.
               for { set i $itk_option(-rows) } { $i < $havrows($j) } { incr i } {
                  destroy $itk_component(${j}${i})
                  set state($i) 0
               }
               set havrows($j) $itk_option(-rows)
            }

         }
         #  Set the scrollregion to this new size.
         _scrollregion {}
      }
   }

   #  If any of the entries have been modified (i.e. keypress of unknown nature).
   itk_option define -modified modified Modified 0 {}

   #  Number of columns that exist in widget.
   itk_option define -columns columns Columns 0 {
      if { $havcols < $itk_option(-columns) } {

         #  Need more columns, so create the frame/label pairs. Once created
         #  place onto the canvas.
         for { set i $havcols } { $i < $itk_option(-columns) } { incr i } {
            itk_component add $i {
               set f [frame $itk_component(canvas).frame$i \
                         -background $itk_option(-background)]
            }
            itk_component add ${i}label {
               label $f.label -text "Column [expr $i +1]"
            }
            $itk_component(canvas) create window $xoff 0 \
               -window $f -anchor nw -tag c$i -width $columnwidth
            pack $f.label -fill x
            lappend tags c$i
            set xoff [expr $xoff +$columnwidth]

            #  No rows created in this column yet.
            set havrows($i) 0
         }
         set havcols $itk_option(-columns)
         configure -rows $itk_option(-rows)

      } elseif { $havcols > $itk_option(-columns) } {

         #  Need to delete some.
         for { set i $itk_option(-columns) } { $i < $havcols } { incr i } {
            destroy $itk_component($i)

            #  No rows in this column.
            set havrows($i) 0
            set xoff [expr $xoff -$columnwidth]
         }
         set havcols $itk_option(-columns)

         #  Set the scrollregion to this new size.
         _scrollregion {}
      }
   }


   #  Width and height of the displayed part of the canvas. Note its
   #  actual size is set by the scrollregion configuration option.
   itk_option define -width width Width {} {
      if { $itk_option(-width) != "" } {
         $itk_component(canvas) configure -width $itk_option(-width)
      }
   }
   itk_option define -height height Height {} {
      if { $itk_option(-height) != "" } {
         $itk_component(canvas) configure -height $itk_option(-height)
      }
   }

   #  Selection method available.
   itk_option define -singleselect singleselect SingleSelect 1 {}

   #  Create the scrollbars and put into the correct position.
   itk_option define -scrollbarplaces scrollbarplaces ScrollBarPlaces {right bottom} {
      foreach side $itk_option(-scrollbarplaces) {
         if { ! [ regexp (left|right|top|bottom|none) $side ] } {
            error "Unknown scrollbar placement \"$side\", should be top bottom left right or none"
         }
      }

      #  Do not create scrollbars if right is none.
      if { [lindex $itk_option(-scrollbarplaces) 0] != "none" } {

         #  Unpack the canvas widget, as needs to be packed last.
         pack forget $itk_component(canvas)

         #  Delete all existing scrollbars and padding frames.
         if { [info exists itk_component(hscroll)] } {
            destroy $itk_component(hscroll)
            destroy $itk_component(bit)
            destroy $itk_component(pad)
            $itk_component(canvas) configure -xscrollcommand {}
         }
         if { [info exists itk_component(vscroll)] } {
            destroy $itk_component(vscroll)
            $itk_component(canvas) configure -yscrollcommand {}
         }
         set vert [lsearch -regexp $itk_option(-scrollbarplaces) (right|left)]
         set hori [lsearch -regexp $itk_option(-scrollbarplaces) (top|bottom)]

         #  Vertical scrollbar is just created.
         if { $vert != -1 } {
            itk_component add vscroll {
               scrollbar $w_.vscroll \
                  -command [code $itk_component(canvas) yview] \
                  -orient vertical
            }
            $itk_component(canvas) configure \
               -yscrollcommand  [code $itk_component(vscroll) set]
         }

         #  Horizontal scrollbar requires packing frames for indentation at corners.
         if { $hori != -1 } {
            itk_component add pad {
               frame $w_.pad
            }
            #  Get width of vertical scrollbar. Make corner frame same width.
            if { $vert != -1 } {
               set padwidth [$itk_component(vscroll) cget -width]
               itk_component add bit {
                  frame $itk_component(pad).bit -width $padwidth
               }
               set padside [lindex $itk_option(-scrollbarplaces) $vert]
            } else {
               itk_component add bit {
                  frame $itk_component(pad).bit
               }
               set padside right
            }
            itk_component add hscroll {
               scrollbar $w_.pad.hscroll \
                  -command [code $itk_component(canvas) xview] \
                  -orient horizontal
            }
            $itk_component(canvas) configure \
               -xscrollcommand [code $itk_component(hscroll) set]
            set side [lindex $itk_option(-scrollbarplaces) $hori]
         }

         #  Now do packing. Place horizontal first to fill extent and get
         #  padding frames to take up extra.
         if { $hori != -1 } {
            pack $itk_component(pad) -fill x -side $side
            pack $itk_component(bit) -side $padside
            pack $itk_component(hscroll) -fill x -side $side
         }
         if { $vert != -1 } {
            pack $itk_component(vscroll) \
               -side [lindex $itk_option(-scrollbarplaces) $vert] -fill y
         }
         pack $itk_component(canvas)  -expand true -fill both
      } else {
         $itk_component(canvas) configure -xscrollcommand {}
         $itk_component(canvas) configure -yscrollcommand {}
         pack $itk_component(canvas)  -expand true -fill both
      }
   }

   #  Pad value, not set unless asked for.
   itk_option define -padvalue padvalue PadValue {} {}

   #  Whether any text is flushed right (so that the end of the insertion can
   #  be viewed or not).
   itk_option define -flushright flushright FlushRight 1 {}

   #  Command to execute when <Return> is pressed in any of the entry
   #  widgets.
   itk_option define -action action Action {} {
      for { set i 0 } { $i < $itk_option(-rows) } { incr i } {
         for { set j 0 } { $j < $itk_option(-columns) } { incr j } {
            ::bind $itk_component(${j}${i}) <Return> [code $itk_option(-action)]
         }
      }
   }

   #  Background colour for deselected cells.
   itk_option define -background background Background lightgrey {}

   #  Background colour for selected cells.
   itk_option define -selectBackground \
      selectBackground SelectBackground lightblue2 {}

   #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   #  Common and protected variables.  Common are visible to all instances
   #  of this class, protected to just this instance (both are available
   #  anywhere in the scope of this class and in derived classes).
   #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   #  Number of rows and columns already created. Notes havrows is an
   #  array index by column number as the creation of rows and columns
   #  happen in separate parts (so a column can be created and is later
   #  filled with rows, possibly at different times to the other columns
   #  and rows).
   protected variable havrows
   protected variable havcols 0

   #  Width of column (pixels).
   protected variable xoff 0
   protected variable columnwidth 0
   protected variable mincolumnwidth 5

   #  Name of any canvas tags.
   protected variable tags ""

   #  Selection control
   protected variable state
   protected variable anchor 0
   protected variable active 0

   #  Height of an entry widget.
   protected variable eheight

   #  End of class defintion.
}

