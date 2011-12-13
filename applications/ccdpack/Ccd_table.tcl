#+
#  Name:
#     Ccd::table

#  Purpose:
#     Defines a table widget with editable cells.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tcl] class

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

#  Notes:
#     Efficiency: changing the number of rows and columns using the
#     "insert" methods can be quite slow (this is because of a
#     "update" that is necessary to get the new size of the canvas to
#     define the scrollable region) it is much better to set the size
#     of the table as a configuration option (with -rows and
#     -columns), before inserting new entries.
#
#     This probably isn't the best implementation for very large tables.

#  Configuration Options:
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
#        -scrollbarplaces (right|left) (top|bottom)
#
#     Sets where the scrollbars are to be placed. The default is
#     "right bottom".
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

#  Inheritance:
#     This class inherits "Ccd::base" and its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Invocations:
#        Ccd::table window [-option value]...
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

#  Methods:
#     constructor [-option value]...
#        This method is invoked automatically by the class command and
#        creates the "Ccd::table" widget with a default configuration,
#        except when overridden by command line options.
#     destructor
#        Destroys the "Ccd::table" instance, invoked by the "delete" method.
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
#     sethelp docname label
#        Sets the document and label for the help associated with
#        this widget as a whole.
#
#     Internal methods (should not be used outside of this class defintion).
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

#  Copyright:
#     Copyright (C) 1995, 2000 Central Laboratory of the Research
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
#     1-JUN-1995 (PDRAPER):
#        Original version.
#     15-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     27-JAN-2006 (PDRAPER):
#        Updated to itcl::class syntax.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

   itcl::class Ccd::table {

#  Inheritances:
      inherit Ccd::base

#.

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construction creates a instance of the class and configures it with
#  the default and command-line options.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { args } {

#  Create a frame widget. This must have the same name as the class
#  command.
#  Create the canvas widget to hold the frames. Stop it from taking
#  the focus and confine the scrollable region.
         CCDTkWidget Canvas canvas \
            canvas $oldthis.canvas -takefocus 0 -confine 1
         set widgetnames($Oldthis:canvas) $Canvas

#  Initialise the minimum width of a column on the canvas.
         entry $oldthis.e
         frame $oldthis.f
         set ewidth [expr [winfo reqwidth $oldthis.e] + [$oldthis.e cget -borderwidth] ]
         set columnwidth [expr $ewidth + [$oldthis.f cget -borderwidth] ]
         set mincolumnwidth $columnwidth

#  Remember height and remove the unwanted widgets.
         set eheight [expr [winfo reqheight $oldthis.e] + [$oldthis.e cget -borderwidth] ]
         destroy $oldthis.e
         destroy $oldthis.f

#  Set default configurations. Note scrollbars etc. created as part of
#  scrollbar placement. Rows as part of column creation.
         eval configure $args
         configure -scrollbarplaces $scrollbarplaces
         configure -columns         $columns

#  Set state(afterId) to {}. This is used in bindings for controlling
#  selection. Also set other states that require initialisation
         set state(afterId) {}
         set state(prev) 0
         set state(selection) {}
         set state(x) 0
         set state(y) 0
         set state(0) 0

#  Finally set binding to control resizing of the entry widget widths.
         ::bind $canvas <Configure> "$Oldthis _reconfigure %w"
      }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Set column label.
      method setlabel {col label} {
         if { [info exists Framelabels($col)] } {
            $Framelabels($col) configure -text $label
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
         if { $nargs == "1" && $columns != "1" } {
            set args [split $args]
         }
         if { [ llength $args ] >= $columns } {

#  Expand $row in case has "end", only want to do this once.
            set minrows [_index $row 0 0 append]
            set i 0
            foreach item $args {
               insertcell $minrows $i $item
               incr i
            }
         } elseif { [info exists pvalue] } {
            set minrows [_index $row 0 0 append]
            set i 0
            foreach item $args {
               insertcell $minrows $i $item
               incr i
            }
            for {} { $i < $columns } { incr i } {
               insertcell $minrows $i $pvalue
            }
         } else {
            error "Need \"$columns\" elements for insertion into row"
         }
      }

#  Insert column of data method.
      method insertcolumn { col args } {

#  Check that the number of elements in the args list is equal to the
#  number of rows. If only one element is present try to expand
#  this as a list.
         set nargs [ llength $args ]
         if { $nargs == "1" && $rows != "1" } {
            set args [split $args]
         }
         if { [ llength $args ] >= $rows } {
            set i 0
            foreach item $args {
               insertcell $i $col $item
               incr i
            }
         } elseif { [info exists pvalue] } {
            set i 0
            foreach item $args {
               insertcell $i $col $item
               incr i
            }
            for {} { $i < $rows } { incr i } {
               insertcell $i $col $pvalue
            }
         } else {
            error "Need \"$rows\" elements for insertion into column"
         }
      }

#  Insert data into cell method. Clears the current contents and makes
#  sure that we can see the end of the new insertion if flushright is set.
      method insertcell { row col value } {
         set row [_index $row 0 0 append]
         if { $row >= $rows } {
            configure -rows [expr $row +1]
         }
         if { $col >= $columns } {
            configure -columns [expr $col +1]
         }
         $Entries($col,$row) delete 0 end
         $Entries($col,$row) insert 0 $value
         if { $flushright } {
            $Entries($col,$row) xview end
         }
      }

#  Get values from a row.
      method getrow { row } { get $row }
      method get { row } {
         set row [_index $row 0 0 ]
         set values ""
         for { set i 0 } { $i < $columns } { incr i } {
            lappend values [$Entries($i,$row) get]
         }
         return $values
      }

#  Get values from a column.
      method getcol { col } {
         set values ""
         for { set i 0 } { $i < $rows } { incr i } {
            lappend values [$Entries($col,$i) get]
         }
         return $values
      }

#  Get value from a cell.
      method getcell { row col } {
         set row [_index $row 0 0]
         return [$Entries($row,$col) get]
      }

#  Enable insertion state for methods for rows, columns and cells.
#  If no value is given then the current state is returned. This may
#  be mixed if a mixture of enabled and disabled is found.
      method colstate { col istate } {
         if { $istate == "normal" || $istate == "disabled" } {
            if { $istate != "" } {
               for { set i 0 } { $i < $rows } { incr i } {
                  $Entries($col,$i) configure -state $istate
               }
            } else {
               set istate ""
               for { set i 0 } { $i < $rows } { incr i } {
                  set cellstate [$Entries($col,$i) cget -state]
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
               for { set i 0 } { $i < $columns } { incr i } {
                  $Entries($i,$row) configure -state $istate
               }
            } else {
               set istate ""
               for { set i 0 } { $i < $columns } { incr i } {
                  set cellstate [$Entries($i,$row) cget -state]
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
               $Entries($col,$row) configure -state $istate
            } else {
               return [$Entries($col,$row) cget -state]
            }
         } else {
            error "Unknown state \"$istate\" must one of \"normal\" or \"disabled\""
         }
      }

#  Get the number of rows.
      method size {}  {
         return $rows
      }

#  Get the number of rows.
      method rownum {}  {
         return $rows
      }
#  Get the number of rows.
      method colnum {}  {
         return $columns
      }

#  Clear a range of rows. Note this actually deletes the entry widgets
#  in these rows and is fundermentally different from clearing a cell.
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
         if { $last > $rows } { set last $rows }

#  Remove any selection from these rows.
         _deselect $first $last

#  Shuffle data so that rows are moved, then reconfigure to remove the
#  extra rows.
         set k $first
         for { set i $last } { $i < $rows } { incr i } {
            for { set j 0 } { $j < $columns } { incr j } {
               set Entryi $Entries($j,$i)
               set Entryk $Entries($j,$k)
               $Entryk delete 0 end
               $Entryk insert 0 [$Entryi get]
               $Entryi delete 0 end
            }
            incr k
         }
         configure -rows [expr $rows - $last + $first]
      }

#  Clear a range of columns. Note this actually deletes the entry widgets
#  in these columns and is fundermentally different from clearing a cell.
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
         if { $last > $columns } { set last $columns }

#  Shuffle data so that columns are moved, then reconfigure to remove the
#  extra ones.
         set k $first
         for { set i $last } { $i < $columns } { incr i } {
            for { set j 0 } { $j < $rows } { incr j } {
               set Entryi $Entries($i,$j)
               set Entryk $Entries($k,$j)
               $Entryk delete 0 end
               $Entryk insert 0 [$Entryi get]
               $Entryi delete 0 end
            }
            incr k
         }
         configure -columns [expr $columns - $last + $first]
      }

#  Clear a cell.
      method clearcell { row col } {
         if [info exists Entries($col,$row)] {
            $Entries($col,$row) clear 0 end
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
         ::focus [CCDPathOf $Entries($col,$active)]
      }

#  Method to make sure can "see" a particular row.
      method see value {
         set newvalue [_index $value 0 -1]
         if { $newvalue == -1 } {
            return
         } elseif {$newvalue == 0} {
            $Canvas yview moveto 0
         } else {
            set y [winfo y $Entries(0,$newvalue)]
            set s [$Canvas cget -scrollregion]
            set h [lindex $s 3]
            set viewfrac [expr double($y)/double($h)]
            set current [$Canvas yview]
            set low [lindex $current 0]
            set high [lindex $current 1]
            if { $viewfrac < $low } {
               $Canvas yview moveto $viewfrac
            } elseif {$viewfrac > $high } {
               set top [expr $y - $height + $eheight]
               set viewfrac [expr double($top)/double($h)]
               $Canvas yview moveto $viewfrac
            }
         }
      }

#  Method curselection; returns current selection as a list of row numbers.
      method curselection {} {
         set values ""
         for { set i 0 } { $i < $rows } { incr i } {
            if { $state($i) } {
               lappend values $i
            }
         }
         return $values
      }


#  Set the context help for this widget (as a whole).
      method sethelp { docname label } {
         if $exists {
            Ccd::base::sethelp $Oldthis $docname $label
         }
      }

#  Set the scrollregion of canvas to used width and height. If the
#  displayed width and height of the canvas haven't been set then set
#  them to the used region so that all is displayed. Note scrollregion
#  is the actual size of the canvas.
      method _scrollregion {bounds} {
         if $exists {
            if { $bounds == "" } {
               update idletasks
               set bounds [eval $Canvas bbox $tags]
               if { $width == "" } {
                  set width [expr [lindex $bounds 2] -[lindex $bounds 0]]
                  incr width
                  configure -width $width
               }
               if { $height == "" } {
                  set height [expr [lindex $bounds 3] -[lindex $bounds 1]]
                  incr height
                  configure -height $height
               }
            }
            $Canvas configure -scrollregion "$bounds"
         }
      }


#  Method (internal) to convert index into row value.
      method _index { arguments element default args } {
         set value [lindex $arguments $element]
         switch -- $value {
            active {
               return $active
            }
            anchor {
               return $anchor
            }
            end {
               if { $rows > 0 } {
                  if { $args == "append" } {
                     return $rows
                  } else {
                     return [expr $rows -1]
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
      method _select { first last } {
         if { $first > $last } {
            set top $first
            set bot $last
         } else {
            set top $last
            set bot $first
         }
         for { set j $bot } { $j <= $top } { incr j } {
            if { ! $state($j) } {
               for { set i 0 } { $i < $columns } { incr i } {
                  $Entries($i,$j) configure -background $select
               }
               set state($j) 1
            }
         }
      }

      method _deselect { first last } {
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
                  for { set i 0 } { $i < $columns } { incr i } {
                     $Entries($i,$j) configure -background $notselect
                  }
                  set state($j) 0
               }
            }
         }
      }


#  Methods (internal) for controlling the selection bindings. These
#  are based on the listbox class procedures.
      method _beginselect {row col x y} {
         selection clear 0 end
         selection set $row
         selection anchor $row
         set state(selection) {}
         set state(prev) $row
         set state(y) $y
         set state(x) $x
      }

      method _motion {row col} {
         if {$row == $state(prev)} {
            return
         }
         if { $singleselect } {
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

      method _beginextend {row col} {
         if { ( ! $singleselect ) && [selection includes anchor]} {
            _motion $row $col
         }
      }

      method _begintoggle {row col} {
         if { ! $singleselect } {
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

      method _scanpos {rx ry} {
         set state(x) $rx
         set state(y) $ry
      }

      method _autoscan {w row col} {
         set xlow [winfo rootx $canvas]
         set ylow [winfo rooty $canvas]
         set xhigh [expr $xlow + $width]
         set yhigh [expr $ylow + $height]
         set state(y) [winfo pointery $w]
         set state(x) [winfo pointerx $w]
         if { $state(y) > $yhigh } {
           $Canvas yview scroll 1 units
         } elseif { $state(y) < $ylow } {
            $Canvas yview scroll -1 units
         }
         if { $state(x) > $xhigh } {
            $Canvas xview scroll 1 units
         } elseif { $state(x) < $xlow } {
            $Canvas xview scroll -1 units
         }

         set curwin [winfo containing $state(x) $state(y)]
         set row ""
         if { $curwin != "" } {
            regexp {[0-9]*$} $curwin row
         } else {
            set cy [winfo y $oldthis.canvas]
            if { $state(y) < $cy } {
               set row 0
            } else {
               set row $rows; incr row -1
            }
         }
         if { $row == "" } {
            set row $state(prev)
         }
         _motion $row $col
         set state(afterId) [after 25 $Oldthis _autoscan $w $row $col]
      }

      method _updown {row col amount} {
         set newrow [expr $state(prev) + $amount]
         if { $newrow > -1 && $newrow < $rows } {
            see $newrow
            selection clear 0 end
            if { ! $singleselect } {
               selection anchor $newrow
            }
            selection set $newrow
            set state(prev) $newrow
            set state(selection) {}
         }
      }

      method _extendupdown {row col amount} {
         if { ! $singleselect } {
            set newrow [expr $state(prev) + $amount]
            if { $newrow > -1 && $newrow < $rows } {
               see $newrow
               _motion $newrow $col
            }
         }
      }

      method _cancelrepeat { x y } {
         after cancel $state(afterId)
         set state(afterId) {}
         set state(x) $x
         set state(y) $y
      }

#  Control reconfiguration of columns on the canvas. The places of any
#  columns on the canvas are changed if the new canvas width makes a
#  column greater or smaller in size, but not smaller than mincolumnwidth.
      method _reconfigure { newwidth } {
         set smallest [expr $newwidth / $columns]
         if { $smallest > $mincolumnwidth } {
            set newcolumnwidth $smallest
         } else {
            set newcolumnwidth $mincolumnwidth
         }
         if { $newcolumnwidth != $columnwidth } {
            set columnwidth $newcolumnwidth

#  And make changes.
            set xoff 0
            for { set j 0 } { $j < $columns } { incr j } {
               $Canvas coords c$j $xoff 0
               $Canvas itemconfigure c$j -width $columnwidth
               set xoff [expr $xoff + $columnwidth]
            }

#  And adjust the positions of the text. Need update to make sure we
#  get the real end not some intermediary position.
            if { $flushright } {
               update
               for { set j 0 } { $j < $columns } { incr j } {
                  for { set i 0 } { $i < $rows } { incr i } {
                     $Entries($j,$i) xview end
                  }
               }
            }
         }
         set height [winfo height $canvas]
         set width  [winfo width $canvas]
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Number of rows of table that actually exist.
      public variable rows 0 {
         if { $exists } {
            for { set j 0 } { $j < $columns } { incr j } {
               if { $havrows($j) < $rows } {
                  for { set i $havrows($j) } { $i < $rows } { incr i } {
                     CCDTkWidget Ent ent \
                        entry $Frames($j).entry$i \
                                 -background $notselect \
                                 -highlightbackground $notselect
                     set widgetnames($Oldthis:entry$i) $Ent
                     set Entries($j,$i) $Ent

#  Set bindings for controlling selection.
                     ::bind $ent <1>         "$Oldthis _beginselect $i $j %X %Y"
                     ::bind $ent <B1-Leave>  "$Oldthis _autoscan $ent $i $j"
                     ::bind $ent <ButtonRelease-1> "$Oldthis _cancelrepeat %X %Y"
                     ::bind $ent <Shift-1>   "$Oldthis _beginextend $i $j"
                     ::bind $ent <Control-1> "$Oldthis _begintoggle $i $j"
                     ::bind $ent <B1-Enter>  "$Oldthis _cancelrepeat %X %Y"
                     ::bind $ent <Up>        "$Oldthis _updown $i $j -1"
                     ::bind $ent <Down>      "$Oldthis _updown $i $j 1"
                     ::bind $ent <Shift-Up>  "$Oldthis _extendupdown $i $j -1"
                     ::bind $ent <Shift-Down> "$Oldthis _extendupdown $i $j 1"

#  And watch for potential modifications.
                     ::bind $ent <KeyPress>   "$Oldthis configure -modified 1"

#  Pack entry widget into frame.
                     pack $ent -fill x
                     set state($i) 0
                  }
                  set havrows($j) $rows
               } elseif { $havrows($j) > $rows } {

#  Need to delete some.
                  for { set i $rows } { $i < $havrows($j) } { incr i } {
                     destroy [CCDPathOf $Entries($j,$i)]
                     unset Entries($j,$i)
                     set state($i) 0
                  }
                  set havrows($j) $rows
               }

            }
#  Set the scrollregion to this new size.
            _scrollregion {}
         }
      }

#  If any of the entries have been modified (i.e. keypress of unknown nature).
      public variable modified 0 {}

#  Number of columns that exist in widget.
      public variable columns 1 {
         if { $exists } {
            if { $havcols < $columns } {

#  Need more columns, so create the frame/label pairs. Once created
#  place onto the canvas.
               for { set i $havcols } { $i < $columns } { incr i } {
                  CCDTkWidget F f \
                     frame $canvas.frame$i -background $notselect
                  set Frames($i) $F
                  CCDTkWidget Labelwidget labelwidget \
                     label $f.label -text "Column [expr $i +1]"
                  set Framelabels($i) $Labelwidget
                  $Canvas create window $xoff 0 \
                     -window $f -anchor nw -tag c$i -width $columnwidth
                  pack $labelwidget -fill x
                  lappend tags c$i
                  set xoff [expr $xoff +$columnwidth]
                  set widgetnames($Oldthis:canvas) $Labelwidget

#  No rows created in this column yet.
                  set havrows($i) 0
               }
               set havcols $columns
               configure -rows $rows
            } elseif { $havcols > $columns } {

#  Need to delete some.
               for { set i $columns } { $i < $havcols } { incr i } {
                  destroy [CCDPathOf $Frames($i)]
                  unset Frames($i)

#  No rows in this column.
                  set havrows($i) 0
                  set xoff [expr $xoff -$columnwidth]
               }
               set havcols $columns

#  Set the scrollregion to this new size.
               _scrollregion {}
            }
         }
      }

#  Width and height of the displayed part of the canvas. Note its
#  actual size is set by the scrollregion configuration option.
      public variable width {} {
         if { $exists } {
            if { $width != "" } {
               $Canvas configure -width $width
            }
         }
      }
      public variable height {} {
         if { $exists } {
            if { $height != "" } {
               $Canvas configure -height $height
            }
         }
      }

#  Selection method available.
      public variable singleselect 1 {}

#  Create the scrollbars and put into the correct position.
      public variable scrollbarplaces { right bottom } {
         foreach side $scrollbarplaces {
            if { ! [ regexp (left|right|top|bottom) $side ] } {
               error "Unknown scrollbar placement \"$side\", should be top bottom left or right"
            }
         }

#  Only proceed if the object exists (this means that constructor has
#  been invoked).
         if { $exists } {


#  Unpack the canvas widet, as needs to be packed last.
            pack forget $canvas

#  Delete all existing scrollbars and padding frames.
            if { [ winfo exists $hscroll ] } {
               destroy $hscroll
               destroy $bit
               destroy $pad
               set hscroll ""
               set bit ""
               set pad ""
               $Canvas configure -xscrollcommand {}
            }
            if { [ winfo exists $vscroll ] } {
               destroy $vscroll
               set vscroll ""
               $Canvas configure -yscrollcommand {}
            }
            set vert [lsearch -regexp $scrollbarplaces (right|left)]
            set hori [lsearch -regexp $scrollbarplaces (top|bottom)]

#  Vertical scrollbar is just created.
            if { $vert != -1 } {
               CCDTkWidget Vscroll vscroll \
                  scrollbar $oldthis.vscroll \
                     -command "$Canvas yview" -orient vertical
               set widgetnames($Oldthis:vscroll) $Vscroll
               $Canvas configure -yscrollcommand "$Vscroll set"
            }

#  Horizontal scrollbar requires packing frames for indentation at corners.
            if { $hori != -1 } {
               CCDTkWidget Pad pad frame $oldthis.pad

#  Get width of vertical scrollbar. Make corner frame same width.
               if { $vert != -1 } {
                  set padwidth [$Vscroll cget -width]
                  CCDTkWidget Bit bit frame $Pad.bit -width $padwidth
                  set padside [lindex $scrollbarplaces $vert]
               } else {
                  CCDTkWidget Bit bit frame $Pad.bit
                  set padside right
               }

               CCDTkWidget Hscroll hscroll \
                  scrollbar $Pad.hscroll \
                             -command "$Canvas xview" -orient horizontal
               set widgetnames($Oldthis:hscroll) $Hscroll
               $Canvas configure -xscrollcommand "$Hscroll set"
               set side [lindex $scrollbarplaces $hori]
            }

#  Now do packing. Place horizontal first to fill extent and get
#  padding frames to take up extra.
            if { $hori != -1 } {
               pack $pad -fill x -side $side
               pack $bit -side $padside
               pack $hscroll -fill x -side $side
            }
            if { $vert != -1 } {
               pack $vscroll -side [lindex $scrollbarplaces $vert] -fill y
            }
            pack $canvas  -expand true -fill both
         }
      }

#  Pad value, not set unless asked for.
      public variable padvalue {} {
         set pvalue $padvalue
      }

#  Whether any text is flushed right (so that the end of the insertion can
#  be viewed or not).
      public variable flushright 1 {}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Common and protected variables.  Common are visible to all instances
#  of this class, protected to just this instance (both are available
#  anywhere in the scope of this class and in derived classes).
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Names of widgets.
      protected variable Canvas
      protected variable canvas ""
      protected variable Frames
      protected variable Framelabels
      protected variable Entries
      protected variable Hscroll
      protected variable hscroll ""
      protected variable Vscroll
      protected variable vscroll ""
      protected variable Bit
      protected variable bit ""
      protected variable Pad
      protected variable pad ""

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

#  Background colour for deselected cells.
      protected variable notselect lightgrey

#  Background colour for selected cells.
      protected variable select lightblue2

#  Selection control
      protected variable state
      protected variable anchor 0
      protected variable active 0

#  Height of an entry widget.
      protected variable eheight

#  Pad value for inserting into row or column when not enough values
#  are given.
      protected variable pvalue

#  End of class defintion.
   }

# $Id$
