#+
#  Name:
#     GaiaPolCat

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Describes a catalogue with an associated selection.

#  Description:
#     A GaiaPolCat object represents a selection of rows from a specific
#     Polpack catalogue. The selected rows may be changed, but the catalogue
#     data itself cannot be changed. If the data has a Z column (e.g.
#     spectropolarimetry data), then the zvals attribute can be used to
#     make it appear as if the catalogue only contains data for a single
#     specified Z column value. All other rows look as if they have been
#     marked for deletion. The values of the zvals attribute can be changed
#     at any time to reveal or hide specific Z values.

#  Invocations:
#
#        GaiaPolCat object_name disk-file
#
#     This creates an instance of a GaiaPolCat object. The returned value
#     is the name of the object. Disk-file is the full path for the disk
#     file holding the Polpack catalogue.
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
#     See the "itk_option define" declarations below.

#  Inheritance:
#     ::gaia::GaiaPolObject

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
#     DSB: David S. Berry  (STARLINK)
#     {enter_new_authors_here}

#  History:
#     25-AUG-2000 (DSB):
#        Original version.
#     2-MAR-2001 (DSB):
#        Added Z filtering though the use of the new attribute zvals_.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaPolCat {

#  Inheritances:
   inherit gaia::GaiaPolObject

#  Constructor:
#  ===========

#  $file = path for a disk file holding a vector catalogue readable by polpack.
#  $w = path for the top-level window associated with $this.
#  $pbar = the ProgressBar object associated with this $this.

   constructor { file w pbar {data ""} } {

#  Now initialize the class data. If this constructor has been invoked
#  to construct the base class part of some super class, do not
#  initialize the data since this will be done as a consequence of
#  initializeing the super class data.
      if { [$this info class] == "::gaia::GaiaPolCat" } {
         init $file $w $pbar $data
      }
   }

#  Destructor:
#  ===========
   destructor {

#  If a large GaiaPolData is about to be destroyed, then pause for a moment.
      if { [catch {set nrow [$data_ getNrow]}] } { set nrow 0 }
      if { $nrow > 10000 && [$data_ refCount] == 1 } {
         after 1000 {set a 1}
         tkwait variable a
         catch {$data_ annull}
         catch {destroy $w}

#  If the catalogue is small, or if this is not the final reference to
#  the GaiaPolData, then just do it.
      } else {
         catch {$data_ annull}
      }
   }

#  Initialiser:
#  ============
#  Override the parent Init method to initialise the contents of the
#  memory allocated by the GaiaPolCat constructor using a user-supplied
#  argument list.
   protected method init { file w pbar data } {

#  First initialize the parent class data
      gaia::GaiaPolObject::init

#  Now initialize this class.
      set w_ $w
      set pbar_ $pbar

#  If not supplied, create a GaiaPolData object to store an immutable
#  description of the catalogue, including the main data array.
      if { $data == "" } {
         set data_ [::gaia::GaiaPolData data#auto $file $w $pbar]
      } else {
         set data_ [$data clone]
      }

#  Initialise an array of state flags (indexed by row number) indicating if
#  the corresponding row is selected (S), unselected (U), or marked for
#  deletion (D).
      set nrow [$data_ getNrow]
      for {set i 0} {$i < $nrow} {incr i} {
         set states_($i) "U"
      }
   }

#  Public methods:
#  ===============

#  Returns a new GaiaPolCat holding a binned copy of $this.
#  -------------------------------------------------------
   public method bin { box method debias minval sigmas {integ 0} } {
      set ret ""

#  Create a new data arrays containing only the required rows.
      if { $integ } {
         set data [purge 0]
      } else {
         set data [purge 1]
      }

#  Report an error if no data remains.
      if { $data == "empty" } {
         error_dialog "No data to bin"

#  Otherwise...
      } else {

#  Create a polpack catalogue holding the binned vectors. The GaiaPolData bin
#  method returns the path to the polpack catalogue created.
         set bincat [$data_ bin $data $box $method $debias $minval $sigmas $integ]

#  If succesful, create a new GaiaPolCat for this polpack catalogue.
         if { $bincat != "" } {
            set ret [code [::gaia::GaiaPolCat PolCat#auto $bincat $w_ $pbar_] ]
            if { $ret != "" } {
               $ret setChanged 1
               $ret setWarned [$data_ getWarned]
               $ret setDesc "bin vectors"
            }
         }
      }

#  Return the object
      return $ret
   }

#  Effectively remove all vectors with state equal to $remove from
#  $this by marking them as deleted. Remove should be one of U or S.
#  -----------------------------------------------------------------
   public method cut { {remove S} } {
      upvar 0 [getStates] states

#  Abort if this operation would remove all vectors from the map.
      set ok 1
      if { $remove == "S" } {
         if { $nuns_ == 0 && $nsel_ > 0 } {
            error_dialog "Request to delete all vectors is being ignored"
            set ok 0
         }
      } else {
         if { $nuns_ > 0 && $nsel_ == 0 } {
            error_dialog "Request to delete all vectors is being ignored"
            set ok 0
         }
      }

      if { $ok } {
         set inc 0
         set nrow [$data_ getNrow]

         for {set i 0} {$i < $nrow} {incr i} {
            if { $states($i) == $remove } {
               set inc 1
               set states($i) "D"
            }
         }

         if { $inc } {
            incr chid_
            set changed_ 1
         }
      }
   }

#  Return 1 if there are no selected rows.
#  --------------------------------------
   public method noSel {} {

#  Ensure the nsel_ data member is up to date.
      getStates

#  Return the answer.
      if { $nsel_ == 0 } {
         return 1
      } else {
         return 0
      }
   }

#  Save $this to a new disk file which can be read by polpack. If $all is
#  non-zero both selected and unselected vectors are saved, otherwise only
#  the selected vectors are saved.
#  ----------------------------------------------------------------------
   public method save { file {all 1}} {
      set ret 0

#  Indicate what is happening
      $pbar_ config -text "Writing $file  ..."
      update idletasks

#  Create a new data array without any deleted rows, or (if all
#  is zero) any unselected rows.
      set data [purge $all]

#  Report an error if no data remains.
      if { $data == "empty" } {
         error_dialog "No data to save"
         set changed_ 0

#  Otherwise...
      } else {

#  Save it to disk and then annul it.
         set ret [$data_ save $data $file]

#  Indicate that the disk file is now up-to-date.
         if { $ret } {
            set changed_ 0
         }
      }

#  Reset the progress bar.
      $pbar_ reset
      update idletasks

      return $ret

   }

#  Modifies the PolCat by marking a specified list of rows as "deleted".
#  --------------------------------------------------------------------
   public method delete { rows } {
      set inc 0
      foreach row $rows {
         set states_($row) "D"
         set inc 1
      }
      if { $inc } {
         incr chid_
         set changed_ 1
      }
   }

#  Modifies the PolCat by selecting a specified rows. $type indicates how
#  the rows are specified:
#     "rows" - select an explicit list of row indices supplied in $data
#     "circle" - select all vectors within a circle (bounding box in $data)
#     "box"  - select all vectors within a rectangle (bounding box in $data)
#     "expr" - select all vectors satisfying an algebraic expression (in
#              $data).
#  --------------------------------------------------
   public method select { type data } {
      return [choose $type $data "S"]
   }

#  Modifies the PolCat by deselecting a specified rows. $type indicates how
#  the rows are specified:
#     "rows" - deselect an explicit list of row indices supplied in $data
#     "circle" - deselect all vectors within a circle (bounding box in $data)
#     "box"  - deselect all vectors within a rectangle (bounding box in $data)
#     "expr" - deselect all vectors satisfying an algebraic expression (in
#              $data).
#  --------------------------------------------------
   public method deselect { type data } {
      return [choose $type $data "U"]
   }

#  Modifies the PolCat by inverting the current selection.
#  -----------------------------------------------------------
   public method invert { } {
      upvar 0 [getStates] states
      set inc 0
      set nrow [$data_ getNrow]

      for {set i 0} {$i < $nrow} {incr i} {
         set state $states($i)
         if { $state == "S" } {
            set inc 1
            set states_($i) "U"
         } elseif { $state == "U" } {
            set inc 1
            set states_($i) "S"
         }
      }

      if { $inc } {
         incr chid_
      }

   }

#  Select all unselected vectors in the catalogue.
#  ----------------------------------------------
   public method selectAll {} {
      upvar 0 [getStates] states
      set nrow [$data_ getNrow]
      set inc 0

      for {set i 0} {$i < $nrow} {incr i} {
         if { $states($i) == "U" } {
            set states_($i) "S"
            set inc 1
         }
      }

      if { $inc } {
         incr chid_
      }
   }

#  Deselect all selected vectors in the catalogue.
#  ------------------------------------------------
   public method deselectAll {} {
      upvar 0 [getStates] states
      set nrow [$data_ getNrow]
      set inc 0

      for {set i 0} {$i < $nrow} {incr i} {
         if { $states($i) == "S" } {
            set states_($i) "U"
            set inc 1
         }
      }

      if { $inc } {
         incr chid_
      }

   }

#  Given a reference to a PolCat to be displayed ($that), returns
#  an indication of how the currently displayed PolCat ($this) should
#  be updated to show $that.
#
#  A list of two elements is returned, the first is a string describing
#  the operation, the second is as follows:
#
#  If $that refers to a different catalogue, the string "redraw" is
#  returned in the second element.
#
#  If $that refers to a different selection from the same catalogue, a
#  list is returned containing the row index for each row that needs to be
#  changed.
#
#  If $this refers to the same catalogue and the same selection, a blank
#  string is returned as the second element.
#  ----------------------------------------------------------------------
   public method changes {that} {
      set des ""

#  If we have already found the differences between $this and $that
#  then return the previously calculated results. A "change identifier"
#  ("chid") is a unique number associated with each change of row states.
      set thatchid [$that getChid]

      if { $that == $that_ && $thatchid == $thatchid_ && $chid_ == $thischid_ } {
         set ret $changes_

#  Otherwise calculate new results.
      } else {

         if { [$data_ getTclFile] != [$that getTclFile] } {
            set catch "redraw"
            set des [$that getDesc]

         } elseif { [getZvals] != [$that getZvals] } {
            set catch "redraw"
            set des "change spectral channel"

         } else {

            upvar 0 [$that getStatesR] thatstate
            set catch ""
            set nrow [$data_ getNrow]
            for {set i 0} {$i < $nrow} {incr i} {
               set newstate $thatstate($i)
               if { $states_($i) != $newstate } {
                  lappend catch $i
                  if { $newstate == "D" } {
                     set des "cut vectors"
                  } else {
                     set des "select vectors"
                  }
               }
            }
         }

         set ret [list $des $catch]

#  Save the current results so that they are quickly available if the
#  same changes are requested again.
         set thatchid_ $thatchid
         set thischid_ $chid_
         set that_ $that
         set changes_ $ret

      }

      return $ret

   }

#  Create a new GaiaPolCat containing a copy of $this. It shares
#  the same GaiaPolData (which stores the data) as $this.
#  -----------------------------------------------------------------
   public method copy {} {
      if { [catch {set ret [::gaia::GaiaPolCat PolCat#auto [$data_ getFile] $w_ $pbar_ $data_] } mess ] } {
         error_dialog "Failed to copy data: $mess"
         set ret ""
      } else {
         upvar 0 [$ret getStatesR] newstate
         set nrow [$data_ getNrow]
         for {set i 0} {$i < $nrow} {incr i} {
            set newstate($i) $states_($i)
         }

         $ret setSexp [getSexp]
         $ret setWarned [$data_ getWarned]
         $ret setZvals [getZvals]
         set ret [code $ret]
      }
      return $ret
   }

#  Return the fully qualified name of a protected data member holding the
#  current state flags including Z filtering (i.e. any row with
#  a Z column value different to the current value of attribute zvals_[0]
#  is set to state "D"). This data member is an array indexed by
#  catalogue row number (zero-based).
#  -----------------------------------------------------------------------
   public method getStates {} {

#  Only calculate new results if something has changed. Last times
#  results are returned otherwise.
      if { $chid_ != $statechid_ } {

#  Initialze things.
         set nsel_ 0
         set nuns_ 0

#  Use the original state values if no Z filtering is being done.
#  Explicitly count the number of selected and unselected rows.
         if { $zmask_ == "" } {
            set getStates_ states_

            set nrow [$data_ getNrow]
            for {set i 0} {$i < $nrow} {incr i} {
               set state $states_($i)
               if { $state == "S" } {
                  incr nsel_
               } elseif { $state == "U" } {
                  incr nuns_
               }
            }

#  Otherwise, we use the Z filtered array, setting all states to "D" which
#  have a "D" in the zmask (copy all other states).
         } else {
            set getStates_ zstates_
            set nrow [$data_ getNrow]
            for {set i 0} {$i < $nrow} {incr i} {

               if { [lindex $zmask_ $i] == "D" } {
                   set zstates_($i) "D"
               } else {
                   set state $states_($i)
                   set zstates_($i) $state
                   if { $state == "S" } {
                      incr nsel_
                   } elseif { $state == "U" } {
                      incr nuns_
                   }
               }
            }
         }

#  Save the current change identifier and the $st value so that we can
#  decide whether anything has changed next time this method is called.
         set statechid_ $chid_

      }

      return [::itcl::scope $getStates_]

   }

#  Accessor methods...
#  -------------------
   public method getChanged {} {return $changed_}
   public method setChanged {s} {set changed_ $s}
   public method getChid {} {return $chid_}
   public method setSexp {s} {set sexp_ $s}
   public method getSexp {} {return $sexp_}
   public method setZvals {z} {set zvals_ $z; newZmask}
   public method getZvals {} {return $zvals_ }

#  Provie convenience routines for getting at the methods of the
#  encapsulated GaiaPolData.
#  ------------------------------------------------------------
   public method getData {} { return [$data_ getData] }
   public method setData {d} { $data_ setData $d}
   public method gotWcs {} { return [$data_ gotWcs] }
   public method getXCol {} { return [$data_ getXCol] }
   public method getYCol {} { return [$data_ getYCol] }
   public method getIdCol {} { return [$data_ getIdCol] }
   public method getRaCol {} { return [$data_ getRaCol] }
   public method getDecCol {} { return [$data_ getDecCol] }
   public method getIDCol {} { return [$data_ getIdCol] }
   public method getCol {x} { return [$data_ getCol $x] }
   public method getHeadings {} { return [$data_ getHeadings] }
   public method getNcol {} { return [$data_ getNcol] }
   public method getNrow {} { return [$data_ getNrow] }
   public method getEquinox {} { return [$data_ getEquinox] }
   public method getEpoch {} { return [$data_ getEpoch] }
   public method getId {} { return [$data_ getId] }
   public method getFile {} { return [$data_ getFile] }
   public method getTclFile {} { return [$data_ getTclFile] }
   public method getPixBounds {} { return [$data_ getPixBounds] }
   public method getNpix {} { return [$data_ getNpix] }
   public method getDesc {} { return [$data_ getDesc] }
   public method setDesc {desc} { $data_ setDesc "$desc" }
   public method getHfmts {} { return [$data_ getHfmts] }
   public method getFmts {} { return [$data_ getFmts] }
   public method getZlo {} { return [$data_ getZlo] }
   public method getZhi {} { return [$data_ getZhi] }
   public method getZcunit {} { return [$data_ getZcunit] }
   public method getZaunit {} { return [$data_ getZaunit] }
   public method getWarned {} { return [$data_ getWarned] }
   public method setWarned { {x 1} } { $data_ setWarned $x}
   public method getColNam {q} { return [$data_ getColNam $q] }

   public method zConv {z type} { return [$data_ zConv $z $type] }
   public method mkImage { rtdimage } { return [$data_ mkImage $rtdimage] }

#  Return the fully qualified name of a protected data member holding the
#  current state flags without Z filtering. This data member is an array
#  indexed by catalogue row number (zero-based).
#  --------------------------------------------------------------------
   public method getStatesR {} {return [::itcl::scope states_]}

#  Return the number of unselected rows.
#  --------------------------------------------------------------------
   public method getNuns {} {
      getStates
      return $nuns_
   }

#  Return the number of selected rows.
#  --------------------------------------------------------------------
   public method getNsel {} {
      getStates
      return $nsel_
   }

#  Indicate that the column with heading $c stores the quantity given by
#  $q.
#  ---------------------------------------------------------------------
   public method setColNam {q c} {

#  Execute the parent setColNam method.
      $data_ setColNam $q $c

#  If the Z column has been changed, zet up a new Z mask.
      if { $q == "Z" } { newZmask }
   }


#  Protected methods:
#  ==================

#  Select or deselect specified rows. $type indicates how the rows are
#  specified:
#     "rows" - use an explicit list of row indices supplied in $data
#     "circle" - use all vectors within a circle (bounding box in $data)
#     "box"  - use all vectors within a rectangle (bounding box in $data)
#     "expr" - use all vectors satisfying an algebraic expression (in
#              $data).
#  --------------------------------------------------
   protected method choose { type sdata state } {
      set inc 0
      set ret 1

      setSexp ""
      upvar 0 [getStates] states

      if { $state == "U" } {
         set need "S"
      } else {
         set need "U"
      }

      if { $type == "rows" } {
         foreach row $sdata {
            if { $states($row) == $need } {
               set states_($row) $state
               set inc 1
            }
         }

      } elseif { $type == "box" } {
         lassign $sdata x1 x2 y1 y2
         if { $x2 < $x1 } {
            set t $x1
            set x1 $x2
            set x2 $t
         }
         if { $y2 < $y1 } {
            set t $y1
            set y1 $y2
            set y2 $t
         }

         set xcol [$data_ getXCol]
         set ycol [$data_ getYCol]
         set i -1
         foreach row [$data_ getData] {
            incr i
            if { $states($i) == $need } {
               set x [lindex $row $xcol]
               set y [lindex $row $ycol]
               if { $x >= $x1 && $x <= $x2 && $y >= $y1 && $y <= $y2 } {
                  set states_($i) $state
                  set inc 1
               }
            }
         }

      } elseif { $type == "circle" } {
         lassign $sdata x1 x2 y1 y2
         set xc [expr 0.5*($x1 + $x2)]
         set yc [expr 0.5*($y1 + $y2)]

         set dx [expr $x2 - $xc]
         set dy [expr $y2 - $yc]
         set r2 [expr 0.5*($dx*$dx + $dy*$dy)]

         set xcol [$data_ getXCol]
         set ycol [$data_ getYCol]

         set i -1
         foreach row [$data_ getData] {
            incr i
            if { $states($i) == $need } {
               set x [lindex $row $xcol]
               set y [lindex $row $ycol]
               set dx [expr $x - $xc]
               set dy [expr $y - $yc]
               set d2 [expr $dx*$dx + $dy*$dy]

               if { $d2 <= $r2 } {
                  set states_($i) $state
                  set inc 1
               }
            }
         }

      } elseif { $type == "expr" } {
         set inc [findRows $sdata $state ]
         if { $inc == -1 } {
            set inc 1
            set ret 0
         } else {
            setSexp $sdata
         }

      }

      if { $inc } {
         incr chid_
      }

      return $ret

   }

#  Select or deselect rows which satisfy the supplied expression. If some
#  rows changed state as a result of this, +1 is returned, if no rows
#  changed state, 0 is returned. If there was an error in the expression -1
#  is returned.
#  ----------------------------------------------------------------------
   protected method findRows {exp state} {
      set ret 0

#  Retain a copy of the original supplied expression.
      set exp0 $exp

#  Identify the variables referenced in the expression. Loop round each
#  column heading. Slightly tricky as we may see occurances of $P and
#  $PI, for which the $PI match must be attempted before the $P.
      set headings ""
      set i -1
      foreach head "[$data_ getHeadings]" {
         incr i
         lappend headings [list $head $i]
      }
      set headings [lsort -decreasing $headings]

      foreach head $headings {
         lassign $head head i

#  Look for references to this column within the expression (i.e. the
#  column name preceeded with a dollar), replacing them with a lindex
#  command to extract the corresponding value from a row of data.
         regsub -nocase -all "\\$$head" $exp "\[lindex \$row $i\]" newexp

#  Use the new expression instead of the old one.
         set exp $newexp
      }

#  The only dollars in the expression should now be "$row". Report an
#  error if there are any others. First replace all occurences of "$row "
#  with a blank string.
      regsub -all {\$row } $exp " " temp

#  Get a list of the variable names left in temp.
      set nbad 0
      while { [regexp {\$([a-zA-Z1-9_ ]+)} $temp match var] } {
         append badvars $var " "
         incr nbad
         regsub -all "\\$$var" $temp " " newexp
         set temp $newexp
      }

#  Report an error and return if any undefined column names were included in
#  the expression.
      if { $nbad == 1 } {
         set msg "The supplied selection expression refers to the unknown column \"$badvars\"."
      } elseif { $nbad > 1 } {
         set msg "The supplied selection expression refers to the following unknown columns \"$badvars\"."
      }
      if { $nbad > 0 } {
         error_dialog "$msg. The following columns are available:\n\n [$data_ getHeadings]"
         return -1
      }

#  Get access to the state of each row.
      upvar 0 [getStates] states

#  Note the state of rows which are to be changed.
      if { $state == "U" } {
         set need "S"
      } else {
         set need "U"
      }

#  Loop round each row in the data for the currently displayed PolCat.
      set tried 0
      set good 0
      set i -1
      foreach row [$data_ getData] {
         incr i

#  Ignore rows which do not have the required original state.
         if { $states($i) == $need } {
            set tried 1

#  Evaluate the expression.
            if { ![catch { set ans [expr $exp] } mess] } {
               set good 1
               if { $ans } {
                  set states_($i) $state
                  set ret 1
               }
            }
         }
      }

#  Report an error if the expression could not be evaluated.
      if { !$good && $tried } {
         error_dialog "Could not evaluate the expression \"$exp0\""
         set ret -1
      }

      return $ret

   }


#  Set up a new Z mask.
#  --------------------
   protected method newZmask {} {

#  Initialize the mask to indicate no Z filtering is to be performed.
      set zmask_ ""

#  Check that we have a non Z column value and a suitable Z column.
      if { $zvals_ != "" } {
         set zcval [lindex $zvals_ 0]
         if { $zcval != "" } {
            set zcol [$data_ getCol Z]
            if { $zcol != -1 } {

#  Compare the Z column values with the required value.
               set irow -1
               set nf 0
               foreach row [$data_ getData] {
                  incr irow
                  if { [lindex $row $zcol] != $zcval } {
                      lappend zmask_ "D"
                      incr nf
                  } else {
                      lappend zmask_ " "
                  }
               }
            }
         }
      }
      incr chid_
   }

#  Return a new data array from which all deleted rows have been
#  removed. If $all is zero, then only selected vectors are retained in the
#  returned data array. If $all is non-zero, all non-deleted vectors, whether
#  selected or not, are retained. Z filtering is included. If the returned
#  data array would be exactly the same as the data array associated with
#  $this, then a blank string is returned. If the returned data array
#  would contain no data, the single word "empty" is returned.
#  ----------------------------------------------------------------------
   protected method purge { all } {

#  Get access to the Z filtered state flags.
      upvar 0 [getStates] states

#  Create a copy of the data array without any deleted rows, or (if all
#  is zero) any unselected rows.
      set nodata 1
      set nondel 1
      set i -1
      set newdata ""
      if { $all } {
         foreach row [$data_ getData] {
            incr i
            if { $states($i) != "D" } {
               lappend newdata $row
               set nodata 0
            } else {
               set nondel 0
            }
         }

      } else {
         foreach row [$data_ getData] {
            incr i
            if {  $states($i) == "S" } {
               lappend newdata $row
               set nodata 0
            } else {
               set nondel 0
            }
         }
      }

      if { $nondel } {
         return ""

      } elseif { $nodata } {
         return "empty"

      } else {
         return $newdata
      }

   }

#  Private methods:
#  ================

#  Public data members:
#  ====================

#  Protected data members:
#  =======================
   protected {

#  The value returned by the previous call to the changes method.
      variable changes_ ""

#  Has the PolCat been changed since it was last modified?
      variable changed_ 0

#  A unique integer identifier for each set of state values. It is
#  incremented each time the states are changed in any way.
      variable chid_ 0

#  The GaiaPolData object which stores a description of the unchangable
#  aspects fo the catalogue (such as the main data array).
      variable data_ ""

#  The value returned by the previous call to the getStates method.
      variable getStates_ ""

#  The number of selected states when the previous call to getStates
#  was made.
      variable nsel_ 0

#  The number of unselected states when the previous call to getStates
#  was made.
      variable nuns_ 0

#  An algebraic expression used to generate the selection
      variable sexp_ ""

#  An array of state flags (indexed by row number). S=selected, U=unselected,
#  D=deleted. This array does not include any Z filtering.
      variable states_

#  The "change identifier" (chid_) value associated with $this when the
#  getStates method was last invoked.
      variable statechid_ ""

#  The GaiaPolCat associated with $that which was last used by the changes
#  method.
      variable that_ ""

#  The "change identifier" (chid_) value associated with $that which was last
#  used by the changes method.
      variable thatchid_ ""

#  The "change identifier" (chid_) value associated with $this which was last
#  used by the changes method.
      variable thischid_ ""

#  If not blank, this is a list of "D" or "" strings, one for each row in
#  the catalogue. A "D" string means that the row does not pass the Z
#  filter and should be treated as if it were deleted. A "" means that he
#  row does pass the Z filter. An entiely blank list means that no Z
#  filtering is being performed.
      variable zmask_ ""

#  An array of state flags (indexed by row number). S=selected, U=unselected,
#  D=deleted. This array is a copy of states_ but with Z filtering applied.
      variable zstates_

#  If not blank, this is a list of two values. The first is a Z column value.
#  All rows which do not have this Z column value are treated as if they
#  were deleted when public requests for data or states are made (i.e. the
#  PolCat looks to the outside world as if it only contains a single Z column
#  value). If $zvals_ is blank, then no such Z filtering occurs. The second
#  element in the list is the corresponding Z axis value.
      variable zvals_ ""

#  Top level window
      variable w_ ""

#  Progress bar.
      variable pbar_ ""

   }

#  Private data members:
#  =====================

#  Common (i.e. static) data members:
#  ==================================

#  End of class definition.
}
