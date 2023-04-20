#+
#  Name:
#     GaiaPolStyle

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Encapsulates the style with which vectors should be displayed.
#     GaiaPolObjects.

#  Description:
#     This class encapsulates information about how to display vectors.
#
#  Invocations:
#
#        GaiaPolStyle object_name [configuration options]
#
#     This creates an instance of a GaiaPolStyle object. The returned value
#     is the name of the object.
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
#     15-SEP-2000 (DSB):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaPolStyle {

#  Inheritances:
#  =============
   inherit gaia::GaiaPolObject

#  Constructor:
#  ============
   constructor {} {

#  Now initialize the class data. If this constructor has been invoked
#  to construct the base class part of some super class, do not
#  initialize the data since this will be done as a consequence of
#  initializeing the super class data.
      if { [$this info class] == "::gaia::GaiaPolStyle" } {
         init
      }
   }

#  Destructor:
#  ============

#  Initializer:
#  ============
#  Override the parent Init method to initialise the contents of the
#  memory allocated by the GaiaPolStyle constructor using a user-supplied
#  argument list.
   protected method init {args} {

#  First initialize the parent class data
      gaia::GaiaPolObject::init

#  Now initialize this class...
      reset

   }

#  Public methods:
#  ===============

#  Reset default values.
#  ---------------------
   public method reset { {readopt 1} } {
      set mag_ ""
      set angrot_ 0
      set lencol_ "P"
      set angcol_ "ANG"
      set selcol_ "#00f"
      set unscol_ "#f00"
      set selwid_ ""
      set unswid_ 1
      set selflash_ 0
      set unsflash_ 0
      set nvec_ ""
   }

#  Make a copy of $this.
#  --------------------
   public method copy {} {
      if { [catch {set ret [::gaia::GaiaPolStyle PolStyle#auto] } mess ] } {
         error_dialog "Failed to copy style: $mess"
         set ret ""
      } else {
         set ret [code $ret]
         {*}$ret setFrom $this
      }
      return $ret
   }

#  Format for display.
#  -------------------
   public method toString {} {
      set ret "GaiaPolStyle: $this\n"
      return $ret
   }

#  Length and angle columns.
#  ----------------------------------------------------------------------
   public method setLcol { col } { set lencol_ $col }
   public method getLcol { } { return $lencol_ }

   public method setAcol { col } { set angcol_ $col }
   public method getAcol { } { return $angcol_ }

#  Width of selected vectors. If blank, use the width of unselected
#  vectors.
#  ------------------------------------------------------------------
   public method setSwid { wid } { set selwid_ $wid }
   public method getSwid {} { return $selwid_ }
   public method getSwidd {} {
      set ret [getSwid]
      if { $ret == "" } {
         set ret [getUwid]
      }
      return $ret
   }

#  Vector scale (image pixels per unit value in the $lencol_ column).
#  If a null value is supplied, a default value is calculated and returned
#  by getMagd.
#  -------------------------------------------------------------------
   public method setMag { mag } { set mag_ $mag }
   public method getMag {} { return $mag_ }
   public method getMagd {cat rtdimage} {
      set ret [getMag]

#  Calculate a default value if the stored value if blank. Otherwise
#  the stored value will be returned.
      if { $ret == "" } {

#  Set a default value of 1 in case a proper default cannot be found.
         set ret 1

#  Get the index of the vector length column within the catalogue headings.
#  Check the column was found.
         set lcol [getLcoli $cat]
         if { $lcol != -1 } {

#  Get the data array from the PolCat.
            set data [{*}$cat getData]

#  Get access to an array of row states (selected, unselected, deleted)
#  indexed by row number.
            upvar 0 [{*}$cat getStates] states

#  Form a string including all non-zero vector lengths, separated by
#  newlines. Also note the length of the last non-zero vector (use $ret
#  if there are no non-zero vectors)..
            set ls ""
            set defval $ret
            set i -1
            set n 0
            foreach row $data {
               incr i
               if { $states($i) != "D" } {
                  set l [lindex $row $lcol]
                  if { $l > 0 } {
                     append ls "$l\n"
                     set defval $l
                     incr n
                  }
               }
            }

#  If only a single non-zero length vector was found, scale it so that it
#  half the length of a diagonal of the displayed image.
            if { $n == 1 } {
               set h [$rtdimage height]
               set w [$rtdimage width]
               set ret [string trim [format "%10.3g" [expr 0.5*sqrt($h*$h+$w*$w)/$defval]]]

#  If more than one non-zero length vectors found, sort them numerically.
            } elseif { $n > 1 } {
               set file [tmpFile]
               set fd [open $file w]
               puts $fd $ls
               close $fd
               set sorted [split [exec sort -n $file] "\n"]
               catch {file delete $file}

#  Find the median value.
               set med [lindex $sorted [expr $n/2]]

#  Use the last non-zero vector length if no median was found.
               if { $med == "" } {
                  set med $defval
               }

#  Number of pixels covered by the catalogue.
               set np [{*}$cat getNpix]

#  Find the default scaling factor.
               set ret [string trim [format "%10.3g" [expr sqrt(1.3*$np/$n)/$med]]]

#  Remember this value for future use.
               setMag $ret
            }
         }
      }
      return $ret
   }

#  Other simple accessor methods.
   public method setNvec {n} { set nvec_ $n }
   public method getNvec {} { return $nvec_ }

   public method setArot { rot } { set angrot_ $rot }
   public method getArot {} { return $angrot_ }

   public method setSclr { col } { set selcol_ $col }
   public method getSclr {} { return $selcol_ }

   public method setUclr { col } { set unscol_ $col }
   public method getUclr {} { return $unscol_ }

   public method setUwid { wid } { set unswid_ $wid }
   public method getUwid {} { return $unswid_ }

   public method setUflash { f } {
      if { $f } {
         set unsflash_ 1
      } else {
         set unsflash_ 0
      }
   }
   public method getUflash {} { return $unsflash_ }

   public method setSflash { f } {
      if { $f } {
         set selflash_ 1
      } else {
         set selflash_ 0
      }
   }
   public method getSflash {} { return $selflash_ }

#  Copy the properties of this GaiaPolStyle to another GaiaPolStyle.
#  -----------------------------------------------------------------
   public method setFrom {that} {
      setMag [$that getMag]
      setArot [$that getArot]
      setLcol [$that getLcol]
      setAcol [$that getAcol]
      setSclr [$that getSclr]
      setUclr [$that getUclr]
      setSwid [$that getSwid]
      setUwid [$that getUwid]
      setUflash [$that getUflash]
      setSflash [$that getSflash]
      setNvec [$that getNvec]
   }

#  Compares this GaiaPolStyle with another GaiaPolStyle. Returns a list
#  of two elements. The first element is a string describing the change. The
#  second element is:
#
#  if they are the same: a blank string
#
#  if vectors drawn with style $this can be made to appear as if they were
#  drawn with style $that just by reconfiguring their Tk canvas options,
#  then a list of two elements is returned as the second element. Both elements
#  consist of one or more "option value" strings (separated by spaces). These
#  are usedto reconfigure previously drawn vectors, so "option" should be a
#  legal option name for a Tk canvas item, and "value" should be a legal value
#  for the option. The first element of the returned two-element list
#  refers to unselected vectors, and the second refers to selected vectors.
#  Note, a pretent option called "-flash" which takes value 0 or 1 is
#  used to indicate if the vectors should flash or not.
#
#  if the two styles differ in ways which cannot be reproduced just by
#  reconfiguring the canvas items, then "redraw" is returned as the
#  second element.
#  ------------------------------------------------------------------------
   public method changes {that} {
      set des ""
      
      if { [getMag] != [{*}$that getMag] } {
         set ret "redraw"
         if { $des == "" } {
            set des "change vector scale"
         } else {
            set des "change rendering options"
         }
      }

      if { [getArot] != [{*}$that getArot] } {
         set ret "redraw"
         if { $des == "" } {
            set des "change vector rotation"
         } else {
            set des "change rendering options"
         }
      }

      if { [getLcol] != [{*}$that getLcol] } {
         set ret "redraw"
         if { $des == "" } {
            set des "change vector length column"
         } else {
            set des "change rendering options"
         }
      }

      if { [getAcol] != [{*}$that getAcol] } {
         set ret "redraw"
         if { $des == "" } {
            set des "change vector angle column"
         } else {
            set des "change rendering options"
         }
      }

      if { [getNvec] != [{*}$that getNvec] } {
         set ret "redraw"
         if { $des == "" } {
            set des "change maximum number of drawn vectors"
         } else {
            set des "change rendering options"
         }
      }

      if { $des == "" } {
         set retu ""
         set val [{*}$that getUclr]
         if { [getUclr] != $val } {
            append retu "-fill \"$val\" "
            if { $des == "" } {
               set des "change vector colour"
            } else {
               set des "change rendering options"
            }
         }

         set val [{*}$that getUwid]
         if { [getUwid] != $val } {
            append retu "-width $val "
            if { $des == "" } {
               set des "change vector width"
            } else {
               set des "change rendering options"
            }
         }

         set val [{*}$that getUflash]
         if { [getUflash] != $val } {
            append retu "-flash $val "
            if { $des == "" } {
               set des "change vector flashing"
            } else {
               set des "change rendering options"
            }
         }

         set rets ""
         set val [{*}$that getSclr]
         if { [getSclr] != $val } {
            append rets "-fill \"$val\" "
            if { $des == "" } {
               set des "change colour of selected vectors"
            } else {
               set des "change rendering options"
            }
         }

         set val [{*}$that getSwidd]
         if { [getSwidd] != $val } {
            append rets "-width $val "
            if { $des == "" } {
               set des "change width of selected vectors"
            } else {
               set des "change rendering options"
            }
         }

         set val [{*}$that getSflash]
         if { [getSflash] != $val } {
            append rets "-flash $val "
            if { $des == "" } {
               set des "change selected vector flashing"
            } else {
               set des "change rendering options"
            }
         }

         if { $retu != "" || $rets != "" } {
            set ret [list $retu $rets]
         } else {
            set ret ""
         }

      }

      return [list $des $ret]

   }

#  Stores the canvas colour and width values used to render unselected
#  and selected vectors in the supplied variables
#  -------------------------------------------------------------------
   public method colwid {} {
      return [list "-fill \"[getUclr]\" -width [getUwid]" \
                   "-fill \"[getSclr]\" -width [getSwidd]" ]
   }

#  Find the index of the length column within the headings of the
#  supplied catalogue. Report an error if it si not found.
#  ---------------------------------------------------------------
   public method getLcoli {cat} {
      return [findCol $cat [getLcol] length]
   }

#  Find the index of the angle column within the headings of the
#  supplied catalogue. Report an error if it si not found.
#  ---------------------------------------------------------------
   public method getAcoli {cat} {
      return [findCol $cat [getAcol] angle]
   }

#  Protected methods:
#  ==================

#  Private methods:
#  ==================

#  Find the index of a column name within the column headings, reporting
#  an error if the column name is not found.
#  ---------------------------------------------------------------------
   private method findCol {cat col w} {
      set headings [{*}$cat getHeadings]
      set ret [lsearch -exact $headings $col]
      if { $ret == -1 } {
         info_dialog "Please use the \"Rendering\" panel to indicate which column holds the vector $w values."
      }
      return $ret
   }

#  Public data members:
#  ====================

#  Protected data members:
#  =======================
   protected {

#  The vector length scale factor (i.e. the number of image pixels for a
#  unit value in the $lencol_ column). If mag has a blank value, then a
#  default value will be found and used.
      variable mag_ ""

#  angrot gives an anti-clockwise angle (in degrees) by which to rotate
#  the vectors from their usual orientations.
      variable angrot_ 0

#  The name of the column defining vector lengths.
      variable lencol_ "P"

#  The name of the column defining vector lengths.
      variable angcol_ "ANG"

#  Colour for selected vectors (blank means "do not draw").
      variable selcol_ "#00f"

#  Colour for unselected vectors (blank means "do not draw").
      variable unscol_ "#f00"

#  Thickness for selected vectors (blank means "same as unselected").
      variable selwid_ ""

#  Thickness for unselected vectors.
      variable unswid_ 1

#  Flags indicating if vectors should flash (default is no).
      variable selflash_ 0
      variable unsflash_ 0

#  Number of vectors to display on the canvas. Blank means no limit.
      variable nvec_ ""

   }

#  Private data members:
#  =====================
#  (none)

#  Common (i.e. static) data members:
#  ==================================

#  End of class definition.
}
