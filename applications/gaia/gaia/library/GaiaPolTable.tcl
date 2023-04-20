#+
#  Name:
#     GaiaPolTable

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Implements a tool for displaying polarimetry data as a table of
#     column values.

#  Description:
#     This class handles the visualisation of polarimetry data by creating
#     a table of column values.
#
#  Invocations:
#
#        GaiaPolTable object_name [configuration options]
#
#     This creates an instance of a GaiaPolTable object. The returned value
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

#  Methods:

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
#     7-MAR-2001 (DSB):
#        Original version, extracted from old version of GaiaPolDisp.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaPolTable {

#  Inheritances:
#  =============
   inherit gaia::GaiaPolObject

#  Required methods, etc:
#  ======================

#  Constructor
#  -----------
   constructor { w rtdimage table pbar selcmd} {

#  Now initialize the class data. If this constructor has been invoked
#  to construct the base class part of some super class, do not
#  initialize the data since this will be done as a consequence of
#  initializeing the super class data.
      if { [$this info class] == "::gaia::GaiaPolTable" } {
         init $w $rtdimage $table $pbar $selcmd
      }
   }

#  Destructor
#  ----------
   destructor {
      catch { clear }
   }

#  Override the parent Init method to initialise the contents of the
#  memory allocated by the GaiaPolTable constructor using a user-supplied
#  argument list.
#  ----------------------------------------------------------------------
   protected method init { w rtdimage table pbar selcmd } {

#  First initialize the parent class data
      gaia::GaiaPolObject::init

#  Now initialize this class.
      set w_ $w
      set rtdimage_ $rtdimage
      set table_ $table
      set pbar_ $pbar
      set selcmd_ $selcmd

   }

#  Public methods:
#  ===============

#  Clear the table.
#  ----------------
   public method clear { } {

#  Do nothing if no data has yet been displayed...
      if { $cat_ != "" } {

#  Clear the table.
         {*}$table_ config -headings {}
         {*}$table_ config -hformats {}
         {*}$table_ config -formats {}
         {*}$table_ config -info {}

#  Clear the properties.
         catch {unset crows_ }
         catch {unset trows_ }
         catch {unset catrow_}

#  Indicate that this GaiaPolTable now has no displayed data.
         $cat_ annull
         set cat_ ""

      }
   }

#  Modify a line of the table prior to display. This current just formats
#  any RA/DEC values into H:M:S format.
#  --------------------------------------------------------------------
   public method tableFilter { var } {
      upvar $var line
      $rtdimage_ convert coords [lindex $line $racol_] [lindex $line $deccol_] $from_ \
                ra dec $to_
      set line [lreplace $line $racol_ $racol_ $ra]
      set line [lreplace $line $deccol_ $deccol_ $dec]
   }

#  Called when the sorting criteria change.
#  ----------------------------------------
   public method tableSort {cols order} {

#  Indicate what is happening.
      setHold "Sorting the table..."

#  Sort the data.
      {*}$table_ config -sort_cols $cols
      {*}$table_ config -sort_order $order
      {*}$table_ new_info

#  Create new lists for converting between catalogue and table row
#  indices, incorporating the effects of the sorting.
      ctrows

#  Ensure all the previously selected catalogue rows are still selected.
      highlight

#  Reset the progress bar.
      resetHold
   }

#  Modify the table column formats to make only the columns requested in
#  the "Table" menu visible.
#  --------------------------------------------------------------------
   public method tableLayout {cols} {

      setHold "Tabulating the vectors..."

      set icol -1
      foreach name $heads_ {
         incr icol

         if { [lsearch $cols $name] != -1 } {
            append formats "[lindex $fmts_ $icol] "
            append hformats "[lindex $hfmts_ $icol] "
         } else {
            append formats "%.0s "
            append hformats "%.0s "
         }
      }

      {*}$table_ config -formats $formats
      {*}$table_ config -hformats $hformats
      $table_ new_info

#  Reset the progress bar.
      resetHold

   }

#  Called when new rows are selected by releasing button 1 over the table.
#  -----------------------------------------------------------------------
   public method tabSel {} {

#  Find the catalogue row indices for the currently selected rows.
      set crows ""
      foreach trow [{*}$table_ component listbox curselection] {
         lappend crows $crows_($trow)
      }

#  Call the script to establish the new selection within the parent
#  GaiaPolarimetry toolbox. This will re-style the vectors on the canvas,
#  etc.
      eval $selcmd_ 1 1 "rows" \{ $crows \}

   }

#  Updates the table so that it represents the data in the supplied
#  catalogue, with the correct rows shown selected. If possible, this
#  is done by reconfiguring the existing table rows. This is possible if
#  the supplied catalogue refers to the same data file as the previously
#  tabulated catalogue (this would be the case for instance if the
#  previous action was to select or deselect some vectors) AND none of
#  the vectors have been removed (marked as deleted by a cut operation)
#  If the supplied catalogue refers to a different data file, (which would
#  be the case for instance if the previous action was a "bin" operation) OR
#  if any vectors have been deleted, then the table is cleared first and
#  the supplied catalogue is tabulated from scratch. If $force is non-zero
#  the table is retabulated from scratch even if it seems not to have
#  changed.
#  --------------------------------------------------------------------------
   public method tabulate {cat prevcat {force 0} } {

#  Indicate what is happening.
      setHold "Tabulating the vectors..."

#  If no catalogiue is currently displayed, or if we have been forced, we need
#  to tabulate the supplied catalogue from scratch.
      if { $prevcat == "" || $force } {
         set clear 1

#  Otherwise, we need to find out what needs to be done to convert the
#  current table into the required table.
      } else {

#  Get a description of the changes which produced the new catalogue, and
#  the changes equired to convert the displayed catalogue into the
#  supplied catalogue.
         lassign [{*}$prevcat changes $cat] catdesc catch

#  If the catalogue requires the table to be tabulated from scratch, indicate
#  this.
          if { $catch == "redraw" } {
             set clear 1

#  Otherwise, attempt to update the current table by changing the selection.
#  This will not be possible if any of the rows have been marked for deletion.
          } else {
             set clear [selTab $cat $catch]
          }
      }

#  If a complete redraw from scratch is required, do it.
      if { $clear } {
         clear
         tab $cat
      }

#  Reset the progress bar.
      resetHold

   }

#  Format the object into a string.
#  --------------------------------
   public method toString {} {
      set ret "GaiaPolTable:\n"
      return $ret
   }

#  Protected methods:
#  ==================

#  Display a given GaiaPolCat as a table of values
#  -----------------------------------------------
   protected method tab {cat} {

#  Store a clone of the supplied catalogue.
      if { $cat_ != "" } {set cat_ [$cat_ annull] }
      set cat_ [{*}$cat clone]

#  Get headings and formats from the supplied GaiaPolCat.
      set heads_ [$cat_ getHeadings]
      set fmts_ [$cat_ getFmts]
      set hfmts_ [$cat_ getHfmts]

#  Save the index of the ID column.
      set id_col_ [$cat_ getIdCol]

#  Assume no RA/DEC cols are available.
      set from_ ""
      set to_ ""
      set racol_ ""
      set deccol_ ""
      {*}$table_ config -filtercmd  "#"

#  If RA and DEC columns are available, add a filter to the TableList which converts
#  RA/DEC values from decimal degrees to H:M:S format.
      if { [$cat_ gotWcs] } {

#  Set up the globals used to communicate with the tableFilter method.
         set racol_ [$cat_ getRaCol]
         set deccol_ [$cat_ getDecCol]
         if { $racol_ > -1 && $deccol_ > -1 } {
            set from_ "deg [$cat_ getEquinox]"
            set to_ "wcs [$cat_ getEquinox]"

#  Configure the TableList to use the tableFilter mathod defined within
#  this class.
            {*}$table_ config -filtercmd  "$this tableFilter"

#  Modify the formats for the RA and DEC columns.
            set fmts_ [lreplace $fmts_ $racol_ $racol_ "%-16s"]
            set fmts_ [lreplace $fmts_ $deccol_ $deccol_ "%-16s"]
            set hfmts_ [lreplace $hfmts_ $racol_ $racol_ "%-16s"]
            set hfmts_ [lreplace $hfmts_ $deccol_ $deccol_ "%-16s"]
         } else {
            set racol_ ""
            set deccol_ ""
            error_dialog "RA and DEC columns are not available.\nSee the \"Column names\" panel."
         }
      }

#  Configure the TableList to call the tableLayout method when the table
#  layout is changed.
      {*}$table_ config -layoutcommand "$this tableLayout"

#  Configure the TableList to call the tableSort method when table
#  sort options change.
      {*}$table_ config -sortcommand "$this tableSort"

#  Add the headings and formats into the TableList.
      {*}$table_ config -headings $heads_
      {*}$table_ config -formats $fmts_
      {*}$table_ config -hformats $hfmts_

#  Ensure all columns are visible.
      {*}$table_ set_options $heads_ Show 1

#  Get access to an array of row states (selected, unselected, deleted)
#  indexed by row number.
      upvar 0 [$cat_ getStates] states

#  Initialise the list of remaining data.
      set remdata ""

#  Loop round each row in the catalogue. If the row has not been marked as
#  deleted, add its data to the list of remaining data.  Also save a list
#  of all row identifiers in the catalogue.
      set crow -1
      foreach row [$cat_ getData] {
         incr crow
         if { $states($crow) != "D" } {
            lappend remdata $row
         }
         set catrow_([lindex $row $id_col_]) $crow
      }

#  Add the remaining data into the TableList.
      {*}$table_ config -info $remdata

#  Create lists which enable conversion between table and catalogue row
#  indices.
      ctrows

#  Highlight any selected rows.
      highlight

   }

#  Changes the selection of rows in the table when a new selection is made
#  using the cursor, selection expression, etc. Returns 1 if the changes
#  can only be implemented by re-tabulating the data from scratch using
#  method tab in this class. Returns 0 if the changes were completely
#  implemented by this method. $crows should be a list of catalogue row
#  indices which have changed their state since the previous tabulation.
#  -----------------------------------------------------------------------
   protected method selTab {cat crows} {

#  Store a clone of the supplied catalogue.
      if { $cat_ != "" } {set cat_ [$cat_ annull] }
      set cat_ [{*}$cat clone]

      set ret 0
      if { [llength $crows] > 0 } {

#  Get access to an array of row states (selected, unselected, deleted)
#  indexed by row number.
         upvar 0 [$cat_ getStates] states

#  For each changed row, find the index of the row within the table which
#  corresponds to the same catalogue row index, and set the state of the table
#  row to selected or deselected as indicated. If the changed row does
#  not exist within the table, return 1 to indicate that the changes can
#  only be implemented by a complete retabulation.
         foreach crow $crows {
            if { [catch {set trow $trows_($crow)}] } {
               set ret 1
               break
            } else {
               set state $states($crow)
               if { $state == "S" } {
                  {*}$table_ select_row $trow 0

               } elseif { $state == "U" } {
                  {*}$table_ deselect_row $trow

               } elseif { $state == "D" } {
                  set ret 1
                  break
               }
            }
         }
      }

#  Ensure the table is scrolled to make the last selected vector visible.
      seeTabSel

      return $ret
   }

#  Ensure that the table is scrolled in order to make the last selected
#  row visible. Do this by re-selecting the last selected row (if any).
#  --------------------------------------------------------------------
   protected method seeTabSel {} {
      set rows [{*}$table_ component listbox curselection]
      if { $rows != "" } {
         {*}$table_ select_row [lindex $rows end] 0
      }
   }

#  Indicate what is going on.
#  --------------------------
   protected method setHold {text} {
      blt::busy hold $w_ -cursor "watch"
      {*}$pbar_ reset
      {*}$pbar_ config -text $text
      update idletasks
   }

#  Clear the progress bar etc.
#  --------------------------
   protected method resetHold {} {
      blt::busy release $w_
      {*}$pbar_ reset
      update idletasks
   }


#  Create arrays which allow row indices to be converted between catalogue
#  (crow) and table (trow). These may not be the same since not all
#  catalogue rows may be included int he table, and the table may have been
#  sorted.
#  ------------------------------------------------------------------------
   protected method ctrows {} {
      catch {unset crows_}
      catch {unset trows_}

      set trow -1
      foreach row [{*}$table_ get_contents] {
         incr trow
         set crow $catrow_([lindex $row $id_col_])
         set crows_($trow) $crow
         set trows_($crow) $trow
      }
   }

#  Highlight any selected rows.
#  ----------------------------
   protected method highlight {} {
      {*}$table_ clear_selection
      upvar 0 [$cat_ getStates] states
      set nrow [$cat_ getNrow]
      for {set crow 0} {$crow < $nrow} {incr crow} {
         if { $states($crow) == "S" } {
            {*}$table_ select_row $trows_($crow) 0
         }
      }
   }

#  Protected data members:
#  =======================
   protected {

#  A clone of the currently tabulated catalogue.
      variable cat_ ""

#  An array of catalogue row indices indexed by row identifier.
      variable catrow_

#  An array of catalogue row indices indexed by table row index.
      variable crows_

#  Info used to communicate with TableList service commands.
      variable from_ ""
      variable to_ ""
      variable racol_ ""
      variable deccol_ ""
      variable fmts_ ""
      variable hfmts_ ""
      variable heads_ ""
      variable id_col_ ""

#  Progress bar widget.
      variable pbar_ ""

#  Name of rtdimage widget.
      variable rtdimage_

#  Command to call when vectors are selected.
      variable selcmd_ ""

#  Table widget.
      variable table_ ""

#  An array of table row indices indexed by catalogue row index.
      variable trows_

#  Top level window.
      variable w_ ""

   }

#  Common (i.e. static) data members:
#  ==================================

#  End of class definition.
}
