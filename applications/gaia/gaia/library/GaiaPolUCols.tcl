#+
#  Name:
#     GaiaPolUCols

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     A control panel for selecting the names of the catalogue column
#     holding I, Q, U, etc.

#  Description:
#     This class allows the user to select the names of the columns
#     holding each known physical quantity (eg I, Q, U, V, DI, DQ, DU
#     etc).
#
#     The columns we would *like* to use may not be available in every
#     catalogue. Therefore, this class uses two groups of column names;
#     the "wanted" column names, and the "used" column names. The used
#     column name will be equal to the wanted column name unless the
#     wanted column name is not available in the currently opened catalogue,
#     in which case the used column name is set blank to indicate that the
#     quantity is "not available".
#
#     The user can indicate his wanted column names explicitly using
#     a set of menu buttons which allow him to select from the currently
#     available column names. If this is not done, then the wanted column
#     name for a given quantity is read from the catalogue itself. If the
#     catalogue does not contain this information, then the wanted column
#     name defaults to the polpack column name.
#
#     Wanted column names which are explicitly selected by the user are
#     retained between invocations of the toolbox.
#
#  Invocations:
#
#        GaiaPolUCols object_name [configuration options]
#
#     This creates an instance of a GaiaPolUCols object. The returned value
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
#     ::util::FrameWidget

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
#     23-NOV-2000 (DSB):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaPolUCols {}

itcl::class gaia::GaiaPolUCols {

#  Inheritances:
#  =============
   inherit ::util::FrameWidget

#  Constructor:
#  ============
   constructor {args} {

#  Evaluate any options.
      eval itk_initialize $args

#  Initialise data for this object.
      set created_ 0

#  Store short help descriptions of eachquantity.
      set shelp_(X) "X coordinate"
      set shelp_(Y) "Y coordinate"
      set shelp_(Z) "Z (spectral) coordinate"
      set shelp_(RA) "Right Ascension"
      set shelp_(DEC) "Declination"
      set shelp_(I) "I (total intensity)"
      set shelp_(DI) "I standard deviation"
      set shelp_(Q) "Stokes Q parameter"
      set shelp_(DQ) "Q standard deviation"
      set shelp_(U) "Stokes U parameter"
      set shelp_(DU) "U standard deviation"
      set shelp_(V) "Stokes V parameter"
      set shelp_(DV) "V standard deviation"
      set shelp_(P) "P (percentage polarization)"
      set shelp_(DP) "P standard deviation"
      set shelp_(ANG) "ANG (polarization angle in degrees)"
      set shelp_(DANG) "ANG standard deviation"
      set shelp_(PI) "PI (polarized intensity)"
      set shelp_(DPI) "PI standard deviation"
      set shelp_(ID) "ID (vector identifier)"

#  Set defaults
      reset
   }

#  Destructor:
#  ============
   destructor {

#  Annul any existign catalogue reference.
      if { $cat_ != "" } { set cat_ [$cat_ annull] }

#  Save the current options values to the options file, over-writing any
#  existing options file. These are the columns names which the user
#  *wants* to use, but which may or may not be available in the catalogue.
      if { $saveopt_ && [file isdirectory $itk_option(-optdir)] } {
         set optfile "$itk_option(-optdir)/GaiaPolUCols.opt"
         if { [catch {set fd [open $optfile w]} mess] } {
            puts "Error writing defaults to file '$optfile' for the polarimetry toolbox 'Column Names' panel : $mess"
         } else {
            foreach name [array names values_] {
               puts $fd "set option($name) \{$values_($name)\}"
               unset values_($name)
            }
            close $fd
         }
      }
   }

#  Public methods:
#  ===============

#  Command which is invoked when a change is made to any GUI control.
#  -------------------------------------------------------------------
   public method activ { args } {

#  Get the name of the changed quantity.
      set q [lindex $args 0]
      set lq [string tolower $q]

#  Ensure the value for the changed control is up to date in the mvalues_
#  array.
      set mvalues_($q) [$itk_component($lq) get]

#  Indicate that the user now "wants" the value in the menu. What he
#  "wants" and what he gets may not always be the same (e.g. if the
#  column he wants is not available in the catalogue), but for the moment,
#  since he has explicitly selected one of the available columns, what he
#  wants and what he gets are in fact the same.
      set values_($q) $mvalues_($q)

#  Use the command specified by the -actioncmd option to store a new
#  undoable action in the actions list.
      newAction $q

#  Implement the requested change.
      newVals $q
   }

#  Reset default values (either read from an options file or hard-wired).
#  ----------------------------------------------------------------------
   public method reset { {readopt 1} } {

#  Store control descriptions, and attribute names (i.e. the name for the
#  corresponding get/set methods). Set the hard-wired defaults.
      foreach q $quantities_ {
         set desc_($q) "the column to use for $shelp_($q) values"
         set attr_($q) $q
         append attr_($q) Col
         set values_($q) ""
      }

#  Over-write these with the values read from the options file created when
#  the last used instance of this class was destroyed. These are the columns
#  names which the user *wants* to use, but which may or may not be available
#  in the catalogue.
      set optfile "$itk_option(-optdir)/GaiaPolUCols.opt"
      if { $readopt && [file readable $optfile] } {
         if { [catch {source $optfile} mess] } {
            puts "Error reading defaults from file '$optfile' for the polarimetry toolbox 'Column Names' panel : $mess"
         } else {
            foreach elem [array names option] {
               set values_($elem) "$option($elem)"
            }
         }
      }

#  Save the original values as next times previous values.
      saveOld

   }

#  Accessor methods:
#  -----------------
#  Return the column name to be used for a given quantity.
   public method getCol {q} {return $mvalues_($q)}

#  Set the wanted column name for a given quantity.
   public method setCol {q c} {set values_($q) $c; new_wants}

#  Indicate if the options should be saved when the object is destroyed.
   public method setSaveOpt {x} {set saveopt_ $x}

#  Called to add a new action to the current list of undoable actions.
#  ------------------------------------------------------------------------
   public method newAction {item} {
      if { "$itk_option(-actioncmd)" != "" } {
         set arglist "object \{change $desc_($item)\} $this \{setCol $item \"$oldvals_($item)\"\} \{setCol $item \"$values_($item)\"\}"
         eval {*}$itk_option(-actioncmd) $arglist
      }
   }

#  Called when a new GaiaPolCat is opened.
#  ---------------------------------------
   public method newCat {cat} {

#  Indicate that the user has not yet been warned about any unused columns
#  in the new catalogue.
      set warned_ 0

#  Annul any existing catalogue reference.
      if { $cat_ != "" } { set cat_ [$cat_ annull] }

#  If a catalogue has been supplied, store a clone of the suppllied catalogue
#  reference, and store the new headings.
      if { $cat != "" } {
         set cat_ [{*}$cat clone]
         set headings_ [$cat_ getHeadings]

#  If no catalogue has been supplied, store an empty headings string.
      } else {
         set headings_ ""
      }

#  Re-populate the menus with the new set of available column names (if
#  any).
      update_headings

#  Try to select the wanted column names.
      new_wants

   }

#  Called to save the current control values as next times previous
#  values, and to implement the new settings by calling the change command.
#  ------------------------------------------------------------------------
   public method newVals {q} {
      saveOld
      if { "$itk_option(-changecmd)" != "" } {
         eval {*}$itk_option(-changecmd) $q
      }
   }

#  Create the page of controls.
#  ----------------------------
   public method create {} {

#  Do nothing if the controls have already been created.
      if { ! $created_ } {

#  Save the mvalues_ array so that hey can be reinstated later (the widget
#  creation commands seem to reset them to blank).
         foreach name [array names mvalues_] {
            set temp($name) $mvalues_($name)
         }

#  Indicate that the controls are being created.
         set created_ 1

#  Number of columns in the grid.
         set ncol 2

#  Horizontal padding for columns.
         set px 2m

#  Vertical space between sections
         set vspace1 3m

#  Vertical space rows within a section
         set vspace2 2m

#  Value width (in characters).
         set vwidth 6

#  Label widths...
         set lwidth 5

#  Initialise the row index within the geometry grid
         set r -1

#  Column names header...
         itk_component add header1 {
	    gaia::LabelRule $w_.header1 -text "Columns to use for:"
	 }
         grid $itk_component(header1) -row [incr r] -column 0 -padx 1m \
                                      -columnspan $ncol -sticky nwe -pady 2m

#  Next row.
         incr r

#  Initialise space counter.
         set isp 1

#  Do each quanity.
         set col 0
         foreach q $quantities_ {
            set lq [string tolower $q]

#  Create a LabelMenu to select the column name for this quantity.
            itk_component add $lq {
	       util::LabelMenu $w_.$lq -text "$q:" \
                                -labelwidth $lwidth \
                                -variable [scope mvalues_($q)]
            }
            grid $itk_component($lq) -row $r -column $col -sticky nw -padx $px
            add_short_help $itk_component($lq) "Column to use for $shelp_($q) values"

#  Increment the column. Start a new row when two columns have been
#  produced.
            if { [incr col] == 2 } {
               set col 0
               grid [frame $w_.space$isp -height $vspace2] -row [incr r]
               incr isp
               incr r
            }

         }

#  Vertical space
         grid [frame $w_.spaceend -height $vspace1] -row [incr r]

#  Allow all cells of the grid to expand equally if the window is resized.
         for {set i 0} {$i < $ncol} {incr i} {
            grid columnconfigure $w_ $i -weight 1
         }
         for {set i 0} {$i < $r} {incr i} {
            grid rowconfigure $w_ $i -weight 1
         }

#  Re-instate the original values_ array.
         foreach name [array names mvalues_] {
            set mvalues_($name) $temp($name)
         }

#  Populate the menu buttons with the currently available column names.
         update_headings

#  Try to select the currently wanted column names.
         new_wants

      }
   }

#  Issue a warning and return zero if the catalogue contains any column names
#  which are unwanted. Only do this once for each catalogue.
#  --------------------------------------------------------------------
   public method colsOK {} {
      set ok 1
      if { !$warned_ } {
         set unwanted ""
         foreach col $headings_ {
            set wanted 0

            foreach q $quantities_ {
               set want [want_column $q]
               if { $want == $col } {
                  set wanted 1
                  break
               }
            }
            if { !$wanted } { lappend unwanted $col }
         }

         if { $unwanted != "" } {
            info_dialog "This catalogue contains unused column names ($unwanted).\n\nPlease use the \"Column Names\" control panel to ensure that the correct column names are being used."
            set ok 0
         }
         set warned_ 1
      }
      return $ok
   }

#  Protected methods:
#  ==================

#  Save the currently wanted column names in oldvals_
#  --------------------------------------------------
   protected method saveOld {} {
      foreach name [array names values_] {
         set oldvals_($name) $values_($name)
      }
   }

#  Ensure that all the menu buttons are populated with the currently
#  available column names.
#  -----------------------------------------------------------------
   protected method update_headings {} {

#  Do nothing if the controls have not been created.
      if { $created_ } {

#  Empty all menus.
         foreach lq [string tolower $quantities_] {
            $itk_component($lq) clear
         }

#  If any column headings are available...
         if { $headings_ != "" } {

#  Add items for each column to the menu.
            foreach colnam $headings_ {

#  Add this column name into all menus.
               foreach q $quantities_ {
                  set lq [string tolower $q]
                  $itk_component($lq) add \
                     -command [code $this activ $q] \
                     -label $colnam \
                     -value $colnam
               }
            }
         }

#  Add a "not available" option to the end of each menu.
         foreach lq [string tolower $quantities_] {
            $itk_component($lq) add -label "(not available)" -value ""
         }
      }
   }

#  This method updates the used column names so that they are the same as
#  the wanted column names, if possible. If a wanted column name is not
#  available in the currently opened and displayed catalogue, then the
#  used column name is set blank ("not available").
#  ----------------------------------------------------------------------
   protected method new_wants {} {
      foreach q $quantities_ {
         set col [want_column $q ]
         if { [lsearch -exact $headings_ $col] == -1 } {
            set mvalues_($q) ""
         } else {
            set mvalues_($q) $col
         }
         newVals $q
      }
   }

#  Returns the wanted column name (that is, the column name which we would
#  like to use if it is available) for a given quantity.
#  -----------------------------------------------------------------
   protected method want_column {q} {

#  If the user has explicitly requested that a particular column be used
#  for this quantity, return it.
      if { $values_($q) != "" } {
         return $values_($q)

#  Otherwise, if the catalogue includes an indication of which column should
#  be used for this quantity, return it.
      } elseif { $cat_ != "" } {
         set col [$cat_ getColNam $q]
         if { $col != "" } {
            return $col

#  Otherwise, assume that the column name is equal to the quantity name
#  (these are the names used by polpack).
         } else {
            return $q
         }

#  Return quantity name if no catalogue has yet been opened.
      } else {
         return $q
      }
   }

#  Private methods:
#  ==================

#  Options:
#  ========

#  A command to call to add an undoable action to the current list of
#  undoable actions.
   itk_option define -actioncmd actioncmd Actioncmd {}

#  The name of a directory in which to store tcl code which will recreate
#  the current GUI settings. This text is created when this object is
#  destroyed.
   itk_option define -optdir optdir Optdir {}

#  A command to call when any control values are changed by the user.
   itk_option define -changecmd changecmd Changecmd {}

#  Protected data members:
#  =======================
   protected {

#  The currently opened and displayed catalogue
      variable cat_ ""

#  Have the control widgets been created yet?
      variable created_ 0

#  The available column headings.
      variable headings_ ""

#  An array of descriptions (one for each control)
       variable desc_

#  An array of attribute names (one for each control)
       variable attr_

#  An array of the previous wanted column names
       variable oldvals_

#  Should current settings be saved when this object is destroyed?
       variable saveopt_ 1

#  An array of short help descriptions, indexed by quantity name.
       variable shelp_

#  Has the user ben warned about unused columns in the current catalogue?
       variable warned_ 0

#  Array indexed by (param).
#  These are the requested value (i.e. what we want but which may not be
#  available in the current headings). A blank value means "use which
#  ever column seems most appropriate".
       variable values_

#  Array indexed by (param).
#  These are the values actually used by the LabelMenus (i.e. these will
#  be set to "(not available)" if the requested value is not available in
#  the current list of column headings.
       variable mvalues_
   }

#  Private data members:
#  =====================
#  (none)

#  Common (i.e. static) data members:
#  ==================================

#  The list of known quantities (using standard polpack names).
   common quantities_ "X Y Z RA DEC I DI Q DQ U DU V DV P DP ANG DANG PI DPI ID"

#  End of class definition.
}
