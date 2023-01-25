#+
#  Name:
#     GaiaPolUInteg

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     A control panel for calculating and displaying the integrated
#     polarization of the selected vectors.

#  Description:
#     This class calculates the integrated polarization of the selected
#     vectors and displays them within a page of the main notebook. Each
#     time a new selection is made, the page is updated to hold the new
#     statistics. In fact, the calculation of new statistics is only
#     performed immediately if this page is currently visible, otherwise
#     the calculations are postponed until the page becomes visible. This
#     avoids spending unnecessary time calculating statistics which may
#     never be seen.

#  Invocations:
#
#        GaiaPolUInteg object_name [configuration options]
#
#     This creates an instance of a GaiaPolUInteg object. The returned value
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
#     24-NOV-2000 (DSB):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaPolUInteg {}

itcl::class gaia::GaiaPolUInteg {

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

#  Set defaults
      reset
   }

#  Destructor:
#  ============
   destructor {

#  Annul any reference to the displayed GaiaPolCat.
      if { $cat_ != "" } {
         set cat_ [$cat_ annull]
      }

#  Save the current options values to the options file, over-writing any
#  existing options file.
      if { $saveopt_ && [file isdirectory $itk_option(-optdir)] } {
         set optfile "$itk_option(-optdir)/GaiaPolUInteg.opt"
         if { [catch {set fd [open $optfile w]} mess] } {
            puts "Error writing defaults to file '$optfile' for the polarimetry toolbox 'Integrate' panel : $mess"
         } else {
            foreach name [array names values_] {
               if { [regexp {([^,]+),(.*)} $name match obj elem] } {
                  if { $obj == $this } {
                     puts $fd "set option($elem) \{$values_($name)\}"
                     unset values_($name)
                  }
               }
            }
            close $fd
         }
      }
   }

#  Public methods:
#  ===============

#  Clear the whole page
#  --------------------
   public method clear {} {

#  Annul any reference to the displayed GaiaPolCat.
      if { $cat_ != "" } {
         set cat_ [$cat_ annull]
      }

#  Reset headings.
      setHeadings ""

#  Clear the table contents.
      if { [info exists itk_component(table)] } {
         $itk_component(table) clear
      }
   }

#  Re-configure the statistics table.
#  ----------------------------------
   public method tabConfig {} {
      if { [info exists itk_component(table)] } {
         if { $cat_ != "" } {

#  Remove any columns which are no longer available from the list of columns
#  to be displayed.
            checkCols

#  Get the column headings to use. If blank, use all columns.
            set cols $values_($this,cols)
            if { $cols == "" } {
               set cols $useheads_
            }

#  Use null values if no data is available.
         } else {
            set cols ""
         }

#  Configure the TableList.
         $itk_component(table) config -height 1 -headings $cols

#  Ensure all columns are visible.
         $itk_component(table) set_options $cols Show 1
      }
   }

#  Called when a new catalogue or selection is displayed.
#  ------------------------------------------------------
   public method newStats { cat } {

#  Save a reference to the supplied GaiaPolCat, annulling any
#  reference to a previous GaiaPolCat first.
      if { $cat_ != "" } {
         $cat_ annull
      }
      set cat_ [$cat clone]

#  Ensure the column names are up-to-date.
      setHeadings [$cat getHeadings]

#  Indicate that statistics have not yet been calculated for this GaiaPolCat
      set done_ 0

#  If this page of the notebook is currently visible, calculate the new
#  statistics immediately and display them. Otherwise, statistics will
#  be calculated when the page is next displayed.
      if { [winfo viewable $w_] } {
         calc
      }

   }

#  Calculates new statistics on the basis of the supplied GaiaPolCat
#  -----------------------------------------------------------------
   public method calc { {item ""} } {

#  Are the column names OK?
      if { "$itk_option(-changecmd)" != "" } {
         set ok [eval $itk_option(-changecmd)]
      } else {
         set ok 1
      }

#  Do nothing if the required statistics have already been calculated or
#  if no data is available, or if the column names need checking.
      if { !$done_ && $cat_ != "" && $ok } {

#  Clear the table.
         $itk_component(table) clear

#  Get the number of selected vectors.
         set nsel [$cat_ getNsel]

#  Only allow up to 100 vectors to be integrated (unless using mean method)
#  if variances are present.
         if { [getMethod] != "mean" && $nsel > 100 && [$cat_ getColNam DI] != "" } {
            error_dialog "You have selected more than 100 vectors ($nsel). This number of vectors can only be integrated if there are no variances or if method \"Mean\" is used."

#  Otherwise (if there are some selected vectors) ...
         } elseif { $nsel > 0 } {

#  If the columns only have changed, we already have the required data in
#  $row_, so we do not need to recalculate.
            if { $item != "cols" || $row_ == "" } {

#  Indicate what is happening.
               setHold "Calculating integrated polarization of selected vectors..."

#  Create a copy of $this.
               set newcat [$cat_ copy]

#  Remove all unselected vectors from $newcat.
               $newcat cut U

#  Create a new GaiaPolCat containing a single row holding the integrated
#  values formed by binning all the selected vectors.
               set single [$newcat bin 1 [getMethod] [getDebias] 1 [getSigmas] 1]

#  Check the above worked ok.
               if { ![catch {$single toString}] } {

#  Get the data for this single row.
                  set row_ [lindex [$single getData] 0]

#  Also store the column names in the binned catalogue.
                  set sheads_ [$single getHeadings]

#  Annull the unrequired objects.
                  $single annull
               }

#  Annull the unrequired objects.
               $newcat annull

#  Reset the progress bar.
               resetHold

            }

#  Remove any columns which are no longer available from the list of columns
#  to be displayed.
            checkCols

#  Get the columns to display. If blank, use all columns.
            set cols $values_($this,cols)
            if { $cols == "" } {
               set cols $useheads_
            }

#  Form a list of the values of the required columns.
            set vals ""
            foreach head $cols {
               set icol [lsearch -exact $sheads_ $head]
               if { $icol != -1 } {
                  lappend vals [lindex $row_ $icol]
               } else {
                  lappend vals ""
               }
            }

#  Update the values shown in the table.
            $itk_component(table) config -info [list $vals]

#  Indicate that the required statistics have now been calculated.
            set done_ 1

         }
      }
   }

#  Command which is invoked when a change is made to any GUI control.
#  -------------------------------------------------------------------
   public method activ { args } {

#  Ensure the values in the values_ array are up to date.
      set values_($this,method) [$itk_component(method) get]

#  Get the name of the changed value.
      set item [lindex $args 0]

#  Use the command specified by the -actioncmd option to store a new
#  undoable action in the actions list.
      newAction $item

#  Implement the requested change.
      newVals $item
   }

#  Reset default values (either read from an options file or hard-wired).
#  ----------------------------------------------------------------------
   public method reset { {readopt 1} } {

#  Store control descriptions:
      set desc_(cols) "the columns to be displayed"
      set desc_(method) "the binning method"
      set desc_(debias) "whether to debias the inegrated polarization value"
      set desc_(sigmas) "the number of standard deviations for sigma-clipping"

#  Store attribute names (i.e. the name for the corresponding get/set
#  methods)
      set attr_(cols) Cols
      set attr_(method) Method
      set attr_(debias) Debias
      set attr_(sigmas) Sigmas

#  Set the hard-wired defaults.
      set values_($this,cols) ""
      set values_($this,method) "mean"
      set values_($this,debias) 1
      set values_($this,sigmas) 4

#  Over-write these with the values read from the options file created when
#  the last used instance of this class was destroyed.
      set optfile "$itk_option(-optdir)/GaiaPolUInteg.opt"
      if { $readopt && [file readable $optfile] } {
         if { [catch {source $optfile} mess] } {
            puts "Error reading defaults from file '$optfile' for the polarimetry toolbox 'Integrate' panel : $mess"
         } else {
            foreach elem [array names option] {
               set values_($this,$elem) "$option($elem)"
            }
         }
      }

#  Replace illegal blank values read from the options file with the hardwired
#  defaults.
      if { $values_($this,method) == "" } { set values_($this,method) "mean" }
      if { $values_($this,debias) == "" } { set values_($this,debias) 1 }
      if { $values_($this,sigmas) == "" } { set values_($this,sigmas) 4 }

#  Use these settings.
      newVals

   }

#  Unselect any columns previously selected using the column selection
#  dialog but which are not now available in the currently displayed catalogue.
#  ---------------------------------------------------------------------
   public method checkCols {} {

      set cols $values_($this,cols)

#  If the cols list is not blank, we need to check that each element in
#  the list is a heading which is available in the currently displayed
#  catalogue. Get a list of the good column names, and a list of the
#  bad column names.
      if { $cols != "" } {
         set badcols ""
         set goodcols ""

         foreach col $cols {
            set indx [lsearch -exact $useheads_ $col]
            if { $indx == -1 } {
               append badcols " $col"
            } else {
               lappend goodcols $col
            }
         }

#  If any requested columns were not available in the current headings,
#  update the common values_ array to hold only the available columns.
         if { $badcols != "" } {
            set values_($this,cols) $goodcols
         }
      }
   }

#  Accessor methods:
#  -----------------
   public method setHeadings {h} {

#  Store the supplied headings.
      set headings_ $h

#  Produce a list of headings without X/Y, RA/DEC or ID columns.
      if { $cat_ != "" } {
         set xhead [lindex $headings_ [$cat_ getXCol]]
         set yhead [lindex $headings_ [$cat_ getYCol]]
         set idhead [lindex $headings_ [$cat_ getIDCol]]

         if { [$cat_ gotWcs] } {
            set rahead [lindex $headings_ [$cat_ getRaCol]]
            set dechead [lindex $headings_ [$cat_ getDecCol]]
         } else {
            set rahead ""
            set dechead ""
         }

         set useheads_ ""
         foreach head $headings_ {
            if { $head != $idhead && $head != $xhead && $head != $yhead && $head != $rahead && $head != $dechead } {
               lappend useheads_ $head
            }
         }

      } else {
         set useheads_ $headings_
      }

#  Configure the table to use the new headings.
      tabConfig

#  Forget any pre-calculated values.
      set row_ ""

   }

   public method getCols {} {return $values_($this,cols)}
   public method setCols {x} {set values_($this,cols) $x; newVals}
   public method setMethod {x} {set values_($this,method) $x}
   public method getMethod {} {return $values_($this,method)}
   public method setDebias {x} {set values_($this,debias) $x}
   public method getDebias {} {return $values_($this,debias)}
   public method setSigmas {x} {set values_($this,sigmas) $x}
   public method getSigmas {} {return $values_($this,sigmas)}
   public method setSaveOpt {x} {set saveopt_ $x}

#  Called to add a new action to the current list of undoable actions.
#  ------------------------------------------------------------------------
   public method newAction {item} {
      if { "$itk_option(-actioncmd)" != "" } {
         set arglist "object \{change $desc_($item)\} $this \{set$attr_($item) \"$oldvals_($item)\"\} \{set$attr_($item) \"$values_($this,$item)\"\}"
         eval $itk_option(-actioncmd) $arglist
      }
   }

#  Called to save the current control values as next times previous
#  values, and to implement the new settings by calling the change command.
#  ------------------------------------------------------------------------
   public method newVals { {item ""} } {
      saveOld

#  Ensure the table columns and rows are as required.
      tabConfig

#  Update the statistics if the change relates to the current method.
      if { $item != "sigmas" || $values_($this,method) == "sigma" } {
         set done_ 0
         if { [winfo viewable $w_] } {
            calc $item
         }
      }
   }

#  Create the page of controls.
#  ----------------------------
   public method create {} {
      global ::env

#  Do nothing if the controls have already been created.
      if { ! $created_ } {

#  Save the values_ array so that hey can be reinstated later (the widget
#  creation commands seem to reset them to blank).
         foreach name [array names values_] {
            set temp($name) $values_($name)
         }

#  Indicate that the controls are being created.
         set created_ 1

#  Number of columns in the grid.
         set ncol 1

#  Horizontal padding for columns.
         set px 2m

#  Vertical space between sections
         set vspace1 3m

#  Vertical space rows within a section
         set vspace2 2m

#  Value width (in characters).
         set vwidth 6

#  Label widths...
         set lwidth 12

#  Initialise the row index withi the geaometry grid
         set r -1

#  Items to display header...
         itk_component add header1 {
	    gaia::LabelRule $w_.header1 -text "Items to display:"
	 }
         grid $itk_component(header1) -row [incr r] -column 0 -padx 1m \
                                      -columnspan $ncol -sticky nwe -pady 2m

#  Next row.
         incr r

#  Create a LabelButton associated with a dialog of check buttons which
#  indicate which catalogue columns are to be displayed.
         itk_component add cols {
            ::gaia::LabelButton $w_.cols -text "Columns:" \
                                 -command [code $this get_cols] \
   			         -labelwidth $lwidth \
                                 -anchor nw \
                                 -buttonbitmap finger
	 }
         grid $itk_component(cols) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(cols) {Click to select the catalogue columns for which statistics are required}

#  Vertical space
         grid [frame $w_.space1 -height $vspace1] -row [incr r]

#  Vector parameters...
         itk_component add header2 {
	    gaia::LabelRule $w_.header2 -text "Parameters for binning:"
	 }
         grid $itk_component(header2) -row [incr r] -column 0 -padx 1m \
                                      -columnspan $ncol -sticky nwe -pady 2m

#  Next row.
         incr r

#  Create a LabelMenu to control the method used for binning.
         itk_component add method {
            util::LabelMenu $w_.method -text "Method:" \
                                 -variable [scope values_($this,method)] \
                                 -labelwidth $lwidth
         }
         grid $itk_component(method) -row $r -columnspan $ncol -column 0 -sticky nw -padx $px
         add_short_help $itk_component(method) {Method used to combine Stokes parameters}
         $itk_component(method) add -label "Mean" -value mean \
                                    -command "[code $this activ method]"
         $itk_component(method) add -label "Median" -value median \
                                    -command "[code $this activ method]"
         $itk_component(method) add -label "Sigma-clipped Mean" -value sigma \
                                    -command "[code $this activ method]"

#  Vertical space.
         grid [frame $w_.space2 -height $vspace2] -row [incr r]

#  Next row
         incr r

#  Create a LabelCheck to set debiassing.
         itk_component add debias {
            gaia::StarLabelCheck $w_.debias -text "Debias:" \
                                     -onvalue 1 \
                                     -offvalue 0 \
                                     -labelwidth $lwidth \
                                     -command [code $this activ debias] \
                                     -anchor nw \
                                     -variable [scope values_($this,debias)]
         }
         grid $itk_component(debias) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(debias) {Should the integrated polarization be debiassed if possible?}

#  Vertical space.
         grid [frame $w_.space3 -height $vspace2] -row [incr r]

#  Next row
         incr r

#  Create an integer entry for the sigmas.
         itk_component add sigmas {
            util::LabelEntry $w_.sigmas -text "Sigmas:" \
                                 -valuewidth $vwidth \
                                 -command [code $this activ sigmas] \
                                 -labelwidth $lwidth \
                                 -anchor nw \
                                 -validate integer \
                                 -textvariable [scope values_($this,sigmas)]
         }
         grid $itk_component(sigmas) -row $r -column 0 -columnspan $ncol -sticky nw -padx $px
         add_short_help $itk_component(sigmas) {The number of standard deviations at which to clip when using method 'sigma-clipped mean'}

#  Vertical space.
         grid [frame $w_.space4 -height $vspace1] -row [incr r]

#  Next row
         incr r

#  Vector parameters...
         itk_component add header3 {
	    gaia::LabelRule $w_.header3 -text "Integrated column values for currently selected vectors:"
	 }
         grid $itk_component(header3) -row [incr r] -column 0 -padx 1m \
                                      -columnspan $ncol -sticky nwe -pady 2m

#  Next row.
         incr r

#  Create a TableList to display the numerical catalogue contents.
         itk_component add table {
            ::util::TableList $w_.table -hscroll 1 -height 4 \
                                        -selectmode extended \
                                        -exportselection 0
         }
         grid $itk_component(table) -row $r -column 0 -columnspan $ncol -sticky nsew -padx $px
         add_short_help $itk_component(table) {Integrated values for the currently selected vectors}
         tabConfig

#  Vertical space
         grid [frame $w_.space5 -height $vspace1] -row [incr r]

#  Allow all cells of the grid to expand equally if the window is resized.
         for {set i 0} {$i < $ncol} {incr i} {
            grid columnconfigure $w_ $i -weight 1
         }
         for {set i 0} {$i < $r} {incr i} {
            grid rowconfigure $w_ $i -weight 1
         }

#  Re-instate the original values_ array.
         foreach name [array names values_] {
            set values_($name) $temp($name)
         }
      }
   }

#  Protected methods:
#  ==================

#  Save the current control settings in oldvals_
#  ---------------------------------------------
   protected method saveOld {} {
      foreach name [array names values_] {
         if { [regexp {[^,]+,(.*)} $name match elem] } {
            set oldvals_($elem) $values_($name)
         }
      }
   }

#  Get a list of the columns to display.
#  -------------------------------------
   protected method get_cols {} {

#  Create the options dialog box.
      set w $w_.colselect
      catch {destroy $w}
      set d [::gaia::GaiaPolOptionsDialog $w \
               -title "Select columns" \
               -transient 1 \
               -default 0 \
               -options $useheads_ \
               -text "Select the columns to be displayed:" \
               -buttons {OK Cancel All}]

#  Check the currently selected column names. Check all names if the
#  string is blank.
      checkCols
      if { $values_($this,cols) != "" } {
         $d setOptions $values_($this,cols)
      } else {
         $d allOptions
      }

#  Wait for the user to press on of the buttons at the bottom of the
#  options dialog box.
      lassign [$d activate] but opts

#  Was OK pressed? If the options have changed, store the new values.
      if { $but == 0 } {
         if { $opts != $values_($this,cols)} {
            set values_($this,cols) $opts
            activ cols
         }

#  Was All pressed? Set the column selection back to blank means "use all
#  available columns"
      } elseif { $but == 2 } {
         if { $values_($this,cols) != "" } {
            set values_($this,cols) ""
            activ cols
         }

#  Do nothing if Cancel was pressed.
      }
   }

#  Private methods:
#  ==================
#  Indicate what is going on.
#  --------------------------
   private method setHold {text} {
      blt::busy hold $w_ -cursor "watch"
      $itk_option(-pbar) reset
      $itk_option(-pbar) config -text $text
      update idletasks
   }

#  Clear the progress bar etc.
#  --------------------------
   private method resetHold {} {
      blt::busy release $w_
      $itk_option(-pbar) reset
      update idletasks
   }

#  Options:
#  ========

#  A command to call to add an undoable action to the current list of
#  undoable actions.
   itk_option define -actioncmd actioncmd Actioncmd {}

#  The name of a directory in which to store tcl code which will recreate
#  the current GUI settings. This text is created when this object is
#  destroyed.
   itk_option define -optdir optdir Optdir {}

#  A command to call when new statistics are about to be calculated.
   itk_option define -changecmd changecmd Changecmd {}

#  Thw window containing the progress bar
   itk_option define -pbar pbar PBar {}

#  Protected data members:
#  =======================
   protected {

#  The GaiaPolUcat which is currently displayed. This encapsulates both
#  the column data and the list of selected vectors.
      variable cat_ ""

#  Have statistics been calculated for the GaiaPolUcat referenced by $cat_?
      variable done_ 0

#  Have the control widgets been created yet?
      variable created_ 0

#  All column headings in the catalogue.
      variable headings_ ""

#  Column headings without position columns
      variable useheads_ ""

#  An array of descriptions (one for each control)
       variable desc_

#  An array of attribute names (one for each control)
       variable attr_

#  An array of the previous control values.
       variable oldvals_

#  Should current settings be saved when this object is destroyed?
       variable saveopt_ 1

#  A row of column values produced from the last binning operation.
       variable row_ ""

#  The column headings from the last binned catalogue.
       variable sheads_ ""

   }

#  Private data members:
#  =====================
#  (none)

#  Common (i.e. static) data members:
#  ==================================

#  Array for passing around at global level. Indexed by ($this,param).
   common values_

#  End of class definition.
}
