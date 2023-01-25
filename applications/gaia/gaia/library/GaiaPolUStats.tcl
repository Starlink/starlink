#+
#  Name:
#     GaiaPolUStats

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     A control panel for calculating and displaying statistics of the
#     selected vectors.

#  Description:
#     This class calculates statistics for the selected vectors and
#     displays them within a page of the main notebook. Each time a new
#     selection is made, the page is updated to hold the new statistics. In
#     fact, the calculation of new statistics is only performed immediately
#     if this page is currently visible, otherwise the calculations are
#     postponed until the page becomes visible. This avoids spending
#     unnecessary time calculating statistics which may never be seen.

#  Invocations:
#
#        GaiaPolUStats object_name [configuration options]
#
#     This creates an instance of a GaiaPolUStats object. The returned value
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

itk::usual GaiaPolUStats {}

itcl::class gaia::GaiaPolUStats {

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
         catch { set cat_ [$cat_ annull] }
      }

#  Save the current options values to the options file, over-writing any
#  existing options file.
      if { $saveopt_ && [file isdirectory $itk_option(-optdir)] } {
         set optfile "$itk_option(-optdir)/GaiaPolUStats.opt"
         if { [catch {set fd [open $optfile w]} mess] } {
            puts "Error writing defaults to file '$optfile' for the polarimetry toolbox 'Statistics' panel : $mess"
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

#  Get the names of the statistics to display, and the number of rows
#  needed in the table (one for each statistic).
            set stats $values_($this,stats)
            if { $stats == "" } {
               set stats $stats_
            }
            set nrow [llength $stats]

#  Remove any columns which are no longer available from the list of columns
#  to be displayed.
            checkCols

#  Get the column headings to use. If blank, use all columns.
            set cols $values_($this,cols)
            if { $cols == "" } {
               set cols $useheads_
            }

#  Add a column at the start for the row labels.
            set heads [lreplace $cols 0 -1 " "]

#  Use null values if no data is available.
         } else {
            set nrow 3
            set heads ""
         }

#  Configure the TableList.
         $itk_component(table) config -height $nrow -headings $heads

#  Ensure all columns are visible.
         $itk_component(table) set_options $heads Show 1
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

#  Calculates statistics for supplied column data
#  -----------------------------------------------
   public method stats {cat stats icols} {

#  Get the data array
      set data [$cat getData]

#  Get access to an array of row states (selected, unselected, deleted)
#  indexed by row number.
      upvar 0 [$cat getStates] states

# Save the size of the table
      set nrow [$cat getNrow]

#  Get the number of passes through the data required.
      if { [lsearch -exact $stats "Sigma-clipped mean"] != -1 ||
           [lsearch -exact $stats "Sigma-clipped count"] != -1 ||
           [lsearch -exact $stats "Sigma-clipped std. devn"] != -1 } {
         set npass [expr $values_($this,niter) + 1]
      } else {
         set npass 1
      }

#  Store the total number of selected rows to pass through
      set srow [$cat getNsel]
      set trow [expr $srow*$npass]

#  Set up the progress bar.
      set inc [expr ($trow+19)/20]
      $itk_option(-pbar) config -to $trow
      update idletasks
      set prow -1
      set pj 0

#  Form the unclipped statistics.
      foreach icol $icols {
         set Count($icol) 0
      }

      for {set i 0} {$i < $nrow} {incr i} {
         if { $states($i) == "S" } {
            incr prow
            set row [lindex $data $i]
            foreach icol $icols {
               set v [lindex $row $icol]

               if { $Count($icol) == 0 } {
                  set Min($icol) $v
                  set Max($icol) $v
                  set Sum($icol) $v

                  if { ![catch { set Sum2($icol) [expr $v*$v] } ] } {
                     incr Count($icol)
                  }

               } elseif { ![catch { set sum0 [expr $Sum($icol) + $v]
                                    set sum20 [expr $Sum2($icol) + $v*$v] } ] } {
                  set Sum($icol) $sum0
                  set Sum2($icol) $sum20
                  if { $v < $Min($icol) } { set Min($icol) $v }
                  if { $v > $Max($icol) } { set Max($icol) $v }
                  incr Count($icol)

               }
            }

#  Update the progress bar value at intervals.
            if { [incr pj] == $inc } {
               set pj -1
               $itk_option(-pbar) config -value $prow
               update idletasks
            }
         }
      }

      set jcols ""
      foreach icol $icols {
         if { $Count($icol) > 0 } {
            if { ![catch { set mean [expr $Sum($icol)/$Count($icol)]
                           set sig [expr sqrt( $Sum2($icol)/$Count($icol) - $mean*$mean)] }] } {
               set Mean($icol) $mean
               set Sigma($icol) $sig
               lappend jcols $icol
            } else {
               lappend jcols ""
               set Mean($icol) ""
               set Sigma($icol) ""
               set Min($icol) ""
               set Max($icol) ""
               set Count($icol) 0
            }
         } else {
            lappend jcols ""
            set Mean($icol) ""
            set Sigma($icol) ""
            set Min($icol) ""
            set Max($icol) ""
         }
      }

#  If clipped statistics are required, calculate them.
      if { [lsearch -exact $stats "Sigma-clipped mean"] != -1 ||
           [lsearch -exact $stats "Sigma-clipped count"] != -1 ||
           [lsearch -exact $stats "Sigma-clipped std. devn"] != -1 ||
           [lsearch -exact $stats "Sigma-clipped minimum"] != -1 ||
           [lsearch -exact $stats "Sigma-clipped maximum"] != -1 } {

         foreach icol $icols {
            set CMean($icol) $Mean($icol)
            set CSigma($icol) $Sigma($icol)
            set CMax($icol) $Max($icol)
            set CMin($icol) $Min($icol)
            set CCount($icol) $Count($icol)
         }

         set nsig $values_($this,nsigma)
         for {set iter 0} {$iter < $values_($this,niter)} {incr iter} {

            foreach icol $jcols {
               if { $icol != "" } {
                  set lo($icol) [expr $CMean($icol) - $nsig*$CSigma($icol)]
                  set hi($icol) [expr $CMean($icol) + $nsig*$CSigma($icol)]
                  set CCount($icol) 0
               }
            }

            for {set i 0} {$i < $nrow} {incr i} {
               if { $states($i) == "S" } {
                  incr prow
                  set row [lindex $data $i]
                  foreach icol $jcols {
                     if { $icol != "" } {
                        set v [lindex $row $icol]
                        if { $v >= $lo($icol) && $v <= $hi($icol) } {
                           if { $CCount($icol) == 0 } {
                              set CSum($icol) $v
                              set CSum2($icol) [expr $v*$v]
                              set CMax($icol) $v
                              set CMin($icol) $v
                              set CCount($icol) 1
                           } else {
                              set CSum($icol) [expr $CSum($icol) + $v]
                              set CSum2($icol) [expr $CSum2($icol) + $v*$v]
                              if { $v < $CMin($icol) } { set CMin($icol) $v }
                              if { $v > $CMax($icol) } { set CMax($icol) $v }
                              incr CCount($icol)
                           }
                        }
                     }
                  }

#  Update the progress bar value at intervals.
                  if { [incr pj] == $inc } {
                     set pj -1
                     $itk_option(-pbar) config -value $prow
                     update idletasks
                  }

               }
            }

            foreach icol $jcols {
               if { $icol != "" } {
                  if { $CCount($icol) > 0 } {
                     set CMean($icol) [expr $CSum($icol)/$CCount($icol)]
                     set tmp [expr $CSum2($icol)/$CCount($icol) - $CMean($icol)*$CMean($icol)]
                     if { $tmp > 0.0 } {
                        set CSigma($icol) [expr sqrt($tmp)]
                     } else {
                        set CSigma($icol) 0.0
                     }
                  } else {
                     set CMax($icol) ""
                     set CMinn($icol) ""
                     set CMean($icol) ""
                     set CSigma($icol) "0.0"
                  }
               }
            }
         }
      }

#  Return a list of lists suitable for use with the TableList (i.e. add an
#  extra column to the start holding statistics names).
      set ret ""
      foreach stat $stats {
         set row [list $stat]
         if { $stat == "Count" } {
            foreach icol $icols {
               lappend row $Count($icol)
            }
         } elseif { $stat == "Mean" } {
            foreach icol $icols {
               lappend row $Mean($icol)
            }
         } elseif { $stat == "Standard deviation" } {
            foreach icol $icols {
               lappend row $Sigma($icol)
            }
         } elseif { $stat == "Minimum" } {
            foreach icol $icols {
               lappend row $Min($icol)
            }
         } elseif { $stat == "Maximum" } {
            foreach icol $icols {
               lappend row $Max($icol)
            }
         } elseif { $stat == "Sigma-clipped count" } {
            foreach icol $icols {
               lappend row $CCount($icol)
            }
         } elseif { $stat == "Sigma-clipped mean" } {
            foreach icol $icols {
               lappend row $CMean($icol)
            }
         } elseif { $stat == "Sigma-clipped std. devn" } {
            foreach icol $icols {
               lappend row $CSigma($icol)
            }
         } elseif { $stat == "Sigma-clipped minimum" } {
            foreach icol $icols {
               lappend row $CMin($icol)
            }
         } elseif { $stat == "Sigma-clipped maximum" } {
            foreach icol $icols {
               lappend row $CMax($icol)
            }
         }
         lappend ret $row
      }

      return $ret
   }

#  Calculates new statistics on the basis of the supplied GaiaPolCat
#  -----------------------------------------------------------------
   public method calc {} {

#  Do nothing if the required statistics have already been calculated or
#  if no data is available.
      if { !$done_ && $cat_ != "" } {

#  Indicate what is happening.
         setHold "Calculating new statistics for selected vectors..."

#  Get the names of the statistics to display. If blank, use all stats.
         set stats $values_($this,stats)
         if { $stats == "" } {
            set stats $stats_
         }

#  Remove any columns which are no longer available from the list of columns
#  to be displayed.
         checkCols

#  Get the columns to display. If blank, use all columns.
         set cols $values_($this,cols)
         if { $cols == "" } {
            set cols $useheads_
         }

#  Find the indices within the catalogue of the required columns.
         set icols ""
         set icol -1
         foreach head $headings_ {
            incr icol
            if { [lsearch -exact $cols $head] != -1 } {
               lappend icols $icol
            }
         }

#  Calculate the statistics and update the values shown in the table.
         $itk_component(table) config -info [stats $cat_ $stats $icols]

#  Indicate that the required statistics have now been calculated.
         set done_ 1

#  Reset the progress bar.
         resetHold

      }
   }

#  Command which is invoked when a change is made to any GUI control.
#  -------------------------------------------------------------------
   public method activ { args } {

#  Ensure the values in the values_ array are up to date.
#     (does not use any LabelMenus as yet)

#  Get the name of the changed value.
      set item [lindex $args 0]

#  Use the command specified by the -actioncmd option to store a new
#  undoable action in the actions list.
      newAction $item

#  Implement the requested change.
      newVals

   }

#  Reset default values (either read from an options file or hard-wired).
#  ----------------------------------------------------------------------
   public method reset { {readopt 1} } {

#  Store control descriptions:
      set desc_(cols) "the columns for which statistics are required"
      set desc_(stats) "the statistics to be calculated for each column"
      set desc_(niter) "the number of iterations for the sigma clipping algorithm"
      set desc_(nsigma) "the number of standard deviations for the sigma clipping algorithm"

#  Store attribute names (i.e. the name for the corresponding get/set
#  methods)
      set attr_(cols) Cols
      set attr_(stats) Stats
      set attr_(niter) Niter
      set attr_(nsigma) Nsigma

#  Set the hard-wired defaults.
      set values_($this,cols) ""
      set values_($this,stats) ""
      set values_($this,niter) "3"
      set values_($this,nsigma) "3.0"

#  Over-write these with the values read from the options file created when
#  the last used instance of this class was destroyed.
      set optfile "$itk_option(-optdir)/GaiaPolUStats.opt"
      if { $readopt && [file readable $optfile] } {
         if { [catch {source $optfile} mess] } {
            puts "Error reading defaults from file '$optfile' for the polarimetry toolbox 'Statistics' panel : $mess"
         } else {
            foreach elem [array names option] {
               set values_($this,$elem) "$option($elem)"
            }
         }
      }

#  Replace illegal blank values read from the options file with the hardwired
#  defaults.
      if { $values_($this,niter) == "" } { set values_($this,niter) "3" }
      if { $values_($this,nsigma) == "" } { set values_($this,nsigma) "3" }

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

#  Sore the supplied headings.
      set headings_ $h

#  Produce a list of headings without X/Y RA/DEC columns.
      if { $cat_ != "" } {
         set xhead [lindex $headings_ [$cat_ getXCol]]
         set yhead [lindex $headings_ [$cat_ getYCol]]

         if { [$cat_ gotWcs] } {
            set rahead [lindex $headings_ [$cat_ getRaCol]]
            set dechead [lindex $headings_ [$cat_ getDecCol]]
         } else {
            set rahead ""
            set dechead ""
         }

         set useheads_ ""
         foreach head $headings_ {
            if { $head != $xhead && $head != $yhead && $head != $rahead && $head != $dechead } {
               lappend useheads_ $head
            }
         }

      } else {
         set useheads_ $headings_
      }

#  Configure the table to use the new headings.
      tabConfig

   }

   public method getCols {} {return $values_($this,cols)}
   public method setCols {x} {set values_($this,cols) $x; newVals}

   public method getStats {} {return $values_($this,stats)}
   public method setStats {x} {set values_($this,stats) $x; newVals}

   public method getNiter {} {return $values_($this,niter)}
   public method setNiter {x} {set values_($this,niter) $x; newVals}

   public method getNsigma {} {return $values_($this,nsigma)}
   public method setNsigma {x} {set values_($this,nsigma) $x; newVals}

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
   public method newVals {} {
      saveOld
      if { "$itk_option(-changecmd)" != "" } {
         eval $itk_option(-changecmd)
      }

#  Ensure the table columns and rows are as required.
      tabConfig

#  Update the statistics
      set done_ 0
      if { [winfo viewable $w_] } {
         calc
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
         set lwidth 12

#  Initialise the row index withi the geaometry grid
         set r -1

#  Items to display header...
         itk_component add header1 {
	    LabelRule $w_.header1 -text "Items to display:"
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

#  Create a LabelButton associated with a dialog of check buttons which
#  indicate which statistics are to be displayed.
         itk_component add stats {
            ::gaia::LabelButton $w_.stats -text "Statistics:" \
                                 -command [code $this get_stats] \
   			         -labelwidth $lwidth \
                                 -anchor nw \
                                 -buttonbitmap finger
	 }
         grid $itk_component(stats) -row $r -column 1 -sticky nw -padx $px
         add_short_help $itk_component(stats) {Click to select which statistics to display}

#  Vertical space
         grid [frame $w_.space1 -height $vspace1] -row [incr r]

#  Vector parameters...
         itk_component add header2 {
	    LabelRule $w_.header2 -text "Parameters for N-sigma clipping:"
	 }
         grid $itk_component(header2) -row [incr r] -column 0 -padx 1m \
                                      -columnspan $ncol -sticky nwe -pady 2m

#  Next row.
         incr r

#  Create a LabelEntry to set the number of iterations for N-sigma clipping
         itk_component add niter {
	    util::LabelEntry $w_.niter -text "Iterations:" \
                              -valuewidth $vwidth \
			      -labelwidth $lwidth \
                              -anchor nw \
			      -validate integer \
                              -command [code $this activ niter] \
                              -textvariable [scope values_($this,niter)]
	 }
         grid $itk_component(niter) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(niter) {Number of iterations for the sigma clipping algorithm}

#  Create a LabelEntry to set the number of standard deviations for N-sigma
#  clipping
         itk_component add nsigma {
	    util::LabelEntry $w_.nsigma -text "N-Sigma:" \
                              -valuewidth $vwidth \
			      -labelwidth $lwidth \
                              -anchor nw \
			      -validate real \
                              -command [code $this activ nsigma] \
                              -textvariable [scope values_($this,nsigma)]
	 }
         grid $itk_component(nsigma) -row $r -column 1 -sticky nw -padx $px
         add_short_help $itk_component(nsigma) {Number of standard deviations for the sigma clipping algorithm}

#  Vertical space
         grid [frame $w_.space2 -height $vspace1] -row [incr r]

#  Vector parameters...
         itk_component add header3 {
	    LabelRule $w_.header3 -text "Statistics for currently selected vectors:"
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
         add_short_help $itk_component(table) {Statistics for the currently selected vectors}
         tabConfig

#  Vertical space
         grid [frame $w_.space3 -height $vspace1] -row [incr r]

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

#  Get a list of the statistics to display.
#  ----------------------------------------
   protected method get_stats {} {

#  Create the options dialog box.
      set w $w_.statselect
      catch {destroy $w}
      set d [::gaia::GaiaPolOptionsDialog $w \
               -title "Select statistics" \
               -transient 1 \
               -default 0 \
               -options $stats_ \
               -text "Select the statistics to be displayed:" \
               -buttons {OK Cancel All}]

#  Check the currently selected statistics. Check all of them if the
#  string is blank.
      if { $values_($this,stats) != "" } {
         $d setOptions $values_($this,stats)
      } else {
         $d allOptions
      }

#  Wait for the user to press on of the buttons at the bottom of the
#  options dialog box.
      lassign [$d activate] but opts

#  Was OK pressed? If the options have changed, store the new values.
      if { $but == 0 } {
         if { $opts != $values_($this,stats)} {
            set values_($this,stats) $opts
            activ stats
         }

#  Was All pressed? Set the selection back to blank means "display all
#  available statistics"
      } elseif { $but == 2 } {
         if { $values_($this,stats) != "" } {
            set values_($this,stats) ""
            activ stats
         }

#  Do nothing if Cancel was pressed.
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

#  A command to call when any control values are changed by the user.
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

#  The names of the statistics which can be displayed.
      variable stats_ {"Count" "Mean" "Standard deviation" "Minimum" "Maximum" "Sigma-clipped count" "Sigma-clipped mean" "Sigma-clipped std. devn" "Sigma-clipped minimum" "Sigma-clipped maximum"}

#  An array of descriptions (one for each control)
       variable desc_

#  An array of attribute names (one for each control)
       variable attr_

#  An array of the previous control values.
       variable oldvals_

#  Should current settings be saved when this object is destroyed?
       variable saveopt_ 1
   }

#  Private data members:
#  =====================
#  (none)

#  Common (i.e. static) data members:
#  ==================================

#  Array for passing around at global level. Indexed by ($this,param).
   common values_

#  Name of stats selection dialog window.
   common statselect_ .polstatselect

#  End of class definition.
}
