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
#     holding I, Q, U, V, DI, DQ, DU and DV columns. The hard-wired
#     defaults correspond to the column names used by polpack.
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

#  Set defaults
      reset 
   }

#  Destructor:
#  ============
   destructor {

#  Save the current options values to the options file, over-writing any
#  existing options file.
      if { $saveopt_ && [file isdirectory $itk_option(-optdir)] } {
         set optfile "$itk_option(-optdir)/GaiaPolUCols.opt"
         if { [catch {set fd [open $optfile w]} mess] } {
            puts "Error writing defaults to file '$optfile' for the polarimetry toolbox 'Column Names' panel : $mess"
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

#  Command which is invoked when a change is made to any GUI control.
#  -------------------------------------------------------------------
   public method activ { args } {

#  Get the name of the changed quantity.
      set q [lindex $args 0]
      set lq [string tolower $q]

#  Ensure the value for the changed control is up to date in the values_ 
#  and mvalues_ array.
      set values_($this,$q) [$itk_component($lq) get]
      set mvalues_($this,$q) [$itk_component($lq) get]

#  Use the command specified by the -actioncmd option to store a new
#  undoable action in the actions list.
      newAction $q

#  Implement the requested change.
      newVals $q
   }

#  Reset default values (either read from an options file or hard-wired).
#  ----------------------------------------------------------------------
   public method reset { {readopt 1} } {

#  Store control descriptions, aand attribute names (i.e. the name for the 
#  corresponding get/set methods). Set the hard-wired defaults.
      foreach q $quantities_ {
         set desc_($q) "the column to use for $q values"
         set attr_($q) $q
         append attr_($q) Col
         set values_($this,$q) $q
      }

#  Over-write these with the values read from the options file created when
#  the last used instance of this class was destroyed.
      set optfile "$itk_option(-optdir)/GaiaPolUCols.opt"
      if { $readopt && [file readable $optfile] } {
         if { [catch {source $optfile} mess] } {
            puts "Error reading defaults from file '$optfile' for the polarimetry toolbox 'Column Names' panel : $mess"
         } else {
            foreach elem [array names option] {
               set values_($this,$elem) "$option($elem)"
            }
         }
      }

#  Save the original values as next times previous values.
      saveOld

   }

#  Accessor methods:
#  -----------------
   public method setHeadings {h} {
      set headings_ $h
      update_headings
   }

   public method getCol {q} {return $mvalues_($this,$q)}
   public method setCol {q c} {set values_($this,$q) $c; newVals $q}
   public method setSaveOpt {x} {set saveopt_ $x}

#  Called to add a new action to the current list of undoable actions.
#  ------------------------------------------------------------------------
   public method newAction {item} {
      if { "$itk_option(-actioncmd)" != "" } {
         set arglist "object \{change $desc_($item)\} $this \{setCol $item \"$oldvals_($item)\"\} \{setCol $item \"$values_($this,$item)\"\}"
         eval $itk_option(-actioncmd) $arglist
      }
   }

#  Called when a new GaiaPolCat is opened.
#  ---------------------------------------   
   public method newCat {cat} {

#  Store the new headings.
      setHeadings [$cat getHeadings]

#  For each quantity, if the column name storing the quantity is not the
#  same as the quantity name, set the column name.
      foreach q $quantities_ {
         set col [$cat getColNam $q]
         if { $col != $q } {
            set values_($this,$q) $col
            newVals $q
         }
      }
   }

#  Called to save the current control values as next times previous
#  values, and to implement the new settings by calling the change command.
#  ------------------------------------------------------------------------
   public method newVals {q} {
      saveOld
      update_menus
      if { "$itk_option(-changecmd)" != "" } {
         eval $itk_option(-changecmd) $q
      }
   }

#  Create the page of controls.
#  ----------------------------
   public method create {} {

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
         set lwidth 5

#  Initialise the row index within the geometry grid
         set r -1

#  Column names header...
         itk_component add header1 { 
	    LabelRule $w_.header1 -text "Columns to use for:"
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
	       LabelMenu $w_.$lq -text "$q:" \
                                -labelwidth $lwidth \
                                -variable [scope mvalues_($this,$q)]
            }
            grid $itk_component($lq) -row $r -column $col -sticky nw -padx $px
            add_short_help $itk_component($lq) "Column to use for $q values"

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

#  Update the contents and settings of the headings controls.
         update_headings

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

#  Update the headings menus
#  -------------------------
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

#  Ensure the menu values reflect the requested values.
         update_menus
      }
   }

#  If the current setting for each column is not available in the list of
#  headings, select the not available option. If the current setting is
#  available, select it.
#  ----------------------------------------------------------------------
   protected method update_menus {} {
      foreach q $quantities_ {
         if { [lsearch -exact $headings_ $values_($this,$q)] == -1 } {
            set mvalues_($this,$q) ""
         } else {
            set mvalues_($this,$q) $values_($this,$q)
         }
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

#  Have the control widgets been created yet?
      variable created_ 0

#  The available column headings.
      variable headings_ ""

#  An array of descriptions (one for each control)
       variable desc_

#  An array of attribute names (one for each control)
       variable attr_

#  An array of the previous control values.
       variable oldvals_ 

#  Should current settings be saved when this object is destroyed?
       variable saveopt_ 1

#  The list of known quantities (using standard polpack names).
       variable quantities_ "X Y Z RA DEC I DI Q DQ U DU V DV P DP ANG DANG PI DPI ID"
   }

#  Private data members: 
#  =====================
#  (none)

#  Common (i.e. static) data members:
#  ==================================

#  Array for passing around at global level. Indexed by ($this,param).
#  These are the requested value (i.e. what we want but which may not be
#  available in the current headings).
   common values_

#  Array for passing around at global level. Indexed by ($this,param).
#  These are the values actually used by the LabelMenus (i.e. these will
#  be set to "(not available)" if the requested value is not available in
#  the current list of column headings.
   common mvalues_

#  End of class definition.
}
