#+
#  Name:
#     GaiaPolUSelOpt

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Encapsulates controls for controlling the way vector selections are
#     made.

#  Description:
#     This class provides GUI controls for the various options which
#     determine how vectors are selected.
#
#  Invocations:
#
#        GaiaPolUSelOpt object_name [configuration options]
#
#     This creates an instance of a GaiaPolUSelOpt object. The returned value
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
#     22-SEP-2000 (DSB):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaPolUSelOpt {}

itcl::class gaia::GaiaPolUSelOpt {

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
         set optfile "$itk_option(-optdir)/GaiaPolUSelOpt.opt"
         if { [catch {set fd [open $optfile w]} mess] } {
            puts "Error writing defaults to file '$optfile' for the polarimetry toolbox 'Selecting' panel : $mess"
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
      activb 1 [lindex $args 0]
   }

#  Command which is invoked when a change is made to any GUI control or
#  the Control-Shift-Return binding on the selection expression entry
#  fires. The reset argument is non-zero if the current selection is to be
#  cleared before selecting (or deselecting) the chosen vectors. Item is
#  the component name of the control which was changed.
#  -------------------------------------------------------------------
   public method activb {reset item} {

#  Ensure the values in the values_ array are up to date.
      set values_($this,select) [$itk_component(select) get]
      set values_($this,shape) [$itk_component(shape) get]

#  Use the command specified by the -actioncmd option to store a new
#  undoable action in the actions list.
      newAction $item

#  Implement the requested change.
      newVals $reset $item
   }

#  Reset default values (either read from an options file or hard-wired).
#  ----------------------------------------------------------------------
   public method reset { {readopt 1} } {

#  Store control descriptions:
      set desc_(sexp) "the algebraic selection expression"
      set desc_(select) "whether to select or de-select chosen vectors"
      set desc_(shape) "the shape used to choose vectors with the cursor"
      set desc_(freeze) "whether vectors can be selected by clicking and dragging"

#  Store attribute names (i.e. the name for the corresponding get/set
#  methods)
      set attr_(sexp) Sexp
      set attr_(select) Select
      set attr_(shape) Shape
      set attr_(freeze) Freeze

#  Set the hard-wired defaults.
      set values_($this,select) 1
      set values_($this,shape) "box"
      set values_($this,sexps) ""
      set values_($this,freeze) 0

#  Over-write these with the values read from the options file created when
#  the last used instance of this class was destroyed.
      set optfile "$itk_option(-optdir)/GaiaPolUSelOpt.opt"
      if { $readopt && [file readable $optfile] } {
         if { [catch {source $optfile} mess] } {
            puts "Error reading defaults from file '$optfile' for the polarimetry toolbox 'Selecting' panel : $mess"
         } else {
            foreach elem [array names option] {
               set values_($this,$elem) "$option($elem)"
            }
         }
      }

#  Replace illegal blank values read from the options file with the hardwired
#  defaults.
      if { $values_($this,select) == "" } { set values_($this,select) 1 }
      if { $values_($this,shape) == "" } { set values_($this,shape) "box" }
      if { $values_($this,freeze) == "" } { set values_($this,freeze) 0 }

#  Set hard-wired defaults for things which are data dependant.
      set values_($this,sexp) ""

#  Save the original values as next times previous values.
      saveOld
   }

#  Ensure the menu holding recently used selection expressions reflects
#  the expressions stored in values_($this,sexps).
#  ---------------------------------------------------------------------
   public method sexpMenu {} {
      set exp0 $values_($this,sexp)
      $itk_component(sexp) clear
      foreach exp $values_($this,sexps) {
         $itk_component(sexp) add \
                  -label $exp \
                  -value $exp \
                  -command "[code $this activ sexp]"
      }
      set values_($this,sexp) $exp0
   }

#  Accessor methods:
#  -----------------
   public method setSelect {s} {
      if { $s } {
         set values_($this,select) 1
      } else {
         set values_($this,select) 0
      }
      newVals 0 select
   }
   public method getSelect {} {return $values_($this,select)}

   public method setShape {s} {
      if { $s == "box" || $s == "circle"} {
         set values_($this,shape) $s
         newVals 0 shape
      } else {
         error_dialog "setShape(GaiaPolUSelOpt): Illegal cursor selection shape \"$s\" supplied."
      }
   }
   public method getShape {} {return $values_($this,shape)}

   public method getSexp {} {return $values_($this,sexp)}

   public method setSaveOpt {x} {set saveopt_ $x}

   public method setFreeze {x} {set values_($this,freeze) $x}
   public method getFreeze {} {return $values_($this,freeze)}

#  Called to add a new action to the current list of undoable actions.
#  Note, a new selection expression does not generate a new undoable action
#  since the undoing of the new selection will be handled within
#  GaiaPolarimetry.
#  ------------------------------------------------------------------------
   public method newAction {item} {
      if { "$item" != "sexp" && "$itk_option(-actioncmd)" != "" } {
         set arglist "object \{change $desc_($item)\} $this \{set$attr_($item) \"$oldvals_($item)\"\} \{set$attr_($item) \"$values_($this,$item)\"\}"
         eval $itk_option(-actioncmd) $arglist
      }
   }

#  Called to save the current control values as next times previous
#  values, and to implement the new settings by calling the change command.
#  ------------------------------------------------------------------------
   public method newVals {reset item} {
      saveOld
      if { "$itk_option(-changecmd)" != "" } {
         eval $itk_option(-changecmd) $reset $item
      }
   }

#  Store a new selection expression...
#  -----------------------------------
   public method setSexp {s isgood} {

#  Strip leading and trailing spaces from the expression
      set tidy [string trim $s]

#  If the expression is good, and it is not blank, and if it is not already
#  in the associated menu, we now add the tidied expression to the menu.
      if { $tidy != "" && $isgood != 0 } {
         if { [lsearch -exact $values_($this,sexps) $tidy] == -1 } {

#  The values to be stored in the menu are kept in element "sexps"
#  of values_. Add the new expression into this list at the start,
#  removing the last entry if there are more than 9.
            if { ![info exists values_($this,sexps)] } {
               set nold 0
            } else {
               set nold [llength $values_($this,sexps)]
            }

            if { $nold == 0 } {
               set values_($this,sexps) [list $tidy]
            } else {
               if { $nold > 9 } {
                  set old [lrange $values_($this,sexps) 0 8]
               } else {
                  set old [lrange $values_($this,sexps) 0 end]
               }
               set values_($this,sexps) "[list $tidy] $old"
            }

#  Put these values into the menu.
            sexpMenu
         }
      }

#  Activate the appropriate menu item.
      if { $created_ } {
         $itk_component(sexp) configure -value $tidy
      }

#  Ensure the correct tidied expression is stored in the entry field
      set values_($this,sexp) $tidy

#  Save the current values as next times previous values.
      saveOld
   }

#  Choose all vectors, or invert the selection.
#  ---------------------------------------------
   public method All {deselect} {
      if { "$itk_option(-changecmd)" != "" } {
         if { $deselect == "invert" } {
            eval $itk_option(-changecmd)  0 invert
         } else {
            eval $itk_option(-changecmd)  $deselect all
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
         set ncol 3

#  Horizontal padding for columns.
         set px 2m

#  Vertical space between sections
         set vspace1 3m

#  Vertical space rows within a section
         set vspace2 2m

#  Value width (in characters).
         set vwidth 6

#  Label widths...
         set lwidth 14

#  Initialise the row index withi the geaometry grid
         set r -1

#  Label parameters...
         itk_component add header1 {
	    gaia::LabelRule $w_.header1 -text "Vector Selection Options:"
	 }
         grid $itk_component(header1) -row [incr r] -column 0 -padx 1m \
                                      -columnspan $ncol -sticky nwe -pady 2m

#  Next row.
         incr r

#  Create a LabelEntryMenu to give the selection expression.
         itk_component add sexp {
            ::gaia::LabelEntryMenu $w_.sexp -text "Expression:" \
                              -textvariable [scope values_($this,sexp)] \
                              -labelwidth $lwidth \
                              -indicatoron 1 \
                              -valuewidth 50 \
                              -command [code $this activ sexp] \
                              -anchor nw
         }
         grid $itk_component(sexp) -row $r -column 0 -columnspan $ncol -sticky nw -padx $px
         add_short_help $itk_component(sexp) {Choose vectors using an algebraic expression (e.g. ' $P < 10 && $DP < 1.5 ')}

#  Set up a binding which triggers if return is pressed in the entry
#  while the shift and control keys are pressed. This will prevent the
#  existing selection being cleared before including the new selection.
         bind [$itk_component(sexp) component entry] <Control-Shift-Return> \
                                                     [code $this activb 0 sexp]

#  Vertical space.
         grid [frame $w_.space1 -height $vspace2] -row [incr r]

#  Next row
         incr r

#  Create a LabelMenu to control whether chosen vectors are selected or
#  deselected.
         itk_component add select {
            util::LabelMenu $w_.select -text "Operation:" \
                                 -variable [scope values_($this,select)] \
                                 -labelwidth $lwidth
         }
         grid $itk_component(select) -row $r -columnspan $ncol -column 0 -sticky nw -padx $px
         add_short_help $itk_component(select) {Should the vectors you choose be selected or deselected?}
         $itk_component(select) add -label "Select" -value 1 \
                                    -command "[code $this activ select]"
         $itk_component(select) add -label "De-select" -value 0 \
                                    -command "[code $this activ select]"

#  Vertical space.
         grid [frame $w_.space2 -height $vspace2] -row [incr r]

#  Next row
         incr r

#  Create a LabelMenu to control whether cursor selection uses a
#  rectangle or a circle.
         itk_component add shape {
            util::LabelMenu $w_.shape -text "Region shape:" \
                              -variable [scope values_($this,shape)] \
                              -labelwidth $lwidth
         }
         grid $itk_component(shape) -row $r -column 0 -columnspan $ncol -sticky nw -padx $px
         add_short_help $itk_component(shape) {The shape to use when choosing vectors by dragging the cursor over the map}
         $itk_component(shape) add -label "Rectangle" -value box \
                                    -command "[code $this activ shape]"
         $itk_component(shape) add -label "Circle" -value circle \
                                    -command "[code $this activ shape]"

#  Vertical space.
         grid [frame $w_.space2b -height $vspace2] -row [incr r]

#  Next row
         incr r

#  Create a LabelCheck to disable clicking and dragging as a means of
#  selecting vectors.
         itk_component add freeze {
            gaia::StarLabelCheck $w_.freeze -text "Disable mouse:" \
                                     -onvalue 1 \
                                     -offvalue 0 \
                                     -labelwidth $lwidth \
                                     -command [code $this activ freeze] \
                                     -anchor nw \
                                     -variable [scope values_($this,freeze)]
         }
         grid $itk_component(freeze) -row $r -column 0 -columnspan $ncol -sticky nw -padx $px
         add_short_help $itk_component(freeze) {Click to prevent vectors being selected or deselected by clicking and dragging over the vector map}

#  Vertical space.
         grid [frame $w_.space3 -height $vspace2] -row [incr r]

#  Next row
         incr r

#  Create a LabelButton which causes all vectors to be selected when
#  pressed.
         itk_component add sall {
            ::gaia::LabelButton $w_.sall -text "Select all:" \
                                 -command [code $this All 0] \
   			         -labelwidth $lwidth \
                                 -anchor nw \
                                 -buttonbitmap finger
	 }
         grid $itk_component(sall) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(sall) {Click to select all vectors in the catalogue}

#  Create a LabelButton which causes all vectors to be selected when
#  pressed.
         itk_component add dall {
            ::gaia::LabelButton $w_.dall -text "De-select all:" \
                                 -command [code $this All 1] \
   			         -labelwidth $lwidth \
                                 -anchor nw \
                                 -buttonbitmap finger
	 }
         grid $itk_component(dall) -row $r -column 1 -sticky nw -padx $px
         add_short_help $itk_component(dall) {Click to de-select all vectors in the catalogue}

#  Create a LabelButton which causes the current selection to be inverted.
         itk_component add invert {
            ::gaia::LabelButton $w_.invert -text "Invert selection:" \
                                 -command [code $this All invert] \
   			         -labelwidth $lwidth \
                                 -anchor nw \
                                 -buttonbitmap finger
	 }
         grid $itk_component(invert) -row $r -column 2 -sticky nw -padx $px
         add_short_help $itk_component(invert) {Click to swap the currently selected and unselected vectors}

#  Vertical space
         grid [frame $w_.space6 -height $vspace1] -row [incr r]

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

#  Ensure the menu holding recently used selection expressions reflects
#  the expressions stored in values_($this,sexps).
         sexpMenu

#  Activate the correct menu item.
         $itk_component(sexp) configure -value "$values_($this,sexp)"
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

#  Currently selected selection expression in menu button.
   common cursexp_

#  End of class definition.
}
