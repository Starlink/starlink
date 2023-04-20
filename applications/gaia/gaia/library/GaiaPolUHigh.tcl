#+
#  Name:
#     GaiaPolUHigh

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     A control panel for controlling and displaying vector highlighting
#     parameters.

#  Description:
#     This class encapsulates the vector highlighting properties of a
#     GaiaPolarimetry toolbox. It is a FrameWidget which contains controls
#     which allow the user to set these properties. The GaiaPolDisp class
#     sets up canvas bindings which cause each vector to be highlighted as
#     the mouse pointer passes over it.

#
#  Invocations:
#
#        GaiaPolUHigh object_name [configuration options]
#
#     This creates an instance of a GaiaPolUHigh object. The returned value
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
#     22-OCT-2000 (DSB):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaPolUHigh {}

itcl::class gaia::GaiaPolUHigh {

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

#  Increment the number of GaiaPolUHigh objects created.
      incr id_

#  Create a unique font name for highlight labels.
      set font_ "GaiaPolHighFont$id_"

#  Create the font
      font create $font_

#  Set defaults
      reset
   }

#  Destructor:
#  ============
   destructor {

#  Save the current options values to the options file, over-writing any
#  existing options file.
      if { $saveopt_ && [file isdirectory $itk_option(-optdir)] } {
         set optfile "$itk_option(-optdir)/GaiaPolUHigh.opt"
         if { [catch {set fd [open $optfile w]} mess] } {
            puts "Error writing defaults to file '$optfile' for the polarimetry toolbox 'Highlighting' panel : $mess"
         } else {
            foreach name [array names values_] {
               puts $fd "set option($name) \{$values_($name)\}"
               unset values_($name)
            }
            close $fd
         }
      }

#  Delete the font
      if { $font_ != "" } { font delete $font_ }
   }

#  Public methods:
#  ===============

#  Command which is involked when a change is made to any GUI control.
#  -------------------------------------------------------------------
   public method activ { args } {

#  Ensure the values in the values_ array are up to date.
      set values_(clr) [$itk_component(clr) get]

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
      set desc_(enable) "whether or not vectors are highlighted"
      set desc_(clr) "the colour for the highlighted vector"
      set desc_(fmt) "the format for the highlighted vector value"
      set desc_(ffam) "the font family for the highlighted vector label"
      set desc_(fsize) "the font size for the highlighted vector label"
      set desc_(fbold) "the font weight for the highlighted vector label"

#  Store attribute names (i.e. the name for the corresponding get/set
#  methods)
      set attr_(enable) Enabled
      set attr_(clr) Colour
      set attr_(fmt) Format
      set attr_(ffam) FontFam
      set attr_(fsize) FontSize
      set attr_(fbold) FontBold

#  Set the hard-wired defaults.
      set values_(enable) 1
      set values_(clr) "#0f0"
      set values_(fmt) "%.1f"
      set values_(ffam) "courier"
      set values_(fsize) 20
      set values_(fbold) 0

#  Over-write these with the values read from the options file created when
#  the last used instance of this class was destroyed.
      set optfile "$itk_option(-optdir)/GaiaPolUHigh.opt"
      if { $readopt && [file readable $optfile] } {
         if { [catch {source $optfile} mess] } {
            puts "Error reading defaults from file '$optfile' for the polarimetry toolbox 'Highlighting' panel : $mess"
         } else {
            foreach elem [array names option] {
               set values_($elem) "$option($elem)"
            }
         }
      }

#  Replace illegal blank values read from the options file with the hardwired
#  defaults.
      if { $values_(enable) == "" } { set values_(enable) 1 }
      if { $values_(clr) == "" } { set values_(clr) "#0f0" }
      if { $values_(ffam) == "" } { set values_(ffam) "courier" }
      if { $values_(fsize) == "" } { set values_(fsize) 20 }
      if { $values_(fbold) == "" } { set values_(fbold) 0 }

#  Ensure the font uses the values set above.
      updateFont

#  Save the original values as next times previous values.
      saveOld

   }

#  Accessor methods:
#  -----------------
   public method setEnabled {enable} {
      if { $enable } {
         set values_(enable) 1
      } else {
         set values_(enable) 0
      }
      newVals
   }
   public method getEnabled {} {return $values_(enable)}

   public method setColour {col} {
      set values_(clr) $col
      newVals
   }
   public method getColour {} {return $values_(clr)}

   public method setFormat {fmt} {
      set values_(fmt) $fmt
      newVals
   }
   public method getFormat {} {
      if { [catch { format $values_(fmt) 0.0 }] } {
         error_dialog "Illegal format string \"$values_(fmt)\" supplied for highlight labels. Reverting to default format \"%.1f\"."
         setFormat "%.1f"
      }
      return $values_(fmt)
   }

   public method getFont {} {return $font_}

   public method setFontFam {f} {
      set values_(ffam) $f
      updateFont
   }

   public method setFontSize {f} {
      set values_(fsize) $f
      updateFont
   }

   public method setFontBold {f} {
      set values_(fbold) $f
      updateFont
   }

   public method setSaveOpt {x} {set saveopt_ $x}

#  Called to add a new action to the current list of undoable actions.
#  ------------------------------------------------------------------------
   public method newAction {item} {
      if { "$itk_option(-actioncmd)" != "" } {
         set arglist "object \{change $desc_($item)\} $this \{set$attr_($item) \"$oldvals_($item)\"\} \{set$attr_($item) \"$values_($item)\"\}"
         eval {*}$itk_option(-actioncmd) $arglist
      }
   }

#  Called to save the current control values as next times previous
#  values, and to implement the new settings by calling the change command.
#  ------------------------------------------------------------------------
   public method newVals {} {
      saveOld
      if { "$itk_option(-changecmd)" != "" } {
         eval {*}$itk_option(-changecmd)
      }
   }

#  Called when the user makes a change to any font-related control.
#  ----------------------------------------------------------------
   public method newFont {args} {

#  Get the new font family from the menu and store in the values_ array.
      if { ![catch {set ffam [$itk_component(ffam) get]}] } {
         set values_(ffam)  $ffam
      }

#  Add an action to the list of undoable actions.
      newAction [lindex $args 0]

#  Ensure the font used for the highlight labels has the requested attributes.
      updateFont

   }

#  Set the font attributes so that they match the current settings of the
#  font-related controls, and save the current values as next times
#  previous values.
#  ----------------------------------------------------------------------
   public method updateFont {} {
      if { $values_(fbold) } {
         set wgt "bold"
      } else {
         set wgt "normal"
      }
      font configure $font_ -family $values_(ffam) \
                                     -size $values_(fsize) \
				     -weight $wgt
      saveOld
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

#  Header...
         itk_component add header1 {
	    gaia::LabelRule $w_.header1 -text "Vector Highlighting:"
	 }
         grid $itk_component(header1) -row [incr r] -column 0 -padx 1m \
                                      -columnspan $ncol -sticky nwe -pady 2m

#  Next row.
         incr r

#  Create a LabelCheck to enable the highlighting of vectors.
         itk_component add enable {
	    gaia::StarLabelCheck $w_.enable -text "Enable:" \
                              -onvalue 1 \
                              -offvalue 0 \
			      -labelwidth $lwidth \
                              -command [code $this activ enable] \
                              -anchor nw \
                              -variable [scope values_(enable)]
	 }
         grid $itk_component(enable) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(enable) {Controls whether the vector under the mouse pointer is highlighted or not}

#  Vertical space.
         grid [frame $w_.space1 -height $vspace2] -row [incr r]

#  Next row.
         incr r

#  Create a LabelMenu to control the colour of the highlighted vector.
         itk_component add clr {
	    util::LabelMenu $w_.clr -text "Colour:" \
			      -labelwidth $lwidth \
	    	              -variable [scope values_(clr)]
	 }
         grid $itk_component(clr) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(clr) {Colour to use when highlighting the vector under the pointer}

         foreach clr $colourmap_ {
            $itk_component(clr) add \
               -label {  } \
               -command [code $this activ clr] \
               -background $clr \
               -value $clr
         }

#  Vertical space
         grid [frame $w_.space2 -height $vspace2] -row [incr r]

#  Next row.
         incr r

#  Create a LabelEntry to control the format string to use when formatting
#  the vector length to create the highlight label.
         itk_component add fmt {
	    util::LabelEntry $w_.fmt -text "Format string:" \
	                       -labelwidth $lwidth \
                               -textvariable [scope values_(fmt)] \
                               -valuewidth 20 \
                               -command [code $this activ fmt] \
                               -anchor nw
	 }
         grid $itk_component(fmt) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(fmt) {Tcl format string to use when labelling the vector under the mouse pointer}

#  Vertical space
         grid [frame $w_.space3 -height $vspace2] -row [incr r]

#  Next row.
         incr r

#  Create a LabelMenu to control the font family.
         itk_component add ffam {
	    util::LabelMenu $w_.ffam -text "Font family:" \
			       -labelwidth $lwidth \
	                       -variable [scope values_(ffam)]
	 }
         grid $itk_component(ffam) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(ffam) {Font family to use when labelling the vector under the pointer}

         foreach fam [font families] {
            $itk_component(ffam) add \
               -label "$fam" \
               -command [code $this newFont ffam] \
               -value "$fam"
         }

#  Vertical space
         grid [frame $w_.space4 -height $vspace2] -row [incr r]

#  Next row.
         incr r

#  Create a LabelEntry to set the font size.
         itk_component add fsize {
	    util::LabelEntry $w_.fsize -text "Font size:" \
                              -valuewidth $vwidth \
                              -command [code $this newFont fsize] \
			      -labelwidth $lwidth \
                              -anchor nw \
			      -validate integer \
                              -textvariable [scope values_(fsize)]
	 }
         grid $itk_component(fsize) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(fsize) {Font size to use when labelling the vector under the pointer}

#  Vertical space
         grid [frame $w_.space5 -height $vspace2] -row [incr r]

#  Next row.
         incr r

#  Create a LabelCheck to set bold font.
         itk_component add fbold {
	    gaia::StarLabelCheck $w_.fbold -text "Bold font:" \
                              -onvalue 1 \
                              -offvalue 0 \
			      -labelwidth $lwidth \
                              -command [code $this newFont fbold] \
                              -anchor nw \
                              -variable [scope values_(fbold)]
	 }
         grid $itk_component(fbold) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(fbold) {Should highlight labels be bold?}

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
      }
   }

#  Protected methods:
#  ==================

#  Save the current control settings in oldvals_
#  ---------------------------------------------
   protected method saveOld {} {
      foreach name [array names values_] {
         set oldvals_($name) $values_($name)
      }
   }

#  Private methods:
#  ==================

#  Options:
#  ========

#  A command to call when any control values are changed by the user.
   itk_option define -changecmd changecmd Changecmd {}

#  A command to call to add an undoable action to the current list of
#  undoable actions.
   itk_option define -actioncmd actioncmd Actioncmd {}

#  The name of a directory in which to store tcl code which will recreate
#  the current GUI settings. This text is created when this object is
#  destroyed.
   itk_option define -optdir optdir Optdir {}

#  Protected data members:
#  =======================
   protected {

#  Have the control widgets been created yet?
      variable created_ 0

#  The colours in which vectors can be drawn.
      variable colourmap_ { "#fff" "#000" "#f00" "#0f0" "#00f" \
                            "#0ff" "#f0f" "#ff0" "#f80" "#8f0" \
                            "#0f8" "#08f" "#80f" "#f08" \
                            "#512751275127" "#a8b4a8b4a8b4"}

#  An array of descriptions (one for each control)
       variable desc_

#  An array of attribute names (one for each control)
       variable attr_

#  The name of the font used for highlight labels.
      variable font_ ""

#  An array of the previous control values.
       variable oldvals_

#  Should current settings be saved when this object is destroyed?
       variable saveopt_ 1

#  Array indexed by (param).
       variable values_
   }

#  Private data members:
#  =====================
#  (none)

#  Common (i.e. static) data members:
#  ==================================

#  The number fo GaiaPolUHigh objects created so far.
   common id_ 0


#  End of class definition.
}
