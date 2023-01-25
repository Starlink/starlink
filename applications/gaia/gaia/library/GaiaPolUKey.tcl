#+
#  Name:
#     GaiaPolUKey

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     A control panel for controlling and displaying parameters realted
#     to the vector key.

#  Description:
#     This class encapsulates the vector key properties of a
#     GaiaPolarimetry toolbox. It is a FrameWidget which contains controls
#     which allow the user to set these properties. The GaiaPolDisp class
#     uses these properties to display a suitable vector key.

#  Invocations:
#
#        GaiaPolUKey object_name [configuration options]
#
#     This creates an instance of a GaiaPolUKey object. The returned value
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
#     6-NOV-2000 (DSB):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaPolUKey {}

itcl::class gaia::GaiaPolUKey {

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

#  Increment the number of objects created.
      incr id_

#  Create a unique font name for key labels.
      set lfont_ "GaiaPolKeyFont$id_"

#  Create the font
      font create $lfont_

#  Set defaults.
      reset
   }

#  Destructor:
#  ============
   destructor {

#  Save the current options values to the options file, over-writing any
#  existing options file.
      if { $saveopt_ && [file isdirectory $itk_option(-optdir)] } {
         set optfile "$itk_option(-optdir)/GaiaPolUKey.opt"
         if { [catch {set fd [open $optfile w]} mess] } {
            puts "Error writing defaults to file '$optfile' for the polarimetry toolbox 'key' panel : $mess"
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

#  Delete the font
      if { $lfont_ != "" } { font delete $lfont_ }
   }

#  Public methods:
#  ===============
#  Command which is involked when a change is made to any GUI control.
#  -------------------------------------------------------------------
   public method activ { args } {

#  Ensure the values in the values_ array are up to date.
      set values_($this,lclr) [$itk_component(lclr) get]
      set values_($this,vclr) [$itk_component(vclr) get]
      set values_($this,bgclr) [$itk_component(bgclr) get]
      set values_($this,bdclr) [$itk_component(bdclr) get]

#  Get the name of the changed value.
      set item [lindex $args 0]

#  Use the command specified by the -actioncmd option to store a new
#  undoable action in the actions list.
      newAction $item

#  Implement the requested change.
      newVals
   }

#  Accessor methods:
#  -----------------
   public method getLFont {} {return $lfont_}

   public method setEnabled {enable} {
      if { $enable } {
         set values_($this,enable) 1
      } else {
         set values_($this,enable) 0
      }
      newVals
   }
   public method getEnabled {} {return $values_($this,enable)}

   public method setLColour {col} {set values_($this,lclr) $col; newVals}
   public method getLColour {} {return $values_($this,lclr)}

   public method setLFormat {fmt} {set values_($this,lfmt) $fmt; newVals}
   public method getLFormat {} {
      if { [catch { format $values_($this,lfmt) 0.0 }] } {
         error_dialog "Illegal format string \"$values_($this,lfmt)\" supplied for the key label. Reverting to default format \"%.1f\"."
         setLFormat "%.1f"
      }
      return $values_($this,lfmt)
   }

   public method setVColour {col} {set values_($this,vclr) $col; newVals}
   public method getVColour {} {return $values_($this,vclr)}

   public method setVValue {val} {set values_($this,vval) $val; newVals}
   public method getVValue {} {return $values_($this,vval)}

   public method setVWidth {wid} {set values_($this,vwid) $wid; newVals}
   public method getVWidth {} {return $values_($this,vwid)}

   public method setBgPad {pad} {set values_($this,pad) $pad; newVals}
   public method getBgPad {} {return $values_($this,pad)}

   public method setBgColour {clr} { set values_($this,bgclr) $clr; newVals}
   public method getBgColour {} {
      set ret $values_($this,bgclr)
      if { $ret == "(clear)" } { set ret "" }
      return $ret
   }

   public method setBdColour {clr} { set values_($this,bdclr) $clr; newVals}
   public method getBdColour {} {
      set ret $values_($this,bdclr)
      if { $ret == "(clear)" } { set ret "" }
      return $ret
   }

   public method setBdWidth {wid} {set values_($this,bdwid) $wid; newVals}
   public method getBdWidth {} {return $values_($this,bdwid)}

   public method setLFontFam {f} {set values_($this,ffam) $f; updateFont}
   public method setLFontSize {f} {set values_($this,fsize) $f; updateFont}
   public method setLFontBold {f} {set values_($this,fbold) $f; updateFont}

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
   }

#  Reset default values (either read from an options file or hard-wired).
#  ----------------------------------------------------------------------
   public method reset { {readopt 1} } {

#  Store control descriptions:
      set desc_(enable) "whether or not a key is displayed"
      set desc_(lclr) "the colour for the key label"
      set desc_(lfmt) "the format for the key label"
      set desc_(ffam) "the font family for the key label"
      set desc_(fsize) "the font size for the key label"
      set desc_(fbold) "the font weight for the key label"
      set desc_(vval) "the value of the key vector"
      set desc_(vclr) "the colour of the key vector"
      set desc_(vwid) "the line thickness of the key vector"
      set desc_(bgclr) "the colour for the key background"
      set desc_(pad) "the width of the margin around the key"
      set desc_(bdclr) "the colour for the key border"
      set desc_(bdwid) "the line thickness of the key border"

#  Store attribute names (i.e. the name for the corresponding get/set
#  methods)
      set attr_(enable) Enabled
      set attr_(lclr) LColour
      set attr_(lfmt) LFormat
      set attr_(ffam) LFontFam
      set attr_(fsize) LFontSize
      set attr_(fbold) LFontBold
      set attr_(vval) VValue
      set attr_(vclr) VColour
      set attr_(vwid) VWidth
      set attr_(bgclr) BgColour
      set attr_(pad) BgPad
      set attr_(bdclr) BdColour
      set attr_(bdwid) BdWidth

#  Set the hard-wired defaults.
      set values_($this,enable) 1
      set values_($this,lclr) "#fff"
      set values_($this,lfmt) "%.1f"
      set values_($this,ffam) "courier"
      set values_($this,fsize) 15
      set values_($this,fbold) 0
      set values_($this,vclr) "current"
      set values_($this,vwid) 2
      set values_($this,bgclr) "#000"
      set values_($this,pad) 0.5
      set values_($this,bdclr) "#fff"
      set values_($this,bdwid) 1

#  Over-write these with the values read from the options file created when
#  the last used instance of this class was destroyed.
      set optfile "$itk_option(-optdir)/GaiaPolUKey.opt"
      if { $readopt && [file readable $optfile] } {
         if { [catch {source $optfile} mess] } {
            puts "Error reading defaults from file '$optfile' for the polarimetry toolbox 'key' panel : $mess"
         } else {
            foreach elem [array names option] {
               set values_($this,$elem) "$option($elem)"
            }
         }
      }

#  Replace illegal blank values read from the options file with the hardwired
#  defaults.
      if { $values_($this,enable) == "" } { set values_($this,enable) 1 }
      if { $values_($this,lclr) == "" } { set values_($this,lclr) "#fff" }
      if { $values_($this,ffam) == "" } { set values_($this,ffam) "courier" }
      if { $values_($this,fsize) == "" } { set values_($this,fsize) 15 }
      if { $values_($this,fbold) == "" } { set values_($this,fbold) 0 }
      if { $values_($this,vclr) == "" } { set values_($this,vclr) "current" }
      if { $values_($this,vwid) == "" } { set values_($this,vwid) 2 }
      if { $values_($this,bgclr) == "" } { set values_($this,bgclr) "#000" }
      if { $values_($this,pad) == "" } { set values_($this,pad) 0.5 }
      if { $values_($this,bdclr) == "" } { set values_($this,bdclr) "#fff" }
      if { $values_($this,bdwid) == "" } { set values_($this,bdwid) 1 }

#  Hard-wired defaults are used for option values which depend on the
#  particular vector map being displayed.
      set values_($this,vval) ""

#  Ensure the font uses the values set above.
      updateFont


#  Save the original values as next times previous values.
      saveOld

   }

#  Called when the user makes a change to any font-related control.
#  ----------------------------------------------------------------
   public method newFont {args} {

#  Get the new font family from the menu and store in the values_ array.
      if { ![catch {set ffam [$itk_component(ffam) get]}] } {
         set values_($this,ffam)  $ffam
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
      if { $values_($this,fbold) } {
         set wgt "bold"
      } else {
         set wgt "normal"
      }

#  For some untracable reason, configuring the font used by an existing key
#  can cause a core dump. To avoid this the key is first removed, then the
#  font is configured, and then a new font is created. First disable the
#  key (if it is currently enabled).
      if { $values_($this,enable) } {
         set values_($this,enable) 0
         eval $itk_option(-changecmd)
         set reinstate 1
      } else {
         set reinstate 0
      }

#  Now configure the font.
      font configure $lfont_ -family $values_($this,ffam) \
                             -size $values_($this,fsize) \
		             -weight $wgt

#  Now re-enable the key if required.
      if { $reinstate } {
         set values_($this,enable) 1
         eval $itk_option(-changecmd)
      }

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
         set lwidth 12

#  Initialise the row index withi the geaometry grid
         set r -1

#  Create a LabelCheck to enable the key.
         itk_component add enable {
	    StarLabelCheck $w_.enable -text "Enable:" \
                              -onvalue 1 \
                              -offvalue 0 \
			      -labelwidth $lwidth \
                              -command [code $this activ enable] \
                              -anchor nw \
                              -variable [scope values_($this,enable)]
	 }
         grid $itk_component(enable) -row [incr r] -column 0 -sticky nw -padx $px
         add_short_help $itk_component(enable) {Controls whether the a vector key is displayed or not}

#  Label parameters...
         itk_component add header1 {
	    LabelRule $w_.header1 -text "Textual Label:"
	 }
         grid $itk_component(header1) -row [incr r] -column 0 -padx 1m \
                                      -columnspan $ncol -sticky nwe -pady 2m

#  Next row.
         incr r

#  Create a LabelMenu to control the colour of the label.
         itk_component add lclr {
	    util::LabelMenu $w_.lclr -text "Colour:" \
			       -labelwidth $lwidth \
 	    	               -variable [scope values_($this,lclr)]
	 }
         grid $itk_component(lclr) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(lclr) {Colour to use for the textual label in the key}

         foreach clr $colourmap_ {
            $itk_component(lclr) add \
               -label {  } \
               -command [code $this activ lclr] \
               -background $clr \
               -value $clr
         }

#  Create a LabelEntry to control the format string to use when formatting
#  the textual label.
         itk_component add lfmt {
	    util::LabelEntry $w_.lfmt -text "Format string:" \
	                        -labelwidth $lwidth \
                                -textvariable [scope values_($this,lfmt)] \
                                -valuewidth 20 \
                                -command [code $this activ lfmt] \
                                -anchor nw
	 }
         grid $itk_component(lfmt) -row $r -column 1 -columnspan 2 -sticky nw -padx $px
         add_short_help $itk_component(lfmt) {Tcl format string to use for the textual label in the key}

#  Vertical space.
         grid [frame $w_.space1b -height $vspace2] -row [incr r]

#  Next row
         incr r

#  Create a LabelMenu to control the font family.
         itk_component add ffam {
	    util::LabelMenu $w_.ffam -text "Font family:" \
			       -labelwidth $lwidth \
	                       -variable [scope values_($this,ffam)]
	 }
         grid $itk_component(ffam) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(ffam) {Font family to use for the textual label in the key}

         foreach fam [font families] {
            $itk_component(ffam) add \
               -label $fam \
               -command [code $this newFont ffam] \
               -value $fam
         }

#  Create a LabelEntry to set the font size.
         itk_component add fsize {
	    util::LabelEntry $w_.fsize -text "Font size:" \
                                 -valuewidth $vwidth \
                                 -command [code $this newFont fsize] \
     			         -labelwidth $lwidth \
                                 -anchor nw \
    			         -validate integer \
                                 -textvariable [scope values_($this,fsize)]
	 }
         grid $itk_component(fsize) -row $r -column 1 -sticky nw -padx $px
         add_short_help $itk_component(fsize) {Font size to use for the textual label in the key}

#  Create a LabelCheck to set bold font.
         itk_component add fbold {
	    StarLabelCheck $w_.fbold -text "Bold font:" \
                                     -onvalue 1 \
                                     -offvalue 0 \
      			             -labelwidth $lwidth \
                                     -command [code $this newFont fbold] \
                                     -anchor nw \
                                     -variable [scope values_($this,fbold)]
	 }
         grid $itk_component(fbold) -row $r -column 2 -sticky nw -padx $px
         add_short_help $itk_component(fbold) {Should the textual label in the key be bold?}

#  Vertical space.
         grid [frame $w_.space1 -height $vspace1] -row [incr r]

#  Vector parameters...
         itk_component add header2 {
	    LabelRule $w_.header2 -text "Key Vector:"
	 }
         grid $itk_component(header2) -row [incr r] -column 0 -padx 1m \
                                      -columnspan $ncol -sticky nwe -pady 2m

#  Next row.
         incr r

#  Create a LabelEntry to set the vector value.
         itk_component add vval {
	    util::LabelEntry $w_.vval -text "Value:" \
                                -valuewidth $vwidth \
                                -command [code $this activ vval] \
  			        -labelwidth $lwidth \
                                -anchor nw \
			        -validate real \
                                -textvariable [scope values_($this,vval)]
	 }
         grid $itk_component(vval) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(vval) {The length of the key vector, in data units (a default value is used if this is blank)}

#  Create a LabelMenu to control the colour of the vector.
         itk_component add vclr {
	    util::LabelMenu $w_.vclr -text "Colour:" \
			       -labelwidth $lwidth \
 	    	               -variable [scope values_($this,vclr)]
	 }
         grid $itk_component(vclr) -row $r -column 1 -sticky nw -padx $px
         add_short_help $itk_component(vclr) {Colour to use for the key vector}

         foreach clr $colourmap_ {
            $itk_component(vclr) add \
               -label {  } \
               -command [code $this activ vclr] \
               -background $clr \
               -value $clr
         }

         $itk_component(vclr) add \
               -value "current" \
               -command [code $this activ vclr] \
               -background "#fff" \
               -label "current"

#  Create a LabelEntry to control the width of the vector.
         itk_component add vwid {
            util::LabelEntry $w_.vwid -text "Thickness:" \
                                -valuewidth $vwidth \
                                -command [code $this activ vwid] \
                                -labelwidth $lwidth \
			        -validate integer \
                                -anchor nw \
                                -textvariable [scope values_($this,vwid)]
         }
         grid $itk_component(vwid) -row $r -column 2 -sticky nw -padx $px
         add_short_help $itk_component(vwid) {Line width for the key vector}

#  Vertical space.
         grid [frame $w_.space2 -height $vspace1] -row [incr r]

#  Background parameters...
         itk_component add header3 {
	    LabelRule $w_.header3 -text "Background:"
	 }
         grid $itk_component(header3) -row [incr r] -column 0 -padx 1m \
                                      -columnspan $ncol -sticky nwe -pady 2m

#  Next row.
         incr r

#  Create a LabelMenu to control the colour of the background.
         itk_component add bgclr {
	    util::LabelMenu $w_.bgclr -text "Colour:" \
			        -labelwidth $lwidth \
 	    	                -variable [scope values_($this,bgclr)]
	 }
         grid $itk_component(bgclr) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(bgclr) {Colour for the key background}

         foreach clr $colourmap_ {
            $itk_component(bgclr) add \
               -label {  } \
               -command [code $this activ bgclr] \
               -background $clr \
               -value $clr
         }
         $itk_component(bgclr) add \
               -label "clear" \
               -command [code $this activ bgclr] \
               -background "#fff" \
               -value "(clear)"

#  Create a LabelEntry to control the width of the margin.
         itk_component add pad {
            util::LabelEntry $w_.pad -text "Margin:" \
                               -valuewidth $vwidth \
                               -command [code $this activ pad] \
                               -labelwidth $lwidth \
			       -validate real \
                               -anchor nw \
                               -textvariable [scope values_($this,pad)]
         }
         grid $itk_component(pad) -row $r -column 1 -columnspan 2 -sticky nw -padx $px
         add_short_help $itk_component(pad) {The width of the margin around the key, in cm}

#  Vertical space.
         grid [frame $w_.space3 -height $vspace1] -row [incr r]

#  Border parameters...
         itk_component add header4 {
	    LabelRule $w_.header4 -text "Border:"
	 }
         grid $itk_component(header4) -row [incr r] -column 0 -padx 1m \
                                      -columnspan $ncol -sticky nwe -pady 2m

#  Next row.
         incr r

#  Create a LabelMenu to control the colour of the border.
         itk_component add bdclr {
	    util::LabelMenu $w_.bdclr -text "Colour:" \
			        -labelwidth $lwidth \
 	    	                -variable [scope values_($this,bdclr)]
	 }
         grid $itk_component(bdclr) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(bdclr) {Colour for the key border}

         foreach clr $colourmap_ {
            $itk_component(bdclr) add \
               -label {  } \
               -command [code $this activ bdclr] \
               -background $clr \
               -value $clr
         }
         $itk_component(bdclr) add \
               -label "clear" \
               -command [code $this activ bdclr] \
               -background "#fff" \
               -value "(clear)"

#  Create a LabelEntry to control the thickness of the border
         itk_component add bdwid {
            util::LabelEntry $w_.bdwid -text "Width:" \
                                 -valuewidth $vwidth \
                                 -command [code $this activ bdwid] \
                                 -labelwidth $lwidth \
			         -validate integer \
                                 -anchor nw \
                                 -textvariable [scope values_($this,bdwid)]
         }
         grid $itk_component(bdwid) -row $r -column 1 -columnspan 2 -sticky nw -padx $px
         add_short_help $itk_component(bdwid) {The line thickness for the key border}

#  Vertical space
         grid [frame $w_.space4 -height $vspace1] -row [incr r]

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

#  The colours in which vectors can be drawn.
      variable colourmap_ { "#fff" "#000" "#f00" "#0f0" "#00f" \
                            "#0ff" "#f0f" "#ff0" "#f80" "#8f0" \
                            "#0f8" "#08f" "#80f" "#f08" \
                            "#512751275127" "#a8b4a8b4a8b4"}

#  An array of descriptions (one for each control)
       variable desc_

#  An array of attribute names (one for each control)
       variable attr_

#  The name of the font used for the key label.
       variable lfont_ ""

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

#  The number of instances of this class created.
   common id_ 0

#  Array for passing around at global level. Indexed by ($this,param).
   common values_

#  End of class definition.
}
