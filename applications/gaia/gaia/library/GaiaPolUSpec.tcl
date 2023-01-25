#+
#  Name:
#     GaiaPolUSpec

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     A control panel for controlling the display of spectral channel
#     information.

#  Description:
#     This class

#  Invocations:
#
#        GaiaPolUSpec object_name [configuration options]
#
#     This creates an instance of a GaiaPolUSpec object. The returned value
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
#     Copyright (C) 2001-2005 Central Laboratory of the Research Councils.
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
#     2-MAR-2001 (DSB):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaPolUSpec {}

itcl::class gaia::GaiaPolUSpec {

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

#  Set defaults.
      reset

   }

#  Destructor:
#  ============
   destructor {

#  Annul any reference to the displayed GaiaPolCat.
      if { $cat_ != "" } {
         catch {set cat_ [$cat_ annull]}
      }

#  Save the current options values to the options file, over-writing any
#  existing options file.
      if { $saveopt_ && [file isdirectory $itk_option(-optdir)] } {
         set optfile "$itk_option(-optdir)/GaiaPolUSpec.opt"
         if { [catch {set fd [open $optfile w]} mess] } {
            puts "Error writing defaults to file '$optfile' for the polarimetry toolbox 'SpecPol' panel : $mess"
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

#  Called when a new catalogue or selection is displayed.
#  ------------------------------------------------------
   public method newCat { cat } {

#  Ensure the controls have been created.
      create

#  Save a reference to the supplied GaiaPolCat, annulling any
#  reference to a previous GaiaPolCat first.
      if { $cat_ != "" } {
         $cat_ annull
      }
      set cat_ [$cat clone]

#  Clear and disable the spectral channel controls if no spectral channel
#  is available.
      if { [$cat getCol Z] == -1 } {
         newZval "" ""
         $itk_component(zc).unit configure -text ""
         $itk_component(za).unit configure -text ""
         $itk_component(zc).val configure -state disabled
         $itk_component(za).val configure -state disabled

#  Otherwise, enable the spectral channel controls and change their
#  labels to indicate the units in which the channel is to be specified.
      } else {
         $itk_component(zc).val configure -state normal
         $itk_component(za).val configure -state normal

         set unit [$cat getZcunit]
         $itk_component(zc).unit configure -text $unit

         set unit [$cat getZaunit]
         $itk_component(za).unit configure -text $unit

#  If the catalogue already has set Z values, use them.
         set z [$cat getZvals]
         if { [llength $z] == 2 } {
            setZvals $z

#  Otherwise, attempt to find the Z column value corresponding to the
#  current Z axis control value, and use this as the new Z column value.
#  Use the lowest Z column value if this is not possible.
         } else {
            set zax $values_($this,zaval)
            if { $zax != "" && $zax != "***" } {
               lassign [$cat zConv $zax "zaval" ] zcnew zanew
               if { $zcnew == "" || $zcnew == "***" } {
                  set zcnew [$cat getZlo]
               }
            } else {
               set zcnew [$cat getZlo]
            }

#  Store this new Z column value.
            newZval $zcnew "zcval"
         }

#  If the catalogue has only a single spectral channel, disable the
#  controls.
         if { [$cat getZlo] == [$cat getZhi] } {
            $itk_component(zc).val configure -state disabled
            $itk_component(za).val configure -state disabled
         }

#  Ensure the selected Z values are used by the catalogue.
         $cat setZvals [list $values_($this,zcval) $values_($this,zaval)]
      }

#  Save the current control values as next times previous values.
      saveOld

   }

#  Assign a new Z value (column or axis)
#  -------------------------------------
   public method newZval {z item} {

      if { $cat_ == "" || $z == "" || $z == "***" } {
         set values_($this,zcval) ""
         set values_($this,zaval) ""

      } else {

         if { $item == "zcval" } {
            set otheritem "zaval"
         } else {
            set otheritem "zcval"
         }

         set zzz [$cat_ zConv $z $item]
         lassign $zzz values_($this,zcval) values_($this,zaval)

      }
   }

#  Command which is invoked when a change is made to any GUI control.
#  -------------------------------------------------------------------
   public method activ { args } {

#  Ensure the values in the values_ array are up to date.
#     (does not use any LabelMenus as yet)

#  Get the name of the changed value.
      set item [lindex $args 0]

#  If the Z column or Z axis value has been changed, find usable values
#  for both.
      if { $item == "zcval" || $item == "zaval" } {
         newZval $values_($this,$item) $item
      }

#  Save the previous Z column value.
      set oldval $oldvals_(zcval)

#  Implement the requested change.
      newVals

#  Issue a warning if nothing has changed.
      if { $values_($this,zcval) == $oldval } {
         info_dialog "The nearest available spectral channel is already displayed."
      }
   }

#  Reset default values (either read from an options file or hard-wired).
#  ----------------------------------------------------------------------
   public method reset { {readopt 1} } {

#  Store control descriptions:
      set desc_(zcval) "displayed spectral channel"

#  Store attribute names (i.e. the name for the corresponding get/set
#  methods)
      set attr_(zcval) Zvals
      set attr_(zaval) Zvals

#  Set the hard-wired defaults.
      set values_($this,zcval) ""
      set values_($this,zaval) ""

#  Over-write these with the values read from the options file created when
#  the last used instance of this class was destroyed.
      set optfile "$itk_option(-optdir)/GaiaPolUSpec.opt"
      if { $readopt && [file readable $optfile] } {
         if { [catch {source $optfile} mess] } {
            puts "Error reading defaults from file '$optfile' for the polarimetry toolbox 'SpecPol' panel : $mess"
         } else {
            foreach elem [array names option] {
               set values_($this,$elem) "$option($elem)"
            }
         }
      }

#  Replace illegal blank values read from the options file with the hardwired
#  defaults.
#     (none at the moment)

#  Re-instate any hard-wired defaults.
      set values_($this,zcval) ""
      set values_($this,zaval) ""

#  Save the current control values as next times previous values.
      saveOld

   }

#  Return the current Z values.
#  ----------------------------
   public method getZvals {} {
      return [list $values_($this,zcval) $values_($this,zaval)]
   }

#  Set new Z values (it is assumed that a usable pair of Z column and
#  axis values is supplied).
#  ------------------------------------------------------------------
   public method setZvals {z} {

#  Ensure the controls are created.
      create

#  If a list with 2 elements has been supplied, store them as the new Z
#  values, and enable the controls.
      if { [llength $z] == 2 } {
         set values_($this,zcval) [lindex $z 0]
         set values_($this,zaval) [lindex $z 1]
         $itk_component(zc).val configure -state normal
         $itk_component(za).val configure -state normal

#  Otherwise, disable the controls.
      } else {
         $itk_component(zc).val configure -state disabled
         $itk_component(za).val configure -state disabled
      }
   }

   public method setSaveOpt {x} {set saveopt_ $x}

#  Called to save the current control values as next times previous
#  values, and to implement the new settings by calling the change command.
#  ------------------------------------------------------------------------
   public method newVals {} {
      saveOld
      if { "$itk_option(-changecmd)" != "" } {
         eval $itk_option(-changecmd)
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
         set ncol 4

#  Horizontal padding for columns.
         set px 2m

#  Vertical space between sections
         set vspace1 3m

#  Vertical space rows within a section
         set vspace2 2m

#  Value width (in characters).
         set vwidth 10

#  Label widths...
         set lwidth 12

#  Initialise the row index withi the geaometry grid
         set r -1

#  Spectral channel header
         itk_component add header1 {
            gaia::LabelRule $w_.header1 -text "Spectral Channel:"
         }
         grid $itk_component(header1) -row [incr r] -column 0 -padx 1m \
                                      -columnspan $ncol -sticky nwe -pady 2m

#  Next row.
         incr r

#  Create a frame containing a LabelEntry to set the spectral channel as a Z
#  column value, and a label to indicate the required units.
         itk_component add zc {
            frame $w_.zc
         }
         util::LabelEntry $itk_component(zc).val -text "Z column value:" \
                                           -valuewidth $vwidth \
                                           -command [code $this activ zcval] \
                                           -labelwidth $lwidth \
                                           -validate real \
                                           -anchor nw \
                                           -state disabled \
                                           -textvariable [scope values_($this,zcval)]
         label $itk_component(zc).unit -text "" \
                                       -anchor w \
                                       -justify left
         pack $itk_component(zc).val $itk_component(zc).unit -side left

         grid $itk_component(zc) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(zc) {Z column value defining the spectral channel to display (only enabled for spectropolarimetry data).}

#  Create a frame containing a LabelEntry to set the spectral channel as a Z
#  axis value, and a label to indicate the required units.
         itk_component add za {
            frame $w_.za
         }
         util::LabelEntry $itk_component(za).val -text "Z axis value:" \
                                           -valuewidth $vwidth \
                                           -command [code $this activ zaval] \
                                           -labelwidth $lwidth \
                                           -validate real \
                                           -anchor nw \
                                           -state disabled \
                                           -textvariable [scope values_($this,zaval)]
         label $itk_component(za).unit -text "" \
                                       -anchor w \
                                       -justify left

         pack $itk_component(za).val $itk_component(za).unit -side left

         grid $itk_component(za) -row $r -column 2 -sticky nw -padx $px
         add_short_help $itk_component(za) {Z axis value defining the spectral channel to display (only enabled for spectropolarimetry data).}

#  Vertical space
         grid [frame $w_.space1 -height $vspace1] -row [incr r]

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

#  Create a new font for the units labels.
         set oldfont [[$itk_component(za).val component label] cget -font]
         eval font create polfont [font actual $oldfont]
         set newsize [expr [font configure polfont -size] - 2]
         font configure polfont -size $newsize -weight normal
         $itk_component(zc).unit configure -font polfont
         $itk_component(za).unit configure -font polfont

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

#  Have the control widgets been created yet?
      variable created_ 0

#  An array of descriptions
       variable desc_

#  An array of attribute names
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

#  End of class definition.
}
