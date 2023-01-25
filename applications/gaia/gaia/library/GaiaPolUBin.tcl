#+
#  Name:
#     GaiaPolUBin

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Encapsulates controls for controlling the binning of vectors.

#  Description:
#     This class provides GUI controls for the various options which
#     determine how vectors are binned.
#
#  Invocations:
#
#        GaiaPolUBin object_name [configuration options]
#
#     This creates an instance of a GaiaPolUBin object. The returned value
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
#     11-DEC-2000 (DSB):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaPolUBin {}

itcl::class gaia::GaiaPolUBin {

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
         set optfile "$itk_option(-optdir)/GaiaPolUBin.opt"
         if { [catch {set fd [open $optfile w]} mess] } {
            puts "Error writing defaults to file '$optfile' for the polarimetry toolbox 'Binning' panel : $mess"
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

#  Ensure the menu values in the values_ array are up to date.
      set values_($this,method) [$itk_component(method) get]

#  Use the command specified by the -actioncmd option to store a new
#  undoable action in the actions list.
      newAction [lindex $args 0]

#  Save the current settings as last times settings.
      saveOld

   }

#  Command which is invoked to bin the data.
#  -------------------------------------------------------------------
   public method bin {} {
      if { "$itk_option(-changecmd)" != "" } {
         eval $itk_option(-changecmd)
      }
   }

#  Reset default values (either read from an options file or hard-wired).
#  ----------------------------------------------------------------------
   public method reset { {readopt 1} } {

#  Store control descriptions:
      set desc_(box) "the bin size"
      set desc_(method) "the binning method"
      set desc_(debias) "whether to debias the binned polarization values"
      set desc_(minval) "the minimum number of values required in a bin"
      set desc_(sigmas) "the number of standard deviations for sigma-clipping"

#  Store attribute names (i.e. the name for the corresponding get/set
#  methods)
      set attr_(box) Box
      set attr_(method) Method
      set attr_(debias) Debias
      set attr_(minval) MinVal
      set attr_(sigmas) Sigmas

#  Set the hard-wired defaults.
      set values_($this,box) 5
      set values_($this,method) "median"
      set values_($this,debias) 1
      set values_($this,minval) 1
      set values_($this,sigmas) 4

#  Over-write these with the values read from the options file created when
#  the last used instance of this class was destroyed.
      set optfile "$itk_option(-optdir)/GaiaPolUBin.opt"
      if { $readopt && [file readable $optfile] } {
         if { [catch {source $optfile} mess] } {
            puts "Error reading defaults from file '$optfile' for the polarimetry toolbox 'Binning' panel : $mess"
         } else {
            foreach elem [array names option] {
               set values_($this,$elem) "$option($elem)"
            }
         }
      }

#  Replace illegal blank values read from the options file with the hardwired
#  defaults.
      if { $values_($this,box) == "" } { set values_($this,box) 5 }
      if { $values_($this,method) == "" } { set values_($this,method) "median" }
      if { $values_($this,debias) == "" } { set values_($this,debias) 1 }
      if { $values_($this,minval) == "" } { set values_($this,minval) 1 }
      if { $values_($this,sigmas) == "" } { set values_($this,sigmas) 4 }

#  Save the original values as next times previous values.
      saveOld
   }

#  Accessor methods:
#  -----------------
   public method setMethod {x} {set values_($this,method) $x}
   public method getMethod {} {return $values_($this,method)}
   public method setBox {x} {set values_($this,box) $x}
   public method getBox {} {return $values_($this,box)}
   public method setDebias {x} {set values_($this,debias) $x}
   public method getDebias {} {return $values_($this,debias)}
   public method setMinVal {x} {set values_($this,minval) $x}
   public method getMinVal {} {return $values_($this,minval)}
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
         set lwidth 14

#  Initialise the row index withi the geaometry grid
         set r -1

#  Label parameters...
         itk_component add header1 {
	    gaia::LabelRule $w_.header1 -text "Binning Options:"
	 }
         grid $itk_component(header1) -row [incr r] -column 0 -padx 1m \
                                      -columnspan $ncol -sticky nwe -pady 2m

#  Next row.
         incr r

#  Create an integer entry for the box size.
         itk_component add box {
            util::LabelEntry $w_.box -text "Box size:" \
                                 -valuewidth $vwidth \
                                 -command [code $this activ box] \
                                 -labelwidth $lwidth \
                                 -anchor nw \
                                 -validate integer \
                                 -textvariable [scope values_($this,box)]
         }
         grid $itk_component(box) -row $r -column 0 -columnspan $ncol -sticky nw -padx $px
         add_short_help $itk_component(box) {The length of a side of each square bin, in pixels}

#  Vertical space.
         grid [frame $w_.space1 -height $vspace2] -row [incr r]

#  Next row
         incr r

#  Create a LabelMenu to control the method used for binnin.
         itk_component add method {
            util::LabelMenu $w_.method -text "Method:" \
                                 -variable [scope values_($this,method)] \
                                 -labelwidth $lwidth
         }
         grid $itk_component(method) -row $r -columnspan $ncol -column 0 -sticky nw -padx $px
         add_short_help $itk_component(method) {Method used to combine values in each bin}
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
         add_short_help $itk_component(debias) {Should the binned polarization values be debiassed if possible?}

#  Vertical space.
         grid [frame $w_.space3 -height $vspace2] -row [incr r]

#  Next row
         incr r

#  Create an integer entry for the min. no. of values in a bin
         itk_component add minval {
            util::LabelEntry $w_.minval -text "Min. values:" \
                                 -valuewidth $vwidth \
                                 -command [code $this activ minval] \
                                 -labelwidth $lwidth \
                                 -anchor nw \
                                 -validate integer \
                                 -textvariable [scope values_($this,minval)]
         }
         grid $itk_component(minval) -row $r -column 0 -columnspan $ncol -sticky nw -padx $px
         add_short_help $itk_component(minval) {The minimum number of good values in a bin}

#  Vertical space.
         grid [frame $w_.space4 -height $vspace2] -row [incr r]

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
         grid [frame $w_.space5 -height $vspace2] -row [incr r]

#  Next row
         incr r

#  Create a LabelButton which causes the vectors to be binned.
         itk_component add binner {
            ::gaia::LabelButton $w_.binner -text "Bin Now:" \
                                 -command [code $this bin] \
                                 -labelwidth $lwidth \
                                 -anchor nw \
                                 -buttonbitmap finger
         }
         grid $itk_component(binner) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(binner) {Click to bin all currently displayed vectors using the current settings}

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

#  End of class definition.
}
