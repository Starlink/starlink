#+
#  Name:
#     GaiaPolUStyle

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     A control panel for controlling and displaying vector style parameters.

#  Description:
#     This class provides a means of controlling a GaiaPolStyle using GUI
#     controls. It inherits from classes GaiaPolStyle and Util::FrameWidget
#     and so can be treated either as a GaiaPolStyle or as a FrameWidget.
#
#  Invocations:
#
#        GaiaPolUStyle object_name [configuration options]
#
#     This creates an instance of a GaiaPolUStyle object. The returned value
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
#     ::gaia::GaiaPolStyle
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
#     27-AUG-2018 (DSB):
#        If vector scale is negative, used constant vector length.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaPolUStyle {}

itcl::class gaia::GaiaPolUStyle {

#  Inheritances:
#  =============
   inherit ::gaia::GaiaPolStyle ::util::FrameWidget

#  Constructor:
#  ============
   constructor {args} {

#  Evaluate any options.
      eval itk_initialize $args

#  Now initialize the class data. If this constructor has been invoked
#  to construct the base class part of some super class, do not
#  initialize the data since this will be done as a consequence of
#  initializeing the super class data.
      if { [$this info class] == "::gaia::GaiaPolUStyle" } {
         init OK
      }
   }

#  Initialiser:
#  ============
#  Override the parent Init method to initialise the contents of the
#  memory allocated by the GaiaPolCat constructor using a user-supplied
#  argument list.
   protected method init { {ok ""} } {

#  This method is called from the FrameWidget constructor, as well as from
#  the constructor for this class. Do nothing when called from the FrameWidget
#  constructor.
      if { $ok == "OK" } {

#  First initialize the parent class data
         gaia::GaiaPolStyle::init

#  Now initialize this class...
         set created_ 0

#  Set defaults.
         reset
      }
   }

#  Destructor:
#  ============
   destructor {

#  Annul any catalogue reference.
      if { $cat_ != "" } { catch { $cat_ annull } }

#  Save the current options values to the options file, over-writing any
#  existing options file.
      if { $saveopt_ && [file isdirectory $itk_option(-optdir)] } {
         set optfile "$itk_option(-optdir)/GaiaPolUStyle.opt"
         if { [catch {set fd [open $optfile w]} mess] } {
            puts "Error writing defaults to file '$optfile' for the polarimetry toolbox 'Rendering' panel : $mess"
         } else {
            foreach name [array names values_] {
               if { [regexp {([^,]+),(.*)} $name match obj elem] } {
                  if { $obj == $this } {
                     if { $elem == "angcol" } {
                        puts $fd "set option($elem) \{$acol_\}"
                     } elseif { $elem == "lencol" } {
                        puts $fd "set option($elem) \{$lcol_\}"
                     } else {
                        puts $fd "set option($elem) \{$values_($name)\}"
                     }
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

#  Called when a new GaiaPolCat is opened.
#  ---------------------------------------
   public method newCat {cat} {

#  Store the new headings.
      setHeadings [$cat getHeadings]

#  If the current vector angle column is no longer available, we need to
#  select a new one. Use the vector angle column stored in the catalogue
#  (if there is one).
      if { [lsearch $headings_ [getAcol]] == -1 } {
         set col [ $cat getColNam ANG ]
         if { $col != "" } { setAcol $col }
      }

#  Do the same for the vector length column.
      if { [lsearch $headings_ [getLcol]] == -1 } {
         set col [ $cat getColNam P ]
         if { $col != "" } { setLcol $col }
      }

   }

#  Reset default values (either read from an options file or hard-wired).
#  ----------------------------------------------------------------------
   public method reset { {readopt 1} } {

#  Set the hard-wired defaults.
      if { !$readopt } {::gaia::GaiaPolStyle::reset 0}
      set values_($this,sclr) [::gaia::GaiaPolStyle::getSclr]
      set values_($this,swid) [::gaia::GaiaPolStyle::getSwid]
      set values_($this,sflash) [::gaia::GaiaPolStyle::getSflash]
      set values_($this,uclr) [::gaia::GaiaPolStyle::getUclr]
      set values_($this,uwid) [::gaia::GaiaPolStyle::getUwid]
      set values_($this,uflash) [::gaia::GaiaPolStyle::getUflash]
      set values_($this,arot) [::gaia::GaiaPolStyle::getArot]
      set values_($this,nvec) [::gaia::GaiaPolStyle::getNvec]
      set values_($this,angcol) [::gaia::GaiaPolStyle::getAcol]
      set values_($this,lencol) [::gaia::GaiaPolStyle::getLcol]

#  Over-write these with the values read from the options file created when
#  the last used instance of this class was destroyed.
      set optfile "$itk_option(-optdir)/GaiaPolUStyle.opt"
      if { $readopt && [file readable $optfile] } {
         if { [catch {source $optfile} mess] } {
            puts "Error reading defaults from file '$optfile' for the polarimetry toolbox 'Rendering' panel : $mess"
         } else {
            foreach elem [array names option] {
               set values_($this,$elem) "$option($elem)"
            }
         }
      }

#  Save the preferred anfle and length columns, read from the options file.
      set acol_ $values_($this,angcol)
      set lcol_ $values_($this,lencol)

#  Replace illegal blank values read from the options file with the hardwired
#  defaults.
      if { $values_($this,sclr) == "" } { set values_($this,sclr) [::gaia::GaiaPolStyle::getSclr] }
      if { $values_($this,swid) == "" } { set values_($this,swid) [::gaia::GaiaPolStyle::getSwid] }
      if { $values_($this,sflash) == "" } { set values_($this,sflash) [::gaia::GaiaPolStyle::getSflash] }
      if { $values_($this,uclr) == "" } { set values_($this,uclr) [::gaia::GaiaPolStyle::getUclr] }
      if { $values_($this,uwid) == "" } { set values_($this,uwid) [::gaia::GaiaPolStyle::getUwid] }
      if { $values_($this,uflash) == "" } { set values_($this,uflash) [::gaia::GaiaPolStyle::getUflash] }
      if { $values_($this,arot) == "" } { set values_($this,arot) [::gaia::GaiaPolStyle::getArot] }

#  Hard-wired defaults are used for option values which depend on the
#  particular vector map being displayed.
      set values_($this,mag) [::gaia::GaiaPolStyle::getMag]

#  Use these values if the controls have been created.
      if { $created_ } { activ }

   }

#  Set the vector length column. Set the value in the associated GaiaPolStyle,
#  and also set the GUI control.
#  ---------------------------------------------------------------------------
   public method setLcol {col} {
      set lcol_ $col
      ::gaia::GaiaPolStyle::setLcol $col
      set values_($this,lencol) $col
   }

#  Get the vector length column. If the current column name stored in the
#  associated GaiaPolStyle is not available in the headings, the widget value
#  will be blank, in which case use the value stored in the associated
#  GaiaPolStyle.
#  -----------------------------------------------------------------------
   public method getLcol {} {
      set ret $values_($this,lencol)
      if { $ret != "" } {
         ::gaia::GaiaPolStyle::setLcol $ret
      } else {
         set ret [::gaia::GaiaPolStyle::getLcol]
      }
      return $ret
   }

#  Set the vector angle column.
#  -----------------------------
   public method setAcol {col} {
      set acol_ $col
      ::gaia::GaiaPolStyle::setAcol $col
      set values_($this,angcol) $col
   }

#  Get the vector angle column.
#  -----------------------------
   public method getAcol {} {
      set ret $values_($this,angcol)
      if { $ret != "" } {
         ::gaia::GaiaPolStyle::setAcol $ret
      } else {
         set ret [::gaia::GaiaPolStyle::getAcol]
      }
      return $ret
   }

#  Set/Get the colour for selected vectors.
#  ----------------------------------------
   public method setSclr {clr} {
      ::gaia::GaiaPolStyle::setSclr $clr
      set values_($this,sclr) $clr
   }

   public method getSclr {} {
      set ret $values_($this,sclr)
      if { $ret != "" } {
         if { $ret == "(invisible)" } { set ret "" }
         ::gaia::GaiaPolStyle::setSclr $ret
      } else {
         set ret [::gaia::GaiaPolStyle::getSclr]
      }
      return $ret
   }

#  Set/Get the colour for unselected vectors.
#  ------------------------------------------
   public method setUclr {clr} {
      ::gaia::GaiaPolStyle::setUclr $clr
      set values_($this,uclr) $clr
   }

   public method getUclr {} {
      set ret $values_($this,uclr)
      if { $ret != "" } {
         if { $ret == "(invisible)" } { set ret "" }
         ::gaia::GaiaPolStyle::setUclr $ret
      } else {
         set ret [::gaia::GaiaPolStyle::getUclr]
      }
      return $ret
   }

#  Set/Get the vector scale value. Check the value entered by the user in
#  the LabelEntry widget is either blank or a legal numeric expression.
#  If not, issue a warning and set the value to blank.
#  ---------------------------------------------------------------
   public method setMag {mag} {
      ::gaia::GaiaPolStyle::setMag $mag
      set values_($this,mag) $mag
   }

   public method getMag {} {
      set ret $values_($this,mag)
      if { $ret != "" && [catch {set t [expr $ret]} ] } {
         if { $ret != "" } {
            error_dialog "Vector scale value \"$ret\" is not legal. Resetting it to blank."
         }
         set ret ""
         set values_($this,mag) ""
      }
      ::gaia::GaiaPolStyle::setMag $ret
      return $ret
   }

   public method getMagd {cat rtdimage} {
      set ret [getMag]
      if { $ret == "" } {
         set ret [::gaia::GaiaPolStyle::getMagd $cat $rtdimage]
         set values_($this,mag) $ret
      }
      return $ret
   }

#  Set/Get the vector rotation. Check the value entered by the user in
#  the LabelEntry widget is a legal numeric expression. If not, issue a
#  warning and set the value to blank.
#  ---------------------------------------------------------------------
   public method setArot {rot} {
      ::gaia::GaiaPolStyle::setArot $rot
      set values_($this,arot) $rot
   }

   public method getArot {} {
      set ret $values_($this,arot)
      if { [catch {set t [expr $ret]} ] } {
         if { $ret != "" } {
            error_dialog "Vector rotation value \"$ret\" is not legal. Reseting it to 0."
         }
         set ret 0.0
         set values_($this,arot) 0.0
      }
      ::gaia::GaiaPolStyle::setArot $ret
      return $ret
   }

#  Set/Get the number of vectors to display in the canvas. Check the value
#  entered by the user in the LabelEntry widget is a legal numeric expression.
#  If not, issue a warning and set the value to blank.
#  ---------------------------------------------------------------------
   public method setNvec {n} {
      ::gaia::GaiaPolStyle::setNvec $n
      set values_($this,nvec) $n
   }

   public method getNvec {} {
      set ret $values_($this,nvec)
      if { [catch {set t [expr $ret]} ] } {
         if { $ret != "" } {
            error_dialog "Number of vectors value \"$ret\" is not legal. Reseting it to blank."
         }
         set ret ""
         set values_($this,nvec) ""
      }
      ::gaia::GaiaPolStyle::setNvec $ret
      return $ret
   }

#  Set/Get the unselected vector width. Check the value entered by the user
#  in the LabelEntry widget is a legal numeric expression. If not, issue a
#  warning and set the value to 1.
#  ---------------------------------------------------------------------
   public method setUwid {wid} {
      ::gaia::GaiaPolStyle::setUwid $wid
      set values_($this,uwid) $wid
   }

   public method getUwid {} {
      set ret $values_($this,uwid)
      if { [catch {set t [expr $ret]} ] } {
         if { $ret != "" } {
            error_dialog "Unselected vector width \"$ret\" is not legal. Reseting it to 1."
         }
         set ret 1
         set values_($this,uwid) 1
      }
      ::gaia::GaiaPolStyle::setUwid $ret
      return $ret
   }

#  Set/Get the selected vector width. Check the value entered by the user
#  in the LabelEntry widget is either blank or a legal numeric expression.
#  If not, issue a warning and set the value blank.
#  ---------------------------------------------------------------------
   public method setSwid {wid} {
      ::gaia::GaiaPolStyle::setSwid $wid
      set values_($this,swid) $wid
   }

   public method getSwid {} {
      set ret $values_($this,swid)
      if { $ret != "" && [catch {set t [expr $ret]} ] } {
         if { $ret != "" } {
            error_dialog "Selected vector width \"$ret\" is not legal. Reseting it to blank."
         }
         set ret ""
         set values_($this,swid) ""
      }
      ::gaia::GaiaPolStyle::setSwid $ret
      return $ret
   }

#  Set/Get the unselected vector flashing state.
#  ---------------------------------------------
   public method setUflash {f} {
      ::gaia::GaiaPolStyle::setUflash $f
      if { $f } {
         set values_($this,uflash) 1
      } else {
         set values_($this,uflash) 0
      }
   }

   public method getUflash {} {
      set ret $values_($this,uflash)
      ::gaia::GaiaPolStyle::setUflash $ret
      return $ret
   }

#  Set/Get the selected vector flashing state.
#  ---------------------------------------------
   public method setSflash {f} {
      ::gaia::GaiaPolStyle::setSflash $f
      if { $f } {
         set values_($this,sflash) 1
      } else {
         set values_($this,sflash) 0
      }
   }

   public method getSflash {} {
      set ret $values_($this,sflash)
      ::gaia::GaiaPolStyle::setSflash $ret
      return $ret
   }

#  Set the available column headings.
#  ----------------------------------
   public method setHeadings {headings} {
      set headings_ $headings
      update_headings
   }

#  Set the flag indicating if options should be saved on close.
#  ------------------------------------------------------------
   public method setSaveOpt {x} {set saveopt_ $x}


#  Command which is involked when a change is made to any GUI control.
#  -------------------------------------------------------------------
   public method activ { args } {

#  Ensure the values in the values_ array are up to date.
      set values_($this,sclr) [$itk_component(sclr) get]
      set values_($this,uclr) [$itk_component(uclr) get]
      set values_($this,lencol) [$itk_component(lencol) get]
      set values_($this,angcol) [$itk_component(angcol) get]
      set acol_ $values_($this,angcol)
      set lcol_ $values_($this,lencol)

#  Use the current values.
      if { "$itk_option(-changecmd)" != "" } {
         eval $itk_option(-changecmd)
      }
   }

#  Create the page of controls.
#  ----------------------------
   public method create {} {

#  Do nothing if the controls have already been created.
      if { ! $created_ } {

#  Save the values_ array so that they can be reinstated later (the widget
#  creation commands seem to reset them to blank).
         foreach name [array names values_] {
            set temp($name) $values_($name)
         }

#  Indicate that the controls are being created.
         set created_ 1

#  Number of columns i the grid.
         set ncol 3

#  Horizontal padding for columns.
         set px 2m

#  Value width (in characters).
         set vwidth 6

#  Label width (in characters).
         set lwidth1 12
         set lwidth2 10

#  Initialise the row index withi the geaometry grid
         set r -1

#  Column names header...
         itk_component add header1 {
	    LabelRule $w_.header1 -text "Columns to use for:"
	 }
         grid $itk_component(header1) -row [incr r] -column 0 -padx 1m \
                                      -columnspan $ncol -sticky nwe -pady 2m

#  Next row.
         incr r

#  Create a LabelMenu to control the vector lengths within the above frame.
         itk_component add lencol {
	    LabelMenu $w_.lencol -text "Vector length:" -labelwidth $lwidth1 \
                                 -variable [scope values_($this,lencol)]
	 }
         grid $itk_component(lencol) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(lencol) {Column to use for vector lengths}

#  Create a LabelMenu to control the vector angles within the above frame.
         itk_component add angcol {
	    LabelMenu $w_.angcol -text "Vector angles:" -labelwidth $lwidth1 \
                                       -variable [scope values_($this,angcol)]
	 }
         grid $itk_component(angcol) -row $r -column 1 -columnspan 2 -sticky nw -padx $px
         add_short_help $itk_component(angcol) {Column to use for vector angles}

#  Update the contents and settings of the headings controls.
         update_headings

#  Vertical space.
         grid [frame $w_.space1 -height 3m] -row [incr r]

#  Attributes for selected vectors...
         itk_component add header2 {
	    LabelRule $w_.header2 -text "Selected vectors:"
	 }
         grid $itk_component(header2) -row [incr r] -column 0  -padx 1m \
                                      -columnspan $ncol -sticky nwe -pady 2m

#  Next row.
         incr r

#  Create a LabelMenu within the above frame to control the colour for
#  selected vectors.
         itk_component add sclr {
	    LabelMenu $w_.sclr -text "Colour:" -labelwidth $lwidth1 \
                               -variable [scope values_($this,sclr)]
         }
         grid $itk_component(sclr) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(sclr) {Colour for selected vectors}

         foreach clr $colourmap_ {
            $itk_component(sclr) add \
               -command [code $this activ] \
               -label {  } \
               -background $clr \
               -value $clr
         }
         $itk_component(sclr) add \
               -command [code $this activ] \
               -label "(invisible)" \
               -background "#fff" \
               -value "(invisible)"

#  Create a LabelEntry within the above frame to control the width for
#  selected vectors.
         itk_component add swid {
	    LabelEntry $w_.swid -text "Thickness:" \
                                -valuewidth $vwidth \
                                -command [code $this activ] \
                                -anchor nw \
                                -labelwidth $lwidth2 \
                                -textvariable [scope values_($this,swid)]
	 }
         grid $itk_component(swid) -row $r -column 1 -sticky nw -padx $px
         add_short_help $itk_component(swid) {Line width for selected vectors (in pixels)}

#  Create a LabelCheck within the above frame to control the flashing
#  state of selected vectors.
         itk_component add sflash {
	    StarLabelCheck $w_.sflash -text "Flashing:" \
                                      -onvalue 1 \
                                      -offvalue 0 \
                                      -command [code $this activ] \
                                      -labelwidth $lwidth2 \
                                      -anchor nw \
                                      -variable [scope values_($this,sflash)]
	 }
         grid $itk_component(sflash) -row $r -column 2 -sticky nw -padx $px
         add_short_help $itk_component(sflash) {Check to make selected vectors flash}

#  Vertical space.
         grid [frame $w_.space2 -height 3m] -row [incr r]

#  Attributes for unselected vectors...
         itk_component add header3 {
	    LabelRule $w_.header3 -text "All other vectors:"
	 }
         grid $itk_component(header3) -row [incr r] -column 0 -padx 1m \
                                      -columnspan $ncol -sticky nwe -pady 2m

#  Next row.
         incr r

#  Create a LabelMenu within the above frame to control the colour for
#  unselected vectors.
         itk_component add uclr {
	    LabelMenu $w_.uclr -text "Colour:" -labelwidth $lwidth1 \
                               -variable [scope values_($this,uclr)]
	 }
         grid $itk_component(uclr) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(uclr) {Colour for unselected vectors}

         foreach clr $colourmap_ {
            $itk_component(uclr) add \
               -label {  } \
               -command [code $this activ] \
               -background $clr \
               -value $clr
         }
         $itk_component(uclr) add \
               -label "(invisible)" \
               -command [code $this activ] \
               -background "#fff" \
               -value "(invisible)"

#  Create a LabelEntry within the above frame to control the width for
#  unselected vectors.
         itk_component add uwid {
	    LabelEntry $w_.uwid -text "Thickness:" \
                                -valuewidth $vwidth \
                                -command [code $this activ] \
                                -labelwidth $lwidth2 \
                                -anchor nw \
                                -textvariable [scope values_($this,uwid)]
	 }
         grid $itk_component(uwid) -row $r -column 1 -sticky nw -padx $px
         add_short_help $itk_component(uwid) {Line width for unselected vectors (in pixels)}

#  Create a LabelCheck within the above frame to control the flashing
#  state of unselected vectors.
         itk_component add uflash {
	    StarLabelCheck $w_.uflash -text "Flashing:" \
                                      -onvalue 1 \
                                      -offvalue 0 \
                                      -command [code $this activ] \
                                      -labelwidth $lwidth2 \
                                      -anchor nw \
                                      -variable [scope values_($this,uflash)]
	 }
         grid $itk_component(uflash) -row $r -column 2 -sticky nw -padx $px
         add_short_help $itk_component(uflash) {Check to make unselected vectors flash}

#  Vertical space.
         grid [frame $w_.space3 -height 3m] -row [incr r]

#  Others header
         itk_component add header4 {
	    LabelRule $w_.header4 -text "Misc.:"
	 }
         grid $itk_component(header4) -row [incr r] -column 0 -padx 1m \
                                      -columnspan $ncol -sticky nwe -pady 2m

#  Next row.
         incr r

#  Vector scale
         itk_component add mag {
	    LabelEntry $w_.mag -text "Vector scale:" \
                               -valuewidth $vwidth \
                               -command [code $this activ] \
                               -labelwidth $lwidth1 \
                               -anchor nw \
                               -textvariable [scope values_($this,mag)]
	 }

         grid $itk_component(mag) -row $r -column 0 -sticky nw -padx $px
         add_short_help $itk_component(mag) {Vector scale in image pixels per unit column value. Default used if blank. All vectors have same length if negative.}

#  Angle rotation
         itk_component add arot {
	    LabelEntry $w_.arot -text "Rotation:" \
                                -valuewidth $vwidth \
                                -command [code $this activ] \
                                -labelwidth $lwidth2 \
                                -anchor nw \
                                -textvariable [scope values_($this,arot)]
         }
         grid $itk_component(arot) -row $r -column 1 -sticky nw -padx $px
         add_short_help $itk_component(arot) {Anti-clockwise angle (in degrees) by which to rotate vectors}

#  Number of vectors
         itk_component add nvec {
	    LabelEntry $w_.nvec -text "Max. vectors:" \
                                -valuewidth $vwidth \
                                -command [code $this activ] \
                                -labelwidth $lwidth2 \
                                -anchor nw \
                                -textvariable [scope values_($this,nvec)]
         }
         grid $itk_component(nvec) -row $r -column 2 -sticky nw -padx $px
         add_short_help $itk_component(nvec) {Maximum number of vectors to draw (blank means no limit)}

#  Vertical space.
         grid [frame $w_.space4 -height 3m] -row [incr r]

#  Allow all cells of the grid to expand equally if the window is resized.
         for {set i 0} {$i < $ncol} {incr i} {
            grid columnconfigure $w_ $i -weight 1
         }
         for {set i 0} {$i < $r} {incr i} {
            grid rowconfigure $w_ $i -weight 1
         }

#  Re-instate the original values_ array.
         foreach name [array names temp] {
            set values_($name) $temp($name)
         }

      }
   }

#  Protected methods:
#  ==================

#  Update the headings menus
#  -------------------------
   protected method update_headings {} {

#  Do nothing if the controls have not been created.
      if { $created_ } {

#  Column name giving vector lengths...

#  Empty the menu.
         $itk_component(lencol) clear

#  Add items for each column to the menu.
         set ok 0
         foreach colnam $headings_ {
            $itk_component(lencol) add \
               -command [code $this activ] \
               -label $colnam \
               -value $colnam
            set ok 1
         }

#  Add a blank value to the end of the menu.
         $itk_component(lencol) add -label "" -value ""

#  Select the menu item which corresponds to the current preferred Lcol
#  property, if any. Otherwise, select the blank item.
         if { $ok } {
            if { [lsearch -exact $headings_ $lcol_] != -1 } {
               set values_($this,lencol) $lcol_
            } else {
               set values_($this,lencol) ""
            }
         } else {
            set values_($this,lencol) ""
         }

#  Do the same for the column name giving vector angles...
         $itk_component(angcol) clear
         set ok 0
         foreach colnam $headings_ {
            $itk_component(angcol) add \
               -command [code $this activ] \
               -label $colnam \
               -value $colnam
            set ok 1
         }
         $itk_component(angcol) add -label "" -value ""
         if { $ok } {
            if { [lsearch -exact $headings_ $acol_] != -1 } {
               set values_($this,angcol) $acol_
            } else {
               set values_($this,angcol) ""
            }
         } else {
            set values_($this,angcol) ""
         }
      }
   }

#  Private methods:
#  ==================

#  Options:
#  ========

#  The name of a directory in which to store tcl code which will recreate
#  the current GUI settings. This text is created when this object is
#  destroyed.
   itk_option define -optdir optdir Optdir {}

#  A command to call when any control values are changed by the user.
   itk_option define -changecmd changecmd Changecmd {}

#  Protected data members:
#  =======================
   protected {

#  Preferred vector angle and length columns.
      variable acol_ ""
      variable lcol_ ""

#  Have the control widgets been created yet?
      variable created_ 0

#  The catalogue to which the style refers.
      variable cat_ ""

#  The colours in which vectors can be drawn.
      variable colourmap_ { "#fff" "#000" "#f00" "#0f0" "#00f" \
                            "#0ff" "#f0f" "#ff0" "#f80" "#8f0" \
                            "#0f8" "#08f" "#80f" "#f08" \
                            "#512751275127" "#a8b4a8b4a8b4"}

#  Should current settings be saved when this object is destroyed?
       variable saveopt_ 1

#  Column headings
       variable headings_ ""

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
