#+
#  Name:
#     GaiaPolDisp

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Implements a tool for displaying polarimetry data as graphical
#     vectors.

#  Description:
#     This class handles the visualisation of polarimetry data by drawing
#     vectors on the canvas. It handles the drawing of the vectors, the key,
#     the selection of vectors using the mouse, and the highlighting of vectors.
#
#  Invocations:
#
#        GaiaPolDisp object_name [configuration options]
#
#     This creates an instance of a GaiaPolDisp object. The returned value
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

#  Methods:

#  Inheritance:
#     ::gaia::GaiaPolObject

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
#     15-JUN-2000 (DSB):
#        Original version.
#     7-MAR-2001 (DSB):
#        Substantially restructured.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaPolDisp {

#  Inheritances:
#  ============
   inherit gaia::GaiaPolObject


#  Required methods, etc:
#  =====================

#  Constructor
#  -----------
   constructor { w image rtdimage canvas pbar selcmd hgFont klfont actcmd} {
      upvar this parent

#  Now initialize the class data. If this constructor has been invoked
#  to construct the base class part of some super class, do not
#  initialize the data since this will be done as a consequence of
#  initializeing the super class data.
      if { [$this info class] == "::gaia::GaiaPolDisp" } {
         init $w $image $rtdimage $canvas $pbar $selcmd $hgFont $klfont $actcmd
      }
   }

#  Destructor
#  ----------
   destructor {
      catch {

#  Remove the trace which causes method newZoom to be changed whenever
#  the zoom factor changes.
         set panel [$image_ component panel]
         set trans [$panel.info component trans]
         set choose [$trans component choose]
         set mb [$choose component mb]
         set m [$mb cget -menu]
         set var [$m entrycget 0 -variable]
         global ::$var
         trace vdelete $var w [code $this newZoom]

#  Clear the data.
         catch {clear}
      }
   }

#  Override the parent Init method to initialise the contents of the
#  memory allocated by the GaiaPolDisp constructor using a user-supplied
#  argument list.
#  ----------------------------------------------------------------------
   protected method init { w image rtdimage canvas pbar selcmd hgFont klfont actcmd} {

#  First initialize the parent class data
      gaia::GaiaPolObject::init

#  Now initialize this class.
      set w_ $w
      set image_ $image
      set rtdimage_ $rtdimage
      set canvas_ $canvas
      set id_ [incr count_]
      set disid_ "PDis$id_"
      set pbar_ $pbar
      set selcmd_ $selcmd
      set hgFont_ $hgFont
      set klfont_ $klfont
      set actcmd_ $actcmd

#  Set up a trace on the variable associated with the radiobutton which
#  is used to select new zoom factors in the image control panel ($image_).
#  The trace arranges for method newZoom in this class to be called
#  whenever the zoom factor changes.
      set panel [$image_ component panel]
      set trans [$panel.info component trans]
      set choose [$trans component choose]
      set mb [$choose component mb]
      set m [$mb cget -menu]
      set var [$m entrycget 0 -variable]
      global ::$var
      trace variable $var w [code $this newZoom]

   }


#  Public methods.
#  ===============

#  Reset the key so that default values will be used when the next key is
#  drawn.
#  ----------------------------------------------------------
   public method resetKey {} {
      noKey
      set kvval_ ""
      set kvdef_ ""
      set klx_ ""
      set kly_ ""
   }

#  Deletes all canvas items associated with this GaiaPolDisp, and forgets
#  the previous catalogue and style.
#  ----------------------------------------------------------
   public method clear {} {

#  Clear the key
      set kvval_ ""
      set kvdef_ ""
      set klx_ ""
      set kly_ ""
      noKey

#  Clear the vectors.
      clearVectors

#  Annull references.
      if { $cat_ != "" } {set cat_ [$cat_ annull] }
      if { $style_ != "" } {set style_ [$style_ annull] }

#  If the catalogue was displayed over a blank image created by a
#  GaiaPolDisp, decrement its reference count. If zero, remove the blank image.
      if { $blankimage_ } {
         set blankimage_ 0

         if { [info exists blankref_($rtdimage_)] } {
            incr blankref_($rtdimage_) -1
            if { $blankref_($rtdimage_) <= 0 } {

#  Reset the zoom factors.
               $image_ scale 1 1

#  Clear the image
               $rtdimage_ clear

#  Unset the array variable.
               unset blankref_($rtdimage_)
            }
         }
      }
   }

#  Updates the canvas so that it contains vectors representing the
#  supplied catalogue, rendered in the supplied style. If possible, this
#  is done by reconfiguring or deleting existing canvas items. This is
#  possible if the supplied catalogue refers to the same data file as the
#  previously drawn catalogue (this would be the case for instance if the
#  previous action was to select, deselect or cut some vectors) AND the
#  supplied style can be implemented merely by reconfiguring some canvas
#  item options. If the supplied catalogue refers to a different data
#  file, (which would be the case for instance if the previous action was
#  a "bin" operation) OR if changes to the the style require the canvas
#  item to be recreated from scratch, then the canvas is cleared first of
#  all canvas items created by this GaiaPolDisp, and new canvas items are
#  created for every (non-deleted) row in the catalogue. Unless $addact
#  is zero, an action is added to the list of undoable action which causes
#  the previous catalogue and style to be re-drawn. If $force is non-zero,
#  then the supplied catalogue is displayed even if it seems to be exactly the
#  same as the currently displayed catalogue.
#  ----------------------------------------------------------------------
   public method draw {cat prevcat style prevsty {addact 1} {force 0} } {

#  Indicate what is happening.
      setHold "Plotting vectors..."

#  Prevent the key from being updated due to changes in the image
#  controls until the end of this method.
      set noNewZoom_ 1

#  If no catalogiue is currently displayed, or if we have no current
#  style, or if a redisplay has been forced, we need to draw the supplied
#  catalogue from scratch.
      if { $prevcat == "" || $prevsty == "" || $force } {
         set clear 1

#  Store info describing the action for the list of undoable actions.
         set catch "redraw"
         set catdesc "load a new catalogue"
         set stych "redraw"
         set stydesc "use a new rendering style"

#  Otherwise, we need to find out what needs to be done to convert the
#  current display into the required display.
      } else {

#  Get a description of the changes which produced the new catalogue, and
#  the changes equired to convert the displayed catalogue into the
#  supplied catalogue.
          lassign [$prevcat changes $cat] catdesc catch

#  Get a description of the changes which produced the new style, and
#  the changes required to convert the currently used style into the
#  supplied style.
          lassign [$prevsty changes $style] stydesc stych

#  If either the catalogue or the style requires the display to be
#  drawn from scratch, indicate this.
          if { $catch == "redraw" || $stych == "redraw" } {
             set clear 1
          } else {
             set clear 0
          }
      }

#  If neither the style or the catalogue has changed, do nothing.
      if { $catch != "" || $stych != "" } {

#  If a complete redraw from scratch is required...
         if { $clear } {

#  Clear any previous vectors drawn on the canvas.
            clearVectors

#  Ensure there is an image to plot over. A blank image is created if
#  necessary.
            checkImage $cat

#  A blank values for $rows means "use all rows".
            set rows ""

#  Set up lists of canvas options for selected and unselected vectors.
#  These include the "pretend" option -flash, which is implemented by this
#  class.
            set usty "-fill \"[$style getUclr]\" -width [$style getUwid] -flash [$style getUflash]"
            set ssty "-fill \"[$style getSclr]\" -width [$style getSwidd] -flash [$style getSflash]"

#  If we do not need to draw every vector again, get a list of the rows
#  which need changing and the corresponding states. ALso get lists of
#  the canvas item options which need to be reconfigured to render a
#  vector as selected or unselected.
         } else {
            set rows $catch
            if { $stych != "" } {
               set usty [lindex $stych 0]
               set ssty [lindex $stych 1]
            } else {
               set usty "-fill \"[$style getUclr]\" -width [$style getUwid] -flash [$style getUflash]"
               set ssty "-fill \"[$style getSclr]\" -width [$style getSwidd] -flash [$style getSflash]"
            }
         }
#  Create or reconfigure the required vectors. The number of new vectors
#  created is returned.
         set new [plot $cat $style $rows $usty $ssty]

#  Store clones of the supplied catalogue and style.
         if { $cat_ != "" } {set cat_ [$cat_ annull] }
         if { $style_ != "" } {set style_ [$style_ annull] }
         set cat_ [$cat clone]
         set style_ [$style clone]

#  Ensure canvas bindings are set up.
         setBindings

#  Draw a new key.
         Key

#  If a suitable action command has been supplied, add actions to the undoable
#  action list, unless inhibited by the caller.
         if { $addact && $actcmd_ != "" } {

#  Add an action if the catalogue has changed.
            if { $catch != "" } {
               eval $actcmd_ {cat} {$cat} {$catdesc}
            }

#  Add an action if the style has changed.
            if { $stych != "" } {
               eval $actcmd_ {style} {$style} {$stydesc}
            }
         }
      }

#  Allow the key to be updated due to changes in the image controls.
      set noNewZoom_ 0

#  Reset the progress bar.
      resetHold

#  Issue a warning if no vectors could be plotted.
      if { $clear && $new == 0 } {
         error_dialog "There are no vectors to draw, or they are all of zero length."
      }

   }

#  Ensure a key is visible and has the current key properties.
#  -----------------------------------------------------------
   public method Key {} {

#  Ensure any old key is deleted.
      noKey

#  If the key is currently enabled, and the vector style is known, draw a
#  new key.
      if { $kenabled_ && $style_ != "" && $cat_ != "" } {

#  Get the vector length to use.
         set kvval [getKvVal]

#  Create a canvas text item for the label. It is initially centred at canvas
#  coords (0,0) and is later moved to its correct position.
         set klid_ [$canvas_ create text 0 0 -anchor center \
                                             -font $klfont_ \
                                             -tags PolKey \
                                             -fill $klcol_ \
                                             -text [format "$klfmt_" $kvval]]

#  Now get its bounding box, and its width and height.
         lassign [$canvas_ bbox $klid_] lx0 ly0 lx1 ly1
         set lw [expr abs($lx1 - $lx0)]
         set lh [expr abs($ly1 - $ly0)]

#  Find the required length of the vector in canvas coords.
         $rtdimage_ convert coords 0.0 0.0 "image" vx0 vy0 "canvas"
         $rtdimage_ convert coords 1.0 0.0 "image" vx1 vy1 "canvas"
         set mag [$style_ getMagd $cat_ $rtdimage_]
         set vcl [expr $kvval*$mag*abs( $vx1 - $vx0 )]

#  Find the coords at the ends, so that the vector is centred at x=0
         set vx0 [expr - 0.5*$vcl]
         set vx1 [expr 0.5*$vcl]

#  If the vector is to be drawn in the current vector colour, get the
#  colour.
         if { $kvcol_ == "current" } {
            set vcol [$style_ getUclr]
         } else {
            set vcol $kvcol_
         }

#  Create a canvas line item for the vector, half a text height under the
#  text label.
	 set vy [expr 1.0*$lh]
         set kvid_ [$canvas_ create line $vx0 $vy $vx1 $vy -fill $vcol \
                                                           -tags PolKey \
	                                                   -width $kvwid_ ]

#  Find the bounding box of the label and vector.
         lassign [$canvas_ bbox $kvid_ $klid_] kx0 ky0 kx1 ky1

#  Get the width of the border in canvas pixels.
         set bd [winfo pixels $canvas_ $kpad_]

#  Now get the size and position of the background rectangle to match
#  the key, with a margin around each edge.
         set kx0 [expr $kx0 - $bd]
         set kx1 [expr $kx1 + $bd]
         set ky0 [expr $ky0 - $bd]
         set ky1 [expr $ky1 + $bd]

#  Create a canvas rectangle item for the background.
         set kbid_ [$canvas_ create rectangle $kx0 $ky0 $kx1 $ky1 \
                                              -fill $kbgcol_ \
					      -outline $kbdcol_ \
                                              -tags PolKey \
                                              -width $kbdwid_ ]

#  We now need to move the all components to their correct initial positions.
#  Get the bounding box of the whole key.
         lassign [$canvas_ bbox $kbid_ $kvid_ $klid_] kx0 ky0 kx1 ky1

#  Find the width and height of the whole key.
         set kh [expr $ky1 - $ky0]
         set kw [expr $kx1 - $kx0]

#  Now find the preferred position for the key. If a key has previously been
#  used, the centre coords of the rectangle (in image coords) will be $klx_ and
#  $kly_. If so we try to centre the new rectangle at the same position as the
#  old one. On this assumption, find the canvas coords of the top left and
#  bottom right of the rectangle.
         if { $klx_ != "" && $kly_ != "" } {
            $rtdimage_ convert coords $klx_ $kly_ "image" x y "canvas"
            set x0 [expr $x - 0.5*$kw]
            set y0 [expr $y - 0.5*$kh]
	    set x1 [expr $x + 0.5*$kw]
            set y1 [expr $y + 0.5*$kh]

#  If no previous key position is available, we try to place the left
#  edge of the key against the right edge of the image (separated by a
#  small gap), and align the top of the key with the top of the image.
         } else {

#  Find the canvas coords at the top right corner of the image
#  (assuming the image is viewed normally - i.e. it has not been flipped, etc).
            set wid [$rtdimage_ width]
            set hgt [$rtdimage_ height]
            $rtdimage_ convert coords $wid $hgt "image" x y "canvas"

#  Find the canvas coords at the top left and bottom right corner of the key
#  bounding box which this would produce.
            set x0 [expr $x + 10]
	    set y0 $y
            set x1 [expr $x0 + $kw ]
	    set y1 [expr $y0 + $kh ]
         }

#  The preferred placement for the key found above may place the key off
#  the screen (for instance if the image has been zoomed). So we now restrict
#  the position of the top right corner of the key to be within the visible
#  part of the image... Find the canvas coords at the top left and bottom right
#  corners of the visible section of the canvas.
         set cx0 [$canvas_ canvasx 0]
         set cy0 [$canvas_ canvasy 0]
         set cx1 [$canvas_ canvasx [winfo width $canvas_]]
         set cy1 [$canvas_ canvasy [winfo height $canvas_]]

#  Restrict the horizontal position of the key.
         if { $x0 < $cx0 } {
	    set x0 $cx0
	    set x1 [expr $cx0 + $kw]
	 } elseif { $x1 > $cx1 } {
	    set x1 $cx1
	    set x0 [expr $cx1 - $kw]
	 }

#  Restrict the vertical position of the key.
         if { $y0 < $cy0 } {
	    set y0 $cy0
	    set y1 [expr $cy0 + $kh]
	 } elseif { $y1 > $cy1 } {
	    set y1 $cy1
	    set y0 [expr $cy1 - $kh]
	 }

#  Find the shift in x and y required to put the rectangle at the above
#  position.
         set dx [expr 0.5*( ($x0 + $x1) - ($kx0 + $kx1 ) )]
         set dy [expr 0.5*( ($y0 + $y1) - ($ky0 + $ky1 ) )]

#  Move all the components by this amount.
         $canvas_ move $klid_ $dx $dy
         $canvas_ move $kvid_ $dx $dy
         $canvas_ move $kbid_ $dx $dy

#  Raise the key to the top of the display stack.
         raiseKey
      }
   }

#  Called when the zoom factor in the "Choose Zoom factor" menubutton
#  associated with $image_ changes. Ensure the key looks right at the
#  new zoom factor.
#  -------------------------------------------------------------------
   public method newZoom {name1 name2 op} {
      if { ! $noNewZoom_ } {
         Key
      }
   }

#  Delete the key.
#  ---------------
   public method noKey {} {

#  Get the bounding box of the outer rectangle forming the background to
#  the key.
      if { $kbid_ != "" } {
         lassign [$canvas_ bbox $kbid_ ] kx0 ky0 kx1 ky1

#  Save the image coords of the centre of the background rectangle.
         $rtdimage_ convert coords [expr 0.5*( $kx0 + $kx1 )] \
                                   [expr 0.5*( $ky0 + $ky1 )] "canvas" \
                                   klx_ kly_ "image"
      }

#  Delete the canvas items forming the key
      if { $kvid_ != "" } {
         $canvas_ delete $kvid_
         set kvid_ ""
      }
      if { $klid_ != "" } {
         $canvas_ delete $klid_
         set klid_ ""
      }
      if { $kbid_ != "" } {
         $canvas_ delete $kbid_
         set kbid_ ""
      }
   }

#  Accessor methods related to highlighting.
#  -----------------------------------------
   public method setHgEnabled {enabled} {

      if { $bound_ } {
         if { $hgEnabled_ && !$enabled } {
            canvasbind $this $canvas_ $disid_ <Enter> remove {}
            canvasbind $this $canvas_ $disid_ <Leave> remove {}
         } elseif { !$hgEnabled_ && $enabled } {
            canvasbind $this $canvas_ $disid_ <Enter> add "[code $this LabelBind 1 %x %y]"
            canvasbind $this $canvas_ $disid_ <Leave> add "[code $this LabelBind 0 %x %y]"
         }
      }

      set hgEnabled_ $enabled

   }

   public method setHgFormat {fmt} {set hgFmt_ $fmt}
   public method setHgColour {col} {set hgColour_ $col}
   public method setHgFont {font} {set hgFont_ $font}

#  Accessor methods related to the key
#  -----------------------------------
   public method setKEnabled {x} { set kenabled_ "$x" }
   public method setKlCol {x} { set klcol_ "$x" }
   public method setKlFont {x} { set klfont_ "$x" }
   public method setKlFmt {x} { set klfmt_ "$x" }
   public method setKvCol {x} { set kvcol_ "$x" }
   public method setKvWid {x} { set kvwid_ "$x" }
   public method setKbgCol {x} { set kbgcol_ "$x" }
   public method setKbdCol {x} { set kbdcol_ "$x" }
   public method setKbdWid  {x} { set kbdwid_ "$x" }
   public method setKPad {x} { set kpad_ "${x}c" }



#  Set the key vector length (x is in data units). If x is blank then a
#  default value will be used instead of the supplied value.
#  --------------------------------------------------------------
   public method setKvVal {x} { set kvval_ $x }

#  Get the key vector length (x is in data units). If kvval_ is blank use a
#  nice default value
#  --------------------------------------------------------------
   public method getKvVal {} {

#  If kvval_ is non-blank value return it.
      if { [string trim $kvval_] != "" } {
        set ret $kvval_

#  If kvval_ is blank, return the defaylt key vector length.
      } else {

#  If a new vector scale is in use, indicate that a new default length is
#  required.
         set mag [$style_ getMagd $cat_ $rtdimage_]
         if { $mag != $mag_ } {
            set kvdef_ ""
            set mag_ $mag
         }

#  If the default key vector length is not yet known, calculate it now,
#  if possible. The default value depends on the contents of the
#  currently displayed catalogue and the current vector scale (part fo the
#  style).
         if { $kvdef_ == "" && $style_ != "" && $cat_ != "" } {

#  Get the length of the image diagonal as a number of image pixels
            set wid [$rtdimage_ width]
            set hgt [$rtdimage_ height]
            set diag [expr sqrt($wid*$wid + $hgt*$hgt)]

#  The nominal default vector length is equal to 0.05 of the image diagonal.
#  Find the corresponding number of data units.
            if { $mag_ != 0.0 } {
               set d [expr 0.05*$diag/$mag_]

#  We now find a "nice" value which is close to this value.
               set kvdef_ [nice $d]
            }
         }

         set ret $kvdef_

      }

      return $ret
   }

#  Accessor methods for the cursor selection shape.
#  ------------------------------------------------
   public method getSelShape {} { return $selshape_ }
   public method setSelShape {s} { set selshape_ $s }

#  Format the object into a string.
#  --------------------------------
   public method toString {} {
      set ret "GaiaPolData:\n"
      return $ret
   }

#  Zoom so that the image fills the screen.
#  -----------------------------------------
   public method zoom_to_image {} {

#  Get the canvas coords of the bounding box enclosing the image.
      $rtdimage_ convert coords 0.0 0.0 "image" x0 y0 "canvas"
      $rtdimage_ convert coords [$rtdimage_ width] [$rtdimage_ height] \
                     "image" x1 y1 "canvas"

#  Zoom the image to fill the screen.
      zoom_to_region $x0 $y0 $x1 $y1

#  Ensure the key looks right.
      Key
   }

#  Zoom so that the selected vectors fill the screen.
#  --------------------------------------------------
   public method zoom_to_selection {} {
      set first 1
      if { ! [info exists vstates_] } {
         return
      }

#  Search the vstates_ array looking for entries for selected vectors.
      set search [array startsearch vstates_]
      while { [set id [array nextelement vstates_ $search]] != "" } {
         if { $vstates_($id) == "S" } {

#  Get the coords of the vector.
            set c [$canvas_ coords $id]
            set x [expr 0.5*( [lindex $c 0] + [lindex $c 2] )]
            set y [expr 0.5*( [lindex $c 1] + [lindex $c 3] )]

            if { $first } {
               set x0 $x
               set y0 $y
               set x1 $x
               set y1 $y
               set first 0

            } else {
               set x0 [min $x0 $x]
               set y0 [min $y0 $y]
               set x1 [max $x1 $x]
               set y1 [max $y1 $y]
            }
         }
      }

#  Release the array search context.
      array donesearch vstates_ $search

#  Report an error if no selected vectors are visible.
      if { $first } {
         error_dialog "None of the drawn vectors are selected"

      } else {

#  If the bounding box of the vector centres has zero size (i.e. if there
#  is only a single selected vector visible), use the bounding box of the
#  whole vector.
         if { $x0 == $x1 || $y0 == $y1 } {
            lassign [$canvas_ bbox "S$disid_"] x0 y0 x1 y1
         }

#  Now zoom so that this region fills the canvas.
         zoom_to_region $x0 $y0 $x1 $y1

#  Ensure the key looks right.
         Key
      }
   }


#  Protected methods.
#  ==================

#  Called when the cursor is moved over the vector map with button 1
#  pressed. Handles dragging of the key, drawing of selection box or
#  circle, and selection of vectors.
#  -----------------------------------------------------------------
   protected method B1MotionBind {x y} {

# Convert the screen coords to canvas coords.
      set cx [$canvas_ canvasx $x]
      set cy [$canvas_ canvasy $y]

#  First handle dragging of the key for $this.
      if { $root_item_ == "key" } {

#  Find the shift in X and Y from the previous position to the supplied
#  current pointer position.
         set dx [expr $cx - $rootx_]
         set dy [expr $cy - $rooty_]

#  Move each component of the key by this amount.
         $canvas_ move $kbid_ $dx $dy
         $canvas_ move $klid_ $dx $dy
         $canvas_ move $kvid_ $dx $dy

#  Save the current position.
         set rootx_ $cx
         set rooty_ $cy

#  Do nothing if we are dragging the key for another GaiaPolDisp.
      } elseif { $root_item_ == "akey" } {

#  Now handle rectangular vector selection...
      } elseif { $selshape_ == "box" } {

# Find the min and max values on each axis of the selected area. The
# position at which the button was pressed (rootx_,rooty_) gives one
# corner of the box, and the current cursor position gives the other.
         if { $cx < $rootx_ } {
            set xmin $cx
            set xmax $rootx_
         } {
            set xmax $cx
            set xmin $rootx_
         }
          if { $cy < $rooty_ } {
            set ymin $cy
            set ymax $rooty_
         } {
            set ymax $cy
            set ymin $rooty_
         }

# If there is currently no recorded selected area, create the canvas item
# (a rectangle) marking the area. Otherwise, configure the existing
# canvas item to match the current selected area.
         if { $sarea_ == "" } {
            $canvas_ create rectangle $xmin $ymin $xmax $ymax -outline cyan -tags selreg
         } {
            $canvas_ coords selreg $xmin $ymin $xmax $ymax
         }

# Record the current selected area.
         set sarea_ [list $xmin $ymin $xmax $ymax]

# Now handle circular vector selection...
      } elseif { $selshape_ == "circle" } {

# Find the the coordinates of two diagonally opposite corners of a
# rectangular region enclosing the circle. The centre is the
# position at which the button was pressed (rootx_,rooty_), and the
# the current cursor position gives one corner.
         set dx [expr $cx - $rootx_]
         set dy [expr $cy - $rooty_]
         set d [expr sqrt($dx*$dx+$dy*$dy)]
         set x1 [expr $rootx_ - $d]
         set x2 [expr $rootx_ + $d]
         set y1 [expr $rooty_ - $d]
         set y2 [expr $rooty_ + $d]

# If there is currently no recorded selected area, create the canvas item
# (an oval) marking the area. Otherwise, configure the existing
# canvas item to match the current selected area.
         if { $sarea_ == "" } {
            $canvas_ create oval $x1 $y1 $x2 $y2 -outline cyan -tags selreg
         } {
            $canvas_ coords selreg $x1 $y1 $x2 $y2
         }

# Record the current selected area.
         set sarea_ [list $x1 $y1 $x2 $y2]
      }
   }

#  Ensure that there is an image over which vectors can be drawn. If we
#  have an image display, but no image is loaded, generate a dummy
#  image.
#  ---------------------------------------------------------------------
   protected method checkImage { cat } {

#  Check that no image is displayed, including blank images created by
#  this class.
      if { [$rtdimage_ isclear] && \
           ![info exists blankref_($rtdimage_)] } {

#  Create the blank image large enough for the area covered by the
#  catalogue and with a suitable WCS system.
         if { ![catch {$cat mkImage} mess] } {

#  Zoom the image to fill the screen.
            zoom_to_image

#  Enable the image options panel, since we now have an image.
            catch {[$image_ component info] configure -state normal}
            catch {[$image_ component info] updateValues}

#  Indicate that this new blank image is currently used as the background
#  for just one catalogue.
            set blankref_($rtdimage_) 1
            set blankimage_ 1

#  If something went wrong creating the image, attempt to delete the image.
         } else {
            puts "$mess"
            catch { $rtdimage_ clear }
         }

#  If an image is already displayed...
      } else {

#  If it is a blank image created by a GaiaPolDisp, then increment its
#  reference count so long as this GaiaPolDisp has not already registered
#  an interest in the image.
         if { !$blankimage_ && [info exists blankref_($rtdimage_)] } {
            incr blankref_($rtdimage_)
            set blankimage_ 1
         }
      }
   }

#  Clear the vectors drawn on the canvas.
#  --------------------------------------
   protected method clearVectors {} {

#  Prevent the key from being updated due to changes in the image
#  controls until the end of this method.
      set noNewZoom_ 1

#  Do nothing if no data has yet been displayed...
      if { $cat_ != "" && $style_ != "" } {

#  Re-instate the original canvas bindings.
         resetBindings

#  Remove all vectors displayed by this GaiaPolDisp.
         $canvas_ delete $disid_

#  Ensure no flashing.
         setFlash 0 0
         setFlash 1 0

#  Reset lists and array properties describing the vectors.
         if { [info exists vstates_] } { unset vstates_ }
         if { [info exists vlens_] } { unset vlens_ }
         if { [info exists vrows_] } { unset vrows_ }
         if { [info exists vectors_] } { unset vectors_ }

      }

#  Allow the key to be updated due to changes in the image controls.
      set noNewZoom_ 0

   }

#  Convert the given input coordinates in the given input units to the
#  given output units and return a list {x y} with the new values.
#  The units may be one of {canvas image wcs deg "wcs $equinox", "deg $equinox"}
#  ----------------------------------------------------------------------
   protected method convert_coords {in_x in_y in_units out_units} {
       return [$rtdimage_ convert coords $in_x $in_y $in_units {} {} $out_units]
   }

#  Flash vectors by changing the colour of them. This method reschedules itself
#  each time it is called, toggling the value of argument on each time. It does
#  not reschedule itself is the appropriate data member (flashsel_ for selected
#  vectors or flashuns_ for unselected vectors) is set to a false value. Argument
#  sel indicates if the selected or unselected vectors are to be flashed. $col
#  is the colour the vectors should have when visible.
#  -----------------------------------------------------------------------
   protected method flash {sel on} {

      if { $sel } {
         set more $flashsel_
         set tag "S$disid_"
      } else {
         set more $flashuns_
         set tag "U$disid_"
      }

      if { $on || !$more } {
         if { $sel } {
            set c "$flashscol_"
         } else {
            set c "$flashucol_"
         }
         set on 0
      } else {
         set c ""
         set on 1
      }

      $canvas_ itemconfigure $tag -fill $c

      if { $more } {
         after 500 [code $this flash $sel $on]
      }

   }

#  Find the anticlockwise angle from vertical (-ve canvas Y) to north
#  at a specified canvas positions. The angle is returned as a list of
#  two values { cos(ang) sin(ang) }.
#  ---------------------------------------------------------------------
   protected method fndNth {x y equ} {

#  Convert the supplied canvas position to ra and dec.
      set pos1 [convert_coords $x $y "image" "deg $equ" ]

#  Convert the pixel above the supplied canvas position to ra and dec.
      set pos2 [convert_coords $x [expr $y - 1] "image" "deg $equ" ]

#  Extract ra and dec values from these positions.
      set a1 [expr [lindex $pos1 0]*0.017453293 ]
      set b1 [expr [lindex $pos1 1]*0.017453293 ]
      set a2 [expr [lindex $pos2 0]*0.017453293 ]
      set b2 [expr [lindex $pos2 1]*0.017453293 ]

#  Do the calculation (see SLALIB routine sla_bear).
      set da [expr $a2-$a1]
      set x [expr sin($b2)*cos($b1)-cos($b2)*sin($b1)*cos($da)]
      set y [expr -sin($da)*cos($b2)]
      set l [expr sqrt( $x*$x + $y*$y )]
      return [list [expr $x/$l] [expr $y/$l] ]

   }

#  Return the name (file or object name) of the currently loaded image,
#  or empty if no image is loaded.
#  -------------------------------------------------------------------
    protected method get_image_name {} {
        set name [$rtdimage_ cget -file]
        if {"$name" == ""} {
            set name [$rtdimage_ object]
        }
        return $name
    }

#  Forget the current key position, causing the next key to be displayed
#  at the top right of the image (assuming normal viewing). If a key is
#  already visible, re-display it so that it moves to the "reset" position.
#  ---------------------------------------------------------------------
   protected method KReset {} {
      set klx_ ""
      set kly_ ""
      if { $klid_ != "" } {
         noKey
         Key
      }
   }

#  Called when the pointer enters or leaves a vector. It controls the
#  labelling and highlighting of the vector under the pointer.
#  ------------------------------------------------------------------
    protected method LabelBind {enter x y} {

#  When leaving, un-highlight the currently highlighted vector (if any).
      if { !$enter } {
         if { $lab_id_ != "" } {

#  Delete the label.
            $canvas_ delete $lab_id_

#  Get the current state of the vector.
            if { [info exists vstates_($vec_id_)] } {
               set state $vstates_($vec_id_)
               if { $state == "S" } {
                  set col [$style_ getSclr]
               } else {
                  set col [$style_ getUclr]
               }

            } else {
               set state "D"
               set col ""
            }

#  Reset the original vector colour so long as the vector has not changed
#  its unselected/selected status.
            if { $state == $vec_st_ } {


               $canvas_ itemconfigure $vec_id_ -fill $col
            }

#  If the vector is currently unselected, lower it below all other vectors.
            if { $state == "U" } {
               $canvas_ lower $vec_id_ $disid_
            }

#  Raise the key above everything else.
            raiseKey

#  Clear globals
            set lab_id_ ""
            set vec_id_ ""
            set vec_st_ ""
	 }

#  When entering, highlight the vector unless highlighting is not enabled. Do
#  nothing if a label is already displayed.
      } elseif { $hgEnabled_ && $lab_id_ == "" } {

#  Format the vector length for the vector currently under the pointer.
         set vec_id_ [$canvas_ find withtag current]
         set text [format "$hgFmt_" $vlens_($vec_id_)]

#  Ignore blank labels
         if { $text != "" } {

#  Decide on the anchor position of the text to minimise the chance of the
#  label obscuring the vector.
            lassign [$canvas_ coords $vec_id_] x0 y0 x1 y1
            if { ($x0-$x1)*($y0-$y1) < 0 } {
               set anc "se"
    	    } else {
   	       set anc "sw"
            }

#  Create the text label. This will be drawn on top of the vector, and so will
#  cause the vector no longer to be the current object. This will trigger the
#  <LEAVE> binding, causing this method to be re-entered in order to erase the
#  label, etc, etc. BUT... we do not yet set lab_id_ (which is checked by the
#  above code), so the re-entered binding will do nothing, and we are saved
#  from an infinite loop.
            set id [$canvas_ create text [$canvas_ canvasx $x] \
	                                 [$canvas_ canvasy $y] \
			      -text $text -anchor $anc \
		              -fill $hgColour_ -font $hgFont_ ]

#  NOW save the canvas identifier for the label.
            set lab_id_ $id

#  Highlight and raise this vector.
            $canvas_ itemconfigure $vec_id_ -fill $hgColour_
            $canvas_ raise $vec_id_

#  Get the current state of the vector.
            set vec_st_ $vstates_($vec_id_)
         }
      }
   }

#  Find the anticlockwise angle in degrees from vertical (-ve canvas Y) to
#  north within the displayed image. If north varies over the image, then
#  a blank value is returned.
#  ---------------------------------------------------------------------
   protected method meanNth {equ} {

#  Get the width and height of the displayed image in pixels
      set wd [$rtdimage_ width]
      set ht [$rtdimage_ height]

#  Find the anti-clockwise angle from -ve canvas Y to north at the 4
#  corners of the image and at the middle (in radians).
      set n(1) [fndNth 0 0 $equ]
      set n(2) [fndNth 0 $ht $equ]
      set n(3) [fndNth [expr $wd/2] [expr $ht/2] $equ]
      set n(4) [fndNth $wd 0 $equ]
      set n(5) [fndNth $wd $ht $equ]

#  If any of these values are unknown, return an unknown value.
      if { $n(1) == "" || $n(2) == "" || $n(3) == "" || $n(4) == "" || \
          $n(5) == "" } {
         set ret ""

#  Otherwise...
      } else {

#  Find the mean of the sin and cos values.
         set smc 0.0
         set sms 0.0
         for { set i 1 } { $i < 6 } { incr i } {
            set sms [expr $sms + [lindex $n($i) 1] ]
            set smc [expr $smc + [lindex $n($i) 0] ]
         }

         set mnsn [expr $sms / 5.0 ]
         set mncs [expr $smc / 5.0 ]

#  Find the cos of the angular deviation between the first north value and
#  the mean north value.
         set dev [expr $mncs*[lindex $n(1) 0] + $mnsn*[lindex $n(1) 1]]

#  Find the largest deviation (i.e. the smallest cos value) for all points.
         set mn $dev
         for { set i 2 } { $i < 6 } { incr i } {
            set dev [expr $mncs*[lindex $n($i) 0] + $mnsn*[lindex $n($i) 1]]
            if { $dev < $mn } { set mn $dev }
         }

#  If the range of deviations is less than +/- 1 degree return the mean value.
#  Otherwise return a blank string indicating that the direction of north
#  varies across the image.
         if { $mn > 0.9998477 } {
            set ret [expr 57.29578*atan2( $mnsn, $mncs ) ]
         } else {
            set ret ""
         }
      }
   }

#  Returns a "nice" round value close to a supplied value.
#  -------------------------------------------------------
   protected method nice { x } {

#  Find the absolute value of the supplied value.
      if { $x < 0.0 } {
         set y [expr -($x)]
      } else {
         set y $x
      }

#  Split the absolute number into a power of ten (n) and a mantissa (a)
#  between zero and ten.
      set l [expr log10($y)]
      set n [expr floor($l)]
      set a [expr pow(10.0,($l-$n) )]

#  Convert the mantissa into a nice round number (the closest of 1, 2,
#  4 or 5). If the mantissa is above 7.5, use 1 and increase the exponent
#  by 1.
      if { $a < 1.5 } {
         set a 1
      } elseif { $a < 3 } {
         set a 2
      } elseif { $a < 4.5 } {
         set a 4
      } elseif { $a < 7.5 } {
         set a 5
      } else {
         set a 1
         set n [expr $n + 1]
      }

#  Return the combination of the exponent and mantissa, with the correct
#  sign.
      if { $x < 0.0 } {
         return [expr -($a*pow(10.0, $n))]
      } else {
         return [expr ($a*pow(10.0, $n))]
      }
   }

#  Draw new vectors or reconfigure existing vectors.
#  -------------------------------------------------
   protected method plot {cat style rows usty ssty} {

#  Initialise the number of new vectors created by this call.
      set ret 0

#  Get data from the catalogue. This is a list of rows, in which
#  each row is a list of column values.
      set data [$cat getData]

#  Get access to an array of row states (selected, unselected, deleted)
#  indexed by row number.
      upvar 0 [$cat getStates] states

#  Find the total number of vectors for display.
      set ntot [expr [$cat getNsel] + [$cat getNuns]]

#  Find the number of rows to draw or reconfigure.
      if { $rows != "" } {
         set nrow [llength $rows]
      } else {
         set nrow [$cat getNrow]
      }

#  Get the maximum number of vectors to draw.
      set nvec [$style getNvec]

#  Store the probability of skipping over any single vector.
      if { $nvec != "" } {
         set prob [expr 1.0 - (double($nvec)/double($ntot))]
      } else {
         set prob 0.0
      }

#  Get the indices of the columns defining vector length (in arbitrary
#  units) and angle (in degrees anti-clockwise from the reference
#  direction - which is north if the catalogue has WCS and vertically
#  upwards, i.e. canvas -Y, if not. Return if either of these column is
#  not found.
      set lcol [$style getLcoli $cat]
      if { $lcol < 0 } { return }

      set acol [$style getAcoli $cat]
      if { $acol < 0 } { return }

#  Find the size of an image pixel in canvas pixels.
      $rtdimage_ convert coords 0.0 0.0 "image" cx0 cy0 "canvas"
      $rtdimage_ convert coords 1.0 1.0 "image" cx1 cy1 "canvas"
      set xsz [expr abs( $cx1 - $cx0 )]
      set ysz [expr abs( $cy1 - $cy0 )]

#  Get the scale factor for vector length (image pixels per unit column
#  value). If no value has been set, a default value is calculated on the
#  basis of the supplied data.
      set mag [$style getMagd $cat $rtdimage_]

#  Get the angle to add on to the angle column values (degrees).
      set rot [$style getArot]

#  Get the pixel origin of the displayed image. If the vectors are
#  displayed over a blank image use the lower pixel bounds stored with
#  the catalogue. Otherwise, use the origin of the displayed ndf.
      if { [info exists blankref_($rtdimage_)] &&
           $blankref_($rtdimage_) > 0 } {
         lassign [$cat getPixBounds] xo yo xh yh
      } else {
         $rtdimage_ origin xo yo
      }

#  Find the offset from pixel coords to grid coords.
      set ox_ [expr $xo - 1.5]
      set oy_ [expr $yo - 1.5]

#  Initialise afac
      set afac 1.0

#  If both the catalogue and the image have WCS, align in ra/dec.
      if { [$cat gotWcs] && [$rtdimage_ astcelestial] == "1" } {

#  Get the indices of the Ra and Dec columns, and the equinox.
         set col1 [$cat getRaCol]
         if { $col1 > -1 } { set col2 [$cat getDecCol] }
         if { $col1 > -1 && $col2 > -1 } {
            set equ [$cat getEquinox]

#  See if north is in the same direction over the entire image. If so,
#  the anti-clockwise angle in degrees from upwards (-ve Y) to north is
#  returned. Otherwise, a blank string is returned.
            set north [meanNth $equ]

#  If the angle is fixed, add it onto the rot value. Also store the name
#  of the procedure to use to find the canvas coords at the end points of
#  the vector.
            if { $north != "" } {
               set rot [expr $rot + $north]
               set draw draw1
            } else {
               set draw draw2
            }

#  Store units.
            set units "deg $equ"

#  Store zero offsets
            set ox 0
            set oy 0

#  Set the factor needed to convert RTD longitude values to degrees. This
#  is 15 for RA axes and 1 for other type of longitude axis.
            set sys [$rtdimage_ astget "System"]
            if { $sys != "FK5" && $sys != "FK4" && $sys != "ICRS" &&
                 $sys != "GAPPT" } {
               set afac 15.0
            }

         } else {
            error_dialog "RA and DEC columns are not available.\nSee the\"Column names\" panel."
            return 1
         }

#  If either the displayed image or the catalogue does not have WCS we
#  cannot align on the sky, so align in pixel coords.
      } else {

#  Use the X and Y columns.
         set col1 [$cat getXCol]
         if { $col1 >= 0 } { set col2 [$cat getYCol] }
         if { $col1 >= 0 && $col2 >= 0 } {

#  If the catalogue does have WCS but the image does not, the angles in the
#  catalogue will be relative to north, but we will not know where north
#  is because the image does not have WCS. Issue a warning and assume
#  that north is upwards. Only do this if the warning has not already been
#  issued.
            if { [$cat gotWcs] && ![$cat getWarned] } {
               error_dialog "The catalogue specifies vectors with respect to north, but no WCS information is available for the displayed image. Continuing on the assumption that north is vertical."
               $cat setWarned
            }

#  Store the offset from pixel coords to grid coords.
            set ox $ox_
            set oy $oy_

#  Store the name of the procedure to use to find the canvas coords at the
#  end points of the vector.
            set draw draw1

#  Store units.
            set units "image"

         } else {
            error_dialog "X and Y columns are not available.\nSee the\"Column names\" panel."
            return 1
         }
      }

#  Check for and remove the "pretend" option "-flash" in the options setting
#  string for unselected vectors.
      set gotuflash [regexp {\-flash +([0-9]+)} $usty match uflash]
      if { $gotuflash } {
         regsub {\-flash +([0-9]+)} $usty "" newsty
         set usty $newsty
      }

#  Check for and remove the "pretend" option "-flash" in the options setting
#  string for selected vectors.
      set gotsflash [regexp {\-flash +([0-9]+)} $ssty match sflash]
      if { $gotsflash } {
         regsub {\-flash +([0-9]+)} $ssty "" newsty
         set ssty $newsty
      }

#  Set up the progress bar accordingly.
      set inc [expr ($nrow+19)/20]
      set irow -1
      set j -1
      $pbar_ config -to $nrow
      update idletasks

#  Loop round each vector to be drawn or reconfigured.
      for {set i 0} {$i < $nrow} {incr i} {

#  Update the progress bar value at intervals.
         incr irow
         if { [incr j] == $inc } {
            set j -1
            $pbar_ config -value $irow
            update idletasks
         }

#  Get the index of the current row within the catalogue.
         if { $rows == "" } {
            set row $i
         } else {
            set row [lindex $rows $i]
         }

#  Get the required state for this row.
         set state $states($row)

#  Get the canvas ID for any existing vector for this row. A blank string
#  is obtained if there is currently no canvas item for the row.
         if { [catch { set cid $vectors_($row) }] } {
            set cid ""
         }

# Indicate that the canvas item ID for this row has not yet changed.
         set newcid 0

#  If the required state is "deleted", delete any existing canvas item
#  for this row.
         if { $state == "D" } {
            if { $cid != "" } {
               $canvas_ addtag "DD" withtag $cid
               unset vstates_($cid)
               unset vlens_($cid)
               unset vrows_($cid)

#  If we have just deleted the highlighted vector, delete the associated
#  label.
               if { $cid == $vec_id_ } { LabelBind 0 0 0 }

               set cid ""
               set newcid 1
            }

#  Otherwise, the required state is "selected" or "unselected".
         } else {

#  If no canvas item currently exists for this vector, create one now,
#  initially invisible. Skip over each vector with a probability of $prob,
#  in order to keep the number of displayed vectors down to something like
#  $nvec.
            if { $cid == "" && $prob <= [random] } {

#  Get the column values for this row.
               set rowdata [lindex $data $row]

#  Get the value of the column defining the vector length.
               set lval [lindex $rowdata $lcol]

#  Ignore zero length vectors.
               if { $lval != 0.0 } {

#  Get the vector length in the required units for drawing.
                  set len [expr $lval*$mag ]

#  Get the required vector angle.
                  set ang [expr 0.017453293*([lindex $rowdata $acol] + $rot) ]

#  Get the required vector centre position.
                  set a1 [expr [lindex $rowdata $col1] - $ox]
                  set a2 [expr [lindex $rowdata $col2] - $oy]

#  If a1 is a celestial longitude, but it is not an RA, multiply it
#  by 15 (i.e. convert from hours to degree) to get round an issue somewhere
#  (not sure where) in the RTD WCS system.
                  if { $afac != 1.0 } {
                     set a1 [expr $a1*$afac]
                  }

#  Get the canvas coords at the end points of the vector.
                  $draw $rtdimage_ $a1 $a2 $units $xsz $ysz $len $ang \
                                   cx cy ex ey px py

#  Create the vector, if draw succeeded.
                  if { [info exists cx] } {
                     set cid [$canvas_ create line $cx $cy $ex $ey -fill "" \
                                 -tags $disid_]
#  Increment the number of new vectors created by this call.
                     incr ret

#  Save information about this canvas item.
                     set vlens_($cid) $lval
                     set vrows_($cid) $row
                     set newcid 1
                  }
               }
            }

#  Now tag the canvas item appropriately for the required state. Note, we may
#  still not have a canvas item since the vector may have zero length, so
#  check $cid first.
            if { $cid != "" } {
               if { $state == "U" } {
                  $canvas_ addtag "UU" withtag $cid
                  set vstates_($cid) "U"

               } else {
                  $canvas_ addtag "SS" withtag $cid
                  set vstates_($cid) "S"
               }
            }
         }

#  Replace the current canvas ID in the vectors_ array.
         if { $newcid } {
             if { $cid != "" } {
                set vectors_($row) $cid
             } else {
                unset vectors_($row)
             }
         }
      }

#  Now configured the canvas items appropriately for the required states,
#  and assign the correct tag.
      eval $canvas_ itemconfigure UU $usty
      $canvas_ dtag UU "S$disid_"
      $canvas_ addtag "U$disid_" withtag UU
      $canvas_ dtag UU

      eval $canvas_ itemconfigure SS $ssty
      $canvas_ dtag SS "U$disid_"
      $canvas_ addtag "S$disid_" withtag SS
      $canvas_ dtag SS

      $canvas_ delete DD

#  Raise all selected vectors.
      $canvas_ raise "S$disid_"

#  Switch flashing on or off as appropriate.
      set flashucol_ [$style getUclr]
      set flashscol_ [$style getSclr]
      if { $gotuflash } { setFlash 0 $uflash }
      if { $gotsflash } { setFlash 1 $sflash }

#  Return the number of new vectors created by this call.
      return $ret

   }

#  Ensure the key is at the top of the display stack. The vector is at
#  the top, with the label underneath it, and the background underneath
#  that.
#  ------------------------------------------------------------------------
   protected method raiseKey {} {
      if { $kbid_ != "" } { $canvas_ raise $kbid_ }
      if { $klid_ != "" } { $canvas_ raise $klid_ }
      if { $kvid_ != "" } { $canvas_ raise $kvid_ }
   }

#  Called when button 1 is released over the vector map.
#  -----------------------------------------------------------------
   protected method ReleaseBind {x y} {

#  If we have been dragging the key, save the new image coords of the centre
#  of the text label.
      if { $root_item_ == "key" } {
         lassign [$canvas_ bbox $klid_] lx0 ly0 lx1 ly1
         $rtdimage_ convert coords [expr 0.5*( $lx0 + $lx1 )] \
                                   [expr 0.5*( $ly0 + $ly1 )] "canvas" \
                                   klx_ kly_ "image"

#  If we have not been been dragging a key...
      } elseif { $root_item_ != "akey" } {

#  We have no selection as yet
         set type ""

#  Find the bounds of the box or circle to be searched.
         if { $sarea_ != "" } {
            set x1 [lindex $sarea_ 0]
            set y1 [lindex $sarea_ 1]
            set x2 [lindex $sarea_ 2]
            set y2 [lindex $sarea_ 3]

#  Delete the selection box or circle.
            $canvas_ delete selreg
            set sarea_ ""

#  Set the type of selection to "box" or "circle".
            set type $selshape_

#  If no region was given...
         } else {

#  If the button was clicked over a vector, set the selection type to
#  "row".
            if { $root_item_ == "vector" } {
               set type "rows"

#  Otherwise, use a small box or circle centred on the root position.
            } else {
               set x1 [expr $rootx_ - 4]
               set y1 [expr $rooty_ - 4]
               set x2 [expr $rootx_ + 4]
               set y2 [expr $rooty_ + 4]
               set type $selshape_
            }
         }

#  If the user clicked on a vector, select it.
         if { $type == "rows" } {
            eval $selcmd_ $reset_ 0 "rows" $vrows_($root_id_)

#  If the user dragged out an area...
         } else {

#  Convert the canvas coords to X and Y column values, including the
#  shift of origin.

            $rtdimage_ convert coords $x1 $y1 "canvas" xc1 yc1 "image"
            set xc1 [expr $xc1 + $ox_]
            set yc1 [expr $yc1 + $oy_]

            $rtdimage_ convert coords $x2 $y2 "canvas" xc2 yc2 "image"
            set xc2 [expr $xc2 + $ox_]
            set yc2 [expr $yc2 + $oy_]

#  Select all vectors in the area.
            eval $selcmd_ $reset_ 0 $selshape_ \{ $xc1 $xc2 $yc1 $yc2 \}

         }
      }

#  Remove the bindings (except for the button press bindings which are
#  left in place in order to allow new selections to be made).
      widgetbind $this $canvas_ <ButtonRelease-1> remove {}
      widgetbind $this $canvas_ <B1-Motion> remove {}

#  Indicate that another <ButtonPress> event can now be processed.
      set sb_active_ 0
   }

#  Remove the bindings set up by setBindings.
#  -----------------------------------------
   protected method resetBindings {} {
      if { $bound_ } {
         widgetbind $this $canvas_ <ButtonPress-1> remove {}
         widgetbind $this $canvas_ <Control-Shift-ButtonPress-1> remove {}
         canvasbind $this $canvas_ $disid_ <ButtonPress-1> remove {}
         canvasbind $this $canvas_ $disid_ <Control-Shift-ButtonPress-1> remove {}

         if { $hgEnabled_ } {
            canvasbind $this $canvas_ $disid_ <Enter> remove {}
            canvasbind $this $canvas_ $disid_ <Leave> remove {}
         }

         if { $image_id_ != "" } {
            canvasbind $this $canvas_ $image_id_ <ButtonPress-1> remove {}
            canvasbind $this $canvas_ $image_id_ <Control-Shift-ButtonPress-1> remove {}
         }

         set bound_ 0
      }
   }

#  Clear the progress bar etc.
#  --------------------------
   protected method resetHold {} {
      blt::busy release $w_
      $pbar_ reset
      update idletasks
   }

#  Set up canvas bindings used to control cursor vector selection and
#  highlighting.
#  ------------------------------------------------------------------
   protected method setBindings {} {

#  Only set them up if they have not been set up already.
      if { !$bound_ } {

#  Find the canvas id for the image.
         set image_id_ ""
         foreach id [$canvas_ find all] {
            if { [$canvas_ type $id] == "image" } {
               set image_id_ $id
               break
            }
         }

#  Set up bindings for the whole canvas so that procedure SingleBind
#  is called when button 1 is pressed anywhere in the canvas. If the
#  shift and control keys are also pressed, set the 3rd arg to 0.
         widgetbind $this $canvas_ <ButtonPress-1> add "[code $this SingleBind %x %y 1]"
         widgetbind $this $canvas_ <Control-Shift-ButtonPress-1> add "[code $this SingleBind %x %y 0]"

#  Now set up bindings for the image so that procedure SingleBind
#  is called when button 1 is pressed anywhere in the image. If the
#  shift and control keys are also pressed, set the 3rd arg to 0.
         if { $image_id_ != "" } {
            canvasbind $this $canvas_ $image_id_ <ButtonPress-1> add "[code $this SingleBind %x %y 1]"
            canvasbind $this $canvas_ $image_id_ <Control-Shift-ButtonPress-1> add "[code $this SingleBind %x %y 0]"
         }

#  Now set up bindings for the image so that procedure SingleBind is called
#  when button 1 is pressed over any vector drawn by this class. If the
#  shift and control keys are also pressed, set the 3rd arg to 1.
         canvasbind $this $canvas_ $disid_ <ButtonPress-1> add "[code $this SingleBind %x %y 1]"
         canvasbind $this $canvas_ $disid_ <Control-Shift-ButtonPress-1> add "[code $this SingleBind %x %y 0]"

#  Now set up bindings for the canvas so that procedure LabelBind
#  is called when the pointer enters or leaves a vector.
         if { $hgEnabled_ } {
            canvasbind $this $canvas_ $disid_ <Enter> add "[code $this LabelBind 1 %x %y]"
            canvasbind $this $canvas_ $disid_ <Leave> add "[code $this LabelBind 0 %x %y]"
         }

#  Indicate that the bindings have now been set up.
         set bound_ 1
      }
   }

#  Switch flashing on or off
#  -------------------------
   protected method setFlash {sel flash} {
      if { $sel } {
         if { $flash } {
            if { !$flashsel_ } {
               set flashsel_ 1
               flash 1 1
            }
         } else {
            if { $flashsel_ } {
               set flashsel_ 0
            }
         }
      } else {

         if { $flash } {
            if { !$flashuns_ } {
               set flashuns_ 1
               flash 0 1
            }
         } else {
            if { $flashuns_ } {
               set flashuns_ 0
            }
         }
      }
   }

#  Indicate what is going on.
#  --------------------------
   protected method setHold {text} {
      blt::busy hold $w_ -cursor "watch"
      $pbar_ reset
      $pbar_ config -text $text
      update idletasks
   }

#  Called when button 1 is pressed over the vector map. It sets up the
#  bindings which handle the selection of vectors using the cursor, and
#  the dragging of the key.
#  ----------------------------------------------------
   protected method SingleBind {x y reset} {

      if { !$sb_active_ } {

#  Get the canvas identifier for the top-most item under the pointer.
         set root_id_ [$canvas_ find withtag current]

#  If there is no canvas item under the pointer, then note it.
         if { $root_id_ == "" } {
            set root_item_ ""

#  Otherwise, get a list of tags for the item under the pointer.
         } else {
            set tags [$canvas_ gettags $root_id_]

#  If the current item is part of the key created by this PolDisp, then note
#  it.
            if { $root_id_ == $kbid_ || $root_id_ == $klid_ || $root_id_ == $kvid_ } {
               set root_item_ key

#  If the current item is part of the key created by another PolDisp, then
#  note it.
            } elseif { [lsearch -exact $tags PolKey] != -1 } {
               set root_item_ akey

#  If the current item is part of the key created by another PolDisp,
#  then note it.
            } elseif { [lsearch -exact $tags $disid_ ] != -1 } {
               set root_item_ vector

#  If the current item is a vector drawn by this PolDisp, then note it.
            } elseif { [lsearch -exact [$canvas_ gettags $root_id_] $disid_ ] != -1 } {
               set root_item_ vector

#  Otherwise, note that the root object is something else.
            } else {
               set root_item_ ""
            }
         }

#  Convert the screen coords to canvas coords, and record this position as
#  the "root" position which is available for use by other procedures.
         set rootx_ [$canvas_ canvasx $x]
         set rooty_ [$canvas_ canvasy $y]

#  Indicate if the selection is to be reset before including the chosen
#  vectors.
         set reset_ $reset

#  Set up bindings to allow ther user to drag out a region.
         widgetbind $this $canvas_ <ButtonRelease-1> add "[code $this ReleaseBind %x %y]"
         widgetbind $this $canvas_ <B1-Motion> add "[code $this B1MotionBind %x %y]"

#  Indicate that binds have been set up to drag.
         set sb_active_ 1
      }
   }


#  Zoom to a displayed region. The arguments are the canvas coords of the
#  bounding box to be zoomed into.
#  ----------------------------------------------------------------------
   protected method zoom_to_region {x0 y0 x1 y1} {
      set dw [$image_ dispwidth]
      set dh [$image_ dispheight]
      set cw [winfo width $canvas_]
      set ch [winfo height $canvas_]
      if {$cw != 1 && $dw && $dh} {

#  Create a box to use as a guide to the area to be zoomed into.
         set box_id [$canvas_ create rectangle $x0 $y0 $x1 $y1 -outline ""]

#  Get the current zoom factors. Change negative values to fractional
#  values.
         lassign [$rtdimage_ scale] xs0 ys0
         if { $xs0 < 0 } { set xs0 [expr -1.0/$xs0] }
         if { $ys0 < 0 } { set ys0 [expr -1.0/$ys0] }

#  Find the zoom factors which make the image fit the marked region.
         set rw [expr abs($x1-$x0)]
         if { $rw < 0.01 } { set rw 0.01 }
         set rh [expr abs($y1-$y0)]
         if { $rh < 0.01 } { set rh 0.01 }
         set tw [max $dw $cw]
         set th [max $dh $ch]
         set xs [expr $xs0*(double($cw)/double($rw))]
         set ys [expr $ys0*(double($ch)/double($rh))]

#  Convert to integers (factors less than 1 become negative integers).
         if { $xs < 1 } {
            set xs [expr -int(1.0/$xs)-1]
         } else {
            set xs [expr int($xs)]
         }

         if { $ys < 1 } {
            set ys [expr -int(1.0/$ys)-1]
         } else {
            set ys [expr int($ys)]
         }

#  Limit the integers to the available range, ensure equals scales are
#  used on both axes, and store the new scales.
         set scale [max [$image_ cget -min_scale] [min $xs $ys [$image_ cget -max_scale]]]
         $image_ scale $scale $scale
         update idletasks

#  Now scroll to new position. Get the new bounding box for the rectangle
#  created above, and then delete the item.
         lassign [$canvas_ bbox $box_id] x0 y0 x1 y1
         $canvas_ delete $box_id
         set x [expr ($x1+$x0)/2.0]
         set y [expr ($y1+$y0)/2.0]
         set dw [$image_ dispwidth]
         set dh [$image_ dispheight]
         $canvas_ xview moveto [expr (($x-$cw/2.0)/$dw)]
         $canvas_ yview moveto [expr (($y-$ch/2.0)/$dh)]

      }
   }

#  Static methods (procs)
#  ======================

#  Allows a GaiaPolDisp to register or de-register bindings with a given
#  widget. $disp is the GaiaPolDisp, $w is the widget, $ev is the event,
#  $opt is "add" or "remove", $cmd is the command to be bound to the event
#  (only used if $opt is "add").
#
#  Any bindings which exist before the first GaiaPolDisp is created are
#  saved and removed before binding the supplied command to the event.
#  These original bindings are re-instated when the bindigns for the final
#  active GaiaPolDisp are removed.
#  ----------------------------------------------------------------------
   proc widgetbind {disp w ev opt cmd} {

#  First deal with cases where a supplied binding is to be added to the
#  widget.
      if { $opt == "add" } {

#  Has any original binding (present before the first active GaiaPolDisp
#  was created) been saved? If not save it now in the wobind_ class array,
#  and remove it from the widget.
         if { ![info exists wobind_($w,$ev)] } {
            if { ![catch { set b  [bind $w $ev] } msg] } {
               set wobind_($w,$ev) $b
               bind $w $ev ""
            } else {
               set wobind_($w,$ev) ""
            }
         }

#  Add the new binding.
         bind $w $ev "+$cmd"

#  Save the binding in a common array.
         set wbind_($disp,$w,$ev) $cmd

#  Now deal with cases where a binding is to be removed from the widget.
      } elseif { $opt == "remove" } {

#  Remove the entry in the class array which holds all bindings.
         unset wbind_($disp,$w,$ev)

#  Remove all bindings from the widget for this event.
         bind $w $ev ""

#  Get a list of any bindings for the GaiaPolDisp class which now
#  need to be reinstated.
         set bs [array names wbind_ "*,$w,$ev" ]

#  If there are now none left, reinstate the original bindings present
#  before the first active GaiaPolDisp was created.
         if { $bs == "" } {
            if { $wobind_($w,$ev) != "" } {
               bind $w $ev $wobind_($w,$ev)
            }
            unset wobind_($w,$ev)

#  Otherwise, reinstate all remaining bindings for this class.
         } else {
            foreach b $bs {
               bind $w $ev "+$wbind_($b)"
            }
         }
      }
   }

#  Allows a GaiaPolDisp to register or de-register bindings with a given
#  canvas item. $disp is the GaiaPolDisp, $c is the canvas, $tag is the
#  item, $ev is the event, $opt is "add" or "remove", $cmd is the
#  command to be bound to the event (only used if $opt is "add").
#
#  Any bindings which exist before the first GaiaPolDisp is created are
#  saved and removed before binding the supplied command to the event.
#  These original bindings are re-instated when the bindigns for the final
#  active GaiaPolDisp are removed.
#  ----------------------------------------------------------------------
   proc canvasbind { disp c tag ev opt cmd} {

#  First deal with cases where a supplied binding is to be added to the
#  canvas item.
      if { $opt == "add" } {

#  Has any original binding (present before the first active GaiaPolDisp
#  was created) been saved? If not save it now in the cobind_ class array,
#  and remove it from the canvas item.
         if { ![info exists cobind_($c,$tag,$ev)] } {
            if { ![catch { set b  [$c bind $tag $ev] } msg] } {
               set cobind_($c,$tag,$ev) $b
               $c bind $tag $ev ""
            } else {
               set cobind_($c,$tag,$ev) ""
            }
         }

#  Add the new binding.
         $c bind $tag $ev "+$cmd"

#  Save the binding in a common array.
         set cbind_($disp,$c,$tag,$ev) $cmd

#  Now deal with cases where a binding is to be removed from the canvas item.
      } elseif { $opt == "remove" } {

#  Remove the entry in the class array which holds all bindings.
         unset cbind_($disp,$c,$tag,$ev)

#  Remove all bindings from the canvas item for this event.
         $c bind $tag $ev ""

#  Get a list of any bindings for the GaiaPolDisp class which now
#  need to be reinstated.
         set bs [array names cbind_ "*,$c,$tag,$ev"]

#  If there are now none left, reinstate the original bindings present
#  before the first active GaiaPolDisp was created.
         if { $bs == "" } {
            if { $cobind_($c,$tag,$ev) != "" } {
               $c bind $tag $ev $cobind_($c,$tag,$ev)
            }
            unset cobind_($c,$tag,$ev)

#  Otherwise, reinstate all remaining bindings for this class.
         } else {
            foreach b $bs {
               $c bind $tag $ev "+$cbind_($b)"
            }
         }
      }
   }

#  Returns a uniform random number in range 0 - 1.
#  -----------------------------------------------
   proc random {} {
      set r_ [expr fmod( $r_*41475557.0, 1.0 )]
      return $r_
   }

#  Returns the canvas coords of the two ends of a vector (angles
#  specified relative to -ve canvas Y axis ("upwards") ).
#
#     rtdim - The rtdimage on which the vector is to be drawn
#     a1 a2 - The vector position
#     units - The units of a1 and a2 ("image" or "deg $equ")
#     xsz   - Width of an image pixel in canvas units
#     ysz   - Height of an image pixel in canvas units
#     len   - length of the vector in image pixels
#     ang   - anticlockwise angle from upwards (-ve canvas Y) to the vector,
#             in radians.
#     ncx ncy - Names of variables to receive the canvas X,Y at one end
#               of the vector.
#     nex ney - Names of variables to receive the canvas X,Y at the other
#               end of the vector.
#     npx npy - Names of variables to receive the canvas X,Y at the centre
#               of the vector.
#
#------------------------------------------------------------------------
   proc draw1 { rtdim a1 a2 units xsz ysz len ang ncx ncy nex ney npx npy} {
      upvar $ncx cx
      upvar $ncy cy
      upvar $nex ex
      upvar $ney ey
      upvar $npx px
      upvar $npy py

#  Convert the given position to canvas coords.
      $rtdim convert coords $a1 $a2 $units px py "canvas"

#  Correct by adding 0.5 onto each (why is this necessary? without it
#  vectors are plotted half a pixel out when plotted over an existing
#  image). Catch errors caused by non-numeric pos values (eg "-inf").
      if { ![catch {set px [expr $px + 0.5]}] &&
           ![catch {set py [expr $py + 0.5]}] } {

#  Check for very large values (eg AST__BAD "-1.79769313486231571D+308").
         if { ![catch {set ss [expr abs( $px ) + abs( $py ) ]}] &&
              $ss < 1.0E20 } {

#  Get canvas coords at the two ends of the vector.
            set dx [expr $xsz*$len*0.5*sin( $ang )]
            set dy [expr $ysz*$len*0.5*cos( $ang )]
            set cx [expr $px + $dx]
            set cy [expr $py + $dy]
            set ex [expr $px - $dx]
            set ey [expr $py - $dy]
         }
      }
   }

#  Returns the canvas coords of the two ends of a vector (angles
#  specified relative to north).
#
#     rtdim - The rtdimage on which the vector is to be drawn
#     a1 a2 - The vector ra and dec
#     units - The units of a1 and a2 (must be "deg $equ")
#     xsz   - Width of an image pixel in canvas units
#     ysz   - Height of an image pixel in canvas units
#     len   - length of the vector in image pixels
#     ang   - anticlockwise angle from north to the vector, in radians.
#     ncx ncy - Names of variables to receive the canvas X,Y at one end
#               of the vector.
#     nex ney - Names of variables to receive the canvas X,Y at the other
#               end of the vector.
#     npx npy - Names of variables to receive the canvas X,Y at the centre
#               of the vector.
#
#------------------------------------------------------------------------
   proc draw2 { rtdim a1 a2 units xsz ysz len ang ncx ncy nex ney npx npy} {
      upvar $ncx cx
      upvar $ncy cy
      upvar $nex ex
      upvar $ney ey
      upvar $npx px
      upvar $npy py

#  Convert the supplied position to canvas coords.
      $rtdim convert coords $a1 $a2 $units px py "canvas"

#  Convert a second position about 1 arcsec to the north of the supplied
#  position.
      $rtdim convert coords $a1 [expr $a2 + 0.0003 ] $units px2 py2 "canvas"

#  Correct by adding 0.5 onto each (why is this necessary? without it
#  vectors are plotted half a pixel out when plotted over an existing
#  image). Catch errors caused by non-numeric pos values (eg "-inf").
      if { ![catch {set px [expr $px + 0.5]}] &&
           ![catch {set py [expr $py + 0.5]}] &&
           ![catch {set px2 [expr $px2 + 0.5]}] &&
           ![catch {set py2 [expr $py2 + 0.5]}] } {

#  Check for very large values (eg AST__BAD "-1.79769313486231571D+308").
         if { ![catch {set ss [expr abs( $px ) + abs( $py ) + abs( $px2 ) + abs( $py2 )]}] &&
                       $ss < 1.0E20 } {

#  Get canvas coords at the two ends of the vector.
            set cos [expr cos( $ang )]
            set sin [expr sin( $ang )]
            set ofx [expr $px - $px2]
            set ofy [expr $py - $py2]
            set amp [expr 0.5*$len/sqrt( $ofx*$ofx + $ofy*$ofy )]
            set dx [expr $xsz*$amp*( $ofy*$sin + $ofx*$cos )]
            set dy [expr $ysz*$amp*( $ofy*$cos - $ofx*$sin )]
            set cx [expr $px + $dx]
            set cy [expr $py + $dy]
            set ex [expr $px - $dx]
            set ey [expr $py - $dy]
         }
      }
   }

   proc StackDump {} {
      for {set i 1} { $i < [info level] } { incr i} {
         puts [info level $i]
      }
      puts " "
   }

#  Options:
#  ========

#  Protected data members:
#  =======================
   protected {

#  Are the vectors displayed over a blank image?
      variable blankimage_ 0

#  A command to call to add an undoable action to the current list of
#  undoable actions. The command is called with 3 args;
#  1 - the string "cat" or "style" indicating if a cat or style has changed
#  2 - a pointer to the new cat or style object
#  3 - a description of the action which created the new cat or style
      variable actcmd_ ""

#  Are canvas bindings currently set up?
      variable bound_ 0

#  Canvas window containing main image
      variable canvas_ ""

#  A clone of the most recently drawn catalogue (GaiaPolCat).
      variable cat_ ""

#  Original canvas bindings.
      variable cb_

#  An identifying string for this GaiaPolDisp
      variable disid_ ""

#  Flash unselected vectors?
      variable flashuns_ 0

#  Flash selected vectors?
      variable flashsel_ 0

#  Flash colout for selected and unselected vectors.
      variable flashscol_ ""
      variable flashucol_ ""

#  Highlighting options...
      variable hgEnabled_ 1
      variable hgColour_ "#0f0"
      variable hgFmt_ "%.1f"
      variable hgFont_ ""

#  Original image bindings.
      variable ib_

#  Name of GaiaImageCtrl widget.
      variable image_ ""

#  The canvas id of the image.
      variable image_id_ ""

#  Key options...
      variable kenabled_ 1
      variable klid_ ""
      variable klcol_ "#fff"
      variable klfont_ ""
      variable klfmt_ "%.1f"
      variable klx_    ""
      variable kly_    ""
      variable kvdef_ ""
      variable kvval_ ""
      variable kvid_ ""
      variable kvcol_ "current"
      variable kvwid_ "1"
      variable kbid_ ""
      variable kbgcol_ "#000"
      variable kbdcol_ ""
      variable kbdwid_ 1
      variable kpad_ "0.5c"

#  Canvas id of displayed vector label.
      variable lab_id_ ""

#  Previous vector scale
      variable mag_ ""

#  Inhibit the newZoom function
      variable noNewZoom_ 0

#  The pixel origin used when the current map was plotted.
      variable ox_ 0
      variable oy_ 0

#  Progress bar widget.
      variable pbar_ ""

#  Is the current selection to be reset before including the chosen rows?
      variable reset_ 1

#  The canvas id of the selected object
      variable root_id_ ""

#  The type of selected object
      variable root_item_ ""

#  Position at which button was was pressed.
      variable rootx_ ""
      variable rooty_ ""

#  Name of rtdimage widget.
      variable rtdimage_ ""

#  The currently selected canvas area.
      variable sarea_ ""

#  Has the user pressed the left mouse button, but not yet released it?
      variable sb_active_ 0

#  Command to call when vectors are selected.
      variable selcmd_ ""

#  A clone of the most recently used style (GaiaPolStyle).
      variable style_ ""

#  The canvas id for the vector associated with currently displayed vector label.
      variable vec_id_ ""

#  The original state of the highlighted vector.
      variable vec_st_ ""

#  An array of canvas IDs, indexed by catalogue row number.
      variable vectors_

#  An array of vector lengths indexed by vector canvas ID.
      variable vlens_

#  An array of row indices indexed by vector canvas ID.
      variable vrows_

#  An array of vector states ("U" or "S") indexed by vector canvas ID.
      variable vstates_

#  Top level window.
      variable w_ ""

   }


#  Common (i.e. static) data members:
#  ==================================

#  An array of canvas item binding scripts, indexed by ($canvas,$tag,$event).
#  These are the bindings which existed before the first GaiaPolDisp was
#  created.
   common cobind_

#  An array of canvas item binding scripts, indexed by
#  ($this,$canvas,$tag,$event). These are the bindings which are created by
#  instances of GaiaPolDisp.
   common cbind_

#  An array of widget binding scripts, indexed by ($widget,$event). These are
#  the bindings which existed before the first GaiaPolDisp was created.
   common wobind_

#  An array of widget binding scripts, indexed by ($this,$widget,$event).
#  These are the bindings which are created by instances of GaiaPolDisp.
   common wbind_

#  A count of the number of GaiaPolDisps  created so far.
   common count_ 0

#  The number of catalogues displayed over each blank image created by
#  instances of this class.
   common blankref_

#  Random number seed
   common r_  0.123456

#  Shape of selection region.
   common selshape_ "box"

#  End of class definition.
}
