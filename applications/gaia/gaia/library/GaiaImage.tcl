#+
#  Name:
#     GaiaImage.tcl

#  Purpose:
#     Defines a class for extending the capabilities of the
#     RtdImage class.

#  Type of Module:
#     [incr Tk] class

#  Description:
#     This module defines a class that adds to the options defined by
#     the RtdImage class that are required for use by GAIA.

#  Invocation:
#     GaiaImage name [configuration options]

#  Notes:
#     This will only run with the gaia_wish installed as part
#     of the GAIA package with a Starlink extended RTD. Would be nice 
#     if using RtdImage didn't mean we had to delete the rtdimage
#     created by it.

#  Inherits:
#     Methods and configuration options of SkyCat (and Rtd).

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     PDRAPER: Peter Draper (STARLINK)
#     {enter_new_authors_here}

#  History:
#     24-SEP-1997 (PDRAPER):
#        Original version
#     19-JAN-1998 (PDRAPER):
#        Modified to use new focus following technique on canvas 
#        enter and leave events (this was causing problems under CDE
#        with users with a click-to-focus policy).
#     12-AUG-1998 (PDRAPER):
#        Added delete image to destructor. This really deletes it
#        (releasing the NDF for instance). Image type now rtdimage not 
#        starrtdimage.
#     {enter_changes_here}

#-
itk::usual GaiaImage {}

class gaia::GaiaImage {
   inherit rtd::RtdImage

   #  Create a new RtdImage widget. This is repeated from the base
   #  class to remove some options and add bindings.
   constructor {args} {

      #  Add a bindtag to the canvas so we can add bindings that will
      #  not be changed by others.
#      set tags [bindtags $canvas_]
#      lappend tags mytag$this
#      bindtags $canvas_ $tags

      #  Make arrow keys move mouse pointer by one pixel.
#      bind $canvas_ <Left> "$image_ warp -1 0"
#      bind $canvas_ <Right> "$image_ warp 1 0"
#      bind $canvas_ <Up> "$image_ warp 0 -1"
#      bind $canvas_ <Down> "$image_ warp 0 1"
#      global ::$w_.focus
#      set $w_.focus {}
#      bind $canvas_ <Enter> "+[code $this focus_ in]" 
#      bind $canvas_ <Leave> "+[code $this focus_ out]" 
#      bind $canvas_ <Configure> [code $this maybe_center]

      #  Remove options we're overriding from base class.
#      itk_option remove RtdImage::scrollbars
#      itk_option remove RtdImage::graphics
#      itk_option remove RtdImage::show_object_menu
#      itk_option remove RtdImage::drag_scroll
#      eval itk_initialize $args
   }

   #  Destructor: clear up any outstanding resources etc.
   destructor {
#      if { $after_id_ != {} } {
#         catch { after cancel $after_id_ }
#      }
#      #  Destroy image_ (should probably be done in RtdImage)
#      #  releasing all resources.
#      catch {image delete $image_}
   }

   #  Control the focussing of the canvas. Only take focus if the
   #  top-level window associated with this canvas has the focus
   #  (i.e. it's not in another toplevel somewhere). If this isn't
   #  done then mysterious raises of the main image window can occur
   #  with some window managers (mainly CDE, with click-to-focus).
   protected method focus_ {way} {
#      global ::$w_.focus
#      set top [winfo toplevel $w_]
#      set focus [focus -displayof $top]
#      if { $focus != {} } {
#         if { [winfo toplevel $focus] == "$top" } { 
#            
#            #  This toplevel has the focus (or at least a child of it
#            #  has), so it's ok to proceed.
#            if { $way == "in" } { 
#               set $w_.focus [focus -displayof .]
#               catch {focus $canvas_}
#            } else {
#               catch {focus [set $w_.focus]}
#            }
#         }
#      }
   }

   #  Display the toolbox window (repeated to use StarCanvasDraw,
   #  instead of CanvasDraw).
   method show_toolbox {} {
#      if {[$image_ isclear]} {
#         warning_dialog "No image is currently loaded" $w_
#         return
#      }
#      if {! [info exists itk_component(draw)]} {
#         # user must have deleted window...
#         itk_component add draw {
#            gaia::StarCanvasDraw $w_.draw \
#               -canvas $canvas_ \
#               -transient 1 \
#               -center 0 \
#               -withdraw 1 \
#               -show_object_menu 1 \
#               -clipping 0 \
#  	       -shorthelpwin $itk_option(-shorthelpwin) \
#	       -withtoolbox $itk_option(-withtoolbox) \
#               -defaultcursor $itk_option(-cursor) \
#               -show_object_menu $itk_option(-show_object_menu) \
#               -rtdimage $image_ \
#               -lowestitem $imageId_ \
#	       -regioncommand $itk_option(-regioncommand) \
#               -ignore_tag $itk_option(-grid_tag)
#         }
#      } else {
#         # $itk_component(draw) center_window
#         wm deiconify $itk_component(draw)
#         wm transient $itk_component(draw) $w_
#      }
   }


   #  Resize the image. Add the ability to rescale pixel-width objects
   #  correctly.
   method scale {x y} {
#      RtdImage::scale $x $y
#      $itk_component(draw) pixel_width_changed
   }

   #  Toggle rotation of the image and canvas items. Extended to add
   #  astrometry grid update.
   method rotate {bool} {
#      if {$bool != [$image_ rotate]} {
#         RtdImage::rotate $bool
#
#         #  Notify the astrometry grid to re-display itself if
#         #  asked.
#         if { $itk_option(-grid_command) != {} } {
#            eval $itk_option(-grid_command)
#         }
#      }
   }


   #  Flip or unflip the image and canvas items about the
   #  x or y axis, as given by $xy. Extended to add astrometry grid
   #  update.
   method flip {xy bool} {
#      if {$bool != [$image_ flip $xy]} {
#         RtdImage::flip $xy $bool
#
#         #  Notify the astrometry grid to re-display itself if
#         #  asked.
#         if { $itk_option(-grid_command) != {} } {
#            eval $itk_option(-grid_command)
#         }
#      }
   }

   #  Arrange to interactively create a spectrum line to display
   #  a graph of the image values along a given line. Changed to not
   #  prompt when wanted.
   method spectrum {{showinfo 1}} {
#      if {[$image_ isclear]} {
#         warning_dialog "No image is currently loaded" $w_
#         return
#      }
#
#      if {[winfo exists $w_.spectrum]} {
#         $w_.spectrum quit
#      }
#
#      if { $showinfo} {
#         set ok [action_dialog \
#                    "Press OK and then drag out a line over the image with button 1" \
#                    $w_]
#      } else {
#         set ok 1
#      }
#      if { $ok } {
#         $itk_component(draw) set_drawing_mode line [code $this make_spectrum]
#      }
   }

   #  Make a hard copy of the image display, just override to remove 
   #  ESO references?
   method print {} {
#        if {[$image_ isclear]} {
#            warning_dialog "No image is currently loaded" $w_
#            return
#        }
#        set object [$image_ object]
#        set file [file tail $itk_option(-file)]
#        set center [$image_ wcscenter]
#        set user [id user]
#        set app [lindex [winfo name .] 0]
#        set date [clock format [clock seconds] -format {%b %d, %Y at %H:%M:%S}]
#        utilReUseWidget RtdImagePrint $w_.print \
#            -image $this \
#            -show_footer 1 \
#            -whole_canvas 0 \
#            -transient 1 \
#            -top_left "GAIA/Skycat\n$object" \
#            -top_right "$file\n$center" \
#            -bot_left "$user/$app" \
#            -bot_right "$date"
   }

   #  Create a graph to display the image data values along the line
   #  just created.
   #  "line_id" is the canvas id of the line.
   #  Extended to call derived class that also saves slice as an
   #  image.
   method make_spectrum {line_id x0 y0 x1 y1} {
#      if {[winfo exists $w_.spectrum]} {
#         $w_.spectrum quit
#      }
#      gaia::GaiaImageSpectrum $w_.spectrum \
#         -x0 [expr int($x0)] \
#         -y0 [expr int($y0)] \
#         -x1 [expr int($x1)] \
#         -y1 [expr int($y1)] \
#         -image $this \
#         -transient 1 \
#         -shorthelpwin $itk_option(-shorthelpwin) \
#         -line_id $line_id
   }

   #  Methods to deal with the autoscroll when dragging off canvas.
   method start_autoscan_ {x y} {
#      set movex 0
#      set movey 0
#      if { $y >= [winfo height $canvas_]} {
#         set movey 10
#      } elseif {$y < 0} {
#         set movey -10
#      } elseif { $x >= [winfo width $canvas_]} {
#         set movex 10
#      } elseif {$x < 0} {
#         set movex -10
#      }
#      autoscan_ $movex $movey
   }
   method autoscan_ {movex movey} {
#      $canvas_ yview scroll $movey units
#      $canvas_ xview scroll $movex units
#      set after_id_ [after 50 [code $this autoscan_ $movex $movey]]
   }
   method cancelrepeat_ {} {
#      after cancel $after_id_
#      set after_id_ {}
   }

   # -- public vars --

   #  Flag: if true, display horizontal and vertical scrollbars,
   #  changed to default to 1.
   itk_option define -scrollbars scrollbars Scrollbars 1 {
      if {$itk_option(-scrollbars)} {
         if {![info exists itk_component(vscroll)]} {
            itk_component add vscroll {
               scrollbar $itk_component(vscrollf).vscroll \
                  -relief sunken \
                  -command [code $canvas_ yview]
            }
            pack $itk_component(vscroll) -side right -fill y
            $canvas_ config -yscrollcommand "$itk_component(vscroll) set"
         }
         if {![info exists itk_component(hscroll)]} {
            itk_component add hscroll {
               scrollbar $itk_component(hscrollf).hscroll \
                  -relief sunken \
                  -orient horiz \
                  -command [code $canvas_ xview]
            }
            pack $itk_component(hscroll) -side bottom -fill x
            $canvas_ config -xscrollcommand "$itk_component(hscroll) set"
         }
      } else {
         $canvas_ config -xscrollcommand "" -yscrollcommand ""
         if {[info exists itk_component(vscroll)]} {
            destroy $itk_component(vscroll)
            destroy $itk_component(hscroll)
            unset itk_component(vscroll)
            unset itk_component(hscroll)
         }
      }
   }

   #  Flag: if true, set bindings to scroll with the middle mouse
   #  button and make a depressed mouse button drag scroll the image.
   #  Note we use the mytag$this level tag for the cancel event as
   #  ButtonRelease-1 is used in other places.
   itk_option define -drag_scroll drag_scroll Drag_scroll 0 {
      if {$itk_option(-drag_scroll)} {
         bind $canvas_ <2> [code $canvas_ scan mark %x %y]
         bind $canvas_ <B2-Motion> [code $canvas_ scan dragto %x %y]
         set after_id_ {}
         bind mytag$this <ButtonRelease-1> [code $this cancelrepeat_]
         bind $canvas_ <B1-Leave> [code $this start_autoscan_ %x %y]
         bind $canvas_ <B1-Enter> [code $this cancelrepeat_]
      } else {
         bind $canvas_ <2> {}
         bind $canvas_ <B2-Motion> {}
         bind $canvas_ <B1-Leave> {}
         bind $canvas_ <B1-Enter> {}
      }
   }

   #  Flag: if true, create a CanvasDraw object to manage the canvas
   #  graphics. Extended to use StarCanvasDraw.
   itk_option define -graphics graphics Graphics 1 {
#      if {[catch {
#         if {$itk_option(-graphics) && ![info exists itk_component(draw)]} {
#            #  Create an object to manage the canvas graphics.
#            itk_component add draw {
#               gaia::StarCanvasDraw $w_.draw \
#                  -canvas $canvas_ \
#                  -transient 1 \
#                  -withdraw 1 \
#                  -center 0 \
#                  -shorthelpwin $itk_option(-shorthelpwin) \
#                  -withtoolbox $itk_option(-withtoolbox) \
#                  -defaultcursor $itk_option(-cursor) \
#                  -show_object_menu $itk_option(-show_object_menu) \
#                  -rtdimage $image_ \
#                  -lowestitem $imageId_ \
#                  -ignore_tag $itk_option(-grid_tag)
#            }
#            set_drawing_area
#
#            #  Clicking on the image or image background deselects
#            #  other objects.
#            $canvas_ bind $image_ <1> [code $itk_component(draw) deselect_objects]
#
#         } else {
#            $canvas_ config -cursor $itk_option(-cursor)
#         }
#      } msg]} {
#         # need this due to bug in itcl2.0 that causes errors in config code to be ignored
#         global ::errorInfo
#         puts stderr "$errorInfo"
#      }
   }

   #  Directory for colormap and ITT files. Hmm forgot why this is changed.
   itk_option define -cmap_dir cmap_dir Cmap_dir {} {
      if {!$colormap_initialized_} {
         global ::rtd_library
         if {"$itk_option(-cmap_dir)" == ""} {
            set itk_option(-cmap_dir) $rtd_library/colormaps
         }
         after 0 [ code $image_ cmap file \
                      $itk_option(-cmap_dir)/$itk_option(-default_cmap).$itk_option(-cmap_suffix)]
         after 0 [code $image_ itt file \
                     $itk_option(-cmap_dir)/$itk_option(-default_itt).$itk_option(-itt_suffix)]
         set colormap_initialized_ 1
      }
   }

   #  Flag: if true, display menus over graphic objects when selected with <3>
#   itk_option define -show_object_menu show_object_menu Show_object_menu 1

   #  Canvas tag for the astrometry grid items. This is set to the
   #  global value and shouldn't normally be changed.
#   itk_option define -grid_tag grid_tag Grid_Tag "ast_element"

   #  Command to re-draw the astrometry grid.
#   itk_option define -grid_command grid_command Grid_Command {}

   #  Component of the NDF that is displayed.
#   itk_option define -component component Component data

   # -- protected vars --

   #  Id of after event (used to autoscroll image).
   protected variable after_id_ {}
}



