#+
#  Name:
#     Gaia3dVtkWindow

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Wrapper class for vtkTkRenderWidget

#  Description:
#     This class creates an instance of vtkTkRenderWidget and sets up
#     the usual interactions used with GAIA3D. It also provides access
#     to the render widget.

#  Invocations:
#
#        Gaia3dVtkWindow object_name [configuration options]
#
#     This creates an instance of a Gaia3dVtkWindow object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Configuration options:
#     See itk_option define's below.

#  Methods:
#     See declarations below.

#  Inheritance:
#     util::FrameWidget

#  Copyright:
#     Copyright (C) 2007-2009 Science and Technology Facilities Council
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
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     29-JUN-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual Gaia3dVtkWindow {}

itcl::class gaia3d::Gaia3dVtkWindow {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      package require vtk

      #  Evaluate any options.
      eval itk_initialize $args

      #  Create the widget and renderer (requires width and height).
      create_renderer_widget_
   }

   #  Destructor:
   #  -----------
   destructor  {
      catch {
         #  Delete vtk objects, if not already done. Note this should
         #  really be done using vtkCommand DeleteAllObjects when the
         #  application exits.
         release
      }
   }

   #  Methods:
   #  --------

   #  Do rendering (after adding or removing props). Don't use "after idle",
   #  as that puts destruction out of sequence...
   public method render {} {
      $renwindow_ Render
   }

   #  Make renderer modified. Seems to help force a re-draw...
   public method modified {} {
      $renderer_ Modified
   }

   #  Do rendering after a small amount of time. This effectively queues fast
   #  calls to render so that they will be skipped (as they unwind with no
   #  VTK updates between them, at least I think that's what occurs).
   public method queue_render {} {
      after 10 [code $this interactor_render_]
   }
   protected method interactor_render_ {} {
      $interactor_ Render
   }

   #  Stop rendering.
   public method stop {} {
      $renwindow_ SetAbortRender 1
   }

   #  Handle destruction of any renderer windows and renderers.
   public method release {} {
      if { $interactor_ != {} } {
         $interactor_ Delete
         set interactor_ {}
      }
      if { $renderer_ != {} } {
         $renderer_ Delete
         set renderer_ {}
      }
      if { $renwindow_ != {} } {
         $renwindow_ Delete
         set renwindow_ {}
      }
      if { $lightkit_ != {} } {
         $lightkit_ Delete
         set lightkit_ {}
      }
   }

   #  Add a prop for rendering as part of the scene.
   public method add_view_prop {prop} {
      $renderer_ AddViewProp $prop
   }

   #  Remove a prop from rendering as part of the scene.
   public method remove_view_prop {prop} {
      $renderer_ RemoveViewProp $prop
   }

   #  Add an observer to the renderer so that events can be observed.
   public method add_renderer_observer {event cmd} {
      $renderer_ AddObserver $event $cmd
   }

   #  Remove an observer from the renderer.
   public method remove_renderer_observer {event} {
      $renderer_ RemoveObserver $event
   }

   #  Add an observer to the window so that events can be observed.
   public method add_window_observer {event cmd} {
      $renwindow_ AddObserver $event $cmd
   }

   #  Remove an observer from the window.
   public method remove_window_observer {event} {
      $renwindow_ RemoveObserver $event
   }

   #  Add an observer to the interactor so that events can be observed.
   public method add_interactor_observer {event cmd} {
      [get_interactor] AddObserver $event $cmd
   }

   #  Remove an observer from the interactor.
   public method remove_interactor_observer {event} {
      [get_interactor] RemoveObserver $event
   }

   #  Set the non-interacting minimum update rate required. Determines what
   #  mapper is used by a level of detail actor when not interacting.
   public method set_interactor_still_update_rate {value} {
      [get_interactor] SetStillUpdateRate $value
   }

   #  Set the interacting minimum update rate required. Determines what
   #  mapper is used by a level of detail actor when interacting via the mouse.
   public method set_interactor_desired_update_rate {value} {
      [get_interactor] SetDesiredUpdateRate $value
   }

   #  Set the expected update rate of the window to the currently desired
   #  value of the interactor. Call this when interacting with the scene and
   #  not using the interactor (i.e. mouse) so that the interactive response of
   #  level of detail actors is used. Undo with set_rate_to_still.
   public method set_rate_to_desired {} {
      $renwindow_ SetDesiredUpdateRate [[get_interactor] GetDesiredUpdateRate]
   }

   #  Set the render window to use the still rate as the desired rate. Undoes
   #  effects of set_rate_to_desired.
   public method set_rate_to_still {} {
      $renwindow_ SetDesiredUpdateRate [[get_interactor] GetStillUpdateRate]
   }

   #  Set the interactor mousing mode, trackerball or joystick (default).
   public method set_interaction_mode {mode} {
      if { $mode == "trackerball" } {
         [[get_interactor] GetInteractorStyle] SetCurrentStyleToTrackballCamera
      } else {
         [[get_interactor] GetInteractorStyle] SetCurrentStyleToJoystickCamera
      }
   }

   #  Set the background colour using VTK RGB values.
   public method set_rgb_background { R G B } {
      set rgb_background_ [list $R $G $B]
      if { $renderer_ != {} } {
         $renderer_ SetBackground $R $G $B
      }
   }

   #  Set the background colour using a Tk colour
   public method set_background { tk_colour } {
      set_rgb_background [get_rgb_colour $tk_colour]
   }

   #  Reset the camera.
   public method reset_camera {} {
      $renderer_ ResetCamera
   }

   #  Set the camera position. Some position, plus Z axis up.
   public method set_camera {x y z} {
      [$renderer_ GetActiveCamera] SetPosition $x $y $z
      [$renderer_ GetActiveCamera] SetViewUp 0 0 1
   }

   #  Set the camera so that a selected axis is pointed at the camera,
   #  with the other axes aligned to the viewport.
   public method point_axis_at_camera {iaxis} {

      #  Position we want for camera is fx,fy,[fz+d] (corrected for axis).
      #  That points back at fx,fy,fz.
      set camera [$renderer_ GetActiveCamera]
      lassign [$camera GetFocalPoint] fx fy fz
      set dist [$camera GetDistance]

      set vx 0
      set vy 0
      set vz 0

      if { $iaxis == 1 } {
         set vz 1
         set x [expr $fx+$dist]
         set y $fy
         set z $fz
      }  elseif  { $iaxis == 2 } {
         set vx 1
         set x $fx
         set y [expr $fy+$dist]
         set z $fz
      } else {
         set vy 1
         set x $fx
         set y $fy
         set z [expr $fz+$dist]
      }

      $camera SetViewUp $vx $vy $vz
      $camera SetPosition $x $y $z
      $camera OrthogonalizeViewUp
      $renderer_ ResetCameraClippingRange
      render
   }

   #  Centre the camera on the middle of all the visible props.
   public method centre {} {

      #  Position we want for camera is fx,fy,[fz+d] (corrected for axis).
      #  That points back at fx,fy,fz.
      set camera [$renderer_ GetActiveCamera]
      lassign [$renderer_ ComputeVisiblePropBounds] x0 x1 y0 y1 z0 z1
      $camera SetFocalPoint [expr ($x1-$x0)*0.5] [expr ($y1-$y0)*0.5] [expr ($y1-$y0)*0.5]

      #$renderer_ ResetCameraClippingRange
      render
   }

   #  Create a widget to display the renderer image, adding a renderer
   #  and setting up the default interactions.
   protected method create_renderer_widget_ {} {

      #  Create a widget to host the render window.
      itk_component add renwidget {
         ::vtkTkRenderWidget $w_.renwidget \
            -width $itk_option(-width) -height $itk_option(-height)
      } {
      }

      #  Get the widget's render window and establish a renderer for it.
      set renwindow_ [$itk_component(renwidget) GetRenderWindow]
      set renderer_ [::vtkRenderer New]
      $renwindow_ AddRenderer $renderer_

      #  Lights for the scene, do this early so we don't get the default
      #  lighting as well.
      add_lightkit_

      #  Initial backingstore state (funny order as options already
      #  configured as need width & height above).
      configure -backingstore_on $itk_option(-backingstore_on)

      #  Set up render window interactions.
      add_bindings_

      #  Set some basic properties of the renderer.
      eval set_rgb_background $rgb_background_

      #  Observe the progress of the renderer so that we can update the UI
      #  during long jobs and execute a command if given.
      add_renderer_observer ProgressEvent [code $this renderer_progress_]

      #  Pack renderer widget into place.
      pack $itk_component(renwidget) -in $w_ -side top -fill both -expand 1
   }

   #  Get the interactor. Need to defer until widget is realized.
   public method get_interactor {} {
      if { $interactor_ == {} } {
         set interactor_ [$renwindow_ GetInteractor]
      }
      return $interactor_
   }

   #  Act on progress event from renderer. Update the UI and execute
   #  an optional command.
   protected method renderer_progress_ {} {
      if { $itk_option(-progress_cmd) != {} } {
         set percent [expr [$renderer_ GetProgress]*100.0]
         eval $itk_option(-progress_cmd) $percent
      }

      #  UI update.
      ::update
   }

   #  Get the renderer (don't use).
   public method get_renderer {} {
      return $renderer_
   }

   #  Add lightkit to light up the scene.
   protected method add_lightkit_ {} {
      if { $lightkit_ == {} } {
         set lightkit_ [::vtkLightKit New]

         #  Set defaults, like the default lighting used in VTK.
         $lightkit_ SetKeyLightIntensity  1.0
         $lightkit_ SetKeyToFillRatio     10.0
         $lightkit_ SetKeyToBackRatio     10.0
         $lightkit_ SetKeyToHeadRatio     15.0
         $lightkit_ SetKeyLightElevation  0.0
         $lightkit_ SetKeyLightAzimuth    0.0
         $lightkit_ SetKeyLightWarmth     0.5
         $lightkit_ SetFillLightWarmth    0.5
         $lightkit_ SetBackLightWarmth    0.5
         $lightkit_ SetHeadLightWarmth    0.5
      }
      $renderer_ LightFollowCameraOn
      $lightkit_ AddLightsToRenderer $renderer_
   }

   #  Get the vtkLightKit object.
   public method get_lightkit {} {
      return $lightkit_
   }

   #-------------------------------------------------------------------------
   #  Bindings, don't use standard vtk::bind_tk_render_widget to avoid
   #  unwanted keyboard interactions ("q" and "e" that exit the application).
   #-------------------------------------------------------------------------
   protected method add_bindings_ {} {
      add_keyboard_bindings_
      ::gaia3d::bind_tk_widget $itk_component(renwidget) $renwindow_
      return
   }

   #  Add a custom binding mouse or keyboard binding.
   public method add_binding {tag cmd} {
      ::bind $itk_component(renwidget) $tag $cmd
   }

   #  Add bindings to control the scene view from the keyboard. These are
   #  needed for case when other interactions are occupying the mouse and
   #  some people like the additional accuracy they provide. Based around
   #  the arrow keys with various modifiers. Note all work on the camera
   #  not the actors, regardless.
   protected method add_keyboard_bindings_ {} {

      # Rotate
      ::bind $itk_component(renwidget) <Up> [code $this rotate_updown -5]
      ::bind $itk_component(renwidget) <Down> [code $this rotate_updown 5]
      ::bind $itk_component(renwidget) <Control-Up> [code $this rotate_updown -1]
      ::bind $itk_component(renwidget) <Control-Down> [code $this rotate_updown 1]
      ::bind $itk_component(renwidget) <Left> [code $this rotate_leftright 5]
      ::bind $itk_component(renwidget) <Right> [code $this rotate_leftright -5]
      ::bind $itk_component(renwidget) <Control-Left> [code $this rotate_leftright 1]
      ::bind $itk_component(renwidget) <Control-Right> [code $this rotate_leftright -1]

      # Zoom
      ::bind $itk_component(renwidget) <Shift-Up> [code $this zoom 1.02]
      ::bind $itk_component(renwidget) <Shift-Down> [code $this zoom 0.98]
      ::bind $itk_component(renwidget) <Control-Shift-Up> [code $this zoom 1.005]
      ::bind $itk_component(renwidget) <Control-Shift-Down> [code $this zoom 0.995]

      # Scroll
      ::bind $itk_component(renwidget) <Alt-Up> [code $this scroll 0 15]
      ::bind $itk_component(renwidget) <Alt-Down> [code $this scroll 0 -15]
      ::bind $itk_component(renwidget) <Alt-Left> [code $this scroll -15 0]
      ::bind $itk_component(renwidget) <Alt-Right> [code $this scroll 15 0]
      ::bind $itk_component(renwidget) <Control-Alt-Up> [code $this scroll 0 5]
      ::bind $itk_component(renwidget) <Control-Alt-Down> [code $this scroll 0 -5]
      ::bind $itk_component(renwidget) <Control-Alt-Left> [code $this scroll -5 0]
      ::bind $itk_component(renwidget) <Control-Alt-Right> [code $this scroll 5 0]

      # Roll
      ::bind $itk_component(renwidget) <Shift-Left> [code $this roll 5]
      ::bind $itk_component(renwidget) <Shift-Right> [code $this roll -5]
      ::bind $itk_component(renwidget) <Control-Shift-Left> [code $this roll 1]
      ::bind $itk_component(renwidget) <Control-Shift-Right> [code $this roll -1]

      # Undo
      ::bind $itk_component(renwidget) <KeyRelease-Up> [code $this set_rate_to_still]
      ::bind $itk_component(renwidget) <KeyRelease-Down> [code $this set_rate_to_still]
      ::bind $itk_component(renwidget) <KeyRelease-Left> [code $this set_rate_to_still]
      ::bind $itk_component(renwidget) <KeyRelease-Right> [code $this set_rate_to_still]

      #  Point axis at camera.
      ::bind $itk_component(renwidget) <KeyPress-1> [code $this point_axis_at_camera 1]
      ::bind $itk_component(renwidget) <KeyPress-2> [code $this point_axis_at_camera 2]
      ::bind $itk_component(renwidget) <KeyPress-3> [code $this point_axis_at_camera 3]

      #  Centre without changing zoom.
      ::bind $itk_component(renwidget) <KeyPress-c> [code $this centre]
   }

   #  Rotate the camera position in up direction by incr degrees.
   public method rotate_updown {incr} {
      set_rate_to_desired
      set camera [$renderer_ GetActiveCamera]
      $camera Elevation $incr
      $camera OrthogonalizeViewUp
      queue_render
   }

   #  Rotate the camera position in right direction by incr degrees.
   public method rotate_leftright {incr} {
      set_rate_to_desired
      set camera [$renderer_ GetActiveCamera]
      $camera Azimuth $incr
      $camera OrthogonalizeViewUp
      queue_render
   }

   #  Zoom (dolly) camera in or out by the given factor (1+/-).
   public method zoom {factor} {
      set_rate_to_desired
      set camera [$renderer_ GetActiveCamera]
      if { [$camera GetParallelProjection] } {
         set parallelScale [expr [$camera GetParallelScale] * $factor];
         $camera SetParallelScale $parallelScale;
      } else {
         $camera Dolly $factor
         $renderer_ ResetCameraClippingRange
      }
      queue_render
   }

   #  Scroll camera position by the given increments in the X and Y directions.
   public method scroll {xincr yincr} {
      set_rate_to_desired
      set camera [$renderer_ GetActiveCamera]

      set centre [$renwindow_ GetSize]
      set centreX [expr [lindex $centre 0] / 2.0]
      set centreY [expr [lindex $centre 1] / 2.0]

      set FPoint [$camera GetFocalPoint]
      set FPoint0 [lindex $FPoint 0]
      set FPoint1 [lindex $FPoint 1]
      set FPoint2 [lindex $FPoint 2]

      set PPoint [$camera GetPosition]
      set PPoint0 [lindex $PPoint 0]
      set PPoint1 [lindex $PPoint 1]
      set PPoint2 [lindex $PPoint 2]

      $renderer_ SetWorldPoint $FPoint0 $FPoint1 $FPoint2 1.0
      $renderer_ WorldToDisplay
      set DPoint [$renderer_ GetDisplayPoint]
      set focalDepth [lindex $DPoint 2]

      set APoint0 [expr $centreX + $xincr]
      set APoint1 [expr $centreY + $yincr]

      $renderer_ SetDisplayPoint $APoint0 $APoint1 $focalDepth
      $renderer_ DisplayToWorld
      set RPoint [$renderer_ GetWorldPoint]
      set RPoint0 [lindex $RPoint 0]
      set RPoint1 [lindex $RPoint 1]
      set RPoint2 [lindex $RPoint 2]
      set RPoint3 [lindex $RPoint 3]
      if { $RPoint3 != 0.0 } {
         set RPoint0 [expr $RPoint0 / $RPoint3]
         set RPoint1 [expr $RPoint1 / $RPoint3]
         set RPoint2 [expr $RPoint2 / $RPoint3]
      }

      $camera SetFocalPoint \
         [expr ($FPoint0 - $RPoint0)/2.0 + $FPoint0] \
         [expr ($FPoint1 - $RPoint1)/2.0 + $FPoint1] \
         [expr ($FPoint2 - $RPoint2)/2.0 + $FPoint2]

      $camera SetPosition \
         [expr ($FPoint0 - $RPoint0)/2.0 + $PPoint0] \
         [expr ($FPoint1 - $RPoint1)/2.0 + $PPoint1] \
         [expr ($FPoint2 - $RPoint2)/2.0 + $PPoint2]

      queue_render
   }

   #  Roll the camera by incr degrees.
   public method roll {incr} {
      set_rate_to_desired
      set camera [$renderer_ GetActiveCamera]
      $camera Roll $incr
      $camera OrthogonalizeViewUp
      queue_render
   }

   #  Static procedure to convert a Tk colour into RGB values for VTK.
   public proc get_rgb_colour { value } {
      if {[catch {set colour [winfo rgb . $value]} ]} {
         return [list 0.0 0.0 0.0]
      }
      set R [expr double([lindex $colour 0])/65535.0]
      set G [expr double([lindex $colour 1])/65535.0]
      set B [expr double([lindex $colour 2])/65535.0]
      return [list $R $G $B]
   }

   #  Print the rendered scene to a graphics file. Format should
   #  be a supported name: TIFF, PNG, JPEG or EPS, if not matched
   #  a TIFF will be created.
   #
   #  Note printing requires either offscreen support or this window
   #  to be completely uncovered (since we're using the graphics buffer).
   #  Cannot rely on offscreen support so you must make sure that this screen
   #  is completely uncovered before calling this method.
   public method hardcopy { filename format scale } {
      busy {
         set lr [::vtkRenderLargeImage New]
         $lr SetInput $renderer_
         $lr SetMagnification $scale

         #  Default format is TIFF (lossless).
         if { $format == "PNG" } {
            set writer [::vtkPNGWriter New]
         } elseif { $format == "JPEG" } {
            set writer [::vtkJPEGWriter New]
            $writer SetQuality 75
         } elseif { $format == "EPS" } {
            set writer [::vtkPostScriptWriter New]
         } else {
            set writer [::vtkTIFFWriter New]
         }

         $writer SetInputConnection [$lr GetOutputPort]
         $writer SetFileName $filename

         #  Try to make sure we're up to date. Switch on offscreen rendering
         #  in case that's supported. Should remove any chance of picking
         #  up effects from the display graphics buffer.
         $renwindow_ OffScreenRenderingOn
         $renwindow_ Render
         $lr UpdateWholeExtent
         ::update
         after 1000

         #  And do the work. Bring window to front, but that should have been
         #  arranged already by the caller.
         ::raise $itk_component(renwidget)
         ::update
         $writer Write

         #  Dispose and reset.
         $lr Delete
         $writer Delete
         $renwindow_ OffScreenRenderingOff
         $renwindow_ Render
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  A command to execute when rendering and an update event has been
   #  dispatched.
   itk_option define -progress_cmd progress_cmd Progress_Cmd {}

   #  Initial width and height in pixels.
   itk_option define -width width Width 600
   itk_option define -height height Height 600

   #  Whether to enable backingstore.
   itk_option define -backingstore_on backingstore_on BackingStore_On 0 {
      if { $renderer_ != {} } {
         if { $itk_option(-backingstore_on) } {
            $renderer_ BackingStoreOn
         } else {
            $renderer_ BackingStoreOff
         }
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  The background colour as an R G B triplet for VTK.
   protected variable rgb_background_ {1.0 1.0 1.0}

   #  Renderer, render window and interactor.
   protected variable renderer_ {}
   protected variable renwindow_ {}
   protected variable interactor_ {}
   protected variable lightkit_ {}

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
