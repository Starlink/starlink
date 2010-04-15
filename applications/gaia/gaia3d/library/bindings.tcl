#
# Bespoke bindings for GAIA3D. Based on VTK bindings.tcl.
#
namespace eval ::gaia3d {

   # -------------------------------------------------------------------
   # vtkTk(ImageViewer/Render)Widget callbacks.
   #    vtkw: Tk pathname of the widget
   #    renwin: render window embedded in the widget
   #    x:      X coord, widget relative
   #    y:      X coord, widget relative
   #    w:      width of an event or update
   #    h:      height of an event or update
   #    ctrl:   1 if the Control key modifier was pressed, 0 otherwise
   #    shift:  1 if the Control key modifier was pressed, 0 otherwise
   #    delta:  delta field for the MouseWheel event.
   #    repeat: 1 if a mouse button is double clicked, 0 if single clicked

   # Called when a Tk mouse motion event is triggered.
   # Helper binding: propagate the event as a VTK event.

   proc cb_vtkw_motion_binding {vtkw renwin x y} {
      set iren [$renwin GetInteractor]
      $iren SetEventPositionFlipY $x $y
      $iren MouseMoveEvent
   }

   # Called when a Tk button mouse event is triggered.
   # Helper binding: propagate the event as a VTK event.
   #    event:  button state, Press or Release
   #    pos:    position of the button, Left, Middle or Right

   proc cb_vtkw_button_binding {vtkw renwin x y ctrl shift event pos repeat} {
      set iren [$renwin GetInteractor]
      $iren SetEventPositionFlipY $x $y
      $iren SetControlKey $ctrl
      $iren SetShiftKey $shift
      $iren ${pos}Button${event}Event
      $iren SetRepeatCount $repeat
   }

   # Called when a Tk wheel motion event is triggered.
   # Helper binding: propagate the event as a VTK event.

   proc cb_vtkw_wheel_motion_binding {vtkw renwin delta} {
      set iren [$renwin GetInteractor]
      if {$delta < 0} {
         $iren MouseWheelBackwardEvent
      } else {
         $iren MouseWheelForwardEvent
      }
   }

   # Called when a Tk key event is triggered.
   # Helper binding: propagate the event as a VTK event (indeed, two).
   #    event:   key state, Press or Release
   #    keycode: keycode field
   #    keysym:  keysym field

   proc cb_vtkw_key_binding {vtkw renwin x y ctrl shift event keycode keysym} {

      #  Don't process "a" or "Alt" they switch to actor interaction, don't
      #  want that.
      if { $keysym == "a" || $keysym == "A" ||
           [string match "Alt*" $keysym] } {
         return
      }

      #  A 7 is really a 3 (rebound in GAIA), that's switch to stereo mode.
      if { $keysym == "7" } {
         set keysym 3
      }

      set iren [$renwin GetInteractor]
      # Not a bug, two times keysym, since 5th param expect a char, and
      # $keycode is %k, which is a number
      $iren SetEventInformationFlipY $x $y $ctrl $shift $keysym 0 $keysym
      $iren Key${event}Event
      if {$event == "Press"} {
         $iren SetEventInformationFlipY $x $y $ctrl $shift $keysym 0 $keysym
         $iren CharEvent
      }

   }

   # Called when a Tk Expose/Configure event is triggered.
   # Helper binding: propagate the event as a VTK event.

   proc cb_vtkw_configure_binding {vtkw renwin w h} {
      set iren [$renwin GetInteractor]
      $iren UpdateSize $w $h
      $iren ConfigureEvent
   }

   proc cb_vtkw_expose_binding {vtkw renwin x y w h} {
      set iren [$renwin GetInteractor]
      $iren SetEventPositionFlipY $x $y
      $iren SetEventSize $w $h
      $iren ExposeEvent
   }

   # Called when a Tk Enter/Leave event is triggered.
   # Helper binding: propagate the event as a VTK event.
   # Note that entering the widget automatically grabs the focus so
   # that key events can be processed.

   proc cb_vtkw_enter_binding {vtkw renwin x y} {
      focus $vtkw
      set iren [$renwin GetInteractor]
      $iren SetEventPositionFlipY $x $y
      $iren EnterEvent
   }

   proc cb_vtkw_leave_binding {vtkw renwin x y} {
      set iren [$renwin GetInteractor]
      $iren SetEventPositionFlipY $x $y
      $iren LeaveEvent
   }

   # Set the above bindings for a vtkTkRenderWidget widget.

   proc create_vtkw_bindings {vtkw renwin} {

      # Find the render window (which creates it if it was not set).
      # Find the interactor, create a generic one if needed.

      if {[$renwin GetInteractor] == ""} {
         # the duh is critical in the following line, it causes
         # vtkTclUtil.cxx to know that the object was created in
         # a Tcl script, otherwise if ${renwin} was a return value
         # from a C++ function it would be called vtkTemp### and
         # the interactor instance would have the same name causing Tcl
         # to think it also was a C++ return value.
         set iren [vtkGenericRenderWindowInteractor duh_${renwin}_iren]
         $iren SetRenderWindow $renwin
         $iren Initialize
      }

      # Mouse motion

      bind $vtkw <Motion> "::gaia3d::cb_vtkw_motion_binding $vtkw $renwin %x %y"

      # Mouse buttons and key events

      foreach {modifier ctrl shift repeat} {
         ""               0 0 0
         "Control-"       1 0 0
         "Shift-"         0 1 0
         "Control-Shift-" 1 1 0
         "Double-" 0 0 1
         "Double-Control-" 1 0 1
         "Double-Shift-" 0 1 1
         "Double-Control-Shift-" 1 1 1
      } {
         foreach event {
            Press
            Release
         } {
            foreach {pos number} {
               Left 1
               Middle 2
               Right 3
            } {
               bind $vtkw <${modifier}Button${event}-${number}> \
                  "::gaia3d::cb_vtkw_button_binding $vtkw $renwin %x %y $ctrl $shift $event $pos $repeat"
            }

            bind $vtkw <${modifier}Key${event}> \
               "::gaia3d::cb_vtkw_key_binding $vtkw $renwin %x %y $ctrl $shift $event %k %K"
         }
      }

      # Wheel motion
      # Only x11 does not understand a mousewheel event [tk windowing-system]
      # can be x11, win32, classic, aqua
      if {[tk windowingsystem] == "x11"} {
         bind $vtkw <Button-4> \
            "::gaia3d::cb_vtkw_wheel_motion_binding $vtkw $renwin 1"
         bind $vtkw <Button-5> \
            "::gaia3d::cb_vtkw_wheel_motion_binding $vtkw $renwin -1"
      } else {
         bind $vtkw <MouseWheel> \
            "::gaia3d::cb_vtkw_wheel_motion_binding $vtkw $renwin %D"
      }

      # Expose/Configure

      bind $vtkw <Configure> \
         "::gaia3d::cb_vtkw_configure_binding $vtkw $renwin %w %h"

      bind $vtkw <Expose> \
         "::gaia3d::cb_vtkw_expose_binding $vtkw $renwin %x %y %w %h"

      # Enter/Leave

      bind $vtkw <Enter> \
         "::gaia3d::cb_vtkw_enter_binding $vtkw $renwin %x %y"

      bind $vtkw <Leave> \
         "::gaia3d::cb_vtkw_leave_binding $vtkw $renwin %x %y"
   }

   # -------------------------------------------------------------------
   # vtkRenderWindow callbacks/observers
   #   renwin: render window object

   # AbortCheckEvent observer.
   # Check if some events are pending, and abort render in that case

   proc cb_renwin_abort_check_event {renwin} {
      if {[$renwin GetEventPending] != 0} {
         $renwin SetAbortRender 1
      }
   }

   # Add the above observers to a vtkRenderWindow

   proc add_renwin_observers {renwin} {

      # Check for aborting rendering

      ::vtk::set_widget_variable_value $renwin AbortCheckEventTag \
         [$renwin AddObserver AbortCheckEvent \
             [list ::gaia3d::cb_renwin_abort_check_event $renwin]]
   }

   # -------------------------------------------------------------------
   # vtk(Generic)RenderWindowInteractor callbacks/observers
   #   iren: interactor object

   # CreateTimerEvent/DestroyTimerEvent observers.
   # Handle the creation of a timer event (10 ms)

   proc cb_iren_create_timer_event {iren} {
      set timer [after 10 "$iren TimerEvent"]
      ::vtk::set_widget_variable_value $iren CreateTimerEventTimer $timer
   }

   proc cb_iren_destroy_timer_event {iren} {
      set timer [::vtk::get_widget_variable_value $iren CreateTimerEventTimer]
      if {$timer != ""} {
         after cancel $timer
      }
   }

   # ConfigureEvent observer.
   #
   # Update the interface as it is being changed. Do this slowly to
   # keep the interactive performance.

   proc cb_iren_configure_event {iren} {

      # Cancel the previous timer if any
      set timer [::vtk::get_widget_variable_value $iren ConfigureEventTimer]
      if {$timer != ""} {
         after cancel $timer
      }
      ::vtk::set_widget_variable_value $iren ConfigureEventTimer \
         [after 1000 [list ::gaia3d::cb_iren_expose_event $iren]]
   }

   # ExposeEvent observer.
   # This event is triggered when a part (or all) of the widget is exposed,
   # i.e. a new area is visible. It usually happens when the widget window
   # is brought to the front, or when the widget is resized.

   # PWD: XXX do render using after so that they can be deferred, seems to fix
   # a problem when the window is resized during an initial iso-surface
   # renderer (something is getting out of sequence).

   proc cb_iren_expose_event {iren} {
      ::update
      after 10 [list [$iren GetRenderWindow] Render]
   }

   # Add the above observers to a vtk(Generic)RenderWindowInteractor

   proc add_iren_observers {iren} {

      # Timer events

      ::vtk::set_widget_variable_value $iren CreateTimerEventTag \
         [$iren AddObserver CreateTimerEvent \
             [list ::gaia3d::cb_iren_create_timer_event $iren]]

      ::vtk::set_widget_variable_value $iren DestroyTimerEventTag \
         [$iren AddObserver DestroyTimerEvent \
             [list ::gaia3d::cb_iren_destroy_timer_event $iren]]

      # Expose and Configure

      ::vtk::set_widget_variable_value $iren ConfigureEventTag \
         [$iren AddObserver ConfigureEvent \
             [list ::gaia3d::cb_iren_configure_event $iren]]

      ::vtk::set_widget_variable_value $iren ExposeEventTag \
         [$iren AddObserver ExposeEvent \
             [list ::gaia3d::cb_iren_expose_event $iren]]
   }

   # -------------------------------------------------------------------
   # Create vtkTk(ImageViewer/Render)Widget bindings, setup observers

   proc bind_tk_widget {vtkw renwin} {
      create_vtkw_bindings $vtkw $renwin
      add_renwin_observers $renwin
      add_iren_observers [$renwin GetInteractor]
   }
}
