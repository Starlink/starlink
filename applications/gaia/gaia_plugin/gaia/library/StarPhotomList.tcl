#+
#  Name:
#     StarPhotomList

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class to manage lists of photometry objects.

#  Description:
#     This class manages a list of StarPhotomObjects, providing the
#     ability to read, write and display a list and create or delete
#     objects in the list. The list can be displayed in a scrolled
#     listbox and bindings may be associated with actions in the list.
#
#     The objects (and more importantly their properties) can be
#     displayed graphically in a GaiaImage widget. A state is
#     associated with each object -- selected, displayed or new -- and
#     the object appears graphically in the correct form. Objects that
#     are displayed are drawn in a different colour to those that are
#     selected.

#  Invocations:
#
#        StarPhotomList object_name [configuration options]
#
#     This creates an instance of a StarPhotom object. The return is
#     the name of the object.
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
#
#        -modified boolean
#
#     Set true when any of the apertures are modified. This should
#     not normally be set from outside this class.
#
#        -allow_resize boolean {1}
#
#     Whether apertures can be interactively resized or not.
#
#         -angle angle {0.0}
#
#     Default position angle of any elliptical apertures.
#
#         -annulus boolean {1}
#
#     Whether sky regions are defined by annuli or not.
#
#         -canvas name_of_canvas {}
#
#     The name of the canvas widget used to display the apertures.
#
#         -canvasdraw name_of_canvasdraw {}
#
#     The name of the StarCanvasDraw widget used to control the
#     apertures.
#
#        -details name_of_details {}
#
#     The name of the widget (StarPhotomDetails) used to display the
#     selected apertures details.
#
#        -eccentricity float {0.0}
#
#     The default eccentricity of elliptical apertures.
#
#        -innerscale float {1.5}
#
#     The default scale factor of the inner locus of sky region, if
#     defined using an annulus
#
#        -linewidth integer {1}
#
#     Linewidth used to draw apertures and annuli.
#
#        -modified boolean
#
#     Set true when any of the apertures are modified. This should
#     not normally be set from outside this class.
#
#        -notify_created_cmd command {}
#
#     Command to execute when an aperture is created (callback).
#
#        -outerscale float {2.0}
#
#     The default scale factor of the outer locus of sky region, if
#     defined using an annulus
#
#        -positions (annulus|regions) {annulus}
#
#     How the sky region are determined. Either from an annulus or
#     from a set of apertures.
#
#        -rtdimage {}
#
#     Name of the rtdimage displayed on the canvas.
#
#        -scrollbox name_of_listbox {.starphotomlist}
#
#     Name of a listbox or Scrollbox used to display the values
#     associated with the current apertures.
#
#        -semimajor float {5}
#
#     The default semi-major axis of elliptical and diameter of
#     circular apertures.
#
#        -shape (circle|ellipse) {circle}
#
#     The default shape of apertures when created.
#
#        -show_list boolean {0}
#
#     Whether to display the list of all measurements in the scrollbox
#     or not.
#
#        -usemags boolean {1}
#
#     Whether calculations are in magnitudes or not. This has no
#     effect at the moment.

#  Methods:
#     public:
#        add update args
#           Add an object to the list. If update is true and the object
#           exists then its current values are changed, otherwise a
#           new object is created with the given values. $args should
#           be a list of the following values.
#                          
#              index xpos ypos mag magerr sky signal code semimajor \
#              eccentricity angle positions shape
#
#        config_selected item value
#           Configure all the selected objects with the given item
#           value configuration.
#        copy
#           Create a new object that is a copy of the currently
#           selected one (the last if many are selected).
#        create_object
#           Create an aperture on the canvas (user provides position
#           using mouse).
#        create_sky_region
#           Create an aperture as a sky region
#        new_object index
#           Create a new object with the current configuration and 
#           store its details using index.
#        read_file filename update
#           Read the details of a list of objects from the given
#           file. If update is true then existing objects with the 
#           same index values are modified.
#        write_file filename {all 1}
#           Write a photom file (extended form). If all is true (default)
#           then all objects are written, otherwise just the currently
#           selected objects or last selected object is used.
#
#     private:
#        add_scrollbox_bindings_
#           Add bindings to elements in listbox so that an object can
#           be selected by double clicking on its entry.
#        changed index
#           Process notification that object has changed.
#        created_object index id
#           Deal with notification that an aperture has been created.
#        deleted index
#           Deal with notification that an object should be deleted.
#        select_scrollbox_line_ y
#           Select a line in a the scrollbox by y position and make 
#           the object at that line the current one.
#        update_details_ 
#           Deal with notication that an objects details should be
#           redisplayed.
#        update_scrollbox_
#           Deal with notication that the object details displayed in
#           the scrollbox should be redisplayed.

#  Inheritance:
#     This widget inherits no other classes.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     12-MAR-1996 (PDRAPER):
#        Original version.
#     8-JUL-1996 (PDRAPER):
#        Converted to itcl2.0.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::StarPhotomList {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor {

      #  Remove all photometry objects.
      for { set i 1 } { $i <= $highest_index_ } { incr i } {
         if { [info exists objects_($i)] } {
            delete object $objects_($i)
         }
      }

      #  Reset allow interactive resize
      $canvasdraw configure -show_selection_grips 1
   }

   #  Methods:
   #  --------

   #  Create a new StarPhotomObject with all the current
   #  configurations and an arbitrary name.
   method new_object {index} {
      set object [StarPhotomObject \#auto \
                     -index $index \
                     -major $semimajor \
                     -eccen $eccentricity \
                     -innerscale $innerscale \
                     -outerscale $outerscale \
                     -positions $positions \
                     -angle $angle \
                     -shape $shape \
                     -linewidth $linewidth \
                     -canvasdraw $canvasdraw \
                     -rtdimage $rtdimage \
                     -canvas $canvas]
      set modified 1
      return $object
   }

   #  Copy the currently "selected" object.
   method copy {} {
      if {[info exists objects_($selected_)] } {

         #  Get current state of object.
         set state [$objects_($selected_) getvalues all]

         #  Get an index for the new object.
         incr highest_index_
         set selected_ $highest_index_

         #  Set the values to those of the original, except we use the
         #  new index. Need to re-parse to extract the sky regions.
         foreach line [split $state "\n"] {
            switch -glob $line {
               \#SKY* {
                  set rest [lassign $line comment index]
                  eval $objects_($selected_) setsky SKY $rest
               }
               \#ANN* {
                  set rest [lassign $line comment index]
                  eval $objects_($selected_) setsky ANN $rest
               }
               default {
                  set rest [lassign $line index]
                  eval add 0 $selected_ $rest
               }
            }
         }
      }

      #  Finally update the scrollbox and the selected object details.
      update_scrollbox_
      update_details_
   }

   #  Create an object on the canvas.
   method create_object {} {
      incr highest_index_
      set selected_ $highest_index_
      set objects_($selected_) [new_object $selected_]

      #  Two basic methods of operation are allowed -- fully
      #  interactive resize and placement, or just placement (size of
      #  aperture is fixed).
      if { $allow_resize } {
         if { $shape == "circle" } {
            $objects_($selected_) create_and_resize_circle \
               [code $this created_object $selected_]
         } else {
            $objects_($selected_) create_and_resize_ellipse \
               [code $this created_object $selected_]
         }
      } else {
         if { $shape == "circle" } {
            $objects_($selected_) create_no_resize_circle \
               [code $this created_object $selected_] $semimajor
         } else {
            $objects_($selected_) create_no_resize_ellipse \
               [code $this created_object $selected_] $semimajor $eccentricity $angle
         }
      }
   }

   #  Interactive creation finished.
   method created_object {index id} {
      set objects_ids_($id) $index

      #  Set the notify command for when this object is changed.
      $objects_($index) configure \
         -notify_change_cmd [code $this changed $index]

      #  Set the notify command for when this object is deleted.
      $objects_($index) configure \
         -notify_delete_cmd [code $this deleted $index]

      #  And simulate a notify now.
      changed $index

      #  Finally notify user of this class that a new object has been
      #  created.
      if { $notify_created_cmd != {} } {
         eval $notify_created_cmd
      }
   }

   #  Create a new sky region and associate with the current object.
   method create_sky_region {} {
      if { $selected_ != {} } {
         if { $allow_resize } {
            if { $shape == "circle" } {
               $objects_($selected_) create_and_resize_sky_circle
            } else {
               $objects_($selected_) create_and_resize_sky_ellipse
            }
         } else {
            if { $shape == "circle" } {
               $objects_($selected_) create_no_resize_sky_circle
            } else {
               $objects_($selected_) create_no_resize_sky_ellipse
            }
         }
      }
   }

   #  Add an object to the list. If update is true and the object
   #  exists then its current values are changed, otherwise a new object
   #  is created with the given values.
   method add {update args} {
      if { [llength $args] > 0 } {

         #  Extract the index from the list and keep the rest for later.
         set trail [lassign $args index]

         #  Create a new object if it doesn't exist already, otherwise
         #  just update the existing object.
         if { ! [info exists objects_($index)] } {

            #  Need to create a new object with the given parameters.
            #  One problem is that we cannot change the shape of an
            #  aperture so we must initialise this correctly. Solution
            #  is to parse the whole string before passing this on.
            if { [llength $args] != 1 } {
               lassign $args selected_ xpos ypos mag magerr sky signal \
                  code semimajor eccentricity angle positions shape
               set objects_($selected_) [new_object $selected_]
               set modified 1
               $objects_($selected_) draw_object
               eval $objects_($selected_) setvalues $args
            }
            if { $selected_ > $highest_index_ } {
               set highest_index_ $selected_
            }
            set id [$objects_($selected_) canvas_id]
            set objects_ids_($id) $selected_

            #  Notify command when this object is changed.
            $objects_($selected_) configure -notify_change_cmd \
               [code $this changed $selected_]

            #  Notify command when this object is deleted.
            $objects_($selected_) configure -notify_delete_cmd \
               [code $this deleted $selected_]
         } elseif { $update } {

            #  Object exists and update is allowed so modify the
            #  current values.
            if { [llength $args] != 1 } {
               eval $objects_($index) setvalues $args
               set modified 1
            }
            if { $index > $highest_index_ } {
               set highest_index_ $index
            }
            set selected_ $index
            set id [$objects_($selected_) canvas_id]
            set objects_ids_($id) $selected_
         }
      }
   }

   #  Read a photom file. Note that update should be set if any
   #  objects that already exist are to be changed. A possible problem
   #  with this method is that aperture with sky regions will have new
   #  sky regions created when being updated (rather than having the
   #  existing ones superceded, so it is not possible to sensibly update
   #  this aperture type.
   method read_file {filename update} {
      if { [file readable $filename] } {
         set fid [open $filename r]
         set ok 1

         #  Loop over non-blank lines. If line starts with # it is
         #  either a comment or a sky region spec. Sky region specs
         #  start with '#SKY' or '#ANN'.
         while { $ok  } {
            set llen [gets $fid line]
            if { $llen > 0 } {
               switch -glob $line {
                  \#SKY* {
                     if { ! $update } {
                        set rest [lassign $line comment index]
                        eval $objects_($index) setsky SKY $rest
                     }
                  }
                  \#ANN* {
                     if { $update } {
                        set rest [lassign $line comment index]
                        eval $objects_($index) setsky ANN $rest
                     }
                  }
                  \#* { ;#  Do nothing for comments
                  }
                  default {
                     eval add $update $line
                  }
               }
            } elseif { $llen < 0 } {
               set ok 0
            }
         }
         close $fid

         #  Finally update the scrollbox and the selected object details.
         update_scrollbox_
         update_details_

         #  Objects have changed, so update modified flag.
         set modified 1
      } else {
         error "Cannot read file: $filename."
      }
   }

   #  Write a photom file (extended form). If all is true (default)
   #  then all objects are written, otherwise just the currently selected
   #  objects are used, or the $selected_ object.
   method write_file {filename {all 1} } {
      set ok 0
      set fid [open $filename w]
      if { $all } {
         for { set i 1 } { $i <= $highest_index_ } { incr i } {
            if { [info exists objects_($i)] } {
               puts $fid "[$objects_($i) getvalues all]"
               set ok 1
            }
         }
      } else {
         set ids [$canvasdraw list_selected]
         if { $ids != {} } {
            foreach id "$ids" {
               if { [info exists objects_ids_($id)] } {
                  puts $fid "[$objects_($objects_ids_($id)) getvalues all]"
                  set ok 1
               }
            }
         } else {
            puts $fid "[$objects_($selected_) getvalues all]"
            set ok 1
         }
      }
      close $fid
      set modified 0
      return $ok
   }

   #  Set a configuration option for the currently selected objects.
   method config_selected {item value} {
      set ids [$canvasdraw list_selected]
      if { $ids != {} } {
         foreach id "$ids" {
            if { [info exists objects_ids_($id)] } {
               if { [info exists objects_($objects_ids_($id))] } {
                  $objects_($objects_ids_($id)) configure -$item $value
               }
            }
         }
      }
   }

   #  Deal with notification that an object has been deleted.
   private method deleted {index} {
      unset objects_($index)
      set selected_ [lindex [array names objects] 0]
      update_details_
      set modified 1
   }

   #  Update the list of objects if allowed.
   private method update_scrollbox_ {} {
      if { [winfo exists $scrollbox] } {
         $scrollbox clear all
         $scrollbox insert 0 [gaia::StarPhotomObject::header short]
         for { set i 0 } { $i <= $highest_index_ } { incr i } {
            if { [info exists objects_($i)] } {
               $scrollbox insert end [$objects_($i) getvalues short]
            }
         }
         raise [winfo toplevel $scrollbox]
      }
   }
   #  Update the displayed details of current object.
   private method update_details_ {} {
      global ::tcl_version
       
      if { [winfo exists $details] && $canvasdraw != {} } {
         if { $selected_ != {} && [info exists objects_($selected_)] } {
            set id [$objects_($selected_) canvas_id]
            if { "$id" != "" } {
               set selected_ $objects_ids_($id)
	       # allan: 21.1.99, added tcl8 check
	       if {$tcl_version >= 8.0} {
		   $details update_display [code $objects_($selected_)]
	       } else {
		   $details update_display [scope $objects_($selected_)]
	       }
            }
         } else {
            # Reset details.
            $details reset_display
         }
      }
   }

   #  Add bindings to allow some control of the objects from the
   #  scrollbox. Double clicking a line selects that object.
   private method add_scrollbox_bindings_ {} {
      set listbox [$scrollbox listname]
      bind $listbox <Double-1> [code $this select_scrollbox_line_ %y]

   }

   #  Select a line in a the scrollbox by y position and make the
   #  object at that line the current one.
   private method select_scrollbox_line_ {y} {
      set listbox [$scrollbox listname]
      set line [$listbox nearest $y]
      if { $line != {} } {
         set contents [$scrollbox get $line]
         set index [lindex $line 0]
         if { [info exists objects_($index)] } {
            $objects_($selected_) deselected
            $objects_($index) selected
            changed $index
         }
      }
   }

   #  Deal with notification that an object has been changed.
   private method changed {index} {
      set selected_ $index
      update_details_
      update_scrollbox_
       
      # These values now become the default (for creation of 
      # new objects without resize).
      if { [info exists objects_($index)] } {
	 lassign [$objects_($index) aperture_details] \
		 index x y mag magerr sky signal code semimajor \
		 eccentricity angle positions innerscale outerscale
      }
      set modified 1
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Whether any objects are new or modified since a file was last
   #  written.
   public variable modified {0} {}

   #  Name of StarCanvasDraw object that controls the overlay graphics
   #  of the objects.
   public variable canvasdraw {} {}

   #  Name of canvas.
   public variable canvas {} {}

   #  Name of GaiaImage type object for converting aperture sizes
   #  into displayed canvas sizes.
   public variable rtdimage {} {}

   #  Show a list of objects in a listbox.
   public variable show_list {0} {
      if { $show_list } {
         if { ![winfo exists $scrollbox] } {
            Scrollbox $scrollbox
            pack $scrollbox -fill both -expand true

            #  Add bindings to change the currently selected object to
            #  the one in the list when double clicked.
            add_scrollbox_bindings_
         }
         update_scrollbox_
      } else {
         pack forget $scrollbox
         delete object $scrollbox
      }
   }

   #  Parent window for Scrollbox (if used).
   public variable scrollbox {.starphotomlist} {}

   #  Display current object details in StarPhotomDetails object if required.
   public variable details {} {
      if { [winfo exists $details] } {
         set show_details_ 1
         update_details_
      } else {
         set show_details 0
      }
   }

   #  Shape of apertures.
   public variable shape {circle} {
      config_selected shape $shape
   }

   #  Default semimajor axis (radius) of apertures. Change the value
   #  of the current object if available.
   public variable semimajor {5} {
      config_selected major $semimajor
   }

   #  Default inner scale of apertures.
   public variable innerscale {1.5} {
      config_selected innerscale $innerscale
   }

   #  Default outer scale of apertures.
   public variable outerscale {2.0} {
      config_selected outerscale $outerscale
   }

   #  Default boolean for annular or region sky methods.
   public variable annulus {1} {
      if { $annulus } {
         configure -positions annulus
      } else {
         configure -positions regions
      }
   }
   public variable positions {annulus} {
      config_selected positions $positions
   }

   #  Default line width of graphical objects.
   public variable linewidth {1} {
      config_selected linewidth $linewidth
   }

   #  Default eccentricity of apertures.
   public variable eccentricity {0.0} {
      config_selected eccen $eccentricity
   }

   #  Default position angle of apertures.
   public variable angle {0.0} {
      config_selected angle $angle
   }

   #  Whether objects in list can be interactively resized or not.
   public variable allow_resize {1} {
       $canvasdraw configure -show_selection_grips $allow_resize
   }

   #  Command to execute when a new object is created.
   public variable notify_created_cmd {} {}

   #  Whether calculations are in magnitudes or counts.
   public variable usemags 1 {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Highest index of objects. Used to limit for loops searching for
   #  existing objects (objects need to be sorted by index no.).
   protected variable highest_index_ 0

   #  Currently selected object.
   protected variable selected_ {}

   #  Array of object names (index by index number).
   protected variable objects_

   #  Array of canvas ids for the objects (index by id, value is object index).
   protected variable objects_ids_

   #  State of details tracking.
   protected variable show_details_ 0

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
