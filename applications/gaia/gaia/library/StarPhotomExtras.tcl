#+
#  Name:
#     StarPhotomExtras

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class of object for controlling optional parameters.

#  Description:
#     This class creates a mega-widget in its own top-level window. It
#     allows the values of the "optional" parameters that are used by the
#     "autophotom" application. At present these are:
#
#        BIASLE CENTRO MAXITER MAXSHIFT PADU PHOTON POSITIVE
#        SATURE SEARCH SKY SKYEST SKYSIG TOLER
#
#     The current values of these parameters (which are public
#     variables of this class) can be queried in one call, "getstate".

#  Invocations:
#
#        StarPhotomExtras object_name [configuration options]
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
#        -bias_level value
#
#     Sets the bias level to be subtracted.
#
#        -centroid adam_boolean
#
#     Sets whether object positions are centroided (ADAM boolean ==
#     "TRUE||FALSE").
#
#        -max_iterations value
#
#     Maximum number of iterations used when centroiding.
#
#        -max_shift value
#
#     Maximum number of pixels that object position can shift when
#     centroiding.
#
#        -photons_per_adu value
#
#     The number of photons per data count (ADUs for CCDs).
#
#        -photon_errors method
#
#     The method to use when calculating errors in the
#     measurements. "method" should be "photon statistics",
#     "sky variance" or "data variance".
#
#        -positive_objects adam_boolean
#
#     Whether to centroid for positive or negative objects (with
#     respect to the sky value).
#
#        -saturation_value value
#
#     The value at which the data in the image saturates.
#
#        -search_box_size value
#
#     Size of the box centred on the current position, from which the
#     centroid is determined.
#
#        -sky_value value
#
#     A value for the image background (used when no sky regions are
#     available).
#
#        -sky_value_error value
#
#     An estimate of the error in the sky value.
#
#        -sky_estimator method
#
#     The method used to estimate the background value in an aperture
#     or annular region. Should be one of "mean", "clipped_mean",
#     "mode" or "user_supplied_value".

#  Methods:
#     See method statements below.

#  Inheritance:
#     This widget inherits the TopLevelWidget class.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     30-APR-1996 (PDRAPER):
#        Original version.
#     8-JUL-1996 (PDRAPER):
#        Converted to itcl2.0.
#     {enter_further_changes_here}

#-

#.
itk::usual StarPhotomExtras {}

class gaia::StarPhotomExtras {

   #  Inheritances:
   #  -------------
   inherit TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {
      TopLevelWidget::constructor -transient 1 -withdraw 1 -center 0
   } {
      wm title $w_ {Set Addition Photometry Parameters}

      #  Add widget for accessing the values.
      set labelwidth 28
      itk_component add Frame1 {
         frame $w_.frame1 -bd 2
      }
      itk_component add Label1 {
         label $itk_component(Frame1).label \
            -text "Image parameters:" -anchor c
      }
      itk_component add Biaslevel {
         LabelEntry $itk_component(Frame1).bias \
            -text "Image bias level:" \
            -labelwidth $labelwidth \
            -command [code $this null_method]
      }
      itk_component add Photons {
         LabelEntry $itk_component(Frame1).padu \
            -text "Photons per data unit" \
            -labelwidth $labelwidth \
            -command [code $this null_method]
      }
      itk_component add Saturation {
         LabelEntry $itk_component(Frame1).satur \
            -text "Saturation value:" \
            -labelwidth $labelwidth \
            -command [code $this null_method]
      }

      itk_component add Frame2 {
         frame $w_.frame2 -bd 2
      }
      itk_component add Label2 {
         label $itk_component(Frame2).label \
            -text "Centroid parameters:" -anchor c
      }
      itk_component add Centroid {
         StarLabelCheck $itk_component(Frame2).cent \
            -text "Perform centroiding:" \
            -onvalue TRUE -offvalue FALSE \
            -labelwidth $labelwidth \
            -variable [scope state_($this,centroid)]
      }
      set state_($this,centroid) TRUE
      itk_component add Maxiterations {
         LabelEntry $itk_component(Frame2).maxiter \
            -text "Maximum iterations:" \
            -labelwidth $labelwidth \
            -command [code $this null_method]
      }
      itk_component add Maxshift {
         LabelEntry $itk_component(Frame2).maxshift \
            -text "Maximum shift in position:" \
            -labelwidth $labelwidth \
            -command [code $this null_method]
      }
      itk_component add Search {
         LabelEntry $itk_component(Frame2).search \
            -text "Size of search box:" \
            -labelwidth $labelwidth \
            -command [code $this null_method]
      }
      itk_component add Toler {
         LabelEntry $itk_component(Frame2).toler \
            -text "Positional accuracy:" \
            -labelwidth $labelwidth \
            -command [code $this null_method]
      }
      itk_component add Positive {
         StarLabelCheck $itk_component(Frame2).positive \
            -text "Positive features:" \
            -onvalue TRUE -offvalue FALSE \
            -labelwidth $labelwidth \
            -variable [scope state_($this,positive)]
      }
      set state_($this,positive) TRUE

      itk_component add Frame3 {
         frame $w_.frame3 -bd 2
      }
      itk_component add Label3 {
         label $itk_component(Frame3).label \
            -text "Measurement parameters:" -anchor c
      }

      itk_component add Photonerr {
         LabelMenu $itk_component(Frame3).photon \
            -text "Measurement errors use:" \
            -labelwidth $labelwidth
      }
      $itk_component(Photonerr) add -label {photon statistics} \
         -command [code $this configure -photon_errors {photon statistics}]
      $itk_component(Photonerr) add -label {sky variance} \
         -command [code $this configure -photon_errors {sky variance}]
      $itk_component(Photonerr) add -label {data variance} \
         -command [code $this configure -photon_errors {data variance}]

      itk_component add Skyest {
         LabelMenu $itk_component(Frame3).skyest -text "Sky estimator:" \
            -labelwidth $labelwidth
      }
      $itk_component(Skyest) add -label {mean} \
         -command [code $this configure -sky_estimator mean]
      $itk_component(Skyest) add -label {clipped mean} \
         -command [code $this configure -sky_estimator {clipped mean}]
      $itk_component(Skyest) add -label {mode} \
         -command [code $this configure -sky_estimator mode]
      $itk_component(Skyest) add -label {constant} \
         -command [code $this configure -sky_estimator constant]

      itk_component add Sky {
         LabelEntry $itk_component(Frame3).sky \
            -text "Default sky level:" \
            -labelwidth $labelwidth \
         -command [code $this null_method]
      }
      itk_component add Skysig {
         LabelEntry $itk_component(Frame3).skysig \
            -text "Default error in sky level:" \
            -labelwidth $labelwidth \
            -command [code $this null_method]
      }

      #  Button to close and accept this window.
      itk_component add Frame4 {
         frame $w_.frame4 -bd 2
      }
      itk_component add Close {
         button $itk_component(Frame4).close \
            -text "Close" \
            -command [code $this hide_window]
      }

      #  All widgets now exist so set default values.
      eval itk_initialize $args

      #  Pack up frame.
      pack $itk_component(Frame1) -fill x -expand true
      pack $itk_component(Label1) -fill x
      pack $itk_component(Biaslevel) -fill x -expand true
      pack $itk_component(Photons) -fill x -expand true
      pack $itk_component(Saturation) -fill x -expand true

      pack $itk_component(Frame2) -fill x -expand true
      pack $itk_component(Label2) -fill x
      pack $itk_component(Centroid) -fill x -expand true
      pack $itk_component(Maxiterations) -fill x -expand true
      pack $itk_component(Maxshift) -fill x -expand true
      pack $itk_component(Toler) -fill x -expand true
      pack $itk_component(Positive) -fill x -expand true

      pack $itk_component(Frame3) -fill x -expand true
      pack $itk_component(Label3) -fill x
      pack $itk_component(Photonerr) -fill x -expand true
      pack $itk_component(Skyest) -fill x -expand true
      pack $itk_component(Sky) -fill x -expand true
      pack $itk_component(Skysig) -fill x -expand true

      pack $itk_component(Frame4) -fill x -expand true
      pack $itk_component(Close)
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   # "Close" the window. What is actually does is make it
   #  invisible. The object must survive to be quieried about its
   #  state. 
   method hide_window {} {
      wm withdraw $w_
   }

   #  Show the window.
   method show_window {} {
      $w_ configure -center 1
   }

   #  Update state. Sets the state to that of the buttons. Note the
   #  menu buttons do not need updating.
   method update_state {} {
      configure -bias_level [$itk_component(Biaslevel) get]
      configure -centroid $state_($this,centroid)
      configure -max_iterations [$itk_component(Maxiterations) get]
      configure -max_shift [$itk_component(Maxshift) get]
      configure -photons_per_adu [$itk_component(Photons) get]
      configure -positive_objects $state_($this,positive)
      configure -saturation_value [$itk_component(Saturation) get]
      configure -search_box_size [$itk_component(Search) get]
      configure -sky_value [$itk_component(Sky) get]
      configure -sky_value_error [$itk_component(Skysig) get]
      configure -positional_accuracy [$itk_component(Toler) get]
   }

   #  Do nothing command (overrides <Return> defaults).
   method null_method {args} {}

   #  Return all current options as a string suitable for passing to
   #  the autophotom application.
   method getstate {} {
      update_state
      set state "BIASLE=$itk_option(-bias_level) \
                 CENTRO=$itk_option(-centroid) \
                 MAXITER=$itk_option(-max_iterations) \
                 MAXSHIFT=$itk_option(-max_shift) \
                 PADU=$itk_option(-photons_per_adu) \
                 POSITIVE=$itk_option(-positive_objects) \
                 SATURE=$itk_option(-saturation_value) \
                 SEARCH=$itk_option(-search_box_size) \
                 SKY=$itk_option(-sky_value) \
                 SKYSIG=$itk_option(-sky_value_error) \
                 TOLER=$itk_option(-positional_accuracy) "
      
      switch $itk_option(-photon_errors) {
         {data variance} {
            append state "PHOTON=3 "
         }
         {sky variance} {
            append state "PHOTON=2 "
         }
         default {
            append state "PHOTON=1 "
         }
      }

      switch $itk_option(-sky_estimator) {
         mean {
            append state "SKYEST=1"
         }
         mode {
            append state "SKYEST=3"
         }
         constant {
            append state "SKYEST=4"
         }
         default {
            append state "SKYEST=2"
         }
      }
      return "$state"
   }

   #  Restore the state from a previous incarnation of this class by
   #  recovering the information returned from the "getstate" method.
   method setstate {state} {
      if { "$state" == {} } {
            return
      }
      #  Parse string into program parameter pairs and process into
      #  proper descriptions. The input string is split into a list,
      #  multiple spaces in this list become {} elements, so we skip
      #  these, we alse need to accomodate the condition 
      #  "keyword = value", rather than the expected "keyword=value".
      set skip 0
      set lstate [split $state]
      for { set i 0 } { $i < [llength $lstate] } { incr i } {
         set element [lindex $lstate $i]
         if { ! $skip || $element == {} } { 
            set value {}
            switch -glob $element {
               {=} {
                  set value [lindex $lstate [expr $i+1]]
                  set skip 1
               }
               {*=*} {
                  set lsub [split $element {=}]
                  set keyword [lindex $lsub 0]
                  set value [lindex $lsub 1]
               }
               default {
                  set keyword "$element"
               }
            }
            if { $value != {} } {
               set keyword [string toupper $keyword]
               switch $keyword {
                  BIASLE {
                     configure -bias_level $value
                  }
                  CENTRO {
                     configure -centroid $value
                  }
                  MAXITER {
                     configure -max_iterations $value
                  }
                  MAXSHIFT {
                     configure -max_shift $value
                  }
                  PADU {
                     configure -photons_per_adu $value
                  }
                  PHOTON {
                     switch $value {
                        3 {
                           configure -photon_errors {data variance}
                        }
                        2 {
                           configure -photon_errors {sky variance}
                        }
                        default {
                           configure -photon_errors {photon statistics}
                        }
                     }
                  }
                  POSITIVE {
                     configure -positive_objects $value
                  }
                  SATURE {
                     configure -saturation_value $value
                  }
                  SEARCH {
                     configure -search_box_size $value
                  }
                  SKY {
                     configure -sky_value $value
                  }
                  SKYEST {
                     switch $value {
                        1 {
                           configure -sky_estimator mean
                        }
                        2 {
                           configure -sky_estimator {clipped mean}
                        }
                        3 {
                           configure -sky_estimator mode
                        }
                        default {
                           configure -sky_estimator constant
                        }
                     }
                  }
                  SKYSIG {
                     configure -sky_value_error $value
                  }
                  TOLER {
                     configure -positional_accuracy $value
                  }
               }
            }
            
         } else {
            set skip 0
         }
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------
   itk_option define -bias_level bias_level Bias_level {0} {
      $itk_component(Biaslevel) configure -value $itk_option(-bias_level)
   }
   itk_option define -centroid centroid Centroid {TRUE} {
      if { $itk_option(-centroid) == "TRUE" } {
         $itk_component(Centroid) select
      } else {
         $itk_component(Centroid) deselect
      }
   }
   itk_option define -max_iterations max_iterations Max_iterations {9} {
      $itk_component(Maxiterations) configure -value $itk_option(-max_iterations)
   }
   itk_option define -max_shift max_shift Max_shift {9} {
      $itk_component(Maxshift) configure -value $itk_option(-max_shift)
   }
   itk_option define -photons_per_adu photons_per_adu Photons_per_adu {1} {
      $itk_component(Photons) configure -value $itk_option(-photons_per_adu)
   }
   itk_option define -photon_errors photon_errors Photon_errors {photon statistics} {
      switch -glob $itk_option(-photon_errors) {
         data* {
            $itk_component(Photonerr) configure -value {data variance}
         }
         sky* {
            $itk_component(Photonerr) configure -value {sky variance}
         }
         default {
            $itk_component(Photonerr) configure -value {photon statistics}
         }
      }
   }
   itk_option define -positive_objects positive_objects Positive_objects {TRUE} {
      if { $itk_option(-positive_objects) == "FALSE" } {
         $itk_component(Positive) deselect
      } else {
         $itk_component(Positive) select
      }
   }
   itk_option define -saturation_value saturation_value Saturation_value {1.7E30} {
      $itk_component(Saturation) configure -value $itk_option(-saturation_value)
   }
   itk_option define -search_box_size search_box_size Search_box_size {9} {
      $itk_component(Search) configure -value $itk_option(-search_box_size)
   }
   itk_option define -sky_value sky_value Sky_value {0} {
      $itk_component(Sky) configure -value $itk_option(-sky_value)
   }
   itk_option define -sky_estimator sky_estimator Sky_estimator {mean} {
      switch $itk_option(-sky_estimator) {
         mean {
            $itk_component(Skyest) configure -value mean
         }
         mode {
            $itk_component(Skyest) configure -value mode
         }
         constant {
            $itk_component(Skyest) configure -value constant
         }
         default {
            $itk_component(Skyest) configure -value {clipped mean}
         }
      }
   }
   itk_option define -sky_value_error sky_value_error Sky_value_error {0} {
      $itk_component(Skysig) configure -value $itk_option(-sky_value_error)
   }
   itk_option define -positional_accuracy positional_accuracy Positional_accuracy {0.05} { 
      $itk_component(Toler) configure -value $itk_option(-positional_accuracy)
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------
   
   #  Global variable to decouple check button states.
   common state_

#  End of class definition.
}
