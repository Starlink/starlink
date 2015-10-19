#+
#  Name:
#     GaiaPhotomExtras

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class of object for controlling optional photometry
#     parameters.

#  Description:
#     This class creates a mega-widget in its own top-level window. It
#     allows the values of the "optional" parameters that are used by the
#     "autophotom" application. At present these are:
#
#        BIASLE CENTRO MAXITER MAXSHIFT PADU PHOTON POSITIVE
#        SATURE SEARCH SKY SKYEST SKYSIG TOLER EXSOURCE ETIME
#
#     The current values of these parameters (which are public
#     variables of this class) can be queried in one call, "getstate".

#  Invocations:
#
#        GaiaPhotomExtras object_name [configuration options]
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
#     "sky variance", "data variance", "gaussian sky"
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
#     This widget inherits the FrameWidget class (also assumes part of
#     a TopLevelWidget).

#  Copyright:
#     Copyright (C) 1996-2005 Central Laboratory of the Research Councils.
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
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     30-APR-1996 (PWD):
#        Original version.
#     8-JUL-1996 (PWD):
#        Converted to itcl2.0.
#     {enter_further_changes_here}

#-

#.
itk::usual GaiaPhotomExtras {}

itcl::class gaia::GaiaPhotomExtras {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      set labelwidth 28

      #  Add widgets for accessing the values.
      #  Source of the exposure time.
      itk_component add Label1 {
         LabelRule $w_.label1 \
            -text "Exposure time:"
      }
      itk_component add ExSource {
         LabelMenu $w_.exsrc \
            -text "Exposure time source:" \
            -labelwidth $labelwidth
      }
      $itk_component(ExSource) add -label {simple constant} -value CONSTANT
      $itk_component(ExSource) add -label {FITS keyword} -value HEADER
      $itk_component(ExSource) add -label {HDS object} -value HDS
      add_short_help $itk_component(ExSource) \
         {How exposure time is located}

      #  Exposure time qualifier.
      itk_component add Etime {
	  LabelEntry $w_.etime \
		  -text {Exposure time/qualifier:} \
		  -value 1.0 \
		  -labelwidth $labelwidth  \
      }
      add_short_help $itk_component(Etime) \
         {Exposure time, FITS keyword or HDS object}

      #  Image description parameters.
      itk_component add Label2 {
         LabelRule $w_.label2 \
            -text "Image parameters:"
      }
      itk_component add Biaslevel {
         LabelEntry $w_.bias \
            -text "Image bias level:" \
            -labelwidth $labelwidth \
            -command [code $this null_method]
      }
      add_short_help $itk_component(Biaslevel) \
         {Bias level in image data units (zero when removed)}
      itk_component add Photons {
         LabelEntry $w_.padu \
            -text "Photons per data unit" \
            -labelwidth $labelwidth \
            -command [code $this null_method]
      }
      add_short_help $itk_component(Photons) \
         {Numbers of photons per ADU, used for noise est.}
      itk_component add Saturation {
         LabelEntry $w_.satur \
            -text "Saturation value:" \
            -labelwidth $labelwidth \
            -command [code $this null_method]
      }
      add_short_help $itk_component(Saturation) \
         {Objects with greater intensity flagged "S"}

      #  Centroid parameters.
      itk_component add Label3 {
         LabelRule $w_.label3 \
            -text "Centroid parameters:"
      }
      itk_component add Centroid {
         StarLabelCheck $w_.cent \
            -text "Perform centroiding:" \
            -onvalue TRUE -offvalue FALSE \
            -labelwidth $labelwidth \
            -variable [scope state_($this,centroid)]
      }
      set state_($this,centroid) TRUE
      add_short_help $itk_component(Centroid) \
         {Centroid initial aperture positions}
      itk_component add Maxiterations {
         LabelEntry $w_.maxiter \
            -text "Maximum iterations:" \
            -labelwidth $labelwidth \
            -command [code $this null_method]
      }
      add_short_help $itk_component(Maxiterations) \
         {Maximum no. iterations when locating centroid}
      itk_component add Maxshift {
         LabelEntry $w_.maxshift \
            -text "Maximum shift in position:" \
            -labelwidth $labelwidth \
            -command [code $this null_method]
      }
      add_short_help $itk_component(Maxshift) \
         {Maximum shift from initial positions (pixels)}
      itk_component add Search {
         LabelEntry $w_.search \
            -text "Size of search box:" \
            -labelwidth $labelwidth \
            -command [code $this null_method]
      }
      add_short_help $itk_component(Search) \
         {Size of box to use when locating centroid}
      itk_component add Toler {
         LabelEntry $w_.toler \
            -text "Positional accuracy:" \
            -labelwidth $labelwidth \
            -command [code $this null_method]
      }
      add_short_help $itk_component(Toler) \
         {Accuracy for centroid corrected position}
      itk_component add Positive {
         StarLabelCheck $w_.positive \
            -text "Positive features:" \
            -onvalue TRUE -offvalue FALSE \
            -labelwidth $labelwidth \
            -variable [scope state_($this,positive)]
      }
      add_short_help $itk_component(Positive) \
         {Look for positive features}
      set state_($this,positive) TRUE

      #  Measurement parameters.
      itk_component add Label4 {
         LabelRule $w_.label4 \
            -text "Measurement parameters:"
      }

      itk_component add Photonerr {
         LabelMenu $w_.photon \
            -text "Measurement errors use:" \
            -labelwidth $labelwidth
      }
      $itk_component(Photonerr) add -label {photon statistics} \
         -command [code $this configure -photon_errors {photon statistics}]
      $itk_component(Photonerr) add -label {sky variance} \
         -command [code $this configure -photon_errors {sky variance}]
      $itk_component(Photonerr) add -label {data variance} \
         -command [code $this configure -photon_errors {data variance}]
      $itk_component(Photonerr) add -label {gaussian sky} \
         -command [code $this configure -photon_errors {gaussian sky}]
      add_short_help $itk_component(Photonerr) \
         {How to calculate measurement errors}

      itk_component add Skyest {
         LabelMenu $w_.skyest -text "Sky estimator:" \
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
      add_short_help $itk_component(Skyest) \
         {Estimator to use when determining sky value}

      itk_component add Sky {
         LabelEntry $w_.sky \
            -text "Default sky level:" \
            -labelwidth $labelwidth \
         -command [code $this null_method]
      }
      add_short_help $itk_component(Sky) \
         {Sky value when estimator is set to constant}
      itk_component add Skysig {
         LabelEntry $w_.skysig \
            -text "Default error in sky level:" \
            -labelwidth $labelwidth \
            -command [code $this null_method]
      }
      add_short_help $itk_component(Skysig) \
         {Error in sky value when using a constant}

      #  All widgets now exist so set default values.
      eval itk_initialize $args

      #  Pack up frame.
      pack $itk_component(Label1) -fill x
      pack $itk_component(ExSource) -fill x -pady 2
      pack $itk_component(Etime) -fill x -pady 2

      pack $itk_component(Label2) -fill x
      pack $itk_component(Biaslevel) -fill x
      pack $itk_component(Photons) -fill x
      pack $itk_component(Saturation) -fill x

      pack $itk_component(Label3) -fill x
      pack $itk_component(Centroid) -fill x
      pack $itk_component(Maxiterations) -fill x
      pack $itk_component(Maxshift) -fill x
      pack $itk_component(Toler) -fill x
      pack $itk_component(Positive) -fill x

      pack $itk_component(Label4) -fill x
      pack $itk_component(Photonerr) -fill x
      pack $itk_component(Skyest) -fill x
      pack $itk_component(Sky) -fill x
      pack $itk_component(Skysig) -fill x
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   public method init {} {
      if { $itk_option(-phottype) != "aperture" } {
         $itk_component(Centroid) configure -state disabled
      }
   }

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
      configure -exsource [$itk_component(ExSource) get]
      configure -etime [$itk_component(Etime) get]
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
                 TOLER=$itk_option(-positional_accuracy) \
                 EXSOURCE=$itk_option(-exsource) \
	         ETIME=$itk_option(-etime) "

      switch $itk_option(-photon_errors) {
         {data variance} {
            append state "PHOTON=3 "
         }
         {sky variance} {
            append state "PHOTON=2 "
         }
         {gaussian sky} {
            append state "PHOTON=4 "
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
                        4 {
                           configure -photon_errors {gaussian sky}
                        }
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
                  EXSOURCE {
                     configure -exsource $value
                  }
                  ETIME {
                     configure -etime $value
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
         gaussian* {
            $itk_component(Photonerr) configure -value {gaussian sky}
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
   itk_option define -sky_estimator sky_estimator Sky_estimator {clipped_mean} {
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
   itk_option define -exsource exsource Exsource CONSTANT {
      switch $itk_option(-exsource) {
         HDS {
            $itk_component(ExSource) configure -value HDS
         }
         HEADER {
            $itk_component(ExSource) configure -value HEADER
         }
         default {
            $itk_component(ExSource) configure -value CONSTANT
         }
      }
   }
   itk_option define -etime etime Etime {1.0} {
      $itk_component(Etime) configure -value $itk_option(-etime)
   }

   #  Type of photometry, aperture or optimal.
   itk_option define -phottype phottype Phottype aperture

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Global variable to decouple check button states.
   common state_

#  End of class definition.
}
