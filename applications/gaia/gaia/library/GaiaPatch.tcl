#+
#  Name:
#     GaiaPatch

#  Type of Module:
#     [incr Tk] widget

#  Purpose:
#     Creates a toolbox for interactively patching an image.

#  Description:
#     This class creates a top-level widget for controlling the
#     interactive patching of an image. Patching is done by replacing
#     all the pixels in a specified region by values derived from a surface
#     fit to pixels in another region, plus a noise estimate that is also
#     derived from the fit region.
#
#     This implementation allows more than one region to specified for
#     replacement and similarly for the parts of the image to be
#     fit. It also allows annuli about the replacement regions to be used.
#     The region themselves can have many different shapes and can be
#     stored in text files and re-read to make replacements on the same
#     regions of related images.
#
#     One level of undo can be used (the disk file serves as a
#     reference if things get too bad).

#  Invocations:
#
#        GaiaPatch object_name [configuration options]
#
#     This creates an instance of a GaiaPhotom object. The return is
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
#     See itk_option definitions below.

#  Methods:
#
#        close
#
#     Closes down the top-level window and reminds the user to save
#     the image if it has been modified.
#
#        do_patch
#
#     Calls the RTD foreign command for patching the image with the
#     current replacement and background regions.
#
#        undo
#
#     Undoes the last patch. Note does not do more than one.
#
#        annuli_changed
#
#     Usage of annuli as background estimates changed. Removes any
#     annuli from the display.
#
#        read_file [filename]
#
#     Reads a file containing an ARD description of the replacement
#     regions, or background regions.
#
#        save_file [filename]
#
#     Writes a file containing an ARD description of the replacement
#     regions, or background regions.
#
#        frequency_changed
#
#     Changes the frequency updates for any annuli (better response
#     for slow machines).

#  Inheritance:
#     TopLevelWidget.

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
#     14-JUN-1996 (PWD):
#        Original version.
#     10-JUL-1996 (PWD):
#        Converted to itcl2.0.
#     07-FEB-2000 (PWD):
#        Changed to work with readonly status of NDFs.
#     14-AUG-2000 (PWD):
#        FITS images can be readonly too (and must remain this).
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaPatch {}

itcl::class gaia::GaiaPatch {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: Patch image ($itk_option(-number))"

      #  Add an options menu for setting options that should probably
      #  be defined once only per-session, or infrequently.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0
      set Options [add_menubutton "Options"]
      configure_menubutton Options -underline 0

      #  Add window help.
      add_help_button patchusage "On Window..."

      #  Add option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      $short_help_win_ add_menu_short_help $File \
         {New window} {Create a new toolbox}

      #  Save measurements to a file.
      $File add command \
         -label {Save replacement regions...} \
         -command [code $this save_file repl] \
         -accelerator {Control-s}
      bind $w_ <Control-s> [code $this save_file repl]

      #  Read measurements from a file.
      $File add command \
         -label {Read replacement regions...} \
         -command [code $this read_file repl] \
         -accelerator {Control-r}
      bind $w_ <Control-r> [code $this read_file repl]

      #  Save measurements to a file.
      $File add command \
         -label {Save background regions...} \
         -command [code $this save_file back] \
         -accelerator {Alt-s}
      bind $w_ <Alt-s> [code $this save_file back]

      #  Read measurements from a file.
      $File add command \
         -label {Read background regions...} \
         -command [code $this read_file back] \
         -accelerator {Alt-r}
      bind $w_ <Alt-r> [code $this read_file back]

      #  Set the exit menu item.
      $File add command -label Exit -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]

      #  Create the two ARD toolboxes for creating the replacement and
      #  background descriptions.
      itk_component add Frame1 {
	  frame $w_.f1
      }
      itk_component add Label1 {
	  label $itk_component(Frame1).label \
		  -text {Add replacement region:}\
		  -anchor center
      }
      set Repl_ [gaia::StarArdAnnTool \#auto \
                   -canvasdraw $itk_option(-canvasdraw) \
	           -canvas $itk_option(-canvas) \
                   -rtdimage $itk_option(-rtdimage) \
                   -selected_colour $repl_select_colour_ \
                   -deselected_colour $repl_deselect_colour_ \
                   -scale $itk_option(-scalefactor) \
                   -continuous_updates 1 ]
      $Repl_ known_types $known_types_
      set Rtools_ [$Repl_ make_types_frame $itk_component(Frame1).repl]

      itk_component add Frame2 {
	  frame $w_.f2
      }
      itk_component add Label2 {
         label $itk_component(Frame2).label \
            -text {Add background region:} \
	    -anchor center
      }
      set Back_ [gaia::StarArdTool \#auto \
                    -canvasdraw $itk_option(-canvasdraw) \
                    -canvas $itk_option(-canvas) \
                    -rtdimage $itk_option(-rtdimage) \
                    -selected_colour $back_select_colour_ \
                    -deselected_colour $back_deselect_colour_ \
                    -continuous_updates 1 ]
      $Back_ known_types $known_types_
      set Btools_ [$Back_ make_types_frame $itk_component(Frame2).back]
      if { ! $itk_option(-use_annuli) } {
         $Back_ disable_types_frame
      }

      #  Whether to use annulus for background regions or not.
      set annuli_ 1
      $Options add checkbutton \
         -label {Fit background using an annulus} \
         -variable [scope annuli_] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this annuli_changed]
      annuli_changed

      #  Whether to update continuously or not.
      set frequency_ 1
      $Options add checkbutton \
         -label {Frequent updates (fast machine)} \
         -variable [scope frequency_] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this frequency_changed]
      frequency_changed

      #  Wether to use data variances if available.
      set variance_ 1
      $Options add checkbutton \
         -label {Use data variances (if available)} \
         -variable [scope variance_] \
         -onvalue 1 \
         -offvalue 0

      #  Set the annulus scale factor.
      itk_component add Frame3 {
	  frame $w_.f3
      }
      itk_component add Sfactor {
	  util::LabelEntryScale $itk_component(Frame3).entry \
             -text {Annulus scale factor:} \
	     -valuewidth 4 \
	     -increment 0.05 \
	     -resolution 0.05 \
	     -from 1.05 \
	     -to 5.0 \
	     -show_arrows 1 \
             -orient vertical \
	     -value $itk_option(-scalefactor) \
	     -command [code $this configure -scalefactor]
      }
      if { ! $itk_option(-use_annuli) } {
         $itk_component(Sfactor) configure -state disabled
      }

      #  The order of the surface fit.
      itk_component add Frame4 {
         frame $w_.f4
      }
      itk_component add Nfit {
	  util::LabelEntryScale $itk_component(Frame4).entry \
             -text {Order of surface fit:} \
	     -value $itk_option(-order) \
	     -valuewidth 4 \
	     -from 1 \
	     -to 5 \
	     -increment 1 \
	     -resolution 1 \
	     -show_arrows 1 \
             -orient vertical \
	     -command [code $this configure -order]
      }

      #  The scale factor for noise estimates.
      itk_component add Frame5 {
	  frame $w_.f5
      }
      itk_component add Nfactor {
	  util::LabelEntryScale $itk_component(Frame5).entry \
             -text {Noise estimate scaling factor:} \
	     -value $itk_option(-noise_factor) \
	     -valuewidth 4 \
	     -from 0.1 \
	     -to 5.0 \
	     -increment 0.05 \
	     -resolution 0.05 \
             -orient vertical \
	     -show_arrows 1 \
	     -command [code $this configure -noise_factor]
      }

      #  Make the changes to the image.
      itk_component add Frame6 {
	  frame $w_.f6
      }
      itk_component add Patch {
	  button $itk_component(Frame6).patch \
		  -text Patch -command [code $this do_patch]
      }
      itk_component add Undo {
	  button $itk_component(Frame6).undo  \
		  -text Undo -command [code $this undo]
      }

      #  Close window button.
      itk_component add Close {
	  button $itk_component(Frame6).close \
             -text Close -command [code $this close]
      }

      #  Pack all widgets into place.
      pack $itk_component(Frame1) $itk_component(Frame2) \
         $itk_component(Frame3) $itk_component(Frame4) \
         $itk_component(Frame5) $itk_component(Frame6) \
         -fill both -expand true
      pack $itk_component(Label1) $itk_component(Label2) -fill x
      pack $Rtools_ $Btools_ -fill both
      pack $itk_component(Sfactor)
      pack $itk_component(Nfit)
      pack $itk_component(Nfactor)
      pack $itk_component(Patch) $itk_component(Undo) \
         $itk_component(Close) -expand true -padx 2m -pady 2m -side left
   }

   #  Destructor:
   #  -----------
   destructor  {
      if { $Repl_ != {} } {
         delete object $Repl_
         set Repl_ {}
      }
      if { $Back_ != {} } {
         delete object $Back_
         set Back_ {}
      }

      #  Release any undo information.
      $itk_option(-rtdimage) foreign patch "-release"
   }

   #  Methods:
   #  --------

   #  Create a new instance of this object.
   protected method clone_me_ {} {
      if { $itk_option(-clone_cmd) != {} } {
         eval $itk_option(-clone_cmd)
      }
   }

   #  Close down.
   method close {} {
      if { $modified_ } {
         info_dialog {Remember to save this image, if you want to keep it.}
      }
      destroy $w_
   }

   #  Patch the current regions.
   method do_patch {} {
      if { [$itk_option(-rtdimage) cget -file] != "" } {

         #  Create the ARD descriptions of the replacement and background
         #  regions.
         if { [$Repl_ save_description {GaiaPatchNew.ard}] } {

            #  Get the extent of the replacement region.
            set replace_region [$Repl_ bbox]

            #  Get the fit region.
            set backok 0
            if { $itk_option(-use_annuli) } {
               set backok [$Repl_ save_annuli_description {GaiaPatchFit.ard}]
               set fit_region [$Repl_ bbox_annuli]
            } else {
               set backok [$Back_ save_description {GaiaPatchFit.ard}]
               if { [catch {set fit_region [$Back_ bbox]} ] } {
                  set backok 0
               }
            }
            if { $backok } {

               #  Make sure that displayed image is modifiable, if not
               #  possible then issue an error.
               set readonly [$itk_option(-rtdimage) readonly 0]
               if { !$readonly } {

                  #  Combine the bounding boxes to derive the extent of the
                  #  image that needs to be processed (saves time over
                  #  processing whole).
                  lassign $fit_region xb1 yb1 xb2 yb2
                  lassign $replace_region xk1 yk1 xk2 yk2
                  set region "[min $xb1 $xb2 $xk1 $xk2] [min $yb1 $yb2 $yk1 $yk2] \
                  [max $xb1 $xb2 $xk1 $xk2] [max $yb1 $yb2 $yk1 $yk2]"

                  #  Run patch command.
                  $itk_option(-rtdimage) foreign patch \
                     "-new GaiaPatchNew.ard -fit GaiaPatchFit.ard \
                   -keep $replace_region -region $region \
                   -order $itk_option(-order) \
                   -scale $itk_option(-noise_factor) \
                   -usevar $variance_"
                  set modified_ 1
               } else {
                  error_dialog "Sorry this image cannot be modified (file is readonly)."
               }
            } else {
               error_dialog "No background regions are available."
            }
         } else {
            error_dialog "No replacement regions available."
         }
      } else {
         error_dialog "No image is displayed."
      }
   }

   #  Undo the last patch.
   method undo {} {
      $itk_option(-rtdimage) foreign patch "-undo"
   }

   #  Usage of annuli as background estimates changed.
   method annuli_changed {} {
      configure -use_annuli $annuli_
   }

   #  Read in an ARD file.
   method read_file {what {filename ""}} {
      if { $filename == "" } {
	 if { $what == "repl" } {
	    $Repl_ read_file
	 } else {
	    $Back_ read_file
	 }
      } else {
	 if { $what == "repl" } {
	    $Repl_ read_description $filename
	 } else {
	    $Back_ read_description $filename
	 }
      }
   }

   #  Write an ARD file.
   method save_file {what {filename ""}} {
      if { $filename == "" } {
	 if { $what == "repl" } {
	    $Repl_ save_file
	 } else {
	    $Back_ save_file
	 }
      } else {
	 if { $what == "repl" } {
	    $Repl_ save_description $filename
	 } else {
	    $Back_ save_description $filename
	 }
      }
   }

   #  Set frequency of updates (continuous or only when command
   #  finished).
   method frequency_changed {} {
      $Repl_ configure -continuous_updates $frequency_
      $Back_ configure -continuous_updates $frequency_
   }

   #  Change modification status of image (for remote control only).
   method set_modified {flag} {
      set modified_ $flag
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of a StarCanvasDraw widget to use to control objects.
   itk_option define -canvasdraw canvasdraw CanvasDraw {} {}

   #  Name of canvas.
   itk_option define -canvas canvas Canvas {} {}

   #  Name of rtdimage widget.
   itk_option define -rtdimage rtdimage Rtdimage {} {}

   #  Order of surface fit.
   itk_option define -order order Order {3} {}

   #  Whether to use annuli or not.
   itk_option define -use_annuli use_annuli Use_Annuli {1} {
      if { $Repl_ != {} } {
         $Repl_ configure -show_annuli $itk_option(-use_annuli)
      }
      if { $Back_ != {} } {
         if { $itk_option(-use_annuli) } {
            $Back_ disable_types_frame
         } else {
            $Back_ enable_types_frame
         }
      }
      if { [info exists itk_component(Sfactor)] } {
         if { $itk_option(-use_annuli) } {
            $itk_component(Sfactor) configure -state normal
         } else {
            $itk_component(Sfactor) configure -state disabled
         }
      }
   }

   #  Annuli scale factor changed.
   itk_option define -scalefactor scalefactor ScaleFactor {1.5} {
      if { $Repl_ != {} } {
         $Repl_ configure -scale $itk_option(-scalefactor)
      }
   }

   #  Use variances if available.
   itk_option define -use_variances use_variances Use_Variances {1} {
      set variance_ $itk_option(-use_variances)
   }

   #  Scaling factor for noise estimates.
   itk_option define -noise_factor noise_factor Noise_Factor {1.0} {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}


   #  Protected variables: (available to instance)
   #  --------------------

   #  Colours of ARD regions.
   protected variable repl_select_colour_    white
   protected variable repl_deselect_colour_  green
   protected variable back_select_colour_    yellow
   protected variable back_deselect_colour_  blue

   #  ARD types that we can use (no column, row or line).
   protected variable known_types_ "Ellipse Circle Rect Poly RotBox Pixel"

   #  Names of various persistent objects.
   protected variable Repl_ {}
   protected variable Rtools_ {}
   protected variable Back_ {}
   protected variable Btools_ {}

   #  Is image modified?
   protected variable modified_ 0

   #  State of annuli in various instances.
   protected variable annuli_

   #  Frequency of updates in instances.
   protected variable frequency_

   #  Whether this instance uses variances.
   protected variable variance_

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
