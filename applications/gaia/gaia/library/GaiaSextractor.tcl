#+
#  Name:
#     GaiaSextractor

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class for controlling a SExtractor process.

#  Description:
#     This class creates a top-levelwidget for displaying and
#     selecting from the list of all possible SExtractor program and
#     catalogue configurations (SExtractor terminology). Each of these
#     parts can be saved and restored from files using the Sextractor
#     standard format. The application itself may be run and the catalogue
#     produced will be displayed (as a local catalogue).
#
#     There are various options, such as running a native version of
#     SExtractor and displaying the results as ellipses or circles
#     overlaid on the image.
#
#     This interface will work with native and Starlink versions of
#     SExtractor (in fact it will work with both at the same
#     time). The native version used during development was 2.0.19
#
#     A specialisation of this class is the production of catalogues
#     for use by AUTOASTROM. This restricts the file format to ASC and
#     fixes the catalogue columns.

#  Invocations:
#
#        GaiaSextractor object_name [configuration options]
#
#     This creates an instance of a GaiaSextractor object. The
#     return is the name of the object.
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
#     See the method declarations below.

#  Inheritance:
#     TopLevelWidget

#  Copyright:
#     Copyright (C) 1998-2000 Central Laboratory of the Research Councils
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
#     25-AUG-1998 (PWD):
#        Original version.
#     25-MAY-1999 (PWD):
#        Updated for 2.0.19. Added BACK_TYPE, BACK_VALUE and THRESH_TYPE.
#     22-MAY-2000 (PWD):
#        Added RAD_TYPE and RAD_THRESH changes. Includes plotting of
#        RAD0 through RAD15 circles.
#     30-MAY-2000 (PWD):
#        Added -textvaribles to all entry related fields, this
#        ends need to press <return>.
#     20-FEB-2002 (PWD):
#        Added X_PIXEL and Y_PIXEL parameters.
#     18-JUL-2003 (PWD):
#        Changed values_ and columns_ arrays to protected from common.
#        Removes chance of crosstalk to other toolboxes (the unset of
#        columns_ was doing this!) and such variables no longer need
#        to be common for scope to work in widgets.
#     26-SEP-2003 (PWD):
#        Added changes to support AUTOASTROM detections.
#     27-APR-2006 (PWD):
#        Added support for volatile images.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaSextractor {}

itcl::class gaia::GaiaSextractor {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: Object detection ($itk_option(-number))"

      #  Add short help window.
      make_short_help

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0

      #  Add the options menu
      set Options [add_menubutton "Options"]
      configure_menubutton Options -underline 0

      #  Add window help.
      add_help_button extractor "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      #  Add option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      $short_help_win_ add_menu_short_help $File \
         {New window} {Create a new toolbox}

      #  Read a previous catalogue, performing the appropriate actions
      #  to display ellipses. etc.
      $File add command \
         -label {Read old catalogue...} \
         -command [code $this read_old_catalogue] \
         -accelerator {Control-o}
      bind $w_ <Control-o> [code $this read_old_catalogue]
      $short_help_win_ add_menu_short_help $File \
         {Read old catalogue...}\
         {Re-read an old catalogue, displaying detection ellipses}

      #  Save config parameters to a file.
      $File add command \
         -label {Save config parameters...} \
         -command [code $this write_conpar_file] \
         -accelerator {Control-s}
      bind $w_ <Control-s> [code $this write_conpar_file]
      $short_help_win_ add_menu_short_help $File \
         {Save config parameters...}\
         {Write the configuration parameters out to a text file}

      #  Read config parameters from a file.
      $File add command \
         -label {Read config parameters...} \
         -command [code $this read_conpar_file] \
         -accelerator {Control-r}
      bind $w_ <Control-r> [code $this read_conpar_file]
      $short_help_win_ add_menu_short_help $File \
         {Read config parameters...}\
         {Read configuration parameters from a text file}

      #  Save catalogue columns to a file.
      $File add command \
         -label {Save catalogue parameters...} \
         -command [code $this write_catpar_file] \
         -accelerator {Control-t}
      bind $w_ <Control-t> [code $this write_catpar_file]
      $short_help_win_ add_menu_short_help $File \
         {Save catalogue parameters...}\
         {Write catalogue parameters out to a text file}

      #  Read catalogue columns from a file.
      $File add command \
         -label {Read catalogue parameters...} \
         -command [code $this read_catpar_file] \
         -accelerator {Control-u}
      bind $w_ <Control-u> [code $this read_catpar_file]
      $short_help_win_ add_menu_short_help $File \
         {Read catalogue parameters...}\
         {Read catalogue parameters from a text file}

      #  Set the exit menu item.
      $File add command -label Exit \
         -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]

      #  Add an option to not use the native version of SExtractor if
      #  the Starlink version is available.
      $Options add checkbutton  -label {Use Native SExtractor} \
         -variable [scope values_(use_native)] \
         -onvalue 1 \
         -offvalue 0
      $short_help_win_ add_menu_short_help $Options \
         {Use Native SExtractor} \
         {Use native SExtractor for FITS images}
      set values_(use_native) 1

      #  Add an option to make sure that we can plot kron or isophotal
      # ellipses. Note that the kron ellipses are the same as those SExtractor
      # produces using CHECK_APERTURES, these isophotal ellipses have no
      # equivalent in SExtractor (and are just ellipses with the correct
      # ELONGATION that have the isophotal area).
      $Options add checkbutton  -label {Draw Kron Ellipses} \
         -variable [scope values_(draw_kron_ellipses)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this toggle_draw_kron_ellipses_]
      $short_help_win_ add_menu_short_help $Options \
         {Draw Kron Ellipses}\
         {Fix output catalogue parameters for drawing Kron ellipses}
      set values_(draw_kron_ellipses) 0

      $Options add checkbutton  -label {Draw Isophotal Ellipses} \
         -variable [scope values_(draw_iso_ellipses)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this toggle_draw_iso_ellipses_]
      $short_help_win_ add_menu_short_help $Options \
         {Draw Isophotal Ellipses}\
         {Fix output catalogue parameters for drawing Isophotal ellipses}
      set values_(draw_iso_ellipses) 1

      #  Add option to draw simple circles (not scaled, so no special
      #  parameters).
      $Options add checkbutton  -label {Draw Circles} \
         -variable [scope values_(draw_circles)] \
         -onvalue 1 \
         -offvalue 0
      $short_help_win_ add_menu_short_help $Options \
         {Draw Circles}\
         {Draw detections as simple circles}
      set values_(draw_circles) 0

      #  Add option to draw isophotal radii, if determined.
      $Options add checkbutton  -label {Draw Radii} \
         -variable [scope values_(draw_radii)] \
         -onvalue 1 \
         -offvalue 0
      $short_help_win_ add_menu_short_help $Options \
         {Draw Radii}\
         {Draw any isophotal radii as circles}
      set values_(draw_radii) 0

      #  Add option to configure for use with AUTOASTROM.
      $Options add checkbutton  -label {AUTOASTROM Mode} \
         -variable [scope itk_option(-autoastrom)] \
         -onvalue 1 \
         -offvalue 0 \
         -command [code $this configure_autoastrom_mode_]
      $short_help_win_ add_menu_short_help $Options \
         {AUTOASTROM Mode}\
         {Configure for generating detection catalogues for AUTOASTROM}

      #  Allow selection of the detection image.
      set lwidth 18
      itk_component add detname {
         gaia::LabelFileChooser $w_.detname \
            -labelwidth $lwidth \
            -text "Detection image:" \
            -textvariable [scope values_(detname)] \
            -filter_types $itk_option(-filter_types)
      }
      pack $itk_component(detname) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(detname) \
         {Name of detection image, blank or NONE for displayed image}
      set values_(detname) "NONE"

      #  Add fields for setting the catalogue name and type(!).
      itk_component add catname {
         util::LabelEntry $w_.catname \
            -labelwidth $lwidth \
            -text "Catalogue name:" \
            -textvariable [scope values_(catname)] \
            -command [code $this set_catname_ to]
      }
      pack $itk_component(catname) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(catname) \
         {Name of output object catalogue (do not include file type)}

      itk_component add cattype {
         util::LabelMenu $w_.cattype \
            -labelwidth $lwidth \
            -text "Catalogue type:" \
            -variable [scope values_(cattype)]
      }
      pack $itk_component(cattype) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(cattype) \
         {Type of output catalogue}
      set_cattypes_

      #  Name for configuration parameters file and the catalogue
      #  parameters file.
      itk_component add conpar {
         util::LabelEntry $w_.conpar \
            -labelwidth $lwidth \
            -text "Config parameters:" \
            -textvariable [scope values_(conpar)]
      }
      pack $itk_component(conpar) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(conpar) \
         {Name of file to store configuration (i.e. program) parameters}

      itk_component add catpar {
         util::LabelEntry $w_.catpar \
            -labelwidth $lwidth \
            -text "Catalogue parameters:" \
            -textvariable [scope values_(catpar)]
      }
      pack $itk_component(catpar) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(catpar) \
         {Name of file to store catalogue parameters (i.e. columns)}


      #  Create the tab notebook for containing each page of options.
      itk_component add notebook {
         ::iwidgets::tabnotebook $w_.notebook -tabpos w -width 410 -height 300
      }
      pack $itk_component(notebook) -side top -fill both -expand 1 \
         -ipadx 1m -ipady 1m

      #  Add a page for the extraction options and get the child site.
      $itk_component(notebook) add -label Extraction \
         -command [code $this reveal_ 0]
      set pages_(0) extraction
      set revealed_(0) 0

      #  Add a page for the photometry options and get the child site.
      $itk_component(notebook) add -label Photometry \
         -command [code $this reveal_ 1]
      set pages_(1) photometry
      set revealed_(1) 0

      #  Add a page for the detector options and get the child site.
      $itk_component(notebook) add -label Detector \
         -command [code $this reveal_ 2]
      set pages_(2) detector
      set revealed_(2) 0

      #  Add a page for the star/galaxy separation options and get the
      #  child site.
      $itk_component(notebook) add -label Classification \
         -command [code $this reveal_ 3]
      set pages_(3) classification
      set revealed_(3) 0

      #  Add a page for the isophotal radii options and get the
      #  child site.
      $itk_component(notebook) add -label "Isophot Radii" \
         -command [code $this reveal_ 4]
      set pages_(4) radii
      set revealed_(4) 0

      #  Add a page for the background options and get the child site.
      $itk_component(notebook) add -label Background \
         -command [code $this reveal_ 5]
      set pages_(5) background
      set revealed_(5) 0

      #  Add a page for the check image options and get the child site.
      $itk_component(notebook) add -label Checkimage \
         -command [code $this reveal_ 6]
      set pages_(6) checkimage
      set revealed_(6) 0

      #  Add a page for the catalogue options and get the child site.
      $itk_component(notebook) add -label Catalogue \
         -command [code $this reveal_ 7]
      set pages_(7) catalogue
      set revealed_(7) 0

      #  Create the button bar
      itk_component add actionframe {frame $w_.action}

      #  Add a button to close window.
      itk_component add close {
         button $itk_component(actionframe).close -text Close \
            -command [code $this close]
      }
      add_short_help $itk_component(close) {Close window}

      #  Add a button to reset the current page.
      itk_component add resetpage {
         button $itk_component(actionframe).resetpage -text {Reset Page} \
            -command [code $this reset_page_ 0]
      }
      add_short_help $itk_component(resetpage) {Reset current page to builtin defaults}

      #  Add a button to reset all page.
      itk_component add resetall {
         button $itk_component(actionframe).resetall -text {Reset All Pages} \
            -command [code $this reset_page_ 1]
      }
      add_short_help $itk_component(resetall) {Reset all pages to their builtin defaults}

      #  Run SExtractor
      itk_component add run {
         button $itk_component(actionframe).run -text {Detect objects} \
            -command [code $this run]
      }
      add_short_help $itk_component(run) {Do object extraction}

      #  Add a status area for monitoring the output of the program.
      itk_component add status {
         gaia::Scrollbox $w_.status
      }
      $w_.status configure -height 5
      add_short_help $itk_component(status) \
         {Displays output from SExtractor}

      #  Pack all the components into place.
      pack $itk_component(notebook) -side top -fill both -expand 1 -pady 5 -padx 5
      pack $itk_component(actionframe) -side top -fill x -pady 5 -padx 5
      pack $itk_component(status) -side bottom -fill both -expand 1 -pady 5 -padx 5
      pack $itk_component(close) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(resetpage) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(resetall) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(run) -side left -expand 1 -pady 3 -padx 3

      #  Set the mapping from SExtractor names.
      set_mapping_

      #  Establish config/catalogue defaults.
      set_defaults_

      #  Select a page.
      $itk_component(notebook) select 0

      #  Set up ellipse drawing.
      toggle_draw_kron_ellipses_
      toggle_draw_iso_ellipses_

      #  Create an object for dealing with image names.
      set namer_ [gaia::GaiaImageName \#auto]
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Remove the control objects for the SExtractor application.
      if { $foreign_sex_ != {} } {
         $foreign_sex_ delete_sometime
      }
      if { $star_sex_ != {} } {
         $star_sex_ delete_sometime
      }
      if { $namer_ != {} } {
         catch {delete object $namer_}
      }
   }

   #  Methods:
   #  --------

   #  Create a new instance of this object.
   protected method clone_me_ {} {
      if { $itk_option(-clone_cmd) != {} } {
         eval $itk_option(-clone_cmd)
      }
   }

   #  Close this window, kill it if needed, otherwise withdraw.
   public method close {} {
      if { $itk_option(-really_die) } {
         delete object $this
      } else {
         wm withdraw $w_
      }
   }

   #  Reveal a page of widgets if not already done (these are deferred
   #  to give a better interactive response).
   protected method reveal_ {index {all 0}} {
      if { ! $all } {
         if { !$revealed_($index) } {
            set revealed_($index) 1
            set child [$itk_component(notebook) childsite $index]
            eval add_$pages_($index)_selections_ $child
         }
      } else {
         #  Reveal all pages.
         for {set i 0} {$i < 8} {incr i} {
            if { !$revealed_($i) } {
               set revealed_($i) 1
               set child [$itk_component(notebook) childsite $i]
               eval add_$pages_($i)_selections_ $child
            }
         }
      }
   }

   #  Reset the current page to its default values.
   protected method reset_page_ {{all 0}} {
      if { ! $all } {

         #  Get the index of the current page.
         set current [$itk_component(notebook) index select]
         eval reset_$pages_($current)_
      } else {

         #  Reset all pages.
         reveal_ 0 1
         for {set i 0} {$i < 7} {incr i} {
            eval reset_$pages_($i)_
         }
      }
   }

   #  Save the current config parameters to a file.
   public method write_conpar_file {} {
      set w [util::FileSelect .\#auto -title "Save config parameters to a file"]
      if {[$w activate]} {
         save_conpars [$w get]
      }
      destroy $w
   }

   #  Restore config parameters from a file.
   public method read_conpar_file {} {
      set w [util::FileSelect .\#auto -title "Read config parameters from a file"]
      if {[$w activate]} {
         read_conpars [$w get]
      }
      destroy $w
   }

   #  Save the catalogue parameters to a file.
   public method write_catpar_file {} {
      set w [util::FileSelect .\#auto -title "Save catalogue parameters to a file"]
      if {[$w activate]} {
         save_catpars [$w get]
      }
      destroy $w
   }

   #  Read columns from a file.
   public method read_catpar_file {} {
      set w [util::FileSelect .\#auto -title "Read catalogue parameters from a file"]
      if {[$w activate]} {
         read_catpars [$w get]
      }
      destroy $w
   }

   #  Write the current configuration parameters to a named file.
   public method save_conpars {filename} {
      if { $filename != {} } {
         busy {
            #  Open the output file.
            set fid [::open $filename w]
            puts $fid "\# GAIA SExtractor configuration parameters file."

            # And add all the known values.
            foreach name $valuenames_ {
               if { $to_($name) != "special" } {
                  if { [llength $values_($name)] == 1 } {
                     puts $fid "[format "%-16s" $to_($name)] $values_($name)"
                  } else {
                     puts $fid "[format "%-16s" $to_($name)] [lindex $values_($name) 0],[lindex $values_($name) 1]"
                  }
               } else {
                  set line [eval $special_($name) $values_($name)]
                  if { $line != "" } {
                     puts $fid $line
                  }
               }
            }
            ::close $fid
         }
      }
   }

   #  Read in configuration parameters that are stored in a file.
   public method read_conpars {filename} {
      if { [file readable $filename] } {
         busy {
            #  Open the file.
            set fid [open $filename r]

            #  Loop over the file skipping comments and blank
            #  lines. Lines are parsed using the first word as the
            #  parameter name and any trailing words as the
            #  value(s). If the translation between the names is
            #  straight-forward then do the assignment to values_,
            #  otherwise pass on all information to a special
            #  filter. Special commands may contain environment
            #  variables. These are expanded before passing on to the
            #  appropriate method.
            set ok 1
            while { $ok  } {
               set llen [gets $fid line]
               if { $llen > 0 } {
                  if { ! [string match {\#*} $line] } {
                     set tmpline [join [split $line]]
                     set keyword [ctoken tmpline {, }]
                     set line $tmpline
                     if { $keyword != {} && $keyword != "\#" } {
                        if { [info exists from_($keyword)] } {
                           set value1 [ctoken tmpline {, }]
                           set value2 [ctoken tmpline {, }]
                           if { $from_($keyword) != "special" } {
                              if { [string match {\#*} $value2] || $value2 == {} } {
                                 set values_($from_($keyword)) $value1
                              } else {
                                 set values_($from_($keyword)) [list $value1 $value2]
                              }
                           } else {
                              set params [ctoken line {\#}]
                              if { [string match {*$*} $params] } {
                                 set params [exec /bin/sh -c "echo $params"]
                              }
                              eval $special_($keyword) $params
                           }
                        } else {
                           info_dialog "Unrecognisable parameter:$keyword.\nIn file: $filename"
                        }
                     }
                  }
               } elseif { $llen < 0 } {

                  # End of file.
                  set ok 0
               }
            }
            ::close $fid
         }
      }
   }

   #  Read back any catalogue parameters (columns) that are stored in a file.
   public method read_catpars {filename} {
      if { [file readable $filename] } {
         busy {
            #  Clear all existing columns
            catch {unset columns_}

            #  Open the file.
            set fid [open $filename r]

            #  Loop over the file skipping comments and blank
            #  lines. The present of a keyword means calculate this
            #  column.
            set ok 1
            while { $ok  } {
               set llen [gets $fid line]
               if { $llen > 0 } {
                  if { ! [string match {\#*} $line] } {
                     set keyword [ctoken line {, }]
                     set columns_($keyword) 1
                  }
               } elseif { $llen < 0 } {

                  # End of file.
                  set ok 0
               }
            }
            ::close $fid
         }
      }
   }

   #  Save catalogue parameters (columns) to a file.
   public method save_catpars {filename} {
      if { $filename != {} } {
         busy {
            #  Open the output file.
            set fid [::open $filename w]
            puts $fid "\# GAIA SExtractor catalogue parameters file."

            # And add all the known columns. Ones which are not set
            # are "commented" out.
            foreach {name desc} $columnnames_ {
               if { [info exists columns_($name)] &&
                    $columns_($name) } {
                  puts $fid "$name"
               } else {
                  puts $fid "\#$name"
               }
            }

            ::close $fid
         }
      }
   }

   #  Control if catalogue parameters (columns) necessary for drawing
   #  ellipses are present.
   protected method toggle_draw_kron_ellipses_ {} {
      if { $values_(draw_kron_ellipses) } {
         foreach param "$commoncols_ $kroncols_" {
            set columns_($param) 1
            if { $revealed_(7) } {
               $colparent_.[string tolower $param] configure -state disabled
            }
         }

      } else {
         set names $kroncols_
         if { !$values_(draw_iso_ellipses) } {
            append names $commoncols_
         }
         foreach param $names {
            set columns_($param) 0
            if { $revealed_(7) } {
               $colparent_.[string tolower $param] configure -state normal
            }
         }
      }
   }
   protected method toggle_draw_iso_ellipses_ {} {
      if { $values_(draw_iso_ellipses) } {
         foreach param "$commoncols_ $isocols_" {
            set columns_($param) 1
            if { $revealed_(7) } {
               $colparent_.[string tolower $param] configure -state disabled
            }
         }

      } else {
         set names $isocols_
         if { !$values_(draw_kron_ellipses) } {
            append names $commoncols_
         }
         foreach param $names {
            set columns_($param) 0
            if { $revealed_(7) } {
               $colparent_.[string tolower $param] configure -state normal
            }
         }
      }
   }

   #  Disable "safe" columns.
   protected method fix_safe_columns_ {} {
      foreach param "$safecols_" {
         set columns_($param) 1
         if { $revealed_(7) } {
            $colparent_.[string tolower $param] configure -state disabled
         }
      }
   }

   #  Add controls for all the extraction parameters.
   protected method add_extraction_selections_ {parent} {
      set lwidth 16
      set vwidth 5

      #  Minimum size of an object, in pixels.
      itk_component add minsize {
         util::LabelEntryScale $parent.minsize \
            -text "Object size:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -from 1 \
            -to 100 \
            -increment 1  \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -show_scale 0 \
            -validate integer \
            -value $values_(minsize) \
            -textvariable [scope values_(minsize)] \
            -command [code $this set_values_ minsize]
      }
      pack $itk_component(minsize) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(minsize) \
         {Minimum number of pixels above detection threshold}

      #  The type of threshold. This can be either relative or
      #  absolute.
      #  Type of background estimate used for detections.
      itk_component add threshtype {
         util::LabelMenu $parent.threshtype \
            -text "Threshold type:" \
            -labelwidth $lwidth \
            -variable [scope values_(threshtype)]
      }
      pack $itk_component(threshtype) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(threshtype) \
         {Type of values in given in threshold}
      foreach {longname shortname} \
         "{Background RMS} RELATIVE {Data units} ABSOLUTE" {
         $itk_component(threshtype) add \
            -label $longname \
            -value $shortname \
            -command [code $this toggle_threshtype_ $shortname]
      }
      set values_(threshtype) $values_(threshtype)

      #  The detection threshold. Either a number of standard
      #  deviations, or a surface brightness and zero point.
      itk_component add detthresh {
         gaia::ManyLabelEntry $parent.detthresh \
            -nentry 2 \
            -text "Detection threshold:" \
            -labelwidth $lwidth \
            -anchor w \
            -validate real \
            -orient horizontal \
            -value $values_(detthresh) \
            -textvariable [scope values_(detthresh)] \
            -command [code $this set_values_ detthresh]
      }
      pack $itk_component(detthresh) -side top -fill x -ipadx 1m -ipady 1m

      #  Analysis threshold.
      itk_component add analthresh {
         gaia::ManyLabelEntry $parent.analthresh \
            -nentry 2 \
            -text "Analysis threshold:" \
            -labelwidth $lwidth \
            -anchor w \
            -validate real \
            -value $values_(analthresh) \
            -orient horizontal \
            -textvariable [scope values_(analthresh)] \
            -command [code $this set_values_ analthresh]
      }
      pack $itk_component(analthresh) -side top -fill x -ipadx 1m -ipady 1m
      toggle_threshtype_ $values_(threshtype)

      #  Detection filter. Either NONE or some known filename. Note
      #  the use of full path names.
      itk_component add detfilter {
         util::LabelMenu $parent.detfilter \
            -text "Detection filter:" \
            -labelwidth $lwidth \
            -variable [scope values_(detfilter)]
      }
      pack $itk_component(detfilter) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(detfilter) \
         {Type of detection filter to use, NONE for no filter}
      set detfiles "NONE [lsort [glob -nocomplain ${config_dir_}/*.conv]]"
      foreach fullname $detfiles {
         set shortname [file rootname [file tail $fullname]]
         $itk_component(detfilter) add \
            -label $shortname \
            -value $fullname \
            -command [code $this set_values_ detfilter $fullname]
      }
      set values_(detfilter) $values_(detfilter)

      #  Number of thresholds used in deblending.
      itk_component add debthresh {
         util::LabelEntryScale $parent.debthresh \
            -text "Deblend thresholds:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -from 1 \
            -to 64 \
            -increment 1 \
            -resolution 1 \
            -show_arrows 1 \
            -show_scale 0 \
            -anchor w \
            -validate integer \
            -fix_range 1 \
            -value $values_(debthresh) \
            -textvariable [scope values_(debthresh)] \
            -command [code $this set_values_ debthresh]
      }
      pack $itk_component(debthresh) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(debthresh) \
         {Number of sub-thresholds used to deblend (1 to 64)}

      #  Contrast parameter.
      itk_component add debcontrast {
         util::LabelEntryScale $parent.debcontrast \
            -text "Contrast parameter:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -increment 0.0001  \
            -resolution 0.0001 \
            -show_arrows 1 \
            -validate real \
            -anchor w \
            -value $values_(debcontrast) \
            -textvariable [scope values_(debcontrast)] \
            -from 0.0 \
            -to [max 0.1 [min 1.0 $values_(debcontrast)]] \
            -command [code $this set_values_ debcontrast]
      }
      pack $itk_component(debcontrast) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(debcontrast) \
         {Minimum deblending contrast parameter (0 to 1)}

      #  Whether to clean detections and the efficiency.
      itk_component add detclean {
         gaia::StarLabelCheck $parent.detclean \
            -text "Clean detections:" \
            -onvalue Y \
            -offvalue N \
            -labelwidth $lwidth \
            -anchor w \
            -variable [scope values_(detclean)] \
            -command [code $this toggle_clean_]
      }
      pack $itk_component(detclean) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(detclean) \
         {Clean catalogue for false detections}

      itk_component add deteffic {
         util::LabelEntryScale $parent.deteffic \
            -text "Clean efficiency:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -increment 0.1  \
            -resolution 0.1 \
            -show_arrows 1 \
            -from 0.1 \
            -to 10.0 \
            -fix_range 1 \
            -validate real \
            -anchor w \
            -value $values_(deteffic) \
            -textvariable [scope values_(deteffic)] \
            -command [code $this set_values_ deteffic]
      }
      pack $itk_component(deteffic) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(deteffic) \
         {Cleaning efficiency (0.1 to 10)}
      toggle_clean_
   }

   #  Method to toggle state of cleaning parameter.
   private method toggle_clean_ {args} {
      if { $values_(detclean) == "Y" } {
         $itk_component(deteffic) configure -state normal
      } else {
         $itk_component(deteffic) configure -state disabled
      }
   }

   #  Method to toggle the short help for the threshold field.
   private method toggle_threshtype_ {value} {
      set values_(threshtype) $value
      if { $value == "RELATIVE" } {
         set help \
            {Standard deviations above background, or surface brightness, zero point}
      } else {
         set help {Data units above background}
      }
      add_short_help $itk_component(analthresh) "$help"
      add_short_help $itk_component(detthresh) "$help"
   }

   #  Method translate SExtractor native filter options to local
   #  ones. These names may contain C-shell environment variables that
   #  need expanding.
   private method set_filter_ {flag args} {
      switch -exact $flag {
         flag {
            #  Filter = Y/N option.
            set filterflag_ [lindex $args 0]
         }
         from {
            #  Set filename of filter. Check that this is a fullname,
            #  otherwise assume a name in $itk_option(-sex_dir) is
            #  being used.
            if { $filterflag_ == "Y" } {
               set name [lindex $args 0]
               if { "[string index $name 0]" != "/"} {
                  set values_(detfilter) "${config_dir_}/$name"
               } else {
                  set values_(detfilter) "$name"
               }
               set filterflag_ "N"
            }
         }
         to {

            #  Convert local setup into a SExtractor description.
            if { $values_(detfilter) == "NONE" } {
               return "FILTER           N\nFILTER_NAME      default.conv"
            } else {
               return "FILTER           Y\nFILTER_NAME      $values_(detfilter)"
            }
         }
      }
   }

   #  Set the file type of the catalogue. This is sychronised with the
   #  name, so that we always use the correct file extension.
   private method set_catname_ {flag {value ""} } {
      if { $value == "" } {
         #  Change by interface, check for extension and remove if
         #  present.
         set $values_(catname) [file rootname $values_(catname)]
         return
      }

      #  Now either return the filename with the correct extension, or
      #  remove the extension and set the main variable.
      if { $flag == "to" } {

         #  Name to SExtractor conversion. Get the full filename.
         return "CATALOG_NAME     [get_catname_]"
      } else {

         #  Convert SEXtractor name to local one, just remove the file
         #  extension. The rest is controlled by the file type.
         set values_(catname) [file rootname $value]
      }
   }

   #  Set the name of the catalogue parameters file.  When read from
   #  the defaults file we just use the default name (we want to store
   #  this and the main defaults file locally.
   private method set_parname_ {flag {value ""}} {

      #  Now either return the filename or write default name.
      if { $flag == "to" } {

         #  Name to SExtractor conversion. Get the full filename.
         return "PARAMETERS_NAME     $values_(catpar)"
      } else {
         set values_(catpar) $defaults_(catpar)
      }
   }

   #  Construct the catalogue name. Use the correct type and make sure
   #  that file name is a rootname. Note that unknown types default to
   #  ".TAB"
   private method get_catname_ {} {
      set type ".TAB"
      switch -glob $values_(cattype) {
         ASCII_SKYCAT {
            set type ".TAB"
         }
         FITS* {
            set type ".FIT"
         }
         ASCII_HEAD {
            set type ".ASC"
         }
      }
      set name [file rootname $values_(catname)]
      return "${name}${type}"
   }

   #  Set the file type, i.e. the correct file extenstion.
   private method set_cattype_ {flag value} {
      if { $flag == "to" } {

         #  Return SExtractor file type.
         return "CATALOG_TYPE     $values_(cattype)"
      } else {

         #  Accept SExtractor file type and make sure that it is
         #  recognised.
         set type "ASCII_SKYCAT"
         switch -glob $value {
            FITS* {
               set type "FITS_1.0"
            }
            ASCII_HEAD {
               set type "ASCII_HEAD"
            }
         }
         set values_(cattype) $type
      }
   }

   #  Reset the extraction page to the built-in defaults.
   protected method reset_extraction_ {} {
      $itk_component(minsize) configure -value $defaults_(minsize)
      $itk_component(detthresh) configure -value $defaults_(detthresh)
      $itk_component(analthresh) configure -value $defaults_(analthresh)
      $itk_component(detfilter) configure -value $defaults_(detfilter)
      $itk_component(debthresh) configure -value $defaults_(debthresh)
      $itk_component(debcontrast) configure -value $defaults_(debcontrast)
      set values_(detclean) $defaults_(detclean)
      toggle_clean_
      $itk_component(deteffic) configure -value $defaults_(deteffic)
   }

   #  Add controls for all the photometry parameters.
   protected method add_photometry_selections_ {parent} {
      set lwidth 13
      set vwidth 5

      #  Use a scrolled frame to get all these in a small amount of
      #  real estate.
      itk_component add photframe {
         ::iwidgets::scrolledframe $parent.photframe -width 75 -height 400
      }
      pack $itk_component(photframe) -fill both -expand 1
      set childsite [$itk_component(photframe) childsite]

      #  Zero point (magnitudes).
      itk_component add photzero {
         util::LabelEntry $childsite.photzero \
            -text "Zero point:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -validate real \
            -value $values_(photzero) \
            -textvariable [scope values_(photzero)] \
            -command [code $this set_values_ photzero]
      }
      pack $itk_component(photzero) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(photzero) \
         {Zero point for magnitudes}

      #  Number and size of apertures for photometry. XXX only 1 works
      #  in SExtractor 2.0.19, so removed for now.
      itk_component add photnum {
         util::LabelEntryScale $childsite.photnum \
            -text "Number of apertures:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -increment 1  \
            -resolution 1 \
            -anchor w \
            -show_arrows 1 \
            -show_scale 0 \
            -from 1 \
            -to 32 \
            -fix_range 1 \
            -validate integer \
            -value $values_(photnum) \
            -command [code $this set_photnum_]
      }
      #  pack $itk_component(photnum) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(photnum) \
         {Number of fixed sized apertures to use (1 to 32)}

      itk_component add photapps {
         gaia::ManyLabelEntry $childsite.photapp \
            -text "Aperture size:" \
            -labelwidth $lwidth \
            -nentry $values_(photnum) \
            -anchor w \
            -orient vertical \
            -textvariable [scope values_(photapps)] \
            -command [code $this set_values_ photapps]
      }
      update idletasks ;#  Needed to stop problem with -borderwidth values.
      eval $itk_component(photapps) setvals $values_(photapps)
      pack $itk_component(photapps) -side top -fill x -ipadx 1m -ipady 1m
      #add_short_help $itk_component(photapps) \
      #   {Aperture sizes (in pixels, up to 32 values)}
      add_short_help $itk_component(photapps) \
         {Aperture size, in pixels}

      #  Kron photometry. Get the Kron factor and the minimum radius
      #  for objects analysed this way.
      itk_component add kronfact {
         util::LabelEntryScale $childsite.kronfact \
            -text "Kron factor:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -increment 0.1  \
            -resolution 0.1 \
            -anchor w \
            -show_arrows 1 \
            -show_scale 1 \
            -from 1.0 \
            -to 5.0 \
            -fix_range 1 \
            -validate real \
            -value $values_(kronfact) \
            -textvariable [scope values_(kronfact)] \
            -command [code $this set_values_ kronfact]
      }
      pack $itk_component(kronfact) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(kronfact) \
         {Kron factor (elliptical total magnitudes)}

      itk_component add kronmin {
         util::LabelEntryScale $childsite.kronmin \
            -text "Kron min radius:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -increment 0.1  \
            -resolution 0.1 \
            -anchor w \
            -show_arrows 1 \
            -show_scale 1 \
            -from 1.0 \
            -to 20.0 \
            -validate real \
            -value $values_(kronmin) \
            -textvariable [scope values_(kronmin)] \
            -command [code $this set_values_ kronmin]
      }
      pack $itk_component(kronmin) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(kronmin) \
         {Minimum applicable radius for Kron magnitudes}


      #  Petrosian photometry. Get the Petrosian factor and the minimum radius
      #  for objects analysed this way.
      itk_component add petrofact {
         util::LabelEntryScale $childsite.petrofact \
            -text "Petrosian factor:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -increment 0.1  \
            -resolution 0.1 \
            -anchor w \
            -show_arrows 1 \
            -show_scale 1 \
            -from 1.0 \
            -to 5.0 \
            -fix_range 1 \
            -validate real \
            -value $values_(petrofact) \
            -textvariable [scope values_(petrofact)] \
            -command [code $this set_values_ petrofact]
      }
      pack $itk_component(petrofact) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(petrofact) {Petrosian magnitudes factor}

      itk_component add petromin {
         util::LabelEntryScale $childsite.petromin \
            -text "Petrosian min radius:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -increment 0.1  \
            -resolution 0.1 \
            -anchor w \
            -show_arrows 1 \
            -show_scale 1 \
            -from 1.0 \
            -to 20.0 \
            -validate real \
            -value $values_(petromin) \
            -textvariable [scope values_(petromin)] \
            -command [code $this set_values_ petromin]
      }
      pack $itk_component(petromin) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(petromin) \
         {Minimum applicable radius for Petrosian magnitudes}

      #  Flux fraction. Get a fractional flux value to determine a radius.
      #  Half-light, or really half kron flux.
      itk_component add fluxfrac {
         util::LabelEntryScale $childsite.fluxfrac \
            -text "Flux fraction:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -increment 0.1  \
            -resolution 0.1 \
            -anchor w \
            -show_arrows 1 \
            -show_scale 1 \
            -from 0.0 \
            -to 1.0 \
            -fix_range 1 \
            -validate real \
            -value $values_(fluxfrac) \
            -textvariable [scope values_(fluxfrac)] \
            -command [code $this set_values_ fluxfrac]
      }
      pack $itk_component(fluxfrac) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(fluxfrac) \
         {Fraction of auto flux, gives a radius parameter}

      #  Type of MASKing used on photometry neighbours.
      itk_component add photmask {
         util::LabelMenu $childsite.detmask \
            -text "Neighbour mask:" \
            -labelwidth $lwidth \
            -variable [scope values_(photmask)]
      }
      pack $itk_component(photmask) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(photmask) \
         {Type of masking for neighbouring objects}
      foreach {longname shortname} \
         "{no masking} NONE {set to zero} BLANK {symmetric correction} CORRECT" {
         $itk_component(photmask) add \
            -label $longname \
            -value $shortname \
            -command [code $this set_values_ photmask $shortname]
      }
      set values_(photmask) $values_(photmask)
   }

   #  Set the number of entry fields for apertures.
   private method set_photnum_ {num} {
      $itk_component(photapps) configure -nentry $num
   }

   #  Method to translate SExtractor native apertures options to local ones.
   #  For this widget set a set of values are translated into a number
   #  and list and vice versa.
   protected method set_apertures_ {flag args} {
      switch -exact $flag {
         from {
            #  Count args, removing any commas.
            regsub -all  {,} $args { } largs
            set nargs [llength $largs]
            if { [info exists itk_component(photapps)] } {
               set_photnum_ $nargs
            } else {
               set values_(photnum) $nargs
            }
            set values_(photapps) $largs
         }
         to {
            set result ""
            set first 1
            for {set i 0} {$i < $values_(photnum)} {incr i} {
               set value [lindex $values_(photapps) $i]
               if { $value != "" } {
                  if { $first } {
                     append result "$value"
                     set first 0
                  } else {
                     append result ",$value"
                  }
               }
            }
            return "PHOT_APERTURES   $result"
         }
      }
   }

   #  Method to translate SExtractor native Kron options to local
   #  ones. The difference here is that we map one parameter into two
   #  widgets.
   protected method set_kronmags_ {flag args} {
      switch -exact $flag {
         from {
            #  Parse args into one or two values.
            set value1 [ctoken args {, }]
            set value2 [ctoken args {, }]
            if { $value1 != {} } {
               set values_(kronfact) $value1
            }
            if { $value2 != {} } {
               set values_(kronmin) $value2
            }
         }
         to {
            return "PHOT_AUTOPARAMS      $values_(kronfact),$values_(kronmin)"
         }
      }
   }

   #  Method to translate SExtractor native Petrosian options to local
   #  ones. The difference here is that we map one parameter into two
   #  widgets.
   protected method set_petromags_ {flag args} {
      switch -exact $flag {
         from {
            #  Parse args into one or two values.
            set value1 [ctoken args {, }]
            set value2 [ctoken args {, }]
            if { $value1 != {} } {
               set values_(petrofact) $value1
            }
            if { $value2 != {} } {
               set values_(petromin) $value2
            }
         }
         to {
            return "PHOT_PETROPARAMS  $values_(petrofact),$values_(petromin)"
         }
      }
   }

   #  Reset photometry parameters page to builtin defaults.
   protected method reset_photometry_ {} {
      $itk_component(photzero) configure -value $defaults_(photzero)
      $itk_component(photnum) configure -value $defaults_(photnum)
      set_photnum_ $defaults_(photnum)
      eval $itk_component(photapps) setvals $defaults_(photapps)
      $itk_component(kronfact) configure -value $defaults_(kronfact)
      $itk_component(kronmin) configure -value $defaults_(kronmin)
      $itk_component(petrofact) configure -value $defaults_(petrofact)
      $itk_component(petromin) configure -value $defaults_(petromin)
      $itk_component(fluxfrac) configure -value $defaults_(kronfact)
      $itk_component(photmask) configure -value $defaults_(photmask)

   }

   #  Add controls for all the detector options.
   protected method add_detector_selections_ {parent} {
      set lwidth 15
      set vwidth 5

      #  Type of detector.
      itk_component add dettype {
         util::LabelMenu $parent.dettype \
            -text "Detector type:" \
            -labelwidth $lwidth \
            -variable [scope values_(dettype)]
      }
      pack $itk_component(dettype) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(dettype) \
         {Type of detector, use CCD for all linear detectors}
      foreach {longname shortname} "CCD CCD {Photographic scan} PHOTO" {
         $itk_component(dettype) add \
            -label $longname \
            -value $shortname \
            -command [code $this toggle_detector_ $shortname]
      }
      set values_(dettype) $values_(dettype)

      #  Size of a pixel in arcseconds.
      itk_component add imagescale {
         util::LabelEntry $parent.imagescale \
            -text "Image scale:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -validate real \
            -value $values_(imagescale) \
            -textvariable [scope values_(imagescale)] \
            -command [code $this set_values_ imagescale]
      }
      pack $itk_component(imagescale) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(imagescale) \
         {Size of a pixel in arcseconds (0=use FITS WCS)}

      #  Data saturation value.
      itk_component add photsat {
         util::LabelEntry $parent.photsat \
            -text "Saturation level:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -validate real \
            -value $values_(photsat) \
            -textvariable [scope values_(photsat)] \
            -command [code $this set_values_ photsat]
      }
      pack $itk_component(photsat) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(photsat) \
         {Value at which data saturates (data units)}

      #  Gain of detector (CCD).
      itk_component add photgain {
         util::LabelEntryScale $parent.photgain \
            -text "Detector gain:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -increment 0.01  \
            -resolution 0.01 \
            -anchor w \
            -show_arrows 1 \
            -show_scale 1 \
            -from 0.01 \
            -to 5.0 \
            -validate real \
            -value $values_(photgain) \
            -textvariable [scope values_(photgain)] \
            -command [code $this set_values_ photgain]
      }
      pack $itk_component(photgain) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(photgain) \
         {Gain of CCD/linear detector (electrons/data unit)}

      #  Gamma for photographic emulsion.
      itk_component add photgamma {
         util::LabelEntry $parent.photgamma \
            -text "Emulsion gamma:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -validate real \
            -value $values_(photgamma) \
            -textvariable [scope values_(photgamma)] \
            -command [code $this set_values_ photgamma]
      }
      pack $itk_component(photgamma) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(photgamma) \
         {Gamma of photographic emulsion (if required)}

      #  Initialise widget states.
      toggle_detector_ $values_(dettype)
   }

   #  Set the detector type and configure related fields.
   protected method toggle_detector_ {value} {
      set values_(dettype) $value
      if { $value == "CCD" } {
         $itk_component(photgain) configure -state normal
         $itk_component(photgamma) configure -state disabled
      } else {
         $itk_component(photgain) configure -state disabled
         $itk_component(photgamma) configure -state normal
      }
   }

   #  Reset the detector options to the builtin defaults.
   protected method reset_detector_ {} {
      $itk_component(dettype) configure -value $defaults_(dettype)
      toggle_detector_ $defaults_(dettype)
      $itk_component(imagescale) configure -value $defaults_(imagescale)
      $itk_component(photsat) configure -value $defaults_(photsat)
      $itk_component(photgain) configure -value $defaults_(photgain)
      $itk_component(photgamma) configure -value $defaults_(photgamma)
   }

   #  Add controls for all the classification parameters.
   protected method add_classification_selections_ {parent} {
      set lwidth 18
      set vwidth 5

      #  Seeing FWHM.
      itk_component add fwhm {
         util::LabelEntry $parent.fwhm \
            -text "Seeing FWHM:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -validate real \
            -value $values_(fwhm) \
            -textvariable [scope values_(fwhm)] \
            -command [code $this set_values_ fwhm]
      }
      pack $itk_component(fwhm) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(fwhm) \
         {FWHM of stellar images in arcseconds}

      #  Neural network weight table.
      itk_component add nettable {
         util::LabelMenu $parent.nettable \
            -text "Neural network table:" \
            -labelwidth $lwidth \
            -variable [scope values_(nettable)]
      }
      pack $itk_component(nettable) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(nettable) \
         {Name of neural network weights file}
      set netfiles "[lsort [glob -nocomplain ${config_dir_}/*.nnw]]"
      foreach longname $netfiles {
         set shortname [file rootname [file tail $longname]]
         $itk_component(nettable) add \
            -label $shortname \
            -value $longname \
            -command [code $this set_values_ nettable $longname]
      }
      set values_(nettable) $values_(nettable)
   }

   #  Reset classification parameters to the builtin defaults.
   protected method reset_classification_ {} {
      $itk_component(fwhm) configure -value $defaults_(fwhm)
      $itk_component(nettable) configure -value $defaults_(nettable)
   }

   #  Add controls for all the isophotal radii parameters.
   protected method add_radii_selections_ {parent} {
      set lwidth 20
      set vwidth 5

      #  Type of values used to estimate radii.
      itk_component add radtype {
         util::LabelMenu $parent.radtype \
            -labelwidth $lwidth \
            -text "Type of values:" \
            -variable [scope values_(radtype)]
      }
      pack $itk_component(radtype) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(radtype) \
         {Type of values used to determine isophotal thresholds}
      foreach {lname sname} \
         "Default NONE {Surface brightness} SB {Data counts} INT" {
         $itk_component(radtype) add \
            -label $lname \
            -value $sname \
            -command [code $this toggle_radtype_ $sname]
      }

      #  Values that determine the thresholds used.
      itk_component add radthresh {
         gaia::ManyLabelEntry $parent.radthresh \
            -nentry 3 \
            -text "Threshold parameters:" \
            -labelwidth $lwidth \
            -anchor w \
            -validate real \
            -orient vertical \
            -value $values_(radthresh) \
            -textvariable [scope values_(radthresh)] \
            -command [code $this set_values_ radthresh]
      }
      pack $itk_component(radthresh) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(radthresh) \
         {Step, [start] and [zeropoint] (SB only) of thresholds}
      toggle_radtype_ $values_(radtype)
   }

   #  Reset radii parameters to the builtin defaults.
   protected method reset_radii_ {} {
      $itk_component(radtype) configure -value $defaults_(radtype)
      $itk_component(radthresh) configure -value $defaults_(radthresh)
   }

   #  Toggle state of other fields according to value of radtype.
   protected method toggle_radtype_ {intype} {
      if { $intype == "NONE" } {
         $itk_component(radthresh) configure -state disabled
      } else {
         $itk_component(radthresh) configure -state normal
      }
      set values_(radtype) $intype
   }

   #  Set the value of RAD_TYPE. This is special as NONE means
   #  do not use this parameter.
   private method set_radtype_ {flag args} {
      switch -exact $flag {
         from {
            #  Read in value, must be OK.
            set values_(radtype) $args
         }
         to {
            #  Return blank if "NONE".
            if { "$args" == "NONE" } {
               return ""
            } else {
               return "RAD_TYPE         $args"
            }
         }
      }
   }

   #  Set the value of RAD_THRESH. This is special as radtype of NONE means
   #  do not use this parameter.
   private method set_radthresh_ {flag args} {
      switch -exact $flag {
         from {
            #  Read in value(s), must be OK. Only catch is that these
            #  may be comma separated.
            regsub -all  {,} $args { } largs
            set values_(radthresh) $largs
         }
         to {
            #  Return blank if radtype "NONE".
            if { "$values_(radtype)" == "NONE" } {
               return ""
            } else {

               return "RAD_THRESH       [join $args {,}]"
            }
         }
      }
   }

   #  Set the value of the network table filename. This will be
   #  expanded to full if reading.
   private method set_nettable_ {flag args} {
      switch -exact $flag {
         from {
            #  Set filename of filter. Check that this is a fullname,
            #  otherwise assume a name in $config_dir_ is being used.
            set name [lindex $args 0]
            if { "[string index $name 0]" != "/"} {
               set values_(nettable) ${config_dir_}/$name
            } else {
               set values_(nettable) $name
            }
         }
         to {
            return "STARNNW_NAME     $values_(nettable)"
         }
      }
   }

   #  Add controls for all the background parameters.
   protected method add_background_selections_ {parent} {
      set lwidth 15
      set vwidth 5

      #  Type of background estimate used for detections.
      itk_component add backtype {
         util::LabelMenu $parent.backtype \
            -text "Background type:" \
            -labelwidth $lwidth \
            -variable [scope values_(backtype)]
      }
      pack $itk_component(backtype) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(backtype) \
         {Type of background estimated used during detection}
      foreach {longname shortname} \
         "{Mesh based} AUTO {Constant} MANUAL" {
         $itk_component(backtype) add \
            -label $longname \
            -value $shortname \
            -command [code $this toggle_backvalue_ $shortname]
      }
      set values_(backtype) $values_(backtype)

      #  Value of background (if backtype is MANUAL).
      itk_component add backvalue {
         util::LabelEntry $parent.backvalue \
            -text "Background value:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -validate real \
            -value $values_(backvalue) \
            -textvariable [scope values_(backvalue)] \
            -command [code $this set_values_ backvalue]
      }
      pack $itk_component(backvalue) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(backvalue) \
         {Value to be used as image background}

      #  Background mesh size.
      itk_component add backmesh {
         gaia::ManyLabelEntry $parent.backmesh \
            -nentry 2 \
            -text "Mesh size:" \
            -labelwidth $lwidth \
            -anchor w \
            -validate integer \
            -value $values_(backmesh) \
            -textvariable [scope values_(backmesh)] \
            -orient horizontal \
            -command [code $this set_values_ backmesh]
      }
      pack $itk_component(backmesh) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(backmesh) \
         {Size of mesh used to estimate local background (size or width,height)}

      #  Background filter size.
      itk_component add backfilter {
         gaia::ManyLabelEntry $parent.backfilter \
            -nentry 2 \
            -text "Filter size:" \
            -labelwidth $lwidth \
            -anchor w \
            -validate integer \
            -value $values_(backfilter) \
            -textvariable [scope values_(backfilter)] \
            -orient horizontal \
            -command [code $this set_values_ backfilter]
      }
      pack $itk_component(backfilter) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(backfilter) \
         {Size of local median filter (size or width,height) max=7}

      #  Type of background estimates used when estimate magnitudes.
      itk_component add backphot {
         util::LabelMenu $parent.backphot \
            -text "Photometry type:" \
            -labelwidth $lwidth \
            -variable [scope values_(backphot)]
      }
      pack $itk_component(backphot) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(backphot) \
         {Type of background estimated used in photometry}
      foreach {longname shortname} \
         "{Background map} GLOBAL {Rectangular annulus} LOCAL" {
         $itk_component(backphot) add \
            -label $longname \
            -value $shortname \
            -command [code $this toggle_annulus_ $shortname]
      }
      set values_(backphot) $values_(backphot)

      #  Thickness of background annulus.
      itk_component add backthick {
         util::LabelEntryScale $parent.backthick \
            -text "Thickness:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -increment 1  \
            -resolution 1 \
            -anchor w \
            -show_arrows 1 \
            -show_scale 0 \
            -from 1 \
            -to 256 \
            -fix_range 1 \
            -validate integer \
            -value $values_(backthick) \
            -textvariable [scope values_(backthick)] \
            -command [code $this set_values_ backthick]
      }
      pack $itk_component(backthick) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(backthick) \
         {Thickness (in pixels) of the background annulus}
      toggle_annulus_ $values_(backphot)
      toggle_backvalue_ $values_(backtype)
   }

   #  Method to toggle state of annulus parameter.
   private method toggle_annulus_ {value} {
      set values_(backphot) $value
      if { "$value" == "LOCAL" } {
         $itk_component(backthick) configure -state normal
      } else {
         $itk_component(backthick) configure -state disabled
      }
   }

   #  Method to toggle state of back ground value parameter.
   private method toggle_backvalue_ {value} {
      set values_(backtype) $value
      if { "$value" == "MANUAL" } {
         $itk_component(backvalue) configure -state normal
         $itk_component(backmesh) configure -state disabled
         $itk_component(backfilter) configure -state disabled
      } else {
         $itk_component(backvalue) configure -state disabled
         $itk_component(backmesh) configure -state normal
         $itk_component(backfilter) configure -state normal
      }
   }

   #  Reset all the background selections to their builtin defaults.
   protected method reset_background_ {} {
      $itk_component(backmesh) configure -value $defaults_(backmesh)
      $itk_component(backfilter) configure -value $defaults_(backfilter)
      $itk_component(backvalue) configure -value $defaults_(backvalue)
      $itk_component(backtype) configure -value $defaults_(backtype)
      toggle_backvalue_ $defaults_(backtype)
      $itk_component(backphot) configure -value $defaults_(backphot)
      toggle_annulus_ $defaults_(backphot)
      $itk_component(backthick) configure -value $defaults_(backthick)
   }

   #  Add controls for all the check image parameters.
   protected method add_checkimage_selections_ {parent} {
      set lwidth 16
      set vwidth 5

      #  Type of checkimage to produce.
      itk_component add checktype {
         util::LabelMenu $parent.checktype \
            -text "Checkimage type:" \
            -labelwidth $lwidth \
            -variable [scope values_(checktype)]
      }
      pack $itk_component(checktype) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(checktype) \
         {Type of information to add to checkimage}
      foreach {shortname longname} "$checktypes_" {
         $itk_component(checktype) add \
            -label $longname \
            -value $shortname \
            -command [code $this set_values_ checktype $shortname]
      }
      set values_(checktype) $values_(checktype)

      #  Name of check image.
      itk_component add checkimage {
         util::LabelEntry $parent.checkimage \
            -labelwidth $lwidth \
            -text "Check image name:" \
            -value $values_(checkimage) \
            -textvariable [scope values_(checkimage)]
      }
      pack $itk_component(checkimage) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(checkimage) \
         {File name for checkimage}
   }

   #  Reset all check image parameters to their builtin defaults.
   protected method reset_checkimage_ {} {
      $itk_component(checktype) configure -value $defaults_(checktype)
      $itk_component(checkimage) configure -value $defaults_(checkimage)
   }

   #  Add controls for all the catalogue parameters (columns).
   protected method add_catalogue_selections_ {parent} {

      #  Use a scrolled frame to get all these in a small amount of
      #  real estate.
      itk_component add catframe {
         ::iwidgets::scrolledframe $parent.catframe -width 75 -height 400
      }
      pack $itk_component(catframe) -fill both -expand 1
      set colparent_ [$itk_component(catframe) childsite]

      #  Note use checkbutton for speed....
      foreach {param desc} $columnnames_ {
         set butt [checkbutton $colparent_.[string tolower $param] \
                      -text "$param" \
                      -onvalue 1 \
                      -offvalue 0 \
                      -variable [scope columns_($param)] \
                      -width  20 \
                      -anchor w \
                      -font $itk_option(-labelfont)]
         pack $butt -side top -fill x -ipadx 1m -ipady 1m
         add_short_help $butt "$desc"
         #  Need to make geometries up to date, otherwise a user define
         #  BorderWidth property seems to leave all widgets size 1.
         update idletasks
      }

      #  Toggle ellipse drawing parameters.
      toggle_draw_kron_ellipses_
      toggle_draw_iso_ellipses_

      #  And fix the safe columns.
      fix_safe_columns_
   }

   #  Reset catalogue columns to their builtin defaults.
   protected method reset_catalogue_ {} {
      set dofull [info exists columns_]
      catch {unset columns_}
      if { $itk_option(-autoastrom) } {
         foreach param "$safecols_ $autoastromcols_ $isocols_" {
            set columns_($param) 1
         }
      } else {
         foreach param "$safecols_ $defcolumns_" {
            set columns_($param) 1
         }
      }
      if { $dofull } {
         toggle_draw_kron_ellipses_
         toggle_draw_iso_ellipses_
      }
   }

   #  Set an element of the values_ array.
   protected method set_values_ {elem value} {
      set values_($elem) $value
   }

   #  Establish the mappings between local names and SExtractor
   #  parameter names (use arrays so we must use a procedure).
   #  Names which do not map directly are flagged as "special".
   protected method set_mapping_ {} {

      #  Catalogue name, special as need control of file extension.
      set from_(CATALOG_NAME) special
      set special_(CATALOG_NAME) {set_catname_ from}
      set to_(catname) special
      set special_(catname) {set_catname_ to}

      #  Catalogue type, special as need control of file extension.
      set from_(CATALOG_TYPE) special
      set special_(CATALOG_TYPE) {set_cattype_ from}
      set to_(cattype) special
      set special_(cattype) {set_cattype_ to}

      #  Name of file for storing catalogue parameters.
      set from_(PARAMETERS_NAME) special
      set special_(PARAMETERS_NAME) {set_parname_ from}
      set to_(catpar) special
      set special_(catpar) {set_parname_ to}

      #  Type of detector
      set from_(DETECT_TYPE) dettype
      set to_(dettype) DETECT_TYPE

      #  Flag image (not used -- variances???).
      set from_(FLAG_IMAGE) special
      set special_(FLAG_IMAGE) {do_nothing_}

      #  Size of images.
      set from_(DETECT_MINAREA) minsize
      set to_(minsize) DETECT_MINAREA

      #  Type of threshold value.
      set from_(THRESH_TYPE) threshtype
      set to_(threshtype) THRESH_TYPE

      #  Detection threshold.
      set from_(DETECT_THRESH) detthresh
      set to_(detthresh) DETECT_THRESH

      #  Analysis threshold.
      set from_(ANALYSIS_THRESH) analthresh
      set to_(analthresh) ANALYSIS_THRESH

      #  Smoothing filter. Special treatment as none filter usage is
      #  equivalent to a filename of NONE.
      set from_(FILTER) special
      set special_(FILTER) {set_filter_ flag}
      set from_(FILTER_NAME) special
      set special_(FILTER_NAME) {set_filter_ from}
      set to_(detfilter) special
      set special_(detfilter) {set_filter_ to}

      #  Number of deblending thresholds.
      set from_(DEBLEND_NTHRESH) debthresh
      set to_(debthresh) DEBLEND_NTHRESH

      #  Contrast parameter for deblending.
      set from_(DEBLEND_MINCONT) debcontrast
      set to_(debcontrast) DEBLEND_MINCONT

      #  Catalogue cleaning flag.
      set from_(CLEAN) detclean
      set to_(detclean) CLEAN

      #  Cleaning efficiency.
      set from_(CLEAN_PARAM) deteffic
      set to_(deteffic) CLEAN_PARAM

      #  Type of masking applied to near neighbours when doing photometry.
      set from_(MASK_TYPE) photmask
      set to_(photmask) MASK_TYPE

      #  Number and size of apertures used for photometry. Special as
      #  number of apertures is determined by number of values
      #  given/received.
      set from_(PHOT_APERTURES) special
      set special_(PHOT_APERTURES) {set_apertures_ from}
      set to_(photapps) special
      set special_(photapps) {set_apertures_ to}
      set to_(photnum) special
      set special_(photnum) {do_nothing_}

      #  Kron magnitudes parameters. Special as factor and minimum
      #  radius are split into different widgets.
      set from_(PHOT_AUTOPARAMS) special
      set special_(PHOT_AUTOPARAMS) {set_kronmags_ from}
      set to_(kronfact) special
      set special_(kronfact) {set_kronmags_ to}
      set to_(kronmin) special
      set special_(kronmin) {do_nothing_}

      #  Same for Petrosians
      set from_(PHOT_PETROPARAMS) special
      set special_(PHOT_PETROPARAMS) {set_petromags_ from}
      set to_(petrofact) special
      set special_(petrofact) {set_petromags_ to}
      set to_(petromin) special
      set special_(petromin) {do_nothing_}

      #  Flux fraction.
      set from_(PHOT_FLUXFRAC) fluxfrac
      set to_(fluxfrac) PHOT_FLUXFRAC

      #  CCD saturation level.
      set from_(SATUR_LEVEL) photsat
      set to_(photsat) SATUR_LEVEL

      #  Magnitude zero point.
      set from_(MAG_ZEROPOINT) photzero
      set to_(photzero) MAG_ZEROPOINT

      #  Photographic emulsion gamma.
      set from_(MAG_GAMMA) photgamma
      set to_(photgamma) MAG_GAMMA

      #  CCD gain factor.
      set from_(GAIN) photgain
      set to_(photgain) GAIN

      #  Imagescale, pixels/arcsec.
      set from_(PIXEL_SCALE) imagescale
      set to_(imagescale) PIXEL_SCALE

      #  Seeing.
      set from_(SEEING_FWHM) fwhm
      set to_(fwhm) SEEING_FWHM

      #  Neural network table. Need to make name full if necessary.
      set from_(STARNNW_NAME) special
      set special_(STARNNW_NAME) {set_nettable_ from}
      set to_(nettable) special
      set special_(nettable) {set_nettable_ to}

      #  Type of isophotoal radii parameters. Special treatment as
      #  NONE means do not use value.
      set from_(RAD_TYPE) special
      set special_(RAD_TYPE) {set_radtype_ from}
      set to_(radtype) special
      set special_(radtype) {set_radtype_ to}

      #  Isophotoal radii threshold parameters. Special as need to
      #  return blank when radtype is NONE.
      set from_(RAD_THRESH) special
      set special_(RAD_THRESH) {set_radthresh_ from}
      set to_(radthresh) special
      set special_(radthresh) {set_radthresh_ to}

      #  Background mesh size
      set from_(BACK_SIZE) backmesh
      set to_(backmesh) BACK_SIZE

      #  Background filter.
      set from_(BACK_FILTERSIZE) backfilter
      set to_(backfilter) BACK_FILTERSIZE

      #  Type of background in photometry.
      set from_(BACKPHOTO_TYPE) backphot
      set to_(backphot) BACKPHOTO_TYPE

      #  Type of background used during detection.
      set from_(BACK_TYPE) backtype
      set to_(backtype) BACK_TYPE

      #  Value of background used during detection (manual).
      set from_(BACK_VALUE) backvalue
      set to_(backvalue) BACK_VALUE

      #  Background annulus thickness.
      set from_(BACKPHOTO_THICK) backthick
      set to_(backthick) BACKPHOTO_THICK

      #  Type of checkimage.
      set from_(CHECKIMAGE_TYPE) checktype
      set to_(checktype) CHECKIMAGE_TYPE

      #  Name of checkimage.
      set from_(CHECKIMAGE_NAME) checkimage
      set to_(checkimage) CHECKIMAGE_NAME

      #  Memory usage parametes (not used at present).
      set from_(MEMORY_OBJSTACK) memory_pixstack
      set to_(memory_objstack) MEMORY_OBJSTACK
      set from_(MEMORY_PIXSTACK) memory_pixstack
      set to_(memory_pixstack) MEMORY_PIXSTACK
      set from_(MEMORY_BUFSIZE) memory_bufsize
      set to_(memory_bufsize) MEMORY_BUFSIZE

      #  Program verbosity (not used at present).
      set from_(VERBOSE_TYPE) verbose_type
      set to_(verbose_type) VERBOSE_TYPE

      #  XML meta-data file. Not used.
      set from_(WRITE_XML) write_xml
      set to_(write_xml) WRITE_XML
      set from_(XML_NAME) xml_name
      set to_(xml_name) XML_NAME

      #  Weighting. Not used.
      set from_(WEIGHT_IMAGE) weight_image
      set to_(weight_image) WEIGHT_IMAGE
      set from_(WEIGHT_TYPE) weight_type
      set to_(weight_type) WEIGHT_TYPE

      #  Flagging. Not used.
      set from_(FLAG_TYPE) flag_type
      set to_(flag_type) FLAG_TYPE
      
      #  Automatic apertures? Not used.
      set from_(PHOT_AUTOAPERS) phot_autoapers
      set to_(phot_autoapers) PHOT_AUTOAPERS

      #  Saturation value keyword. Not used.
      set from_(SATUR_KEY) satur_key
      set to_(satur_key) SATUR_KEY

      #  Gain keyword. Not used.
      set from_(GAIN_KEY) gain_key
      set to_(gain_key) GAIN_KEY

      #  ASCII file to associate. Not used.
      set from_(ASSOC_NAME) assoc_name
      set to_(assoc_name) ASSOC_NAME
      set from_(ASSOC_DATA) assoc_data
      set to_(assoc_data) ASSOC_DATA
      set from_(ASSOC_PARAMS) assoc_params
      set to_(assoc_params) ASSOC_PARAMS
      set from_(ASSOC_RADIUS) assoc_radius
      set to_(assoc_radius) ASSOC_RADIUS
      set from_(ASSOC_TYPE) assoc_type
      set to_(assoc_type) ASSOC_TYPE
      set from_(ASSOCSELEC_TYPE) assocselec_type
      set to_(assocselec_type) ASSOCSELEC_TYPE

      #  Extension for headers. Not used.
      set from_(HEADER_SUFFIX) header_suffix
      set to_(header_suffix) HEADER_SUFFIX

      #  Style sheet for VOTables. Not used.
      set from_(XSL_URL) xsl_url
      set to_(xsl_url) XSL_URL
      
   }

   #  Set the initial defaults for the values_ array. These are
   #  established locally and then overridden by values in default.sex
   #  and default.param in either the local directory, or
   #  -itk_option(-sex_dir)/config or -itk_option(-sex_dir).
   protected method set_defaults_ {} {

      # Set the local defaults.
      set values_(conpar) "default.sex"
      set values_(catname) "GaiaCatalog[incr catnames_]"
      set values_(catpar) "default.param"
      set values_(cattype) "ASCII_SKYCAT"
      set values_(minsize) 5
      set values_(threshtype) "RELATIVE"
      set values_(detthresh) {{1.5} {}}
      set values_(analthresh) {{1.5} {}}
      set values_(detfilter) "${config_dir_}/default.conv"
      set values_(debthresh) 32
      set values_(debcontrast) 0.005
      set values_(detclean) "Y"
      set values_(deteffic) 1.0
      set values_(photzero) 50.0
      set values_(photnum) 1
      set values_(photapps) 5.0
      set values_(kronfact) 2.5
      set values_(kronmin) 3.5
      set values_(petrofact) 2.0
      set values_(petromin) 3.5
      set values_(fluxfrac) 0.5
      set values_(photmask) "NONE"
      set values_(dettype) "CCD"
      set values_(imagescale) 1.0
      set values_(photsat) 50000
      set values_(photgain) 1.0
      set values_(photgamma) 4.0
      set values_(fwhm) 1.2
      set values_(nettable) "${config_dir_}/default.nnw"
      set values_(backmesh) {{64} {}}
      set values_(backfilter) {{3} {}}
      set values_(backtype) "AUTO"
      set values_(backvalue) 0.0
      set values_(backphot) "GLOBAL"
      set values_(backthick) 24
      set values_(checktype) "NONE"
      set values_(checkimage) "check.fits"

      set values_(radtype)  "NONE"
      set values_(radthresh) ""

      #  Memory parameters. These are not optional and may be changed
      #  using the local default.sex file.
      set values_(memory_objstack) 3000
      set values_(memory_pixstack) 300000
      set values_(memory_bufsize) 1024

      #  Verbosity. Not used, but may be modified using local
      #  default.sex file.
      set values_(verbose_type) "NORMAL"

      #  Set list of parameter names (this is so we can order them in
      #  a specific format -- useful for human readable output).
      set valuenames_ {
         catname catpar cattype minsize threshtype detthresh
         analthresh detfilter debthresh debcontrast detclean deteffic
         photzero photnum photapps kronfact kronmin petrofact petromin
         fluxfrac photmask dettype imagescale photsat photgain photgamma fwhm
         nettable backmesh backfilter backtype backvalue backphot backthick
         checktype checkimage memory_objstack memory_pixstack memory_bufsize
         verbose_type radtype radthresh
      }

      #  Record defaults so they can be restored.
      foreach name $valuenames_ {
         set defaults_($name) $values_($name)
      }

      #  Open SExtractor config parameter file and read in
      #  defaults. This can be local, or from the main installation.
      set localfile 0
      if { [file isfile "default.sex"] } {
         set deffile "default.sex"
         set localfile 1
      } elseif { [file isfile "${config_dir_}/default.sex"] } {
         set deffile "${config_dir_}/default.sex"
      } elseif { [file isfile "$itk_option(-sex_dir)/default.sex"] } {
         set deffile "$itk_option(-sex_dir)/default.sex"
      } else {
         set deffile ""
      }
      if { $deffile != "" } {
         #  Read file, but if not a local configuration file use our
         #  unique name.
         set localname $values_(catname)
         read_conpars $deffile
         if { ! $localfile } {
            set values_(catname) $localname
         }
      }

      #  Now look for catalogue parameters file and read it in.
      if { [file isfile "default.param"] } {
         set deffile "default.param"
      } elseif { [file isfile "${config_dir_}/default.param"] } {
         set deffile "${config_dir_}/default.param"
      } elseif { [file isfile "$itk_option(-sex_dir)/default.param"] } {
         set deffile "$itk_option(-sex_dir)/default.param"
      } else {
         set deffile ""
      }
      if { $deffile != "" } {
         read_catpars $deffile
      }
   }

   #  Run the SExtractor program. This writes the current configs out,
   #  runs the program and then displays the resultant
   #  catalogue. "args" are a command to run when the measurements are
   #  available.
   public method run {args} {
      if { $itk_option(-sex_dir) != "" } {

         #  Which version of SExtrator do we have available. Look for
         #  both. Only use native version if allowed.
         set starlink 0
         set native 0
         if { [file isfile $itk_option(-sex_dir)/extractor] } {
            set starlink 1
         }
         if { [file isfile $itk_option(-sex_dir)/sex] } {
            if { $values_(use_native) } {
               set native 1
            }
         }
         if { ! $native && ! $starlink } {
            error_dialog "Failed to locate any SExtractor executables"
            return
         }

         #  OK now save all config parameters and catalogue params.
         save_conpars $values_(conpar)
         save_catpars $values_(catpar)

         #  Get name of the image we need to use for measuring. We
         #  also need the detection image, which is the displayed
         #  image by default.
         set image [$itk_option(-rtdimage) fullname]
         if { $image != "" } {
            $namer_ configure -imagename $image
            set image [$namer_ ndfname]
            set diskimage [$namer_ diskfile]

            set detect $values_(detname)
            if { $detect != "" && $detect != "NONE" } {
               $namer_ configure -imagename $detect
               set detect [$namer_ ndfname]
               set diskdetect [$namer_ diskfile]
            } else {
               set detect ""
               set diskdetect ""
            }

            #  The image may be volatile (a slice from a cube) so make sure
            #  the disk file is upto date.
            $itk_option(-image) save_if_volatile

            #  Set the command to run when measurements are available.
            if { $args != "" } {
               set complete_cmd_ $args
            }

            #  And run the application as required. Note NDFs cannot
            #  be processed by native version, nor can FITS not stored
            #  in the primary extension.
            if { $native && [native_ok_] } {
               busy {
                  #  Establish a control object for this foreign task,
                  #  if not already done.
                  if { $foreign_sex_ == {} } {
                     set foreign_sex_ [GaiaForeignExec \#auto \
                                          -show_output $itk_component(status) \
                                          -use_error 1 \
                                          -preprocess [code $this clean_] \
                                          -application $itk_option(-sex_dir)/sex]
                  }

                  #  Clear the log window.
                  $itk_component(status) clear 0 end

                  #  Remove the existing catalogue (so we know when
                  #  command fails).
                  file delete [get_catname_]

                  #  Generate file names, include MEF extension so that only
                  #  this extension is processed. Otherwise all image
                  #  extensions would be done. -1 as SExtractor changed
                  #  the indexing from the initial Starlink version.
                  set hdunum [$itk_option(-rtdimage) hdu]
                  incr hdunum -1
                  set fitsname "$diskimage\[$hdunum\]"

                  #  Run program, monitoring output...
                  if { $detect == "" } {
                     catch {$foreign_sex_ runwith \
                               -c $values_(conpar) $fitsname } msg
                  } else {
                     catch {$foreign_sex_ runwith \
                               -c $values_(conpar) \
                               $diskdetect,$fitsname } msg
                  }

                  #  Now display the catalogue overlaid on the image.
                  display_cat [get_catname_]
               }
            } elseif { $starlink } {

               #  Establish a control object for this task,if not already done.
               blt::busy hold $w_
               if { $star_sex_ == {} } {
                  set star_sex_ [gaia::GaiaApp \#auto \
                                    -show_output $itk_component(status) \
                                    -notify [code $this completed_] \
                                    -see_end 1 \
                                    -application $itk_option(-sex_dir)/extractor]
               }

               #  Clear the log window.
               $itk_component(status) clear 0 end

               #  Remove the existing catalogue (so we know when
               #  command fails).
               file delete [get_catname_]

               #  Run program, monitoring output...
               if { $detect == "" } {
                  $star_sex_ runwiths \
                     "config=$values_(conpar) image=$image \
                      reset accept"
               } else {
                  $star_sex_ runwiths \
                     "config=$values_(conpar) \
                     image=\"$detect,$image\" reset accept"
               }

            } else {
               error_dialog "Cannot process non-FITS images with native SExtractor"
            }
         } else {
            error_dialog "No image displayed"
         }
      }
   }

   #  Message when task is completed (GaiaApp only).
   protected method completed_ {} {

      #  Now display the catalogue overlaid on the image.
      catch { display_cat [get_catname_] } msg
      blt::busy release $w_
      if { "$msg" != "" } {
         error "$msg"
      }
   }

   #  Clean any SExtractor output of known escape sequences etc.
   protected method clean_ {output} {
      regsub -all "\[\033\]" $output {} output
      regsub -all {\[1A|\[1M>} $output {} output
      return $output
   }

   #  Determine if the native version SExtractor could process the
   #  current image. This means that the file must be fits (not a
   #  compressed form) and must be a numbered HDU.
   protected method native_ok_ {} {
      set ext [string tolower [$namer_ type]]
      set hdunum [$itk_option(-rtdimage) hdu]
      if { ( $ext == ".fit" || $ext == ".fits" || $ext == ".fts" ) && $hdunum > 0 } {
         return 1
      } else {
         return 0
      }
   }

   #  Restore an old catalogue, displaying the detection ellipses.
   public method read_old_catalogue {} {
      set w [util::FileSelect .\#auto -title "Read existing compatible catalogue"]
      if {[$w activate]} {
         set catalogue [$w get]
         if { ! [file readable $catalogue] } {
            return
         }

         #  Before we can display this catalogue we need to match the
         #  columns that it has.
         set astrocat [astrocat $w_.cat]
         if { ! [catch "$astrocat entry get $catalogue"] } {
            #  Catalogue already known, remove as may not match setup.
            $astrocat entry remove $catalogue
         }
         create_entry_ $astrocat $catalogue
         $astrocat open $catalogue

         #  Reset all known columns
         foreach {param desc} $columnnames_ {
            set columns_($param) 0
         }

         #  Now get the actual columns in this catalogue and switch them on.
         foreach param [$astrocat headings] {
            set columns_($param) 1
         }
         $astrocat delete

         #  Check that we can display the required symbols.
         if { $values_(draw_kron_ellipses) } {
            set n 0
            set i 0
            foreach param "$safecols_ $commoncols_ $kroncols_" {
               if { [info exists columns_($param)] &&
                    $columns_($param) } {
                  incr i
               }
               incr n
            }
            if { $i != $n } {
               set values_(draw_kron_ellipses) 0
               toggle_draw_kron_ellipses_
               warning_dialog "Cannot draw kron ellipses (wrong columns available)"
            }
         }
         if { $values_(draw_iso_ellipses) } {
            set n 0
            set i 0
            foreach param "$safecols_ $commoncols_ $isocols_" {
               if { [info exists columns_($param)] &&
                    $columns_($param) } {
                  incr i
               }
               incr n
            }
            if { $i != $n } {
               set values_(draw_iso_ellipses) 0
               toggle_draw_iso_ellipses_
               warning_dialog "Cannot draw isophotal ellipses (wrong columns available)"
            }
         }
         if { $values_(draw_circles) } {
            set n 0
            set i 0
            foreach param "$safecols_" {
               if { [info exists columns_($param)] &&
                    $columns_($param) } {
                  incr i
               }
               incr n
            }
            if { $i != $n } {
               set values_(draw_iso_ellipses) 0
               toggle_draw_circles_
               warning_dialog "Cannot draw detection circles (wrong columns available)"
            }
         }

         #  And display the catalogue as per normal.
         display_cat $catalogue
      }
      destroy $w
   }

   #  Display a named catalogue in the local catalogue window that is
   #  controlled by this instance.
   public method display_cat {catalogue} {
      if { ! [file readable $catalogue] } {
         return
      }
      $itk_option(-astrocat)::allow_searches 0

      #  Create an info entry for this catalogue, if not already done
      #  so. If already done then just update the coordinate columns
      #  to reflect the current usage.
      set astrocat [astrocat $w_.cat]
      if { [catch "$astrocat entry get $catalogue" msg] } {
         create_entry_ $astrocat $catalogue
      } else {
         #  Catalogue already known. If also already created by this
         #  interface then check the state of the coordinates. If
         #  these have changed then we need to remove the window and
         #  recreate it.
         if { [info exists astrocatnames_($catalogue)] &&
              [winfo exists $astrocatnames_($catalogue)] } {
            set catwin $astrocatnames_($catalogue)
            if { [info exists columns_(X_IMAGE)] &&
                 $columns_(X_IMAGE) &&
                 [info exists columns_(Y_IMAGE)] &&
                 $columns_(Y_IMAGE) } {
               set havpix 1
            } else {
               set havpix 0
            }
            if { [info exists columns_(X_WORLD)] &&
                 $columns_(X_WORLD) &&
                 [info exists columns_(Y_WORLD)] &&
                 $columns_(Y_WORLD) } {
               set havwcs 1
            } else {
               set havwcs 0
            }
            set hadpix [$catwin ispix]
            set hadwcs [$catwin iswcs]

            if { $hadwcs && $havwcs } {
               set die 0
            } else {
               if { $hadpix && $havwcs } {
                  set die 1
               } else {
                  if { $hadwcs && $havpix } {
                     set die 1
                  } else {
                     if { $hadpix && $havpix } {
                        set die 0
                     } else {
                        set die 1
                     }
                  }
               }
            }
            if { $die } {

               #  Destroy the window, clear the entry and then add an
               #  initial entry.
               $catwin clear
               $catwin remove_catalog
               destroy $catwin
               create_entry_ $astrocat $catalogue
            }
         } else {
            #  Catalogue known (from a previous time), but doesn't
            #  exist yet. The problem here is that when it is created
            #  it might have RA/Dec ranging (which will mess up
            #  plotting using X and Y coordinates), so we need to
            #  remove this reference and recreate it from scratch.
            $astrocat entry remove $catalogue
            create_entry_ $astrocat $catalogue
         }
      }

      #  Set the columns in which the various coordinates are found.
      if { [info exists columns_(NUMBER)] &&
           $columns_(NUMBER) } {
         set ncol 0
      } else {
         set ncol -1
      }
      foreach "id name" "X_WORLD ra Y_WORLD dec X_IMAGE x Y_IMAGE y" {
         catch {
            if { [info exists columns_($id)] && $columns_($id) } {
               incr ncol
               $astrocat entry update [list "${name}_col $ncol"] $catalogue
            } else {
               $astrocat entry update [list "${name}_col -1"] $catalogue
            }
         } msg
      }
      catch {
         set equinox [$itk_option(-rtdimage) astget equinox]
         $astrocat entry update [list "equinox $equinox"] $catalogue
      } msg
      $astrocat delete

      #  Now open the catalogue window.
      set astrocat [gaia::GaiaSearch::new_local_catalog $catalogue \
                       [code $itk_option(-image)] $itk_option(-astrocat) \
                       0 catalog $w_]

      #  Now display the catalogue.
      if { $astrocat == {} } {

         #  Catalogue window exists, switch searching back on and kick
         #  it to force a reload.
         $itk_option(-astrocat)::allow_searches 1
         set_maxobjs_ $catalogue
         set_plot_symbols_ $astrocatnames_($catalogue)
         $astrocatnames_($catalogue) search
      } else {

         #  Created a new window so display the catalogue.
         set astrocatnames_($catalogue) $astrocat
         set_maxobjs_ $catalogue

         #  Define ellipse plotting symbols, if needed.
         set_plot_symbols_ $astrocat

         #  Switch searching back on.
         $itk_option(-astrocat)::allow_searches 1

         #  Stop use of image wcs for scaling/orienting ellipses.
         $astrocat configure -plot_wcs 0

         #  Do search to display symbols.
         $astrocat search
      }

      #  Issue the measurements available command if needed.
      if { $complete_cmd_ != {} } {
         eval $complete_cmd_
         set complete_cmd_ {}
      }
   }

   #  Get the name of the catalogue window (used for demo control).
   public method get_catname {} {
       set name [get_catname_]
       if { [info exists astrocatnames_($name)] } {
           return $astrocatnames_($name)
       }
       return ""
   }

   #  Wait for a command to return 1 (non-blocking?).
   protected method wait_ {cmd} {
      set ok [eval $cmd]
      if { ! $ok } {
         after 500
         update idletasks
         wait_ $cmd
      }
   }

   #  Find out how many objects have been detected. Look in the output
   #  window for this. It should be the last line with "sextracted"
   #  mentioned.
   protected method find_maxobjs_ {} {
      set w $itk_component(status)
      set n [$w size]
      set line {}
      for {set i $n} {$i > 0} {incr i -1} {
         set line [$w get $i]
         if { [string match {*sextracted*} $line] } {
            break
         }
      }
      set v2 {}
      if { $line != {} } {
         scan $line " Objects: detected %d / sextracted %d" v1 v2
         if { $v2 != {} } {
            set maxobjs_ $v2
         }
      }
      return $maxobjs_
   }

   #  Set the number of objects to display. Wait for command to
   #  complete, so we can proceed. The number we should use here
   #  is 1000 or the size of the catalogue.
   protected method set_maxobjs_ {catalogue} {
      set maxobjs [find_maxobjs_]
      set ok [$astrocatnames_($catalogue) set_maxobjs $maxobjs]
      if { ! $ok } {
         wait_ "$astrocatnames_($catalogue) set_maxobjs $maxobjs"
      }
   }

   #  Set the plotting symbol. This is for the appropriate ellipses,
   #  or, if the first time a simple circle.
   protected method set_plot_symbols_ {astrocat} {
      set symbol {}
      if { $values_(draw_kron_ellipses) } {
         lappend symbol \
            [list KRON_RADIUS B_IMAGE THETA_IMAGE ELONGATION] \
            [list ellipse blue \$ELONGATION \$THETA_IMAGE {} {}] \
            [list \$KRON_RADIUS*\$B_IMAGE {}]
      }
      if { $values_(draw_iso_ellipses) } {
         if {$values_(draw_kron_ellipses) } {
            lappend symbol {:}
         }
         lappend symbol \
            [list ISOAREA_IMAGE THETA_IMAGE ELONGATION] \
            [list ellipse green \$ELONGATION \$THETA_IMAGE {} {}] \
            [list sqrt(\$ISOAREA_IMAGE/(3.142*\$ELONGATION)) {}]
      }

      # Use a simple plot symbol.
      if { $values_(draw_circles) } {
         if {$values_(draw_kron_ellipses) ||
             $values_(draw_iso_ellipses) } {
            lappend symbol {:}
         }
         lappend symbol \
            {} \
            [list circle red {} {} {} {}] \
            [list {4.0} {}]
      }

      #  Isophotal radii, if available.
      if { $values_(draw_radii) } {
         if { "[array get columns_ RAD*]" != "" } {
            if {$values_(draw_kron_ellipses) ||
                $values_(draw_iso_ellipses) ||
                $values_(draw_circles) } {
               lappend symbol {:}
            }
            for {set i 0} {$i < 16} {incr i} {
               if { [info exists columns_(RAD$i)] &&
                    $columns_(RAD$i) } {
                  lappend symbol \
                     RAD$i \
                     [list circle yellow {} {} {} \$RAD$i>0] \
                     [list \$RAD$i {}] {:}
               }
            }
         }
      }

      if { $symbol != {} } {
         eval $astrocat set_symbol $symbol
      }
   }

   #  Do nothing method.
   private method do_nothing_ {args} {
   }

   #  Expand name to full path relative to current directory.
   protected method full_name_ {name} {
      if { "[string index $name 0]" != "/"} {
         set fname [pwd]/$name
      } else {
         set fname $name
      }
      return $fname
   }

   #  Create a initial entry describing the given local catalogue
   #  (used so that coordinate columns may be set before opening the real
   #  catalogue).
   protected method create_entry_ {astrocat catalogue} {
      set fname [full_name_ $catalogue]
      $astrocat entry add \
         [list "serv_type local" "long_name $fname" "short_name $catalogue" "url $fname"]
   }

   #  Set/reset the toolbox to work in AUTOASTROM mode.
   protected method configure_autoastrom_mode_ {} {
      set_cattypes_
      reset_catalogue_
   }

   #  Set up the menu with the types of catalogue available. Only
   #  get ASC for autoastrom mode.
   protected method set_cattypes_ {} {
      if { [info exists itk_component(cattype)] } {
         if { $itk_option(-autoastrom) } {
            set types "ASCII ASCII_HEAD"
            set_cattype_ from "ASCII_HEAD"
         } else {
            set types "SKYCAT ASCII_SKYCAT FITS FITS_1.0 ASCII ASCII_HEAD"
         }
         $itk_component(cattype) clear
         foreach {sname lname} $types {
            $itk_component(cattype) add \
               -label $sname \
               -value $lname \
               -command [code $this set_cattype_ from $lname]
         }
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------
   #  Name of canvas.
   itk_option define -canvas canvas Canvas {} {}

   #  Name of rtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {} {}

   #  Name of the GaiaImageCtrl widget.
   itk_option define -image image Image {} {}

   #  Name of Astrocat subclass to use to create new catalog widgets.
   itk_option define -astrocat astrocat AstroCat gaia::GaiaSearch {}

   #  Name of CanvasDraw widget.
   itk_option define -canvasdraw canvasdraw CanvasDraw {} {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  If this is a clone, then it should die rather than be withdrawn.
   itk_option define -really_die really_die Really_Die 0

   #  Name of directory that contains extractor/sextractor. Look for
   #  Starlink version first.
   itk_option define -sex_dir sex_dir Sex_Dir {} {
      if { $itk_option(-sex_dir) == {} } {

         #  Make it up. Check EXTRACTOR_DIR and then SEX_DIR.
         global env
         if { [info exists env(EXTRACTOR_DIR)] } {
            set itk_option(-sex_dir) $env(EXTRACTOR_DIR)
         } elseif { [info exists env(SEX_DIR)] } {
            set itk_option(-sex_dir) $env(SEX_DIR)
         } else {
            if { [file isdirectory "/star/bin/extractor"] }  {
               set itk_option(-sex_dir) "/star/bin/extractor"
            } else {
               info_dialog "Cannot locate EXTRACTOR directory. \
               Define EXTRACTOR_DIR or SEX_DIR and restart."
            }
         }
         if { $itk_option(-sex_dir) != {} } {
            #  Locate the configuration directory.
            if { [file isdirectory "$itk_option(-sex_dir)/config"] } {
               set config_dir_ $itk_option(-sex_dir)/config
            } else {
               set config_dir_ $itk_option(-sex_dir)
            }
         }
      }
   }

   #  The filter types of images.
   itk_option define -filter_types filter_types Filter_Types {} {}

   #  Whether to configure for AUTOASTROM.
   itk_option define -autoastrom autoastrom Autoastrom 0 {
      configure_autoastrom_mode_
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Catalogue parameters (columns) and their descriptions. Commented
   #  out parts are documented in the SExtractor source code, but not
   #  in the default.param file.
   protected variable columnnames_ {
      {NUMBER} {Running object number}
      {X_WORLD} {Barycenter position along world x axis}
      {Y_WORLD} {Barycenter position along world y axis}
      {XWIN_WORLD} {Windowed position along world x axis}
      {YWIN_WORLD} {Windowed position along world y axis}
      {X_IMAGE} {Object position along x (FITS coords)}
      {Y_IMAGE} {Object position along y (FITS coords)}
      {XWIN_IMAGE} {Windowed position estimate along x}
      {YWIN_IMAGE} {Windowed position estimate along y}
      {X_PIXEL} {Object position along x (NDF pixel coordinates)}
      {Y_PIXEL} {Object position along y (NDF pixel coordinates)}
      {FLUX_ISO} {Isophotal flux}
      {FLUXERR_ISO} {RMS error for isophotal flux}
      {MAG_ISO} {Isophotal magnitude}
      {MAGERR_ISO} {RMS error for isophotal magnitude}
      {FLUX_ISOCOR} {Corrected isophotal flux}
      {FLUXERR_ISOCOR} {RMS error for corrected isophotal flux}
      {MAG_ISOCOR} {Corrected isophotal magnitude}
      {MAGERR_ISOCOR} {RMS error for corrected isophotal magnitude}
      {FLUX_APER} {Flux vector within fixed circular aperture(s)}
      {FLUXERR_APER} {RMS error vector for aperture flux(es)}
      {MAG_APER} {Fixed aperture magnitude vector}
      {MAGERR_APER} {RMS error vector for fixed aperture mag.}
      {FLUX_AUTO} {Flux within a Kron-like elliptical aperture}
      {FLUXERR_AUTO} {RMS error for AUTO flux}
      {MAG_AUTO} {Kron-like elliptical aperture magnitude}
      {MAGERR_AUTO} {RMS error for AUTO magnitude}
      {FLUX_PETRO} {Flux within a Petrosian-like elliptical aperture}
      {FLUXERR_PETRO} {RMS error for PETRO flux}
      {MAG_PETRO} {Petrosian-like elliptical aperture magnitude}
      {MAGERR_PETRO} {RMS error for PETRO magnitude}
      {FLUX_BEST} {Best of FLUX_AUTO and FLUX_ISOCOR}
      {FLUXERR_BEST} {RMS error for BEST flux}
      {MAG_BEST} {Best of MAG_AUTO and MAG_ISOCOR}
      {MAGERR_BEST} {RMS error for MAG_BEST}
      {FLUX_WIN} {Gaussian-weighted flux}
      {FLUXERR_WIN} {RMS error for WIN flux}
      {MAG_WIN}  {Gaussian-weighted magnitude}
      {MAGERR_WIN} {RMS error for MAG_WIN}
      {KRON_RADIUS} {Kron apertures in units of A or B}
      {PETRO_RADIUS} {Petrosian apertures in units of A or B}
      {FLUX_RADIUS} {Fraction-of-light radius}
      {BACKGROUND} {Background at centroid position}
      {THRESHOLD} {Detection threshold above background}
      {FLUX_MAX} {Peak flux above background}
      {ISOAREA_IMAGE} {Isophotal area above Analysis threshold}
      {XMIN_IMAGE} {Minimum x-coordinate among detected pixels}
      {YMIN_IMAGE} {Minimum y-coordinate among detected pixels}
      {XMAX_IMAGE} {Maximum x-coordinate + 1 among detected pixels}
      {YMAX_IMAGE} {Maximum y-coordinate + 1 among detected pixels}
      {ALPHA_SKY} {Right ascension of barycenter (native)}
      {DELTA_SKY} {Declination of barycenter (native)}
      {ALPHA_J2000} {Right ascension of barycenter (J2000)}
      {DELTA_J2000} {Declination of barycenter (J2000)}
      {ALPHA_B1950} {Right ascension of barycenter (B1950)}
      {DELTA_B1950} {Declination of barycenter (B1950)}
      {X2_IMAGE} {Variance along x}
      {Y2_IMAGE} {Variance along y}
      {XY_IMAGE} {Covariance between x and y}
      {X2_WORLD} {Variance along X-WORLD (alpha)}
      {Y2_WORLD} {Variance along Y-WORLD (delta)}
      {XY_WORLD} {Covariance between X-WORLD and Y-WORLD}
      {CXX_IMAGE} {Cxx object ellipse parameter}
      {CYY_IMAGE} {Cyy object ellipse parameter}
      {CXY_IMAGE} {Cxy object ellipse parameter}
      {CXX_WORLD} {Cxx object ellipse parameter (WORLD units)}
      {CYY_WORLD} {Cyy object ellipse parameter (WORLD units)}
      {CXY_WORLD} {Cxy object ellipse parameter (WORLD units)}
      {A_IMAGE} {Profile RMS along major axis}
      {B_IMAGE} {Profile RMS along minor axis}
      {THETA_IMAGE} {Position angle (CCW/x)}
      {A_WORLD} {Profile RMS along major axis (world units)}
      {B_WORLD} {Profile RMS along minor axis (world units)}
      {THETA_WORLD} {Position angle (CCW/world-x)}
      {THETA_SKY} {Position angle (east of north) (native)}
      {THETA_J2000} {Position angle (east of north) (J2000)}
      {THETA_B1950} {Position angle (east of north) (B1950)}
      {ERRX2_IMAGE} {Variance of position along x}
      {ERRY2_IMAGE} {Variance of position along y}
      {ERRXY_IMAGE} {Covariance of position between x and y}
      {ERRX2_WORLD} {Variance of position along X-WORLD (alpha)}
      {ERRY2_WORLD} {Variance of position along Y-WORLD (delta)}
      {ERRXY_WORLD} {Covariance of position X-WORLD/Y-WORLD}
      {ERRCXX_IMAGE} {Cxx error ellipse parameter}
      {ERRCYY_IMAGE} {Cyy error ellipse parameter}
      {ERRCXY_IMAGE} {Cxy error ellipse parameter}
      {ERRCXX_WORLD} {Cxx error ellipse parameter (WORLD units)}
      {ERRCYY_WORLD} {Cyy error ellipse parameter (WORLD units)}
      {ERRCXY_WORLD} {Cxy error ellipse parameter (WORLD units)}
      {ERRA_IMAGE} {RMS position error along major axis}
      {ERRB_IMAGE} {RMS position error along minor axis}
      {ERRTHETA_IMAGE} {Error ellipse position angle (CCW/x)}
      {ERRA_WORLD} {World RMS position error along major axis}
      {ERRB_WORLD} {World RMS position error along minor axis}
      {ERRTHETA_WORLD} {Error ellipse pos. angle (CCW/world-x)}
      {ERRTHETA_SKY} {Native error ellipse pos. angle (east of north)}
      {ERRTHETA_J2000} {J2000 error ellipse pos. angle (east of north)}
      {ERRTHETA_B1950} {B1950 error ellipse pos. angle (east of north)}
      {ERRAWIN_IMAGE} {RMS windowed position error along major axis}
      {ERRBWIN_IMAGE} {RMS windowed position error along minor axis}
      {ERRTHETAWIN_IMAGE} {Windowed error ellipse position angle (CCW/x)}
      {AWIN_IMAGE} {Windowed profile RMS along major axis}
      {BWIN_IMAGE} {Windowed profile RMS along minor axis}
      {THETA_WINIMAGE} {Windowed position angle (CCW/x)}
      {X2WIN_IMAGE} {Windowed variance along x}
      {Y2WIN_IMAGE} {Windowed variance along y}
      {XYWIN_IMAGE} {Windowed covariance between x and y}
      {CXXWIN_IMAGE} {Windowed Cxx object ellipse parameter}
      {CYYWIN_IMAGE} {Windowed Cyy object ellipse parameter}
      {CXYWIN_IMAGE} {Windowed Cxy object ellipse parameter}
      {MU_THRESHOLD} {Detection threshold above background}
      {MU_MAX} {Peak surface brightness above background}
      {ISOAREA_WORLD} {Isophotal area above Analysis threshold}
      {ISO0} {Isophotal area at level 0}
      {ISO1} {Isophotal area at level 1}
      {ISO2} {Isophotal area at level 2}
      {ISO3} {Isophotal area at level 3}
      {ISO4} {Isophotal area at level 4}
      {ISO5} {Isophotal area at level 5}
      {ISO6} {Isophotal area at level 6}
      {ISO7} {Isophotal area at level 7}
      {RAD0} {Isophotal radius at level 0}
      {RAD1} {Isophotal radius at level 1}
      {RAD2} {Isophotal radius at level 2}
      {RAD3} {Isophotal radius at level 3}
      {RAD4} {Isophotal radius at level 4}
      {RAD5} {Isophotal radius at level 5}
      {RAD6} {Isophotal radius at level 6}
      {RAD7} {Isophotal radius at level 7}
      {RAD8} {Isophotal radius at level 8}
      {RAD9} {Isophotal radius at level 9}
      {RAD10} {Isophotal radius at level 10}
      {RAD11} {Isophotal radius at level 11}
      {RAD12} {Isophotal radius at level 12}
      {RAD13} {Isophotal radius at level 13}
      {RAD14} {Isophotal radius at level 14}
      {RAD15} {Isophotal radius at level 15}
      {FLAGS} {Extraction flags}
      {FWHM_IMAGE} {FWHM assuming a gaussian core}
      {FWHM_WORLD} {FWHM assuming a gaussian core}
      {ELONGATION} {A_IMAGE/B_IMAGE}
      {ELLIPTICITY} {1 - B_IMAGE/A_IMAGE}
      {CLASS_STAR} {S/G classifier output}
      {VIGNET} {Pixel data around detection}
   }
#      {IMAFLAGS_ISO} {FLAG-image flags OR'ed over the iso. profile}
#      {NIMAFLAGS_ISO} {Number of flagged pixels entering IMAFLAGS_ISO}
#      {X_IMAGE_DBL} {Object position along x (double precision)}
#      {Y_IMAGE_DBL} {Object position along y (double precision)}
#      {X_MAMA} {Barycenter position along MAMA x axis}
#      {Y_MAMA} {Barycenter position along MAMA y axis}
#      {FLUX_SOMFIT} {Flux derived from SOM fit}
#      {FLUXERR_SOMFIT} {RMS error for SOMFIT flux}
#      {MAG_SOMFIT} {Magnitude derived from SOM fit}
#      {MAGERR_SOMFIT} {Magnitude error derived from SOM fit}
#      {ERROR_SOMFIT} {Reduced Chi-square error of the SOM fit}
#      {VECTOR_SOMFIT} {Position vector of the winning SOM node}
#      {ISOAREAF_IMAGE} {Isophotal area (filtered) above Detection threshold}
#      {XPEAK_IMAGE} {x-coordinate of the brightest pixel}
#      {YPEAK_IMAGE} {y-coordinate of the brightest pixel}
#      {XPEAK_WORLD} {World-x coordinate of the brightest pixel}
#      {YPEAK_WORLD} {World-y coordinate of the brightest pixel}
#      {ALPHAPEAK_SKY} {Right ascension of brightest pix (native)}
#      {DELTAPEAK_SKY} {Declination of brightest pix (native)}
#      {ALPHAPEAK_J2000} {Right ascension of brightest pix (J2000)}
#      {DELTAPEAK_J2000} {Declination of brightest pix (J2000)}
#      {ALPHAPEAK_B1950} {Right ascension of brightest pix (B1950)}
#      {DELTAPEAK_B1950} {Declination of brightest pix (B1950)}
#      {ISOAREAF_WORLD} {Isophotal area (filtered) above Detection threshold}
#      {VIGNET_SHIFT} {Pixel data around detection, corrected for shift}
#      {VECTOR_ASSOC} {ASSOCiated parameter vector}
#      {NUMBER_ASSOC} {Number of ASSOCiated IDs}
#      {THRESHOLDMAX} {Maximum threshold possible for detection}
#      {FLUX_GROWTH} {Cumulated growth-curve}
#      {FLUX_GROWTHSTEP} {Step for growth-curves}
#      {MAG_GROWTH} {Cumulated magnitude growth-curve}
#      {MAG_GROWTHSTEP} {Step for growth-curves}
#      {FLUX_RADIUS} {Fraction-of-light radii}
#      {XPSF_IMAGE} {X coordinate from PSF-fitting}
#      {YPSF_IMAGE} {Y coordinate from PSF-fitting}
#      {XPSF_WORLD} {PSF position along world x axis}
#      {YPSF_WORLD} {PSF position along world y axis}
#      {ALPHAPSF_SKY} {Right ascension of the fitted PSF (native)}
#      {DELTAPSF_SKY} {Declination of the fitted PSF (native)}
#      {ALPHAPSF_J2000} {Right ascension of the fitted PSF (J2000)}
#      {DELTAPSF_J2000} {Declination of the fitted PSF (J2000)}
#      {ALPHAPSF_B1950} {Right ascension of the fitted PSF (B1950)}
#      {DELTAPSF_B1950} {Declination of the fitted PSF (B1950)}
#      {FLUX_PSF} {Flux from PSF-fitting}
#      {FLUXERR_PSF} {RMS flux error for PSF-fitting}
#      {MAG_PSF} {Magnitude from PSF-fitting}
#      {MAGERR_PSF} {RMS magnitude error from PSF-fitting}
#      {NITER_PSF} {Number of iterations for PSF-fitting}
#      {CHI2_PSF} {Reduced chi2 from PSF-fitting}
#      {ERRX2PSF_IMAGE} {Variance of PSF position along x}
#      {ERRY2PSF_IMAGE} {Variance of PSF position along y}
#      {ERRXYPSF_IMAGE} {Covariance of PSF position between x and y}
#      {ERRX2PSF_WORLD} {Variance of PSF position along X-WORLD (alpha)}
#      {ERRY2PSF_WORLD} {Variance of PSF position along Y-WORLD (delta)}
#      {ERRXYPSF_WORLD} {Covariance of PSF position X-WORLD/Y-WORLD}
#      {ERRCXXPSF_IMAGE} {Cxx PSF error ellipse parameter}
#      {ERRCYYPSF_IMAGE} {Cyy PSF error ellipse parameter}
#      {ERRCXYPSF_IMAGE} {Cxy PSF error ellipse parameter}
#      {ERRCXXPSF_WORLD} {Cxx PSF error ellipse parameter (WORLD units)}
#      {ERRCYYPSF_WORLD} {Cyy PSF error ellipse parameter (WORLD units)}
#      {ERRCXYPSF_WORLD} {Cxy PSF error ellipse parameter (WORLD units)}
#      {ERRAPSF_IMAGE} {PSF RMS position error along major axis}
#      {ERRBPSF_IMAGE} {PSF RMS position error along minor axis}
#      {ERRTHTPSF_IMAGE} {PSF error ellipse position angle (CCW/x)}
#      {ERRAPSF_WORLD} {World PSF RMS position error along major axis}
#      {ERRBPSF_WORLD} {World PSF RMS position error along minor axis}
#      {ERRTHTPSF_WORLD} {PSF error ellipse pos. angle (CCW/world-x)}
#      {ERRTHTPSF_SKY} {Native PSF error ellipse pos. angle (east of north)}
#      {ERRTHTPSF_J2000} {J2000 PSF error ellipse pos. angle (east of north)}
#      {ERRTHTPSF_B1950} {B1950 PSF error ellipse pos. angle (east of north)}

   #  Columns required when drawing ellipses/circles.
   protected variable kroncols_ {
      B_IMAGE KRON_RADIUS
   }
   protected variable isocols_ {
      ISOAREA_IMAGE
   }
   protected variable commoncols_ {
      ELONGATION THETA_IMAGE
   }

   #  Columns we always generate -- need to plot positions somehow.
   protected variable safecols_ {
      NUMBER X_IMAGE Y_IMAGE
   }

   #  Parent widget of all column checkbuttons.
   protected variable colparent_ {}

   #  Names of default catalogue parameters.
   protected variable defcolumns_ {
      FLUX_ISO FLUXERR_ISO FLUX_AUTO FLUXERR_AUTO FLUX_MAX
      ISOAREA_IMAGE CXX_IMAGE CYY_IMAGE CXY_IMAGE B_IMAGE
      THETA_IMAGE KRON_RADIUS ELLIPTICITY ELONGATION FLAGS
   }

   #  Names of AUTOASTROM catalogue parameters (includes X_IMAGE and Y_IMAGE).
   #  Note should also include those needed for plotting.
   protected variable autoastromcols_ {
      FLUX_ISO A_IMAGE B_IMAGE X2_IMAGE Y2_IMAGE ERRX2_IMAGE
      ERRY2_IMAGE ISOAREA_IMAGE
   }

   #  Names and descriptions of check image types.
   protected variable checktypes_ {
      {NONE} {No check image}
      {IDENTICAL} {Identical to input}
      {BACKGROUND} {Full res. background}
      {BACKGROUND_RMS} {Full res. noise background}
      {MINIBACKGROUND} {Low res. background}
      {MINIBACK_RMS} {Low res. noise background}
      {-BACKGROUND} {Background subtracted}
      {FILTERED} {Filtered background subtracted}
      {OBJECTS} {Detected objects}
      {-OBJECTS} {Minus background & objects}
      {APERTURES} {Aperture limits (fixed & Kron)}
      {SEGMENTATION} {Segmented into object patches}
   }

   #  Names of all entries in values_ array and their default values.
   protected variable valuenames_

   #  Symbolic names of pages (used to access names via index).
   protected variable pages_
   protected variable revealed_

   #  Variables for translating names and filtering parameters.
   protected variable from_
   protected variable to_
   protected variable special_

   #  Filter status.
   protected variable filterflag_ N

   #  Default values for all parameters.
   protected variable defaults_

   #  Name of catalogue windows.
   protected variable astrocatnames_

   #  Name of controlling foreign and Starlink SExtractor object.
   protected variable foreign_sex_ {}
   protected variable star_sex_ {}

   #  Maximum number of objects to display.
   protected variable maxobjs_ 2000

   #  Directory that contains configuration files.
   protected variable config_dir_ "/star/bin/extractor/config"

   #  Command to execute when measurements are available.
   protected variable complete_cmd_ {}

   #  Name of object to deal with image names.
   protected variable namer_ {}

   #  Array for catalogue columns. Indexed by (param).
   protected variable columns_

   #  Array for local values index by a name.
   protected variable values_

   #  Common variables: (shared by all instances)
   #  -----------------

   #  A counter for generating unique catalogue names.
   common catnames_ -1

#  End of class definition.
}
