#+
#  Name:
#     GaiaContour

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class for image contouring

#  Description:
#

#  Invocations:
#
#        GaiaContour object_name [configuration options]
#
#     This creates an instance of a GaiaContour object. The
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
#     Copyright (C) 1999 Central Laboratory of the Research Councils

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     15-APR-1999 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaContour {}

itcl::class gaia::GaiaContour {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: Contouring ($itk_option(-number))"

      #  Add short help window.
      make_short_help

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File" left]
      configure_menubutton File -underline 0

      #  Add the options menu
      set Options [add_menubutton "Options" left]
      configure_menubutton Options -underline 0

      #  Add window help.
      global env
      add_help_button $env(GAIA_DIR)/GaiaContour.hlp "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      #  Add option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      $short_help_win_ add_menu_short_help $File \
         {New window} {Create a new toolbox}

      #  Save configuration to a file.
      $File add command \
         -label {Save configuration...} \
         -command [code $this write_config_file] \
         -accelerator {Control-s}
      bind $w_ <Control-s> [code $this write_config_file]
      $short_help_win_ add_menu_short_help $File \
         {Save configuration...}\
         {Write the current configuration to a text file}

      #  Read configuration from a file.
      $File add command \
         -label {Read configuration...} \
         -command [code $this read_config_file] \
         -accelerator {Control-r}
      bind $w_ <Control-r> [code $this read_config_file]
      $short_help_win_ add_menu_short_help $File \
         {Read configuration...}\
         {Read previous configuration back from a text file}

      #  Set the exit menu item.
      $File add command -label Exit \
         -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]

      #  Add an option plot carefully, or not.
      $Options add checkbutton \
         -label {Draw contours using geodesics (slow, but precise)} \
         -variable [scope careful_] \
         -onvalue 1 \
         -offvalue 0

      #  Allow selection of the contoured image.
      add_image_controls_

      #  Add controls for line attributes.
      add_att_controls_

      #  Add controls for level generation.
      add_gen_controls_

      #  Create the button bar
      itk_component add actionframe {frame $w_.action}

      #  Add a button to close window.
      itk_component add close {
         button $itk_component(actionframe).close -text Close \
            -command [code $this close]
      }
      add_short_help $itk_component(close) {Close window}

      #  Add a button to clear all contour levels.
      itk_component add clear {
         button $itk_component(actionframe).clear -text {Clear levels} \
            -command [code $this clear_levels]
      }
      add_short_help $itk_component(clear) {Clear all contour levels}

      #  Draw the contours.
      itk_component add draw {
         button $itk_component(actionframe).draw -text {Draw Contours} \
            -command [code $this draw]
      }
      add_short_help $itk_component(draw) {Draw Contours}

      #  Pack all the components into place.
      pack $itk_component(actionframe) -side bottom -fill x -pady 5 -padx 5
      pack $itk_component(close) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(clear) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(draw) -side left -expand 1 -pady 3 -padx 3

   }

   #  Destructor:
   #  -----------
   destructor  {

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
         #  Remove the contours.
         catch {remove_contours_}
      }
   }

   #  Save the current configuration to a file.
   public method write_config_file {} {
      set w [FileSelect .\#auto -title "Save configuration to a file"]
      if {[$w activate]} {
         save_config [$w get]
      }
      destroy $w
   }

   #  Restore configuration from a file.
   public method read_config_file {} {
      set w [FileSelect .\#auto -title "Read configuration from a file"]
      if {[$w activate]} {
         read_config [$w get]
      }
      destroy $w
   }

   #  Write the current configuration to a named file.
   public method save_config {filename} {
      if { $filename != {} } {
         busy {
            #  Open the output file.
            set fid [::open $filename w]
            puts $fid "\# GAIA Contours configuration file."

            # XXX fill in details

            # And add all the known values.
            ::close $fid
         }
      }
   }

   #  Read in configuration from a file.
   public method read_config {filename} {
      if { [file readable $filename] } {
         busy {
            #  Open the file.
            set fid [open $filename r]

            #  Loop over the file skipping comments and blank
            #  lines.
            set ok 1
            while { $ok  } {
               set llen [gets $fid line]
               if { $llen > 0 } {
                  if { ! [string match {\#*} $line] } {
                     # XXX fill in details...

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

   #  Add controls for selecting the image to be contoured. This can
   #  an image displayed in another window, or retained in a file.
   protected method add_image_controls_ {} {

      #  Separator.
      itk_component add namerule {
         LabelRule $w_.namerule -text "Contour image:"
      }
      pack $itk_component(namerule) -side top -fill x

      #  Menu button for selection from displayed images.
      itk_component add targets {
         LabelMenu $w_.targets \
            -labelwidth 14 \
            -valuewidth 20 \
            -valueanchor e \
            -text "Displayed image:"
      }
      pack $itk_component(targets) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(targets) \
         {Displayed image that will be contoured}

      #  Add a binding to update the menu item whenever it is pressed.
      #  XXX bit of a cheat to get menubutton name.
      set menu [$itk_component(targets) component mb]
      bind $menu <ButtonPress-1> "+[code $this update_targets_]"

      #  Add the menu items.
      update_targets_

      #  Add a control for selecting a image stored in disk file.
      itk_component add conimg {
         LabelFileChooser $w_.conimg \
            -labelwidth 14 \
            -text "Other image:" \
            -filter_types $itk_option(-filter_types) \
            -textvariable [scope imagefile_]
      }
      pack $itk_component(conimg) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(conimg) \
         {File name of undisplayed image to contour}
   }

   #  Add the controls for the contour levels and attributes.
   protected method add_att_controls_ {} {

      itk_component add attrule {
         LabelRule $w_.attrule -text "Contour levels & attributes:"
      }
      pack $itk_component(attrule) -side top -fill x

      #  Use a scrolled frame to get all these in a small amount of
      #  real estate.
      itk_component add atframe {
         scrolledframe $w_.atframe -width 100 -height 400
      }
      pack $itk_component(atframe) -fill both -expand 1
      set parent [$itk_component(atframe) childsite]

      #  Add headings.
      itk_component add athead1 {
         label $parent.value -text "Level"
      }
      itk_component add athead2 {
         label $parent.colour -text "Colour"
      }
      itk_component add athead3 {
         label $parent.width -text "Width"
      }

      grid $itk_component(athead1) $itk_component(athead2) \
           $itk_component(athead3)


      #  Set up the colour index arrays and default values.
      foreach {index xname} $colourmap_ {
         set colindex_($xname) $index
      }
      for {set i 1} {$i <= $itk_option(-maxcnt)} {incr i} {
         set index [expr int(fmod($i-1,16)*2)+1]
         set coldefault_($i) "[lindex $colourmap_ $index]"
      }

      #  Now add the controls for the actual values.
      for {set i 1} {$i <= $itk_option(-maxcnt)} {incr i} {

         #  Entry widget for the contour values.
         itk_component add value$i {
            LabelEntry $parent.value$i \
               -validate real \
               -text "$i:" \
               -labelwidth 3
         }

         #  Menu for selecting the colour.
         itk_component add colour$i {
            util::LabelMenu $parent.colour$i \
               -relief raised
         }

         #  Now add all the colours to it.
         foreach {index xname} $colourmap_ {
            $itk_component(colour$i) add \
               -label {    } \
               -value $xname \
               -background $xname
         }

         #  Set to next colour in list.
         $itk_component(colour$i) configure -value $coldefault_($i)

         #  Add menu for selecting the width.
         itk_component add width$i {
            util::LabelMenu $parent.width$i \
               -relief raised
         }

         #  Now add the range of widths to it.
         for {set j 1} {$j <= $itk_option(-maxwidth)} {incr j} {
            $itk_component(width$i) add \
               -label $j \
               -value $j
         }
         $itk_component(colour$i) configure -value 1

         #  Add these to the grid.
         grid $itk_component(value$i) $itk_component(colour$i) \
              $itk_component(width$i)
      }
      pack $itk_component(atframe) -fill both -expand 1
   }


   #  Update (or initialise) the possible target images.
   protected method update_targets_ {} {

      #  Remove any existing menu items.
      $itk_component(targets) clear

      #  Locate and add all images. The current image is "$target_".
      set images [skycat::SkyCat::get_skycat_images]

      #  Add the local rtdimage, this needs to be selected
      #  first.
      set name [$itk_option(-image) cget -file]
      $itk_component(targets) add \
         -label "$name ($itk_option(-number))" \
         -value "$itk_option(-image)" \
         -command [code $this set_target_ "$itk_option(-image)"]

      #  And add to the menu.
      foreach w $images {
         if { $w != $itk_option(-image) } {
            set name [$w cget -file]
            set clone [[winfo toplevel $w] cget -number]
            $itk_component(targets) add \
               -label "$name ($clone)" \
               -value "$w" \
               -command [code $this set_target_ "$w"]
         }
      }

      #  Contour self first.
      set_target_ $itk_option(-image)
   }

   #  Set the "target" image.
   protected method set_target_ {name} {
      set target_ $name
   }

   #  Draw the contours.
   public method draw {} {
      busy {

         #  Check the image to be contoured.
         set rtdimage [get_rtdimage_]

         #  Clear existing contours.
         if { $drawn_ } {
            catch {remove_contours_}
            update
         }

         #  Get the levels.
         set levels [get_levels_]

         #  Get the attributes.
         set atts [get_attributes_]

         #  If requested just display over the visible canvas +/- a little.
         set frac 0.80
         set xf [expr 0.5*(1.0-$frac)]
         set yf [expr 0.5*(1.0-$frac)]
         set w [winfo width $itk_option(-canvas)]
         set h [winfo height $itk_option(-canvas)]
         set x0 [$itk_option(-canvas) canvasx 0]
         set y0 [$itk_option(-canvas) canvasy 0]
         set dw [expr $w*$xf]
         set dh [expr $h*$yf]
         set x1 [expr $x0+$w-$dw]
         set y1 [expr $y0+$h-$dh]
         set x0 [expr $x0+$dw]
         set y0 [expr $y0+$dh]
         set bounds [list $x0 $y0 $x1 $y1]

         #  Set the tag used to control clear etc.
         $itk_option(-rtdimage) configure -grid_tag $itk_option(-contour_tag)

         #  Draw the contours.
         set drawn_ 1
         $itk_option(-rtdimage) contour \
            $levels $rtdimage $careful_ $atts $bounds
      }
   }

   #  Get the contour levels from the appropriate entry fields.
   protected method get_levels_ {} {
      set levels {}
      for {set i 1} {$i <= $itk_option(-maxcnt)} {incr i} {
         set value [$itk_component(value$i) get]
         if { $value != {} } {
            lappend levels $value
         }
      }
      if { $levels != {} } {
         return $levels
      } else {
         info_dialog "You must give some valid contour levels"
         return {}
      }
   }

   #  Get the attributes from the colour and width widgets.
   protected method get_attributes_ {} {
      set atts {}
      for {set i 1} {$i <= $itk_option(-maxcnt)} {incr i} {
         set value [$itk_component(value$i) get]
         if { $value != {} } {
            set colour [$itk_component(colour$i) get]
            set colour $colindex_($colour)
            set width [expr [$itk_component(width$i) get]*0.005]
            lappend atts "colour(curve)=$colour,width(curve)=$width"
         }
      }
      return $atts
   }

   #  Clear the attributes and levels.
   public method clear_levels {} {
      for {set i 1} {$i <= $itk_option(-maxcnt)} {incr i} {
         $itk_component(value$i) configure -value {}
         $itk_component(colour$i) configure -value $coldefault_($i)
         $itk_component(width$i) configure -value 1
      }
   }

   #  Get the rtdimage that is needed for contouring. This can be the
   #  current image, a one displayed elsewhere or an image in a disk
   #  file. A filename takes preference over a one displayed already.
   protected method get_rtdimage_ {} {
      if { $imagefile_ != {} } {

         #  Displayed on disk, create an rtdimage and return this.
         if { [catch {image create rtdimage -file $imagefile_} rtdimage] != 0} {
            error_dialog "Failed to access image: $imagefile_, for contouring ($rtdimage)"
            set rtdimage {}
         }

      } else {

         #  Name of an rtdimage, just check that this isn't the
         #  current one and that it exists.
         if { [catch {$target_ get_image} rtdimage] != 0 }  {
            error "Failed to locate the displayed image for contouring"
            set rtdimage {}
         } else {
            if { $rtdimage == $itk_option(-rtdimage) } {
               set rtdimage {}
            }
         }
      }
      return $rtdimage
   }

   #  Remove contours.
   protected method remove_contours_ {} {
      $itk_option(-canvas) delete $itk_option(-contour_tag)
      set drawn_ 0
   }

   #  Level generation commands. XXX Just use simple generation commands,
   #  no need to look at image data levels.
   protected method add_gen_controls_ {} {

      #  Add section header and frame to contain generation commands
      #  in a line.
      itk_component add genrule {
         LabelRule $w_.genrule -text "Contour generation:"
      }
      pack $itk_component(genrule) -side top -fill x
      itk_component add gframe1 {
         frame $w_.gframe1
      }
      itk_component add gframe2 {
         frame $w_.gframe2
      }

      #  Number of contours to generate.
      itk_component add ncont {
         util::LabelMenu $itk_component(gframe1).ncont \
            -relief raised \
            -text {Number:}
      }
      pack $itk_component(ncont) -side left -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(ncont) \
         {Number of contour levels to generate}
      for {set i 0} {$i < $itk_option(-maxcnt)} {incr i} {
         $itk_component(ncont) add \
            -label $i \
            -value $i
      }
      $itk_component(ncont) configure -value 10

      #  Starting value.
      itk_component add start {
         LabelEntry $itk_component(gframe1).start \
            -validate real \
            -text "Start:"
      }
      pack $itk_component(start) -side left -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(start) \
         {Starting point for level generation}

      #  Increment.
      itk_component add incre {
         LabelEntry $itk_component(gframe1).incre \
            -validate real \
            -text "Increment:"
      }
      pack $itk_component(incre) -side left -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(incre) \
         {Increment between generated levels}

      #  Type of generation.
      itk_component add ctype {
         util::LabelMenu $itk_component(gframe2).ctype \
            -relief raised \
            -text {Algorithm:}
      }
      pack $itk_component(ctype) -side left -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(ctype) \
         {Algorithm to use for contour generation}
      foreach type {linear magnitude} {
         $itk_component(ctype) add \
            -label $type \
            -value $type
      }
      $itk_component(ctype) configure -value linear

      #  Button to generate contours.
      itk_component add generate {
         button $itk_component(gframe2).gen \
            -text "Generate" \
            -command [code $this generate_contours_]
      }
      pack $itk_component(generate) -side left -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(generate) \
         {Generate contours}

      pack $itk_component(gframe1) -side top -fill x -ipadx 1m -ipady 1m
      pack $itk_component(gframe2) -side top -fill x -ipadx 1m -ipady 1m
   }

   #  Configuration options: (public variables)
   #  ----------------------
   #  Name of canvas.
   itk_option define -canvas canvas Canvas {} {}

   #  Name of rtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {} {}

   #  Name of RtdImageCtrl widget or a derived class.
   itk_option define -image image Image {} {}

   #  Name of CanvasDraw widget.
   itk_option define -canvasdraw canvasdraw CanvasDraw {} {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  If this is a clone, then it should die rather than be withdrawn.
   itk_option define -really_die really_die Really_Die 0

   #  The filter types of images.
   itk_option define -filter_types filter_types Filter_Types {} {}

   #  Whether contours are plotted carefully (slow, but precise) or not.
   itk_option define -careful careful Careful 0 {
      set careful_ $itk_option(-careful)
   }

   #  Canvas tag used to control redraws etc.
   itk_option define -contour_tag contour_tag Contour_Tag {} {
      if { $itk_option(-contour_tag) == {} } {
         set itk_option(-contour_tag) "ast_contour[incr contour_count_]"
      }
   }

   #  Maximum number of contours, only works once.
   itk_option define -maxcnt maxcnt Maxcnt 30

   #  Maximum width of contour line (as multiple of 0.005).
   itk_option define -maxwidth maxwidth Maxwidth 10

   #  Protected variables: (available to instance)
   #  --------------------
   #  Whether contours are plotted carefully, used for checkbutton var.
   protected variable careful_ 0

   #  Name of rtdimage that we are contouring or the filename to use.
   protected variable target_ {}
   protected variable imagefile_ {}

   #  Whether contours have been drawn already.
   protected variable drawn_ 0

   #  Names of the possible colours and their AST index equivalents.
   protected variable colourmap_ {
      0 "#fff" 2 "#f00" 3 "#0f0" 4 "#00f" 5 "#0ff" 6 "#f0f"
      7 "#ff0" 8 "#f80" 9 "#8f0" 10 "#0f8" 11 "#08f" 12 "#80f"
      13 "#f08" 14 "#512751275127" 15 "#a8b4a8b4a8b4" 1 "#000" }

   #  Colours-v-indices (set up from colourmap_) and default colours.
   protected variable colindex_
   protected variable coldefault_

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Number of contours drawn.
   common contour_count_ 0


#  End of class definition.
}
