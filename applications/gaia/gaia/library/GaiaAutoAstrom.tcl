#+
#  Name:
#     GaiaAutoAstromSimple

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Toolbox for performing "simple" astrometry using autoastrom.

#  Invocations:
#
#        GaiaAutoAstromSimple object_name [configuration options]
#
#     This creates an instance of a GaiaAutoAstromSimple object. The return is
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
#     See itk_option define statements.

#  Methods:
#     See below

#  Inheritance:
#     TopLevelWidget

#  Copyright:
#     Copyright (C) 2003 Central Laboratory of the Research Councils

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     24-JUL-2003 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaAutoAstromSimple {}

itcl::class gaia::GaiaAutoAstromSimple {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: Simple auto astrometry ($itk_option(-number))"

      #  Create the short help window.
      make_short_help

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File" left]
      configure_menubutton File -underline 0
      add_short_help $itk_component(menubar).file {File menu: close window}

      #  Add option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      $short_help_win_ add_menu_short_help $File \
         {New window} {Create a new toolbox}

      #  Set the exit menu items.
      $File add command -label {Cancel changes and close window} \
         -command [code $this cancel] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this cancel]

      $File add command -label {Accept changes and close window}\
         -command [code $this accept] \
         -accelerator {Control-a}
      bind $w_ <Control-a> [code $this accept]

      #  Add window help.
      add_help_button astrometry "Astrometry Overview..."
      add_help_button autoastromsimple "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      #  Get the WCS to use as bootstrap. This can come from the
      #  image, or as a set of fixed parameters.
      set lwidth 20
      itk_component add wcssource {
         StarLabelCheck $w_.wcssource \
            -text "Initial WCS from image:" \
            -onvalue Y \
            -offvalue N \
            -labelwidth $lwidth \
            -anchor w \
            -variable [scope values_(wcssource)] \
            -command [code $this toggle_wcssource_]
      }
      add_short_help $itk_component(wcssource) \
         {Source of the bootstrap WCS information}
      set values_(wcssource) N
      
      #  If WCS source isn't the image, then we need some parameters.
      itk_component add racentre {
         FITSLabelEntry $w_.racentre \
            -text "RA centre:" \
            -labelwidth $lwidth \
            -textvariable [scope values_(racentre)] \
            -rtdimage $itk_option(-rtdimage)
      }
      add_short_help $itk_component(racentre) \
         {Centre in Right Ascension (hh:mm:ss.ss/dd.dd)}
      set values_(racentre) "00:00:00"

      itk_component add deccentre {
         FITSLabelEntry $w_.deccentre \
            -text "Dec centre:" \
            -labelwidth $lwidth \
            -textvariable [scope values_(deccentre)] \
            -rtdimage $itk_option(-rtdimage)
      }
      add_short_help $itk_component(deccentre) \
         {Centre in Declination (dd:mm:ss.ss/dd.dd)}
      set values_(deccentre) "00:00:00"
      
      itk_component add imagescale {
         FITSLabelEntry $w_.imagescale \
            -text "Image scale:" \
            -labelwidth $lwidth \
            -textvariable [scope values_(imagescale)] \
            -rtdimage $itk_option(-rtdimage)
      }
      add_short_help $itk_component(imagescale) \
         {Image scale in arc-seconds per pixel}
      set values_(imagescale) "1.0"

      itk_component add angle {
         FITSLabelEntry $w_.angle \
            -text "Position Angle:" \
            -labelwidth $lwidth \
            -textvariable [scope values_(angle)] \
            -rtdimage $itk_option(-rtdimage)
      }
      add_short_help $itk_component(angle) \
         {Position angle, Dec axis anti-clock degrees}
      set values_(angle) "0.0"

      itk_component add invert {
         StarLabelCheck $w_.invert \
            -text "Axes are flipped:" \
            -onvalue 1 \
            -offvalue 0 \
            -labelwidth $lwidth \
            -anchor w \
            -variable [scope values_(invert)]
      }
      add_short_help $itk_component(invert) \
         {One of the axes is flipped WRT to normal convention}
      set values_(invert) 0

      #  Choose an calibration catalogue. This should contain
      #  all the remote reference RA and Dec catalogues.
      itk_component add refcat {
         set m [util::LabelMenu $w_.refcat \
                   -text "Reference catalogue:" \
                   -relief raised \
                   -labelwidth $lwidth \
                   -valuewidth 20 \
                   -variable [scope values_(refcat)]]
      }
      add_short_help $itk_component(refcat) \
         {Choose a reference catalogue}
      add_reference_catalogues_

      #  Region for showing the output from AUTOASTROM.
      itk_component add status {
         Scrollbox $w_.status -singleselect 0 -exportselection 1  
      }
      $w_.status configure -height 5
      add_short_help $itk_component(status) \
         {Displays output from Autoastrom}

      #  Create the button bar
      itk_component add actionframe {frame $w_.action}

      #  Add a button to close window and accept the new WCS.
      itk_component add accept {
         button $itk_component(actionframe).accept -text Accept \
            -command [code $this accept]
      }
      add_short_help $itk_component(accept) \
         {Accept astrometric calibration and close window}

      #  Add a button to ask for a fit.
      itk_component add fit {
         button $itk_component(actionframe).fit -text Fit \
            -command [code $this fit]
      }
      add_short_help $itk_component(fit) \
         {Perform the fit and associate solution with image}

      #  Add a button to close window and not accept the new WCS.
      itk_component add cancel {
         button $itk_component(actionframe).cancel -text Cancel \
            -command [code $this cancel]
      }
      add_short_help $itk_component(cancel) \
         {Close window and restore original astrometric calibration}

      #  Add a button to reset the WCS to the last version.
      itk_component add reset {
         button $itk_component(actionframe).reset -text Reset \
            -command [code $this reset_]
      }
      add_short_help $itk_component(reset) \
         {Reset image to the original astrometric calibration}

      #  Pack widgets into place.
      pack $itk_component(wcssource) -side top -fill x -pady 5 -padx 5
      pack $itk_component(racentre) -side top -fill x -pady 5 -padx 5
      pack $itk_component(deccentre) -side top -fill x -pady 5 -padx 5
      pack $itk_component(imagescale) -side top -fill x -pady 5 -padx 5
      pack $itk_component(angle) -side top -fill x -pady 5 -padx 5
      pack $itk_component(invert) -side top -fill x -pady 5 -padx 5
      pack $itk_component(refcat) -side top -fill x -pady 5 -padx 5
      pack $itk_component(status) -side top -fill both -expand 1 -pady 5 -padx 5

      pack $itk_component(actionframe) -side bottom -fill x -pady 5 -padx 5
      pack $itk_component(accept) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(cancel) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(reset) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(fit)   -side right -expand 1 -pady 3 -padx 3

      #  Create an object for dealing with image names.
      set namer_ [GaiaImageName \#auto]
   }

   #  Destructor:
   #  -----------
   destructor  {
      if { $autoastrom_ != {} } {
         $autoastrom_ delete_sometime
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

   #  Withdraw this window without accepting any new WCS information.
   public method cancel {} {

      #  Restore WCS system to the original (if available).
      if { $itk_option(-rtdimage) != {} } {
         reset_
      }
      if { $itk_option(-really_die) } {
         delete object $this
      } else {
         wm withdraw $w_
      }
   }

   #  Withdraw window and write new WCS to image permanently.
   public method accept {} {
      $itk_option(-rtdimage) astfix
      if { $itk_option(-really_die) } {
         delete object $this
      } else {
         wm withdraw $w_
      }
   }

   #  Reset the WCS of the displayed image back to the original
   #  version.
   protected method reset_ {} {
      catch {
         $itk_option(-rtdimage) astrestore original
      }
      notify_
   }

   #  Run autoastrom to perform the fit.
   public method fit {} {
      global env

      set image [$itk_option(-rtdimage) fullname]
      if { $image != "" } {
         $namer_ configure -imagename $image
         set image [$namer_ ndfname]
         set diskimage [$namer_ diskfile]

         busy {
            #  Establish a control object for this foreign task,
            #  if not already done.
            if { $autoastrom_ == {} } {
               set autoastrom_ [GaiaForeignExec \#auto \
                                   -use_error 1 \
                                   -keepnewlines 0 \
                                   -show_output $itk_component(status) \
                                   -preprocess [code $this clean_] \
                                   -notify [code $this completed_] \
                                   -application \
                                   $env(AUTOASTROM_DIR)/autoastrom]
            }

            #  Clear the log window.
            $itk_component(status) clear 0 end

            #  Remove previous results.
            catch {::file delete -force "autoastrom_tmp"} msg
            puts "delete msg = $msg"

            catch {::file delete -force "$solution_"} msg
            puts "delete msg = $msg"

            #  Construct WCS bootstrap source.
            if { $values_(wcssource) == "Y" } {
               set wcssource "--obsdata=source=AST:FITS"
            } else {
               #  Do this carefully, obsdata doesn't like spaces.
               set wcssource "--obsdata=source=USER"
               append wcssource ",ra=[string trim $values_(racentre)]"
               append wcssource ",dec=[string trim $values_(deccentre)]"
               append wcssource ",scale=[string trim $values_(imagescale)]"
               append wcssource ",angle=[string trim $values_(angle)]"
               append wcssource ",invert=[string trim $values_(invert)]"
            }

            #  Run program, monitoring output...

            set cmd "$autoastrom_ runwith \
               $wcssource \
               --match=match \
               --skycatconfig=$env(CATLIB_CONFIG) \
               --catalogue=$values_(refcat) \
               --noinsert \
               --keepfits=$solution_ \
               --temp=autoastrom_tmp \
               --matchcatalogue=$solution_catalogue_ \
               --nomessages \
               $diskimage"

            #  Use local SExtractor catalogue...
            #--xxxccdcat=ngc1275.autoext
            puts "Running: $cmd"
            catch {eval $cmd} msg
            puts "msg = $msg"
         }
      }
   }

   protected method completed_ {} {
      puts "Completed"
      if { [file exists $solution_] } {

         #  Need to read the file to extract the solution so GAIA can
         #  display this. Simplest thing to do is read as a new image
         #  and copy the WCS.
         $itk_option(-rtdimage) astcopy $solution_
         $itk_option(-rtdimage) astreplace
         notify_

         #  Re-draw the positions used in the solution.
         puts "calling display_solution_cat_"
         display_solution_cat_
      } else {
         error_dialog "Failed to determine a solution"
      }
   }

   #  Do the notify_cmd option if needed.
   protected method notify_ {} {
      if { $itk_option(-notify_cmd) != {} } {
         eval $itk_option(-notify_cmd)
      }
   }

   #  Clean output of known non-printing characters.
   protected method clean_ {output} {
      # Tcl 8.3 regsub -all {[^[:print:] ]} $output {.} clean
      regsub -all "\[\033\]" $output {} output
      regsub -all "\[\011\]" $output {      } output
      regsub -all {\[1A|\[1M>} $output {} output
      return $output
   }

   #  Configure the reference catalogue window menu to show the
   #  available catalogues. These are the "remote" catalogues
   #  available to GAIA. A suitable default is chosen.
   protected method add_reference_catalogues_ {} {

      #  Get list of catalogues.
      if { [catch {set catalog_list [lsort [$astrocat_ info "catalog"]]} msg] } {
         error_dialog $msg
         return
      }
        
      if {[llength $catalog_list]} {
         foreach i $catalog_list {
            set longname [$astrocat_ longname $i]
            set shortname [$astrocat_ shortname $i]
            $itk_component(refcat) add \
               -label $longname \
               -value $shortname \
               -command [code $this set_value_ refcat $shortname]
         }

         #  Set the default.
         set values_(refcat) "usno@eso"
      }
   }

   #  Set a values_ array element.
   protected method set_value_ {element value} {
      set values_($element) $value
   }

   protected method toggle_wcssource_ {} {
      if { $values_(wcssource) == "Y" } {
         set state "disabled"
      } else {
         set state "normal"
      }
      $itk_component(racentre) configure -state $state
      $itk_component(deccentre) configure -state $state
      $itk_component(imagescale) configure -state $state
      $itk_component(angle) configure -state $state
      $itk_component(invert) configure -state $state
   }

   #  Display the solution positions.
   protected method display_solution_cat_ {} {
      puts "in display_solution_cat_"

      #  Open the catalogue window. Returns {} is already around.
      set astrocat [gaia::GaiaSearch::new_local_catalog \
                       $solution_catalogue_ [code $itk_option(-image)] \
                       ::gaia::GaiaSearch 0 catalog $w_]
      
      if { $astrocat != {} } {
         set astrocatname_ $astrocat
         after idle "$astrocatname_ set_symbol {} {circle red {} {} {} {}} {{4.0} {}}"
      }
      $astrocatname_ search
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of rtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {} {}

   #  Name of RtdImageCtrl widget or a derived class.
   itk_option define -image image Image {} {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Command to execute when the WCS is changed.
   itk_option define -notify_cmd notify_cmd Notify_Cmd {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  If this is a clone, then it should die rather than be withdrawn.
   itk_option define -really_die really_die Really_Die 0

   #  Protected variables: (available to instance)
   #  --------------------
   
   #  The AUTOASTROM task.
   protected variable autoastrom_ {}

   #  Name resolver.
   protected variable namer_ {}

   #  Values from widgets.
   protected variable values_

   #  Name of astrocat instance we're using to display the positions
   #  catalogue.
   protected variable astrocatname_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Name of file to store the solution.
   common solution_ GaiaAutoAstSolution.fits

   #  Name of file to store the catalogue of positions used in fit.
   common solution_catalogue_ GaiaAutoAstSolutionPos.ASC

   # C++ astrocat object used here to access catalog entries
   common astrocat_ [astrocat ::gaia::.cat]

#  End of class definition.
}
