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

      #  Options menu.
      set Options [add_menubutton "Options" left]
      configure_menubutton Options -underline 0

      #  Whether to show verbose output from AUTOASTROM.
      $Options add checkbutton -label {Verbose log} \
         -variable [scope values_(verbose)] \
         -onvalue 1 \
         -offvalue 0
      $short_help_win_ add_menu_short_help $Options \
         {Verbose log} \
         {Show verbose output from AUTOASTROM}
      set values_(verbose) 0

      #  Whether to keep any temporary files.
      $Options add checkbutton -label {Keep temps} \
         -variable [scope values_(keeptemps)] \
         -onvalue 1 \
         -offvalue 0
      $short_help_win_ add_menu_short_help $Options \
         {Keep temps} \
         {Keep temporary files created by AUTOASTROM (debug only)}
      set values_(keeptemps) 0

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
         {Image centre in Right Ascension (hh:mm:ss.ss/dd.dd)}
      set values_(racentre) "00:00:00"

      itk_component add deccentre {
         FITSLabelEntry $w_.deccentre \
            -text "Dec centre:" \
            -labelwidth $lwidth \
            -textvariable [scope values_(deccentre)] \
            -rtdimage $itk_option(-rtdimage)
      }
      add_short_help $itk_component(deccentre) \
         {Image centre in Declination (dd:mm:ss.ss/dd.dd)}
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

      #  Button to guess initial values based on FITS headers.
      itk_component add guess {
         button $w_.guess \
            -text "Guess" \
            -command [code $this fits_based_guess_]
      }
      add_short_help $itk_component(guess) \
         {Make a guess based on FITS headers}

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

      #  Attempt linear fit.
      itk_component add linear  {
         StarLabelCheck $w_.linear \
            -text "Perform linear fit:" \
            -onvalue 1 \
            -offvalue 0 \
            -labelwidth $lwidth \
            -anchor w \
            -variable [scope values_(linear)]
      }
      add_short_help $itk_component(linear) \
         {Only attempt a linear fit (otherwise attempt distortion)}
      set values_(linear) 1

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
      pack $itk_component(guess) -side top -pady 5 -padx 5
      pack $itk_component(invert) -side top -fill x -pady 5 -padx 5
      pack $itk_component(linear) -side top -fill x -pady 5 -padx 5
      pack $itk_component(refcat) -side top -fill x -pady 5 -padx 5
      pack $itk_component(status) -side top -fill both -expand 1 -pady 5 -padx 5

      pack $itk_component(actionframe) -side bottom -fill x -pady 5 -padx 5
      pack $itk_component(accept) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(cancel) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(reset) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(fit)   -side right -expand 1 -pady 3 -padx 3

      #  Create an object for dealing with image names.
      set namer_ [GaiaImageName \#auto]

      #  Do a guess.
      fits_based_guess_
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
         set image [$namer_ ndfname 0]
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
            catch {::file delete -force "$solution_"} msg
            set error_messages_ {}

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

            #  Level of fit.
            if { $values_(linear) } {
               set fitopts "--maxfit=6"
            } else {
               set fitopts "--maxfit=9"; # or maybe 7.
            }

            #  Verbosity
            if { $values_(verbose) } {
               set verbosity "--verbose"
            } else {
               set verbosity "--noverbose"
            }

            #  Keep temporary files (debugging).
            if { $values_(keeptemps) } {
               set keeptemps "--keeptemps"
            } else {
               set keeptemps "--nokeeptemps"
            }

            #  Run program, monitoring output...

            set args "$wcssource \
               --match=match \
               --skycatconfig=$env(CATLIB_CONFIG) \
               --catalogue=$values_(refcat) \
               --noinsert \
               --keepfits=$solution_ \
               --temp=autoastrom_tmp \
               --matchcatalogue=$solution_catalogue_ \
               --nomessages \
               $fitopts \
               $verbosity \
               $keeptemps \
               --bestfitlog=$bestfitlog_"

            #--catalogue=temp.TAB \
            #  Use local SExtractor catalogue...
            #--xxxccdcat=ngc1275.autoext
            puts "Running: $args $image"
            catch {eval $autoastrom_ runwith $args \$image} msg
            #if { "$msg" != "" && "$msg" != "0" } {
            #   warning_dialog "$msg"
            #}
            if { $error_messages_ != {} } {
               warning_dialog "$error_messages"
            }
         }
      }
   }

   protected method completed_ {} {
      if { [file exists $solution_] } {

         #  Report the best fit parameters.
         if { [file exists $bestfitlog_] } {
            report_bestfit_
         }

         #  Need to read the file to extract the solution so GAIA can
         #  display this. Simplest thing to do is read as a new image
         #  and copy the WCS.
         $itk_option(-rtdimage) astcopy $solution_
         $itk_option(-rtdimage) astreplace
         notify_

         #  Re-draw the positions used in the solution.
         display_solution_cat_
      } else {
         error_dialog "Failed to determine a solution"
      }
   }

   #  Make a report about the best fit to the status window.
   protected method report_bestfit_ {} {
      $itk_component(status) insert end ""
      $itk_component(status) insert end "Attempted Solutions"
      $itk_component(status) insert end "-------------------"
      set fid [::open $bestfitlog_ "r"]
      set ok 1
      set nlines 0
      while { [::gets $fid line] > -1 && $ok } {
         if { [string match "ASTROM:*" $line] } {
            if { [string match "ASTROM: best fit*" $line] } {
               set ok 0
            }
            continue
         }
         if { $nlines == 0 } {
            set t1 "Parameters"
            set t2 "Plate Centre"
            set t3 "RMS"
            set t4 "Q"
         } else {
            lassign "$line" n t1 ra dec t3 t4 fits
            set t2 "$ra $dec"

         }

         if { $t1 != "NO" } {
            set line [format "%-12s %-20s %-6s %-4s" $t1 $t2 $t3 $t4]
            $itk_component(status) insert end $line
         }
         incr nlines
      }

      #  Get rest of file as parsed arrays of parameter names and values.
      set nlines 0
      while { [::gets $fid line] > -1 } {
         set line [clean_ $line]
         lassign "$line" param delim value
         set params($param) $value
      }
      ::close $fid

      $itk_component(status) insert end ""
      $itk_component(status) insert end "Best Fit Parameters"
      $itk_component(status) insert end "-------------------"

      #  Match these against those we can describe and write them out.
      foreach {name desc} "$pnames_" {
         if { [info exists params($name)] } {
            $itk_component(status) insert end "$name = $params($name)  / $desc"
         }
      }
      $itk_component(status) see end
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

      #  Gather any error messages.
      if { [string match "--E*" $output] } {
         append error_messages $output
      }
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

      #  Open the catalogue window. Returns {} if already around.
      set astrocat [gaia::GaiaSearch::new_local_catalog \
                       $solution_catalogue_ [code $itk_option(-image)] \
                       ::gaia::GaiaSearch 0 catalog $w_]

      if { $astrocat != {} } {
         set astrocatname_ $astrocat
         after idle "$astrocatname_ set_symbol {} {circle red {} {} {} {}} {{4.0} {}}"
      }

      #  Set display area to whole image (doesn't update if image is changed).
      $astrocatname_ set_from_image

      #  Search and display.
      $astrocatname_ search
   }

   #  Guess the initial WCS parameters from the FITS headers of the
   #  image. This is purely guess work based on a look around any
   #  images that I've come across.
   protected method fits_based_guess_ {} {

      if { $itk_option(-rtdimage) != {} } {

         #  Reset to defaults first.
         set values_(racentre) "00:00:00"
         set values_(deccentre) "00:00:00"
         set values_(imagescale) "1.0"
         set values_(angle) "0.0"

         #  RA.
         set ra_keys_ "RA OBSRA OBJCTRA RABASE RA_TARG CRVAL1"
         foreach key $ra_keys_ {
            set value [$itk_option(-rtdimage) fits get $key]
            if { $value != {} } {
               regsub -all { } [string trim $value] {:} value
               set values_(racentre) $value
               break
            }
         }

         #  Dec.
         set dec_keys_ "DEC OBSDEC OBJCTDEC DECBASE DEC_TARG CRVAL2"
         foreach key $dec_keys_ {
            set value [$itk_option(-rtdimage) fits get $key]
            if { $value != {} } {
               regsub -all { } [string trim $value] {:} value
               set values_(deccentre) $value
               break
            }
         }

         #  Image scale. Arc sec
         set imagescale_done 0
         set arcsec_scale_keys_ \
            "SECPIX SECPIX1 SECPIX2 PIXSCAL1 PIXSCAL2 SECPPIX"
         foreach key $arcsec_scale_keys_ {
            set value [$itk_option(-rtdimage) fits get $key]
            if { $value != {} } {
               set values_(imagescale) [expr abs($value)]
               set imagescale_done 1
               break
            }
         }

         #  Image scale. Degrees.
         if { ! $imagescale_done } {
            set degree_scale_keys_ "CDELT1 CDELT2"
            foreach key $degree_scale_keys_ {
               set value [$itk_option(-rtdimage) fits get $key]
               if { $value != {} } {
                  set values_(imagescale) [expr abs($value*3600.0)]
                  break
               }
            }
         }

         #  Rotation.
         set rotation_keys_ "CROTA2 CROTA1 DECPANGL"
         foreach key $rotation_keys_ {
            set value [$itk_option(-rtdimage) fits get $key]
            if { $value != {} } {
               set values_(angle) $value
               break
            }
         }
      }
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

   #  Any error messages issued by AUTOASTROM.
   protected variable error_messages_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Name of file to store the solution.
   common solution_ GaiaAutoAstSolution.fits

   #  Name of file to store the catalogue of positions used in fit.
   common solution_catalogue_ GaiaAutoAstSolutionPos.ASC

   #  Name of file to store the best fit parameters.
   common bestfitlog_ GaiaAutoAstromFit.Log

   # C++ astrocat object used here to access catalog entries
   common astrocat_ [astrocat ::gaia::.cat]

   #  Names of any parameters that we want to report from AUTOASTROM best
   #  fit log file and their descriptions as pairs (use foreach name desc).
   common pnames_ {
      "nstars"    "no. ref stars"
      "rrms"      "radial rms, arcsec"
      "xrms"      "x-axis rms, arcsec"
      "yrms"      "y-axis, rms arcsec"
      "plate"     "plate scale (mean), arcsec"
      "prms"      "radial rms, pixels"
      "nterms"    "no. terms in fit"
      "rarad"     "projection pole Dec, radians"
      "decrad"    "projection pole RA, radians"
      "rasex"     "projection pole RA, sexagesimal"
      "decsex"    "projection pole Dec, sexagesimal"
      "q"         "radial distortion, (rad^{-2})"
      "deltaq"    "change in radial distortion"
      "deltaqsd"  "standard deviation of change in radial distortion"
      "deltapc"   "change in plate centre"
      "deltapcsd" "standard deviation of change in plate centre"
   }

#  End of class definition.
}
