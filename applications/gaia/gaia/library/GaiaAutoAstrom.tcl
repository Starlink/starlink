#+
#  Name:
#     GaiaAutoAstromSimple

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Toolbox for performing "simple" astrometry using autoastrom.

#  Description:

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

      itk_component add status {
         Scrollbox $w_.status
      }
      $w_.status configure -height 5
      add_short_help $itk_component(status) \
         {Displays output from Autoastrom}

      #  Pack widgets into place.
      pack $itk_component(status) -side top -fill both -expand 1 -pady 5 -padx 5

      pack $itk_component(actionframe) -side bottom -fill x -pady 5 -padx 5
      pack $itk_component(accept) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(cancel) -side right -expand 1 -pady 3 -padx 3
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
         catch {
            $itk_option(-rtdimage) astrestore original
         }
         notify_
      }
      if { $itk_option(-really_die) } {
         delete object $this
      } else {
         wm withdraw $w_
      }
   }

   #  Withdraw window and write new WCS to image permanently.
   public method accept {} {
      set_wcs_
      $itk_option(-rtdimage) astfix
      if { $itk_option(-really_die) } {
         delete object $this
      } else {
         wm withdraw $w_
      }
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
                                   -show_output $itk_component(status) \
                                   -preprocess [code $this clean_] \
                                   -notify [code $this completed_] \
                                   -application $env(AUTOASTROM_DIR)/autoastrom]
                                   #-use_error 1
            }

            #  Clear the log window.
            $itk_component(status) clear 0 end

            #  Remove previous results.
            catch {::file delete -force "autoastrom_tmp"} msg
            puts "delete msg = $msg"

            catch {::file delete -force "solution.fits"} msg
            puts "delete msg = $msg"

            #  Run program, monitoring output...

            set cmd "$autoastrom_ runwith \
               --match=match \
               --skycatconfig=$env(CATLIB_CONFIG) \
               --catalogue=ngc1275.TAB \
               --xxxccdcat=ngc1275.autoext \
               --noinsert \
               --keepfits=solution.fits \
               --temp=autoastrom_tmp \
               --keeptemps \
               $diskimage"

            # Use local catalogues...

            # --verbose

            puts "Running: $cmd"
            puts "$env(http_proxy)"
            catch {eval $cmd} msg
            puts "msg = $msg"

         }
      }
   }

   protected method completed_ {} {
      puts "Completed"
      if { [file exists "solution.fits"] } {
         puts "solution.fits exists"
      } else {
         puts "solution.fits doesn't exist"
      }

      #  Read the channel to create an AST object and then replace
      #  the current WCS using it.
      #      $image astread $chan
      #      $image astreplace
      #      notify_
      #      $image astdelete $chan
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

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of rtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {} {}

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

   protected variable autoastrom_ {}
   protected variable namer_ {}

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
