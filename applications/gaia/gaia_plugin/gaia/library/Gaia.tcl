#+
#  Name:
#     Gaia.tcl

#  Purpose:
#     Defines a class for creating a GAIA window.

#  Type of Module:
#     [incr Tk] class

#  Description:
#     This is the class that creates the GAIA display tool.

#  Invocation:
#     Gaia name [configuration options]

#  Notes:
#     This will only run with the gaia_wish installed as part
#     of the GAIA package with a Starlink extended RTD.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK)
#     ALLAN: Allan Brighton (ESO)
#     {enter_new_authors_here}

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Inherits:
#     Methods and configuration options of SkyCat (and Rtd).

#  History:
#     24-SEP-1997 (PDRAPER):
#        Original version
#     24-APR-1998 (ALLAN):
#        Changed "clone" method to work with new tclutil version.
#        Added check for "convert" package.
#        Pass new itk_options from command line in make_rtdimage.
#     29-APR-1998 (ALLAN):
#        Remove "Save region as..." from skycat menu, since the operation
#        is not supported by the StarFitsIO class (the same functionality
#        is found under "Image regions..." anyway).
#     {enter_changes_here}

#-

#  Version.
set gaia_version "2.0"

#  Make a local copy of about_skycat so we can divert bug reports.
set about_gaia "\

GAIA/SkyCat version $gaia_version

Copyright (C) 1997-1998 Central Laboratory of the Research Councils (U.K.)

Authors:
Peter W. Draper (P.W.Draper@durham.ac.uk)

GAIA is derived from SkyCat version [skycat_version]
Copyright (C) 1996-1998 ESO - European Southern Observatory

Authors:  
Allan Brighton (abrighto@eso.org)
Thomas Herlin (therlin@eso.org) 
Miguel Albrecht (malbrech@eso.org)
Daniel Durand (durand@dao.nrc.ca)
Peter Biereichel (pbiereic@eso.org)

Bug reports and suggestions to: ussc@star.rl.ac.uk

"

set about_skycat ""

itk::usual Gaia {}

#  Create a class for the application.
class gaia::Gaia {
   inherit skycat::SkyCat

   #  Constructor: create a toplevel window.
   constructor {args} { 

      #  Remove any options we're overriding and evaluate all
      #  options. 
      itk_option remove rtd::Rtd::scrollbars
      eval itk_initialize $args

      #  Override about_skycat message.
      global about_skycat about_gaia
      set about_skycat $about_gaia
   }  

   #  Destructor: delete file if temporary.
   destructor {
      delete_temporary_
   }


   #  Called after the options have been evaluated. Note this method is
   #  a hybrid of the Rtd/SkyCat inits. 
   method init {} {
      SkyCat::init
      #  Get the clone number for this window.
      set clone_ $itk_option(-number)

      #  Set/get X defaults - can be overridden in subclass and/or
      #  in user's .Xdefaults file.
      tk appname GAIA

      feedback "GAIA toolboxes..."
      if { $itk_option(-gaia) } { 
         add_gaia_menu
      }
      #  Add the filters menu if required (not used at present).
      feedback "filters..."
      if { $itk_option(-filters) } { 
         make_filters_menu
      }

      # remove unsupported item from File menu
      set m [get_menu File]
      catch {$m delete "Save region as..."}

      # add item to View menu
      set m [get_menu View]
      $m add separator
      add_menuitem $m cascade "Image background" \
         {Change the background colour of the main window} \
         -menu [menu $m.back]
      foreach colour $colours_ {
         $m.back add radiobutton \
            -background $colour \
            -variable $w_.colour \
            -value $colour \
            -label {    } \
            -command [code $this set_background_ $colour]
      }      
   }


   #  Add help for GAIA and SkyCat.
   method add_help_menu {} {
      global ::gaia_library
      set m [add_help_button $gaia_library/Gaia.hlp "On Window..." \
             {Display help on this window and general features}   ]

      add_menuitem $m command "About GAIA/SkyCat..." \
         {Display a window with information about this GAIA/SkyCat version} \
         -command [code $itk_component(image) about]
      
      add_menuitem $m command "SkyCat..." \
         {Display information about SkyCat in netscape (if netscape is available)} \
         -command [code $itk_component(image) send_to_netscape $itk_option(-help_url)]
      
      add_short_help $itk_component(menubar).help \
         {Help menu: display information about this application}
   }


   #  Set default X resources for colors and fonts, and set some default key
   #  Bindings.
   method setXdefaults {} {
       SkyCat::setXdefaults
       gaia::setXdefaults
   }

   #  Create the rtd image widget with the extended RTD functionality
   #  needed by GAIA.
   method make_rtdimage {} {
      set image_ $w_.image
      itk_component add image {
         GaiaImageCtrl $image_ \
            -file $itk_option(-file) \
            -file_change_cmd [code $this configure -file] \
            -file_types $itk_option(-file_types) \
            -usexshm $itk_option(-usexshm) \
            -verbose $itk_option(-verbose) \
            -shm_header $itk_option(-shm_header) \
            -shm_data $itk_option(-shm_data) \
            -min_colors $itk_option(-min_colors) \
            -max_colors $itk_option(-max_colors) \
            -drag_scroll $itk_option(-drag_scroll) \
            -scrollbars $itk_option(-scrollbars) \
            -subsample $itk_option(-subsample) \
            -use_zoom_view $itk_option(-use_zoom_view) \
            -zoom_view_propagate $itk_option(-zoom_view_propagate) \
            -with_zoom_window $itk_option(-with_zoom_window) \
            -dozoom $itk_option(-dozoom) \
            -with_pan_window $itk_option(-with_pan_window) \
            -zoom_factor $itk_option(-zoom_factor) \
            -zoom_width $itk_option(-zoom_width) \
            -zoom_height $itk_option(-zoom_height) \
            -pan_width $itk_option(-pan_width) \
            -pan_height $itk_option(-pan_height) \
            -colorramp_height $itk_option(-colorramp_height) \
            -default_cmap $itk_option(-default_cmap) \
            -default_itt $itk_option(-default_itt) \
            -with_colorramp $itk_option(-with_colorramp) \
            -feedback [code $this feedback] \
            -port $itk_option(-port) \
            -shorthelpwin $this \
            -debug $itk_option(-debug) \
            -float_panel $itk_option(-float_panel) \
            -newimagecmd [code $this cleared] \
            -temporary $itk_option(-temporary) \
            -grid_tag "grid_${this}" \
            -grid_command [code $this maybe_draw_grid_] \
       	    -with_warp 1 \
    	    -panel_layout $itk_option(-panel_layout) \
	    -regioncommand [code $this select_region] \
            -component $itk_option(-component) \
	    -min_scale $itk_option(-min_scale) \
	    -max_scale $itk_option(-max_scale) \
	    -pickobjectorient $itk_option(-pickobjectorient)
      }

	# keep a list of gaia (skycat) instances
	global ::skycat_images
	lappend skycat_images $itk_component(image)
   }


   #  Add a menubutton with the GAIA options.
   method add_gaia_menu {} {
      global ::env

      set m [add_menubutton Image-Analysis]

      add_short_help $itk_component(menubar).image-analysis \
         {Image analysis menu: do astronomy with image}

      add_menuitem $m cascade "Aperture photometry" \
         {Perform aperture photometry on image} \
         -menu [menu $m.photom]

      add_menuitem $m.photom command "Results in magnitudes..." \
         {Display aperture photometry toolbox (results in magnitudes)} \
         -command [code $this make_photom_toolbox 1] \
         -accelerator {Control-m}

      add_menuitem $m.photom command "Results in data counts..." \
         {Display aperture photometry toolbox (results in image data units)} \
         -command [code $this make_photom_toolbox 0] \
         -accelerator {Control-c}

      # check if the required Starlink "convert" package is installed
      if {[info exists env(NDF_FORMATS_IN)]} {
	  set state normal
      } else {
  	  set state disabled
	  catch {puts "Warning: The Starlink \"convert\" package does not appear to be installed.\n\
                 Some operations will not be available."}
      }

      add_menuitem $m command "Image regions..." \
         {Perform operations on regions of image} \
         -command [code $this make_ard_toolbox] \
	 -state $state \
         -accelerator {Control-r} \

      add_menuitem $m command "Patch image..." \
         {Realistically replace parts of image} \
         -command [code $this make_patch_toolbox] \
         -accelerator {Control-i}

      add_menuitem $m command "Blink images..." \
         {Blink compare all the displayed images} \
         -command [code $this blink] \
         -accelerator {Control-b}

      add_menuitem $m command "Overlay axes grid..." \
         {Draw axes over image } \
         -command [code $this make_astgrid_toolbox] \
         -accelerator {Control-g}

      add_menuitem $m cascade "Astrometry calibration" \
         {Create and manipulate astrometry information} \
         -menu [menu $m.astrom]

      add_menuitem $m.astrom command "Fit to star positions..." \
         {Create a WCS for image using reference positions} \
         -command [code $this make_astreference_toolbox] \
         -accelerator {Control-F}

      add_menuitem $m.astrom command "Tweak an existing calibration..." \
         {Use linear transforms to refine the WCS associated with this image} \
         -command [code $this make_astrefine_toolbox] \
         -accelerator {Control-z}

      add_menuitem $m.astrom command "Copy from another image..." \
         {Copy a WCS from another image} \
         -command [code $this make_astcopy_toolbox] \
         -accelerator {Control-y}

      add_menuitem $m.astrom command "Type in known calibration..." \
         {Define a WCS for image using FITS-like description} \
         -command [code $this make_astdefine_toolbox] \
         -accelerator {Control-k}

      add_menuitem $m command "Celestial coordinates...  " \
         {Change the image celestial coordinate system} \
	 -command [code $this make_astsystem_toolbox] \
	 -accelerator {Control-e}
   }

   #  Make a photometry toolbox or make it visible.
   method make_photom_toolbox { usemags } {
      if { [$image_ cget -file] != "" } {
         if { [info exists itk_component(photom${usemags}) ] &&
              [winfo exists $itk_component(photom${usemags}) ] } {
            wm deiconify $itk_component(photom${usemags})
            raise $itk_component(photom${usemags})
         } else {
            busy {
               itk_component add photom${usemags} {
                  StarPhotom $w_.photom\#auto $usemags \
                     -canvasdraw [$image_ component draw] \
                     -canvas [$image_ get_canvas] \
                     -rtdimage [$image_ get_image] \
                     -transient $itk_option(-transient_tools) \
                     -number $clone_
               }
            }
         }
      }
   }

   #  Make an ARD toolbox or make it visible.
   method make_ard_toolbox {} {
      if { [$image_ cget -file] != "" } {
         if { [info exists itk_component(ard) ] &&
              [winfo exists $itk_component(ard) ] } {
            wm deiconify $itk_component(ard)
            raise $itk_component(ard)
         } else {
            busy {
               itk_component add ard {
                  StarArd $w_.ard\#auto \
                     -canvasdraw [$image_ component draw] \
                     -canvas [$image_ get_canvas] \
                     -rtdimage [$image_ get_image] \
                     -gaia $w_ \
                     -transient $itk_option(-transient_tools) \
                     -number $clone_
               }
            }
         }
      }
   }

   #  Make an AST grid toolbox or make it visible.
   method make_astgrid_toolbox {} {
      if { [$image_ cget -file] != "" } {
         if { [info exists itk_component(astgrid) ] &&
              [winfo exists $itk_component(astgrid) ] } {
            wm deiconify $itk_component(astgrid)
            raise $itk_component(astgrid)
         } else {
            busy {
               itk_component add astgrid {
                  StarAstGrid $w_.astgrid\#auto \
                     -canvas [$image_ get_canvas] \
                     -rtdimage [$image_ get_image] \
                     -transient $itk_option(-transient_tools) \
                     -number $clone_
               }
            }
         }
      }
   }

   #  Make an AST reference WCS toolbox or make it visible.
   method make_astreference_toolbox {} {
      if { [$image_ cget -file] != "" } {
         if { [info exists itk_component(astreference) ] &&
              [winfo exists $itk_component(astreference) ] } {
            wm deiconify $itk_component(astreference)
            raise $itk_component(astreference)
         } else {
            busy {
               itk_component add astreference {
                  StarAstReference $w_.astreference\#auto \
                     -image $image_ \
                     -rtdimage [$image_ get_image] \
                     -canvas [$image_ get_canvas] \
                     -transient $itk_option(-transient_tools) \
                     -number $clone_ \
                     -notify_cmd [code $this maybe_draw_grid_ 1]
               }
            }
         }
      }
   }

   #  Make an AST define WCS toolbox or make it visible.
   method make_astdefine_toolbox {} {
      if { [$image_ cget -file] != "" } {
         if { [info exists itk_component(astdefine) ] &&
              [winfo exists $itk_component(astdefine) ] } {
            wm deiconify $itk_component(astdefine)
            raise $itk_component(astdefine)
         } else {
            busy {
               itk_component add astdefine {
                  StarAstDefine $w_.astdefine\#auto \
                     -rtdimage [$image_ get_image] \
                     -transient $itk_option(-transient_tools) \
                     -number $clone_ \
                     -notify_cmd [code $this maybe_draw_grid_ 1]
               }
            }
         }
      }
   }

   #  Make an AST copy WCS toolbox or make it visible.
   method make_astcopy_toolbox {} {
      if { [$image_ cget -file] != "" } {
         if { [info exists itk_component(astcopy) ] &&
              [winfo exists $itk_component(astcopy) ] } {
            wm deiconify $itk_component(astcopy)
            raise $itk_component(astcopy)
         } else {
            busy {
               itk_component add astcopy {
                  StarAstCopy $w_.astcopy\#auto \
                     -rtdimage [$image_ get_image] \
                     -transient $itk_option(-transient_tools) \
                     -number $clone_ \
		     -filter_types $itk_option(-file_types) \
                     -notify_cmd [code $this maybe_draw_grid_ 1]
               }
            }
         }
      }
   }

   #  Make an AST refine WCS toolbox or make it visible.
   method make_astrefine_toolbox {} {
      if { [$image_ cget -file] != "" } {
         if { [info exists itk_component(astrefine) ] &&
              [winfo exists $itk_component(astrefine) ] } {
            wm deiconify $itk_component(astrefine)
            raise $itk_component(astrefine)
         } else {
            busy {
               itk_component add astrefine {
                  StarAstRefine $w_.astrefine\#auto \
                     -image $image_ \
                     -rtdimage [$image_ get_image] \
                     -canvas [$image_ get_canvas] \
                     -transient $itk_option(-transient_tools) \
                     -number $clone_ \
                     -notify_cmd [code $this maybe_draw_grid_ 1]
               }
            }
         }
      }
   }

   #  Make an AST set celestial coordinates system toolbox or make it visible.
   method make_astsystem_toolbox {} {
      if { [$image_ cget -file] != "" } {
         if { [info exists itk_component(astsystem) ] &&
              [winfo exists $itk_component(astsystem) ] } {
            wm deiconify $itk_component(astsystem)
            raise $itk_component(astsystem)
         } else {
            busy {
               itk_component add astsystem {
                  StarAstSystem $w_.astsystem\#auto \
                     -rtdimage [$image_ get_image] \
                     -transient $itk_option(-transient_tools) \
                     -number $clone_ \
                     -notify_cmd [code $this maybe_draw_grid_ 1]
               }
            }
         }
      }
   }

   #  When image is flipped etc. we may want to redraw the grid, if
   #  it is visible (that is not withdrawn). If auto is set 1 then 
   #  the grid is only redrawn if automatic redraws are on.
   protected method maybe_draw_grid_ { {auto 0} } {
      if { [info exists itk_component(astgrid) ] &&
           [winfo exists $itk_component(astgrid) ] } {

         #  Check that window isn't withdrawn
         if { [wm state $itk_component(astgrid)] != "withdrawn" } { 
            $itk_component(astgrid) draw_grid 0 $auto
         }
      }
   }

   #  Make a patch toolbox or make it visible.
   method make_patch_toolbox {} {
      if { [$image_ cget -file] != "" } {
         set create 0
         if { [info exists itk_component(patch) ] &&
              [winfo exists $itk_component(patch)] } {
            wm deiconify $itk_component(patch)
            raise $itk_component(patch)
         } else {
            busy {
               itk_component add patch {
                  StarPatch $w_.patch\#auto \
                     -canvasdraw [$image_ component draw] \
                     -canvas [$image_ get_canvas] \
                     -rtdimage [$image_ get_image] \
                     -transient $itk_option(-transient_tools) \
                     -number $clone_
               }
            }
         }
      }
   }

   #  Blink any displayed images.
   method blink {} {
      if { $clone_ > 1 } {
	  if { [info exists itk_component(blink) ] &&
	       [winfo exists $itk_component(blink) ] } {
             wm deiconify $itk_component(blink)
             raise $itk_component(blink)
	   } else {
              busy {
                 itk_component add blink {
                    StarBlink .\#auto \
                       -transient $itk_option(-transient_tools) \
                       -number $clone_
                 }
              }
	   }
      } else {
         error_dialog "Not enough images are displayed to blink."
      }
   }


   #  Make the "Filters" menu.
   method make_filters_menu {} {
      StarAppFilter \#auto $w_
   }

   #  Open a new file without a filebrowser, or return the name of the
   #  displayed file. Always use an absolute name (for matching etc.)
   method open {args} {
      if {"$args" != ""} {

         if { $itk_option(-file) != "" } {
            #  Already have a displayed image. Check that we do
            #  not need to delete it, before accepting the new one.
            maybe_delete_
            delete_temporary_
         }
         set file [lindex $args 0]
         if {[file isfile $file]} {
            if { [catch {set here [pwd]}] } {
               set here ""
            }
            if { ! [string match {/*} $file] } {
               set file "$here/[file tail $file]"
            }
            configure -file $file

            #  Make sure image exists before using it.
            $image_ configure -file $file

            #  Lower the image on the canvas so that any existing
            #  graphics are revealed.
            [$image_ get_canvas] lower [$image_ get_image]
         } else {
            error_dialog "There is no file named '$file'" $w_
         }
      } else {
         return $itk_option(-file)
      }
   }

   #  Make a new main window, named either the next in sequence
   #  or using a given name.
   method clone {args} {
       global ::argv ::argc
       set argv $args
       set argc [llength $argv]
       # use the -noop option to avoid reloading the main image (part of $argv list)
       after 0 [code TopLevelWidget::start Gaia "-file"]
       return $prefix_[expr $clone_+1]
   }

   #  Image has been cleared so reset any toolboxes that require it
   #  (note most just have canvas objects which are deleted when
   #  a new image is drawn and do not require any other information).
   method cleared {} {
      if { [info exists itk_component(astgrid) ] } {
         if { [winfo exists $itk_component(astgrid) ] } {
            $itk_component(astgrid) remove_grid
         }
      }
      if { [info exists itk_component(astsystem) ] } {
         if { [winfo exists $itk_component(astsystem) ] } {
            $itk_component(astsystem) image_changed
         }
      }

      #  If image has been cleared and this previously displayed a
      #  temporary image then delete it.
      maybe_delete_
      delete_temporary_
   }

   #  Return the name of the GaiaImageCtrl so that other external
   #  routines may talk to it.
   method get_image {} { return $image_ }

  #  Check if any clone is displaying the current image.
  private method only_user_ {} {
	global ::skycat_images
	if {[llength $skycat_images] != 1} {
            foreach w $skycat_images {
		if {! [winfo exists $w]} {
		    continue
		}
		set w [winfo toplevel $w]
		if { [$w open] == "$itk_option(-file)" } {
		    if { $w != $w_ } {
			return 0
		    }
		}
            }
	}
	return 1
  }

   #  Private method to return the temporary status of the
   #  GaiaImageCtrl object (this is the main place for storing
   #  this value, the option in this class is just a convenient
   #  way of setting this).
   private method is_temp_ {} {
      if { [info exists image_] } {
         if { [winfo exists $image_] } {
            return [$image_ cget -temporary]
         } else {
            return 0
         }
      } else {
         return 0
      }
   }

   #  See if user wants to change the temporary status of image
   #  before exit etc.
   private method maybe_delete_ {} {
      if { [is_temp_] } {
         
         #  File is temporary, need to check that no other clone has
         #  an interest in it.
         if { [only_user_] } {
            raise $w_
            set d [DialogWidget .#auto \
                      -title {Temporary image} \
                      -text \
                      "The image displayed in window GAIA/SkyCat ($clone_) \
                       is temporary. Are you sure you want to delete it?" \
                      -buttons [list Yes No]]
            set answer [$d activate]
            if { $answer } {
               configure -temporary 0
               info_dialog "The image is stored in file $itk_option(-file). \
                            You should rename this immediately."
            }
         }
      }
   }
   
   #  Delete the image if temporary.
   private method delete_temporary_ {} {
      if { [is_temp_] } {
         catch { file delete $itk_option(-file) }
         configure -temporary 0
      }
   }

   #  Set the colour of the main canvas background.
   protected method set_background_ {colour} {
      [$image_ get_canvas] configure -background $colour
   }
   
   # -- options and public variables (also program options) --

   #  Mark displayed image as temporary, these are deleted on exit
   #  (after a request to save it is made), try to disable options
   #  database configuring this.
   itk_option define -temporary teMpoRaRy TeMpoRaRy 0 {
      if { [info exists image_] } {
         if { [winfo exists $image_] } {
            $image_ configure -temporary $itk_option(-temporary)
         }
      }
   }
   
   #  Whether toolboxes are transient (iconize with main window).
   itk_option define -transient_tools transient_tools Transient_Tools 0

   #  The known file types.
   itk_option define -file_types file_types File_Types {{any *}}

   #  Whether to reveal the GAIA menu or not.
   itk_option define -gaia gaia Gaia 1

   #  Whether to reveal the filters menu or not.
   itk_option define -filters filters Filters 0

   #  Redefine scrollbars to be true.
   itk_option define -scrollbars scrollbars ScrollBars 1

   #  The NDF component to display.
   itk_option define -component component Component data

   # -- Protected variables --

   #  Clone number of this window.
   protected variable clone_ 0

   #  Initialization progress count.
   protected variable sofar_ 0

   #  Initialization windows.
   protected variable Init_
   protected variable Progress_

   #  Colours of the main background.
   protected variable colours_ {
      white 
      grey90 grey80 grey70 grey60 grey50 grey40 grey30 grey20 grey10         
      black 
      red green blue cyan magenta yellow
   }


   # -- Common variables --

   # prefix to use to create new main windows
   common prefix_ ".gaia"
}
