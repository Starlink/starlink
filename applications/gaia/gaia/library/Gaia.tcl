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
#     of the GAIA package with a Starlink extended Skycat.

#  Authors:
#     PWD: Peter Draper (STARLINK)
#     ALLAN: Allan Brighton (ESO)
#     MBT: Mark Taylor
#     {enter_new_authors_here}

#  Copyright:
#     Copyright (C) 1998-2001 Central Laboratory of the Research Councils
#     Copyright (C) 2006-2007 Particle Physics & Astronomy Research Council.
#     Copyright (C) 2007-2014 Science and Technology Facilities Council.
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

#  Inherits:
#     Methods and configuration options of SkyCat (and Rtd).

#  History:
#     24-SEP-1997 (PWD):
#        Original version
#     10-MAR-1998 (PWD):
#        Clone method now accepts a file name and additional options.
#     06-APR-1998 (PWD):
#        Added demo toolbox.
#     07-APR-1998 (PWD):
#        Moved temporary code to GaiaImageCtrl.
#     09-APR-1998 (PWD):
#        Changed clone method to not use TopLevelWidget::start.
#        This simplifies passing on new options and makes it
#        possible to wait for the window to appear (start blocks
#        with a tkwait which means that it is impossible to
#        work out when the clone is running).
#     10-JUL-1998 (PWD):
#        Added changes to support cloning of toolboxes.
#     10-SEP-1998 (PWD):
#        Added SExtractor toolbox.
#     10-MAR-1999 (PWD):
#        Attempt merge of Allan's GAIA plugin differences...
#     01-MAY-1999 (PWD):
#        Added contouring toolbox.
#     28-MAY-1999 (PWD):
#        Added optimal photometry toolbox.
#     28-JUN-1999 (PWD):
#        Added ramp printing changes, hidden development code for now.
#     22-NOV-1999 (PWD):
#        Added focus_follows_mouse option to stop funny effects
#        with click-to-focus + autoraise under CDE.
#     06-DEC-1999 (PWD):
#        Added Norman Gray's ESP toolbox. Commented out as not ready.
#     05-MAY-2000 (PWD):
#        Changed so that CATLIB_CONFIG is used in preference to
#        all other configuration files when set (otherwise need
#        to delete ~/.skycat/skycat.cfg before can use another
#        configuration file).
#     12-MAY-2000 (PWD):
#        Added positions toolbox.
#     18-JUL-2000 (PWD):
#        Added XY profiles toolbox.
#     22-MAR-2001 (PWD):
#        Added Polarimetry toolbox.
#        Revealed ramp printing option.
#     23-JUL-2001 (PWD):
#        Added UKIRT quick look option.
#     21-JUL-2003 (PWD):
#        Added support for the new tabbed interface.
#     13-OCT-2004 (PWD):
#        Added cube display toolbox and associated changes.
#     05-MAY-2005 (PWD):
#        Added RICE compression changes.
#     17-NOV-2005 (PWD):
#        Update to Skycat version 2.7.4. No longer need to play with tkwait in
#        noblock_clone.
#     19-JUL-2006 (MBT):
#        Added plastic support.
#     23-JUL-2008 (PWD):
#        Start adding VO support.
#     09-JUN-2009 (PWD):
#        Add mask toolbox.
#     24-JUN-2009 (MBT):
#        Replace PLASTIC support with SAMP support.
#     {enter_changes_here}

#-

#  Version.

#  Make a local copy of about_skycat so we can divert bug reports.
set about_skycat ""
set about_gaia "
Starlink GAIA version $gaia_version

Copyright (C) 1997-2005 Central Laboratory of the Research Councils
Copyright (C) 2006-2007 Particle Physics and Astronomy Research Council
Copyright (C) 2007-2009 Science and Technology Facilities Council

Authors:
Peter W. Draper (p.w.draper@durham.ac.uk)
Norman Gray (norman@astro.gla.ac.uk)
David S. Berry (dsb@ast.man.ac.uk)
Mark Taylor (m.b.taylor@bristol.ac.uk)

GAIA is derived from SkyCat version $skycat_version
Copyright (C) 1996-2008 ESO - European Southern Observatory

Authors:
Allan Brighton (abrighto@eso.org)
Thomas Herlin (therlin@eso.org)
Miguel Albrecht (malbrech@eso.org)
Daniel Durand (durand@dao.nrc.ca)
Peter Biereichel (pbiereic@eso.org)
"

#  Set the modification to about for the UKIRT quick look facility.
set about_ukirt_ql {
A modified GAIA for UKIRT quick look display

by Min Tan, Alan Bridger, Alan Pickup, Len Lawrance at UK ATC

based on:
}

#  Where to send bugs.
set gaia_bugs {
Bug reports and suggestions to: starlink@jiscmail.ac.uk
}
set ukirt_ql_bugs {
Bug reports and suggestions to: ab@roe.ac.uk
}

set gaia_usage {
Usage: gaia ?NDF/fitsFile? ?-option value ...?

Options:
 -always_merge <bool>     - always merge primary & extension headers (MEFs).
 -autofit <bool>          - scale new image to fit window (keeps zoom).
 -autoscale <bool>        - scale image to fit window (disables zoom).
 -cat <bool>              - include ESO/Archive catalog extensions (default).
 -catalog  "<c1> <c2> .." - open windows for the given catalogs on startup.
 -check_for_cubes <bool>  - Check input files to see if they are cubes (default: 1) \\
 -cupidcat catalog        - open catalog as CUPID output (requires cube) \\
 -colorramp_height <n>    - height of colorramp window (default: 12).
 -component <component>   - NDF component to display (one of: data, variance)
 -debug <bool>            - debug flag: run bg processes in fg.
 -deep_search <bool>      - search NDF extensions for related NDFs
 -default_cmap <cmap>     - default colormap.
 -default_itt <itt>       - default intensity transfer table.
 -extended_precision      - show extra readout precision (default: 0).
 -file <file>             - image file to load.
 -float_panel <bool>      - put info panel in a popup window (default: 0).
 -focus_follows_mouse <bool> - entry focus follows mouse (default: 0).
 -force_degrees <bool>    - force the display of degrees in main window (default: 0)
 -geometry <wxh+x+y>      - geometry of the main window (default: last session).
 -hdu <n>                 - HDU to display (default: 1)
 -ident <string>          - identifying string that will be prefixed to the window title.
 -interop_menu <bool>     - reveal interop menu for SAMP interactions (default: 1).
 -isize <n>               - search box for centroiding (default: 9).
 -linear_cartesian <bool> - assuming CAR projections are a linear mapping (default: 1).
 -max_scale <n>           - maximum scale for magnification menu (default: 20).
 -maxshift <n.5>          - maximum shift when centroiding (default: 5.5).
 -min_scale <n>           - minimum scale for magnification menu (default: -10).
 -panel_layout <layout>   - panel layout, one of: "saoimage", "reverse" or "default" .
 -panel_orient <orient>   - panel orientation, one of: "horizontal", "vertical"
 -pick_zoom_factor <n>    - scale factor used in pick object zoom window.
 -pickobjectorient <v>    - orientation for pick object win: "horizontal", "vertical"
 -pixel_indices <bool>    - show NDF pixel indices as X,Y readout values (default: 0).
 -port <port>             - listen for remote cmds on port (default: 0 = choose port).
 -remote <bool>           - use existing skycat process, if available, with Tk send.
 -rtd <bool>              - include ESO/VLT Real-Time Features.
 -rtd_autocut <bool>      - whether to apply the default autocut to realtime images.
 -scrollbars <bool>       - display scrollbars (not displayed by default).
 -shm_data <bool>         - put image data in sysV shared memory.
 -shm_header <bool>       - put image header in sysV shared memory.
 -transient_spectralplot <bool>  - spectral plot is a transient window. (default: 1)
 -transient_tools <bool>  - toolboxes are transient windows. (default: 0)
 -ukirt_ql <bool>         - show UKIRT Quick Look Facilities (default: 0).
 -ukirt_xy <bool>         - show XY profile UKIRT Quick Look Facilities (default: 0).
 -use_zoom_view <bool>    - use a "view" of the image for the zoom window (default).
 -usexshm <bool>          - use X shared mem, if available (default).
 -verbose <bool>          - print diagnostic messages.
 -visual <visual_id>      - X visual to use (pseudocolor, truecolor, visual id...)
 -with_colorramp <bool>   - display the color bar (default).
 -with_pan_window <bool>  - display the pan window (default).
 -with_warp <bool>        - add bindings to move mouse ptr with arrow keys (default: 1).
 -with_zoom_window <bool> - display the zoom window (default).
 -zoom_factor <n>         - scale factor for zoom window (default: 4).
}

itk::usual Gaia {}

#  Create a class for the application.
itcl::class gaia::Gaia {
   inherit ::skycat::SkyCat

   #  Constructor: create a toplevel window.
   constructor {args} {

      #  And start things going.
      configure -center 0

      #  Remove any options we're overriding and evaluate all
      #  options.
      itk_option remove rtd::Rtd::scrollbars
      itk_option remove rtd::Rtd::panel_layout
      itk_option remove rtd::Rtd::panel_orient

      eval itk_initialize $args

      #  Set the default application icon.
      global ::gaia_library
      set default_icon [image create photo .gaia_logo -format gif \
                           -file $::gaia_library/gaia_small_logo.gif]
      wm iconphoto . -default $default_icon

      #  Override about_skycat message.
      global ::about_skycat ::about_gaia ::gaia_bugs
      global ::about_ukirt_ql ::ukirt_ql_bugs
      if { ! $itk_option(-ukirt_ql) } {
         set about_skycat "$about_gaia $gaia_bugs"
      } else {
         set about_skycat "$about_ukirt_ql $about_gaia $ukirt_ql_bugs"
      }

      #  Make sure our HelpWin class is used by TopLevelWidget.
      set util::TopLevelWidget::help_window_class gaia::HelpWin
   }

   #  Destructor:
   destructor {

      #  If this is the final remaining instance of this class, do some
      #  application-wide clear up to clear any current registration with a
      #  SAMP hub.
      if {"[itcl::find objects -class gaia::Gaia]" == "$this"} {

         #  Inform the SAMP hub that we are ceasing operations.
         stop_samp_
      }

      #  Delete temporary files used by SAMP components.
      catch {
         if { $samp_sender_ != {} } {
            $samp_sender_ destructor
         }
      }
      catch {
         if { $samp_agent_ != {} } {
            $samp_agent_ destructor
         }
      }

      #  Clear up the images list (this isn't done correctly in
      #  SkyCat, it uses $w_ instead of $image_).
      global ::skycat_images
      if {[info exists skycat_images]} {
         set tmp {}
         foreach w $skycat_images {
            if {[winfo exists $w] && "$w" != "$image_"} {
               lappend tmp $w
            }
         }
         set skycat_images $tmp
      }

      #  Kill the importer dialog, if used.
      if { [info exists $w_.importer] && [winfo exists $w_.importer] } {
         catch {delete object $w_.importer}
      }

      #  The FITS browser needs to release any temporary files
      #  used for storing in-line compressed images.
      catch {gaia::GaiaHduBrowser::release_temporary_files}
   }

   # Restore the position of the top level window from the previous
   # session, or not depending on mode and whether an explicit value
   # has been given on the command-line (0x0 resets to default size).
   #  Overridden from Skycat.
   protected method load_toplevel_geometry {} {
      if { $itk_option(-tabbedgaia) } {
         return
      }
      if { [info exists ::geometry] } {
         if { $::geometry != "0x0" } {
            catch {wm geometry $w_ $::geometry}
         }
      } else {
         if {[catch {set fd [::open $toplevel_geometry_]}]} {
            return
         }
         catch {wm geometry $w_ [gets $fd]}
         ::close $fd
      }
   }

   #  Quit the application. Really....
   #  If being paranoid then ask for confirmation. Note I think this
   #  traps all ways of exiting that can be handled.
   public method quit {} {
      if { ! $itk_option(-quiet_exit) } {
         if { ! [confirm_dialog \
                    "Are you sure you want to exit the application?" $w_]} {
            return
         }
      }

      #  Permission supplied so continue with exit.
      catch {
         release_vtk_
      }
      delete object $w_
      after idle exit
   }

   #  Delete this object. Invoke the on_close_cmd if set.
   public method delete_window {} {
      delete object $w_
      if { $itk_option(-on_close_cmd) != {} } {
         catch {
            eval $itk_option(-on_close_cmd)
         }
      }
   }

   #  VTK requires that some objects are released before the associated
   #  windows are destroyed, so any method that deletes this object should
   #  call this method.
   protected method release_vtk_ {} {
      if { [::namespace exists "::gaia3d"] } {
         gaia3d::Gaia3dVtk::release_all
      }
   }

   #  Called after the options have been evaluated. Add GAIA menu and
   #  extra items for other menus.
   public method init {} {

      #  If not first window then show splash screen (better than no feedback).
      if { $itk_option(-number) != 1 } {
         make_init_window 1
      }

      #  Do base class inits. Note stop creation of cat menu and opening
      #  of catalogues from the command-line so we can override use of
      #  SkySearch class.
      set curval $itk_option(-cat)
      set curcats $itk_option(-catalog)
      set itk_option(-cat) 0
      set itk_option(-catalog) {}
      skycat::SkyCat::init
      set itk_option(-cat) $curval
      set itk_option(-catalog) $curcats

      #  Get the clone number for this window.
      set clone_ $itk_option(-number)

      #  On openwindows iconwindows are displayed but do not
      #  redirect events, so add a fake deiconify binding.
      if {$itk_option(-disp_image_icon)} {
         bind $itk_component(icon) <Double-1> "wm deiconify $w_"
      }

      #  Add the GAIA menubar.
      feedback "GAIA toolboxes..."
      if { $itk_option(-gaia) } {
         add_gaia_menu
      }

      #  Add the filters menu if required (not used at present).
      feedback "filters..."
      if { $itk_option(-filters) } {
         make_filters_menu
      }

      #  Add the catalogue menu.
      if {$itk_option(-cat)} {
         cat::AstroCat::add_catalog_menu \
            $w_ [code $image_] ::gaia::GaiaSearch $itk_option(-debug)
      }

      #  Open any local catalogues.
      if { $itk_option(-catalog) != {} } {
         #  Make sure we use full path name for local catalogs
         #  and process option as a list
         foreach f "$itk_option(-catalog)" {
            if { [file exists $f] && "[string index $f 0]" != "/" } {
               set $f [pwd]/$f
            }
            cat::AstroCat::open_catalog_window $f \
                   [code $image_] ::gaia::GaiaSearch $itk_option(-debug) $w_
         }
      }

      #  Add the SAMP menu.
      if {$itk_option(-interop_menu)} {
         add_interop_menu
      }

      #  Add the SkyCat graphics features (really a plugin, but we're
      #  not using these yet).
      add_graphics_features $w_

      #  And the other changes to menus that we require.
      make_menu_changes

      #  Load user cmaps.
      if { [::file exists $::env(HOME)/.skycat/colormaps] } {
         set files [glob -nocomplain -directory $::env(HOME)/.skycat/colormaps "*.lasc"]
         if { $files != {} } {
            foreach file $files {
               $image_ cmap file $file
            }
         }
      }

      #  Attempt to register as SAMP listener, adding callbacks to be
      #  informed when the status of the SAMP connection changes.
      if {$itk_option(-interop_menu)} {
         init_samp_
         if { $samp_client_ != "" } {
            $samp_client_ reg_change_command [code $this samp_reg_changed_]
            samp_reg_changed_
            set tracker [$samp_client_ cget -client_tracker]
            {*}$tracker client_change_command [code $this samp_client_changed_]
            samp_client_changed_
         }
      }

      #  Center image first time.
      after 0 [code $image_ center]

      #  Check if any post display tasks are required.
      after 0 [code $this file_loaded_]

      #  If autoscaling, need to wait for realization the first time.
      if { $itk_option(-autofill) } {
         after 0 [code $this configure -autofill 1]
      } elseif { $itk_option(-autoscale) } {
         after 0 [code $this configure -autoscale 1]
      } elseif { $itk_option(-autofit) } {
         after 0 [code $this configure -autofit 1]
      }


      #  Start the internal debug logging, if required.
      start_debuglog_

      #  Set the blank and background colours for the first time.
      set_blankcolour
      set_image_background

      #  Trap window closing and handle that.
      wm protocol $w_ WM_DELETE_WINDOW [code $this close]

   }

   #  Set/get X defaults - can be overridden in subclass and/or
   #  in user's .Xdefaults file.
   protected method setXdefaults {} {
      util::setXdefaults
      skycat::SkyCat::setXdefaults
      gaia::setXdefaults
   }


   #  Start or stop the debug log. If the debuglog value is greater than 1
   #  then buffering is switched off so that the updates are immediate, useful
   #  when dealing with an aborting process that might miss output. If logging
   #  is already active new output is appended.
   protected method start_debuglog_ {} {
      if { $itk_option(-debuglog) } {

         #  If already started the log, we append new output.
         if { $debug_started_ } {
            set mode "a"
         } else {
            set mode "w"
         }
         set fid [::open "GaiaDebug.log" $mode]
         if { $itk_option(-debuglog) > 1 } {
            fconfigure $fid -buffering none
         }
         cmdtrace on notruncate $fid
         set debug_started_ 1
      } else {
         if { $debug_started_ } {
            cmdtrace off
         }
      }
   }

   #  Display a window while the application is starting up, overriden
   #  to remove skycat logo and add plain option for showing
   #  minimalist stuff when creating a clone.
   protected method make_init_window {{plain 0}} {

      global ::about_skycat ::gaia_library
      if { $itk_option(-tabbedgaia) } {

         #  If tabbedgaia then use a simple component not a window.
         set w [frame $w_.init]
         place $w  -relx 0.5 -rely 0.5 -anchor s
      } else {
         set w [util::TopLevelWidget $w_.init -center 1 -cursor watch]
         wm title $w "$appname_ loading..."
         wm withdraw $w_
      }
      if { ! $plain } {
         set gaia_logo [image create photo -file $gaia_library/gaia_logo.xpm]
         pack \
            [label $w.logo -image $gaia_logo -borderwidth 2 -relief groove] \
            -side top -padx 1m -pady 1m
         pack \
            [message $w.msg -text $about_skycat \
                -justify center \
                -borderwidth 2 -relief groove] \
            [util::ProgressBar $w.progress \
                -from 0 -to 10 -value 0 \
                -borderwidth 2 -relief groove] \
            -side top -fill x -padx 1m -pady 2m -expand 1
      } else {
         pack \
            [util::ProgressBar $w.progress \
                -from 0 -to 10 -value 0 \
                -borderwidth 2 -relief groove] \
            -side top -fill x -padx 1m -pady 2m -expand 1
      }

      if { ! $itk_option(-tabbedgaia) } {
         ::tkwait visibility $w
      }
   }

   #  Add help for GAIA and SkyCat. Gets called a lot from base classes, so
   #  make sure done just once.
   public method add_help_menu {} {
      if { ! $help_menu_done_ } {
         set help_menu_done_ 1
         set m [add_help_button index "Help..." \
                   {Display the main help window and index}]

         add_menuitem $m command "About ${appname_}..." \
            {Display a window with information about this GAIA/SkyCat version}\
            -command [code $itk_component(image) about]

         add_menuitem $m command "About SkyCat..." \
            {Display information about SkyCat in browser} \
            -command [code $itk_component(image) send_to_browser \
                         $itk_option(-help_url)]

         add_short_help $itk_component(menubar).help \
            {Help menu: display information about this application}
      }
   }

   #  Create the rtd image widget with the extended RTD functionality
   #  needed by GAIA.
   public method make_rtdimage {} {
      set image_ $w_.image
      itk_component add image {
         gaia::GaiaImageCtrl $image_ \
            -file_change_cmd [code $this file_loaded_] \
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
            -ast_tag $ast_tag_ \
            -grid_command [code $this redraw_specials_] \
            -with_warp 1 \
            -panel_layout $itk_option(-panel_layout) \
            -panel_orient $itk_option(-panel_orient) \
            -regioncommand [code $this select_region] \
            -component $itk_option(-component) \
            -min_scale $itk_option(-min_scale) \
            -max_scale $itk_option(-max_scale) \
            -pickobjectorient $itk_option(-pickobjectorient) \
            -pick_zoom_factor $itk_option(-pick_zoom_factor) \
            -hdu $itk_option(-hdu) \
            -ukirt_ql $itk_option(-ukirt_ql) \
            -ukirt_xy $itk_option(-ukirt_xy) \
            -pixel_indices $itk_option(-pixel_indices) \
            -appname $appname_ \
            -extended_precision $itk_option(-extended_precision) \
            -linear_cartesian $itk_option(-linear_cartesian) \
            -force_degrees $itk_option(-force_degrees) \
            -always_merge $itk_option(-always_merge) \
            -show_hdu_chooser $itk_option(-show_hdu_chooser) \
            -default_cut $itk_option(-default_cut) \
            -ident $itk_option(-ident) \
            -deep_search $itk_option(-deep_search) \
            -unicoderadec $itk_option(-unicoderadec)
      }

      #  Keep a list of SkyCat/GAIA instances.
      global ::skycat_images
      lappend skycat_images $itk_component(image)

      #  Some modes require realtime events immediately (not just when
      #  the toolboxes are ready).
      if { $itk_option(-ukirt_xy) || $itk_option(-rtd_autocut) } {
         $image_ configure -real_time_command [code $this real_time_event_]
      }
   }

   #  Make changes to Skycat menus that we require.
   public method make_menu_changes {} {

      #  Note bindings are not really needed, unless working with
      #  plugin (GAIA version of TopLevelWidget is fixed).

      #  File menu. Change to use local opening dialog (not the Ctrl version,
      #  that is inefficient for cubes, these go straight to the toolbox),
      #  also needs the bindings changing to work with the keyboard shortcuts
      #  and the "save region" removing.
      set m [get_menu File]
      $m entryconfigure "Open..." -command [code $this open_file]
      bind $w_  <Control-o> [code $image_ open]
      bind $w_  <Control-v> [code $this reopen]
      bind $w_  <Control-s> [code $image_ save_as]
      catch {$m delete "Save region as..."}

      #  Make reopen work locally so we can release cubes.
      $m entryconfigure "Reopen" -command [code $this reopen]

      #  Insert cube entry.
      set index [$m index "Reopen"]
      insert_menuitem $m $index command "Open cube..." \
         {Open a cube and display image sections from it} \
         -command [code $this make_opencube_toolbox]

      #  Close also needs the command changing to delete the object.
      $m entryconfigure "Close" \
         -label "Close Window" \
         -accelerator {Control-d} \
         -command [code $this delete_window]
      bind $w_  <Control-d> [code $this delete_window]
      add_menu_short_help $m "Close Window" \
         {Close this window, exit application if last}
      add_menu_short_help $m "New Window" \
         {Create a new main window}

      #  If this is the tabbedgaia instance, exit is controlled
      #  elsewhere.
      if { $itk_option(-tabbedgaia) } {
         $m delete {Exit}
      }

      #  Add window for configuring the startup options. Put this just
      #  before the "Clear" item.
      set index [$m index "Clear"]
      insert_menuitem $m $index command "Startup options..." \
         {Set startup-level configuration options} \
         -command [code $this make_toolbox startup 0 1]

      #  Print becomes cascade menu with options for image and colourramp.
      set index [$m index "Print..."]
      catch {$m delete "Print..."}
      insert_menuitem $m $index cascade "Print..." \
         {Print image or colour ramp to postscript file or printer} \
         -menu [menu $m.print]
      add_menuitem $m.print command "Image..." \
         {Print image and graphics to postscript file or printer} \
         -command [code $image_ print] \
         -accelerator {Control-p}
      bind $w_ <Control-p> [code $image_ print]
      add_menuitem $m.print command "Ramp..." \
         {Print annotated postscript copy of colour ramp to file or printer} \
         -command [code $this print_ramp_]

      #  Capture main window to a graphics format.
      insert_menuitem $m $index command "Visible snapshot..." \
         {Capture visible image to a graphic format: GIF, JPEG, PNG, TIFF} \
         -command [code $image_ capture 0]
      incr index
      insert_menuitem $m $index command "Full snapshot..." \
         {Capture image, including offscreen parts, to a graphic format:
            GIF, JPEG, PNG, TIFF (can be slow for large images)} \
         -command [code $image_ capture 1]

      #  Shortcuts for closing window.
      bind $w_  <Control-n> [code $this clone]
      bind $w_  <Control-q> [code $this quit]

      #  View menu. Add new items, rename "Cuts..." to Slice, add
      #  bindings for accelerators.
      set m [get_menu View]
      bind $w_  <Control-c> [code $image_ set_colors]
      bind $w_  <Control-l> [code $image_ component info cut_level_dialog]
      $m entryconfigure "Cuts..." -label "Slice..." -accelerator {Control-a}
      bind $w_  <Control-a> [code $image_ spectrum 0]
      $m entryconfigure "Pick Object..." -accelerator {Control-i}
      bind $w_  <Control-i> [code $image_ pick_dialog]
      bind $w_  <Control-f> [code $image_ view_fits_header]

      #  Add item to show any AST warnings about the header associated
      #  with the image.
      set index [$m index "Fits header..."]
      incr index
      insert_menuitem $m $index command "Astrometry warnings..." \
         {See any problems found with current astrometry headers} \
         -command [code $image_ display_astwarn]

      #  Locate "Auto scale" item and change to activate our local
      #  method.
      set index [$m index "Auto scale"]
      $m entryconfigure $index -command [code $this autoscale_]

      #  Append experimental auto fill autoscale variant.
      incr index
      insert_menuitem $m $index checkbutton "Auto fill*" \
         {Scale the image to fit the window in both dimensions (experimental)} \
         -variable [scope itk_option(-autofill)] \
         -onvalue 1 -offvalue 0 \
         -command [code $this autofill_ 1]

      #  Auto fit to complement autoscale.
      incr index
      insert_menuitem $m $index checkbutton "Auto fit" \
         {Scale the image to the max. visible size when loaded} \
         -variable [scope itk_option(-autofit)] \
         -onvalue 1 -offvalue 0 \
         -command [code $this autofit_]


      #  HDUs are for NDFs too.
      $m entryconfigure "Select FITS HDU..." \
         -label "Select FITS HDU/NDF..."
      add_menu_short_help $m "Select FITS HDU/NDF..." \
         {Display the available FITS HDUs and NDFs}

      #  Change background and blank pixel colours.
      set index [$m index "Magnification"]
      incr index
      insert_menuitem $m $index cascade "Blank pixel color" \
         {Change the colour of blank pixels} \
         -menu [menu $m.blank]
      foreach colour $colours_ {
         $m.blank add radiobutton \
            -background $colour \
            -variable [scope itk_option(-blank_color)] \
            -value $colour \
            -label {    } \
            -command [code $this set_blankcolour $colour]
      }

      insert_menuitem $m $index cascade "Image background" \
         {Change the background colour of the main window} \
         -menu [menu $m.back]
      foreach colour $colours_ {
         $m.back add radiobutton \
            -background $colour \
            -variable [scope itk_option(-image_background)] \
            -value $colour \
            -label {    } \
            -command [code $this set_image_background $colour]
      }

      #  UKIRT quick look likes to attach to camera immediately.
      if { $itk_option(-ukirt_ql) || $itk_option(-ukirt_xy) } {
         attach_camera
      }

      #  Toggle debug logging. Only show in developer mode.
      if { $itk_option(-developer) } {
         $m add separator
         add_menuitem $m checkbutton "Debug logging" \
            {Switch debug logging to GaiaDebug.log on and off} \
            -variable [scope itk_option(-debuglog)] \
            -onvalue 1 -offvalue 0 \
            -command [code $this start_debuglog_]
      }
   }

   #  Add a menubutton with the GAIA options.
   public method add_gaia_menu {} {

      set toolmenu_ [add_menubutton Image-Analysis]
      set m $toolmenu_
      configure_menubutton Image-Analysis -underline 0
      add_short_help $itk_component(menubar).image-analysis \
         {Image analysis menu: do astronomy with image}

      add_menuitem $m cascade "Aperture photometry" \
         {Perform aperture photometry on image} \
         -menu [menu $m.photom]

      add_menuitem $m.photom command "Results in magnitudes..." \
         {Display aperture photometry toolbox (results in magnitudes)} \
         -command [code $this make_toolbox magphotom] \
         -accelerator {Control-m}
      bind $w_ <Control-m> [code $this make_toolbox magphotom]

      add_menuitem $m.photom command "Results in data counts..." \
         {Display aperture photometry toolbox (results in image data units)} \
         -command [code $this make_toolbox countphotom] \
         -accelerator {Control-g}
      bind $w_ <Control-g> [code $this make_toolbox countphotom]

      add_menuitem $m cascade "Optimal photometry" \
         {Perform optimal photometry on image} \
         -menu [menu $m.optphotom]

      add_menuitem $m.optphotom command "Results in magnitudes..." \
         {Display optimal photometry toolbox (results in magnitudes)} \
         -command [code $this make_toolbox magoptphotom]

      add_menuitem $m.optphotom command "Results in data counts..." \
         {Display optimal photometry toolbox (results in image data units)} \
         -command [code $this make_toolbox countoptphotom]

      add_menuitem $m command "Image regions..." \
         {Perform operations on regions of image} \
         -command [code $this make_toolbox ard] \
         -accelerator {Control-r}
      bind $w_ <Control-r> [code $this make_toolbox ard]

      add_menuitem $m command "STC-S regions..." \
         {Draw STC-S regions over image} \
         -command [code $this make_toolbox stcs]

      add_menuitem $m command "MOC outline regions..." \
         {Draw IVOA HEALpix MOC outline over image} \
         -command [code $this make_toolbox moc]

      add_menuitem $m command "Patch image..." \
         {Realistically replace parts of image} \
         -command [code $this make_toolbox patch] \
         -accelerator {Control-u}
      bind $w_ <Control-u> [code $this make_toolbox patch]

      add_menuitem $m command "Blink images..." \
         {Blink compare all the displayed images} \
         -command [code $this make_toolbox blink] \
         -accelerator {Control-b}
      bind $w_ <Control-b> [code $this make_toolbox blink]

      add_menuitem $m command "Overlay axes grid..." \
         {Draw axes over image } \
         -command [code $this make_toolbox astgrid 0 1] \
         -accelerator {Control-t}
      bind $w_ <Control-t> [code $this make_toolbox astgrid 0 1]

      add_menuitem $m cascade "Astrometry calibration" \
         {Create and manipulate astrometry information} \
         -menu [menu $m.astrom]

      if { [info exists ::env(AUTOASTROM_DIR)] } {
         add_menuitem $m.astrom cascade "Automatic position matching" \
            {Create and manipulate astrometry information} \
            -menu [menu $m.astrom.auto]

         add_menuitem $m.astrom.auto command "Simple..." \
            {Create a WCS for image using AUTOASTROM} \
            -command [code $this make_toolbox simpleautoastrom]

         add_menuitem $m.astrom.auto command "Advanced..." \
            {Create a WCS for image using AUTOASTROM} \
            -command [code $this make_toolbox advancedautoastrom]
      }

      add_menuitem $m.astrom command "Fit to star positions..." \
         {Create a WCS for image using reference positions} \
         -command [code $this make_toolbox astreference] \
         -accelerator {Control-k}
      bind $w_ <Control-k> [code $this make_toolbox astreference]

      add_menuitem $m.astrom command "Tweak an existing calibration..." \
         {Use linear transforms to refine the WCS associated with this image} \
         -command [code $this make_toolbox astrefine] \
         -accelerator {Control-z}
      bind $w_ <Control-z> [code $this make_toolbox astrefine]

      add_menuitem $m.astrom command "Copy from another image..." \
         {Copy a WCS from another image} \
         -command [code $this make_toolbox astcopy] \
         -accelerator {Control-y}
      bind $w_ <Control-y> [code $this make_toolbox astcopy]

      add_menuitem $m.astrom command "Type in known calibration..." \
         {Define a WCS for image using FITS-like description} \
         -command [code $this make_toolbox astdefine] \
         -accelerator {Control-w}
      bind $w_ <Control-w> [code $this make_toolbox astdefine]

      add_menuitem $m cascade "Change coordinates" \
         {Change the secondary (alpha/delta) coordinate system} \
         -menu [menu $m.coords]

      add_menuitem $m.coords command "Built-in coordinates..." \
         {Choose a coordinate system } \
         -command [code $this make_toolbox astdomain]

      add_menuitem $m.coords command "Show all coordinates..." \
         {Display coordinates for all known systems} \
         -command [code $this make_toolbox astdisplay]

      add_menuitem $m.coords command "Celestial coordinates...  " \
         {Change the celestial coordinate system} \
         -command [code $this make_toolbox astsystem]

      add_menuitem $m command "Object detection...  " \
         {Automatically detect and parameterize objects} \
         -command [code $this make_toolbox sextractor] \
         -accelerator {Control-j}
      bind $w_ <Control-j> [code $this make_toolbox sextractor]

      add_menuitem $m command "Contouring...  " \
         {Contour this or another image over the displayed image...} \
         -command [code $this make_toolbox contour] \
         -accelerator {Control-h}
      bind $w_ <Control-h> [code $this make_toolbox contour]

      add_menuitem $m command "Surface photometry...  " \
         {Perform interactive galaxy surface photometry} \
         -command [code $this make_toolbox esp] \

      add_menuitem $m cascade "Positions..." \
         {Select or import object positions} \
         -menu [menu $m.positions]

      add_menuitem $m.positions command "Select positions...  " \
         {Record or import object positions and measure properties} \
         -command [code $this make_toolbox positions] \

      add_menuitem $m.positions command "Import plain text file..." \
         {Import a space or fixed width plain text file as a catalogue} \
         -command [code $this import_catalogue_]

      add_menuitem $m.positions command "Import CUPID catalogue..." \
         {Import a CUPID catalogue selecting RA and Dec columns} \
         -command [code $this import_cupid_cat_]

      add_menuitem $m command "Mean X & Y profiles...  " \
         {Show X and Y averaged profiles of a rectangular region} \
         -command [code $this make_toolbox xyprofile 0 1] \
         -accelerator {Control-e}
      bind $w_ <Control-e> [code $this make_xyprofile_toolbox xyprofile 0 0]

      add_menuitem $m command "Histograms of XY region...  " \
         {Show histogram of a rectangular region} \
         -command [code $this make_toolbox xyhistogram 0 1]
      bind $w_ <Control-e> [code $this make_xyhistogram_toolbox xyhistogram 0 0]

      add_menuitem $m command "Polarimetry toolbox... " \
         {Display and manipulate POLPACK vector maps} \
         -command [code $this make_toolbox polarimetry 0 1] \

      add_menuitem $m command "Mask image..." \
         {Display regions using an integer mask as a stencil} \
         -command [code $this make_toolbox mask 0 0]

      if { $itk_option(-demo_mode) } {
         add_menuitem $m command "Demonstration mode..." \
               {See a demonstration of GAIA (needs an empty directory)} \
               -command [code $this make_toolbox demo]
      }
   }

   #  Saving graphics with the image doesn't work so disable it.
   protected method add_graphics_save_menu_item {} {
   }

   #  Make or clone a GAIA toolbox. Allow the toolbox to be created
   #  if no image displayed, only on special request.
   public method make_toolbox {type {clone 0} {noimage 0} } {

      #  Do nothing if no image is displayed, unless allowed.
      if { [$image_ cget -file] != "" || $type == "demo" || $noimage } {
         set basename $type
         if { $clone } {
            #  Request to create a clone (i.e. another) toolbox. Make
            #  extended name for this.
            set basename "$basename[incr tool_clones_]"
         }

         #  If the window exists then just raise it.
         if { [info exists itk_component($basename) ] &&
              [winfo exists $itk_component($basename) ] } {
            wm deiconify $itk_component($basename)
            raise $itk_component($basename)
         } else {
            busy {
               make_${type}_toolbox $basename $clone
            }
         }
      }
   }

   #  Make a magnitude aperture photometry toolbox.
   public method make_magphotom_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaApPhotom $w_.\#auto 1 \
            -canvasdraw [$image_ component draw] \
            -image $image_ \
            -canvas [$image_ get_canvas] \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox magphotom 1]
      }
   }

   #  Make a counts aperture photometry toolbox.
   public method make_countphotom_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaApPhotom $w_.\#auto 0 \
            -canvasdraw [$image_ component draw] \
            -image $image_ \
            -canvas [$image_ get_canvas] \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox countphotom 1]
      }
   }

   #  Make a magnitude optimal photometry toolbox.
   public method make_magoptphotom_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaOptPhotom $w_.\#auto 1 \
            -canvasdraw [$image_ component draw] \
            -image $image_ \
            -canvas [$image_ get_canvas] \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox magoptphotom 1]
      }
   }

   #  Make a counts optimal photometry toolbox.
   public method make_countoptphotom_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaOptPhotom $w_.\#auto 0 \
            -canvasdraw [$image_ component draw] \
            -image $image_ \
            -canvas [$image_ get_canvas] \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox countoptphotom 1]
      }
   }

   #  Make an ARD toolbox.
   public method make_ard_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaArd $w_.\#auto \
            -canvasdraw [$image_ component draw] \
            -image $image_ \
            -canvas [$image_ get_canvas] \
            -rtdimage [$image_ get_image] \
            -gaia $w_ \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox ard 1]
      }
   }

   #  Make an STC-S toolbox.
   public method make_stcs_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaStcs $w_.\#auto \
            -canvasdraw [$image_ component draw] \
            -canvas [$image_ get_canvas] \
            -rtdimage [$image_ get_image]\
            -ast_tag ast_tag_ \
            -image $image_ \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox stcs 1] \
            -really_die $cloned
      }
   }

   #  Make a MOS toolbox.
   public method make_moc_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaMOC $w_.\#auto \
            -canvasdraw [$image_ component draw] \
            -canvas [$image_ get_canvas] \
            -rtdimage [$image_ get_image]\
            -ast_tag ast_tag_ \
            -image $image_ \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox moc 1] \
            -really_die $cloned
      }
   }

   #  Make an AST grid toolbox.
   public method make_astgrid_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaAstGrid $w_.\#auto \
            -canvas [$image_ get_canvas] \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -ast_tag $ast_tag_ \
            -clone_cmd [code $this make_toolbox astgrid 1] \
            -really_die $cloned
      }
   }

   #  Make the simple autoastrom toolbox.
   public method make_simpleautoastrom_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaAutoAstrom $w_.\#auto \
            -expert 0 \
            -rtdimage [$image_ get_image] \
            -image $image_ \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -notify_cmd [code $this redraw_specials_ 1] \
            -clone_cmd [code $this make_toolbox simpleautoastrom 1] \
            -really_die $cloned
      }
   }

   #  Make the advanced autoastrom toolbox.
   public method make_advancedautoastrom_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaAutoAstrom $w_.\#auto \
            -expert 1 \
            -rtdimage [$image_ get_image] \
            -image $image_ \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -notify_cmd [code $this redraw_specials_ 1] \
            -clone_cmd [code $this make_toolbox advancedautoastrom 1] \
            -really_die $cloned
      }
   }

   #  Make an AST reference WCS toolbox.
   public method make_astreference_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaAstReference $w_.\#auto \
            -image $image_ \
            -rtdimage [$image_ get_image] \
            -canvas [$image_ get_canvas] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -notify_cmd [code $this redraw_specials_ 1] \
            -clone_cmd [code $this make_toolbox astreference 1] \
            -really_die $cloned \
            -isize $itk_option(-isize) \
            -maxshift $itk_option(-maxshift)
      }
   }

   #  Make an AST define WCS toolbox.
   public method make_astdefine_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaAstDefine $w_.\#auto \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -notify_cmd [code $this redraw_specials_ 1] \
            -clone_cmd [code $this make_toolbox astdefine 1] \
            -really_die $cloned
      }
   }

   #  Make an AST copy WCS toolbox.
   public method make_astcopy_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaAstCopy $w_.\#auto \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -filter_types $itk_option(-file_types) \
            -notify_cmd [code $this redraw_specials_ 1] \
            -clone_cmd [code $this make_toolbox astcopy 1] \
            -really_die $cloned
      }
   }

   #  Make an AST refine WCS toolbox or make it visible.
   public method make_astrefine_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaAstRefine $w_.\#auto \
            -image $image_ \
            -rtdimage [$image_ get_image] \
            -canvas [$image_ get_canvas] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -notify_cmd [code $this redraw_specials_ 1] \
            -clone_cmd [code $this make_toolbox astrefine 1] \
            -really_die $cloned \
            -isize $itk_option(-isize) \
            -maxshift $itk_option(-maxshift)
      }
   }

   #  Make an AST set celestial coordinates system toolbox.
   public method make_astsystem_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaAstSystem $w_.\#auto \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -notify_cmd [code $this redraw_specials_ 1] \
            -clone_cmd [code $this make_toolbox astsystem 1] \
            -really_die $cloned
      }
   }

   #  Make an AST set secondary coordinates system toolbox.
   public method make_astdomain_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaAstDomain $w_.\#auto \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -notify_cmd [code $this redraw_specials_ 1] \
            -clone_cmd [code $this make_toolbox astdomain 1] \
            -really_die $cloned
      }
   }

   #  Make an AST display all known coordinates readout.
   public method make_astdisplay_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaAstDisplayDomains $w_.\#auto \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox astdisplay 1] \
            -really_die $cloned
      }
   }

   #  Make a patch toolbox.
   public method make_patch_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaPatch $w_.\#auto \
            -canvasdraw [$image_ component draw] \
            -canvas [$image_ get_canvas] \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox patch 1]
      }
   }

   #  Blink any displayed images.
   public method make_blink_toolbox {name {cloned 0}} {
      if { [llength [skycat::SkyCat::get_skycat_images] ] > 1 } {
         itk_component add $name {
            gaia::GaiaBlink $w_.\#auto \
               -transient $itk_option(-transient_tools) \
               -number $clone_ \
               -clone_cmd [code $this make_toolbox blink 1] \
               -usexshm $itk_option(-usexshm) \
               -verbose $itk_option(-verbose) \
               -subsample $itk_option(-subsample)
         }
      } else {
         error_dialog "Not enough images are displayed to blink."
      }
   }

   #  Make a SExtractor toolbox.
   public method make_sextractor_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaSextractor $w_.\#auto \
            -canvasdraw [$image_ component draw] \
            -canvas [$image_ get_canvas] \
            -rtdimage [$image_ get_image]\
            -image $image_ \
            -filter_types $itk_option(-file_types) \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox sextractor 1] \
            -really_die $cloned
      }
   }

   #  Make a contour toolbox.
   public method make_contour_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaContour $w_.\#auto \
            -canvasdraw [$image_ component draw] \
            -canvas [$image_ get_canvas] \
            -rtdimage [$image_ get_image]\
            -ast_tag ast_tag_ \
            -image $image_ \
            -filter_types $itk_option(-file_types) \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox contour 1] \
            -really_die $cloned
      }
   }

   #  Make an ESP toolbox.
   public method make_esp_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaEsp $w_.\#auto \
            -canvasdraw [$image_ component draw] \
            -canvas [$image_ get_canvas] \
            -image $image_ \
            -rtdimage [$image_ get_image]\
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox esp 1] \
            -really_die $cloned
      }
   }

   #  Make positions toolbox.
   public method make_positions_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaPositions $w_.\#auto \
            -image $image_ \
            -rtdimage [$image_ get_image] \
            -canvas [$image_ get_canvas] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox positions 1] \
            -really_die $cloned \
            -maxshift $itk_option(-maxshift) \
            -isize $itk_option(-isize)
      }
   }

   #  Make XY profiles toolbox. Slightly different as need to get
   #  rectangle on canvas first. Note don't need a backing image.
   public method make_xyprofile_toolbox {name {cloned 0} {prompt 1}} {
      if { [$image_ isclear] } {
         warning_dialog "No image is currently loaded" $w_
         return
      }

      #  If the window exists then just raise it. Can happen if control-e is
      #  pressed repeatably.
      if { [info exists itk_component($name)] &&
           [winfo exists $itk_component($name)] } {
         wm deiconify $itk_component($name)
         raise $itk_component($name)
         if { $itk_option(-ukirt_xy) } {
            $itk_component($name) restore
         }
         return
      }

      if { $prompt } {
         if {[action_dialog "Press OK and then drag out a \
                             rectangle over the image with button 1" $w_]} {
            set proceed 1
         } else {
            set proceed 0
         }
      } else {
         set proceed 1
      }
      if { $proceed } {
         [$image_ component draw] set_drawing_mode rectangle \
            [code $this make_xyprofile_ $name $cloned]
      }
   }
   public method make_xyprofile_ {name cloned rect_id x0 y0 x1 y1} {
      itk_component add $name {
         gaia::GaiaXYProfile $w_.\#auto \
            -rtdimage [$image_ get_image] \
            -canvasdraw [$image_ component draw] \
            -canvas [$image_ get_canvas] \
            -transient 1 \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox xyprofile 1] \
            -rect_id $rect_id \
            -ukirt_options $itk_option(-ukirt_xy)
      }

      #  Trap real-time events for this tool.
      $image_ configure -real_time_command [code $this real_time_event_]
   }

   #  Make XY histogram toolbox. Slightly different as need to get
   #  rectangle on canvas first. Note don't need a backing image.
   public method make_xyhistogram_toolbox {name {cloned 0} {prompt 1}} {
      if { [$image_ isclear] } {
         warning_dialog "No image is currently loaded" $w_
         return
      }

      #  If the window exists then just raise it. Can happen if control-e is
      #  pressed repeatably.
      if { [info exists itk_component($name)] &&
           [winfo exists $itk_component($name)] } {
         wm deiconify $itk_component($name)
         raise $itk_component($name)
         if { $itk_option(-ukirt_xy) } {
            $itk_component($name) restore
         }
         return
      }

      if { $prompt } {
         if {[action_dialog "Press OK and then drag out a \
                             rectangle over the image with button 1" $w_]} {
            set proceed 1
         } else {
            set proceed 0
         }
      } else {
         set proceed 1
      }
      if { $proceed } {
         [$image_ component draw] set_drawing_mode rectangle \
            [code $this make_xyhistogram_ $name $cloned]
      }
   }
   public method make_xyhistogram_ {name cloned rect_id x0 y0 x1 y1} {
      itk_component add $name {
         gaia::GaiaXYHistogram $w_.\#auto \
            -rtdimage [$image_ get_image] \
            -canvasdraw [$image_ component draw] \
            -canvas [$image_ get_canvas] \
            -transient 1 \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox xyhistogram 1] \
            -rect_id $rect_id
      }
   }

   #  Make polarimetry toolbox.
   public method make_polarimetry_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaPolarimetry $w_.\#auto \
            -image $image_ \
            -rtdimage [$image_ get_image] \
            -canvasdraw [$image_ component draw] \
            -canvas [$image_ get_canvas] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox polarimetry 1 1] \
            -really_die $cloned
      }
   }

   #  Create the polarimetry toolbox and display a catalogue (intended for us
   #  as a remote control method).
   public method display_polarimetry_catalog {catalog} {
      make_toolbox polarimetry 0 1
      return [$itk_component(polarimetry) opener $catalog]
   }

   #  Create the mask image toolbox.
   public method make_mask_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaMask $w_.\#auto \
            -gaia $w_ \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -filter_types $itk_option(-file_types)
      }
   }

   #  Start the demonstration toolbox.
   public method make_demo_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaDemo $w_.\#auto \
            -gaiamain $w_ \
            -rtdimage [$image_ get_image] \
            -gaiactrl $image_ \
            -canvasdraw [$image_ component draw] \
            -canvas [$image_ get_canvas] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox demo 1] \
            -really_die $cloned
      }
   }

   #  Create the startup options toolbox.
   public method make_startup_toolbox {name {cloned 0}} {
      itk_component add $name {
         gaia::GaiaStartup $w_.\#auto \
            -gaia $w_ \
            -image $image_ \
            -transient $itk_option(-transient_tools) \
            -number $clone_
      }
   }

   #  Create the open cube toolbox. Note this is different from the usual
   #  toolboxes, as it lives in the File menu and requires that certain
   #  actions are taken when the toolbox is reused and there can only be one.
   public method make_opencube_toolbox {} {

      #  If the window exists then just raise it.
      if { [info exists itk_component(opencube) ] &&
           [winfo exists $itk_component(opencube) ] } {

         wm deiconify $itk_component(opencube)
         raise $itk_component(opencube)
         $itk_component(opencube) open
      } else {

         busy {
            itk_component add opencube {
               gaia::GaiaCube $w_.\#auto \
                  -gaia $w_ \
                  -canvas [$image_ get_canvas] \
                  -rtdimage [$image_ get_image] \
                  -transient $itk_option(-transient_tools) \
                  -transient_spectralplot \
                     $itk_option(-transient_spectralplot) \
                  -number $clone_ \
                  -filter_types $itk_option(-file_types)
            }
         }
      }
   }

   #  Return the instance of GaiaCube being used, or the empty string
   #  if the cube toolbox has not been created or is withdrawn.
   public method get_gaia_cube {} {
      if { [info exists itk_component(opencube) ] &&
           [winfo exists $itk_component(opencube) ] } {
         if { [wm state $itk_component(opencube)] != "withdrawn" } {
            return $itk_component(opencube)
         }
      }
      return ""
   }

   #  Notification that a file has been loaded into the GaiaImageCtrl.
   protected method file_loaded_ { {filename {}} } {

      if { $filename != {} } {
         configure -file $filename
      }
      if { $itk_option(-file) == {} } {
         return
      }

      #  Restore the blank colour and background.
      set_blankcolour
      set_image_background

      if { ! $itk_option(-check_for_cubes) } {
         return
      }

      #  See if this is a cube, if so offer to load it using the cube
      #  browser. Cheat bigtime by looking for a NAXIS3 card. This should work
      #  for FITS and NDF cubes.
      set iscube 0
      set rtdimage [$image_ get_image]
      set naxis3 [$rtdimage fits get NAXIS3]
      set naxis4 [$rtdimage fits get NAXIS4]

      #  If this isn't the first HDU then we need the fully qualified name.
      set fullname [$rtdimage fullname]

      #  Load it into cube browser. Note allow trivial cubes with redundant
      #  dimensions 1, or 2, but not 3.
      if {( $naxis4 == {} || $naxis4 == 1 ) && $naxis3 != {} && $naxis3 != 1} {
         open_cube_ $fullname

         #  If a CUPID catalogue is defined, open it.
         if { $itk_option(-cupidcat) != {} } {
            open_cupid_cat_ $itk_option(-cupidcat)
            set itk_option(-cupidcat) {}
         }
      } else {
         #  Make sure toolbox is withdrawn.
         if { [info exists itk_component(opencube)] } {
            $itk_component(opencube) close
         }
      }
   }

   #  When image is flipped etc. we may want to redraw some items that
   #  are too expensive to flip via Tcl commands.  If auto is set 1
   #  then the grid is only redrawn if automatic redraws are on.
   protected method redraw_specials_ { {auto 0} } {
      if { [info exists itk_component(astgrid) ] &&
           [winfo exists $itk_component(astgrid) ] } {

         #  Check that window isn't withdrawn
         if { [wm state $itk_component(astgrid)] != "withdrawn" } {
            $itk_component(astgrid) draw_grid 0 $auto
         }
      }

      if { [info exists itk_component(contour) ] &&
           [winfo exists $itk_component(contour) ] } {

         #  Check that window isn't withdrawn
         if { [wm state $itk_component(contour)] != "withdrawn" } {
            $itk_component(contour) redraw 0
         }
      }

      if { [info exists itk_component(positions) ] &&
           [winfo exists $itk_component(positions) ] } {
         $itk_component(positions) redraw
      }
   }

   #  A real time event has been issued by the camera. Some tools may
   #  want to respond to these. If so do that here.
   protected method real_time_event_ {} {
      if { [info exists itk_component(xyprofile) ] &&
           [winfo exists $itk_component(xyprofile) ] } {
         $itk_component(xyprofile) notify_cmd realtime
      } else {

         #  If this is from UKIRT we need to startup the XY profile.
         if { $itk_option(-ukirt_xy) } {

            #  Create a dummy rectangle on the image, without interaction.
            set draw [$image_ component draw]
            set canvas [$image_ get_canvas]
            set id [$canvas create rectangle 0 0 10 10]
            $canvas itemconfigure $id -fill white -outline grey90 -stipple pat7
            $canvas addtag $draw.objects withtag $id
            $draw add_object_bindings $id
            $draw select_object $id

            #  Now create the toolbox and send the realtime event again.
            make_xyprofile_ xyprofile 0 $id 0 0 0 0
            after 0 [code $this real_time_event_]
         } else {
            #  Some failure so remove the command (only way to trap this),
            #  unless it is required for other purposes.
            if { !$itk_option(-rtd_autocut) } {
               $image_ configure -real_time_command {}
            }
         }
      }

      #  When the rtd_autocut flag is true, we want to update the
      #  the image cuts.
      if { $itk_option(-rtd_autocut) } {
         if { $itk_option(-default_cut) != 100 } {
            [$image_ get_image] autocut -percent $itk_option(-default_cut)
         } else {
            [$image_ get_image] autocut
         }
         [$image_ component info] updateValues
      }
   }

   #  Image has been cleared so reset any toolboxes that require it
   #  (note most just have canvas objects which are deleted when
   #  a new image is drawn and do not require any other information).
   public method cleared {} {
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
      if { [info exists itk_component(astdomain) ] } {
         if { [winfo exists $itk_component(astdomain) ] } {
            $itk_component(astdomain) image_changed
         }
      }
      if { [info exists itk_component(astdisplay) ] } {
         if { [winfo exists $itk_component(astdisplay) ] } {
            $itk_component(astdisplay) image_changed
         }
      }
      if { [info exists itk_component(contour) ] } {
         if { [winfo exists $itk_component(contour) ] } {
            $itk_component(contour) remove_contours
         }
      }

      #  XY toolbox can be persistent.
      if { [info exists itk_component(xyprofile) ] } {
         if { [winfo exists $itk_component(xyprofile) ] } {
            $itk_component(xyprofile) restore
         }
      }

      #  Disable toolbox menus if image is a Compound.
      if { $toolmenu_ != {} } {
         set rtdimage [$image_ get_image]
         set state normal
         if { [$rtdimage iscompound] } {
            set state disabled
         }
         set end [$toolmenu_ index end]
         for {set i 0} {$i < $end} {incr i} {
            catch {$toolmenu_ entryconfigure $i -state $state}
         }
      }
   }

   #  Make the "Filters" menu.
   public method make_filters_menu {} {
      gaia::StarAppFilter \#auto $w_
   }

   #  Open a new file using a filebrowser. The filebrowser will open
   #  images, cubes and tables, and offers the ability to select HDUs
   #  in FITS and NDF containers using a "Browse" button. Replaces the
   #  one in GaiaImageCtrl.
   public method open_file {args} {
      set file [get_file_]
      if { $file != {} } {
         open_image_ $file
      }
   }

   #  Get a file using a suitably configured dialog for images. Also
   #  provides browsing of NDFs and FITS MEFS for HDUs.
   protected method get_file_ {{dir "."} {pattern "*"}} {
      if { ! [info exists itk_component(fileselect)] ||
           ! [winfo exists $itk_component(fileselect)] } {
         itk_component add fileselect {
            util::FileSelect $w_.select \
               -dir $dir \
               -filter $pattern \
               -transient 1 \
               -withdraw 1 \
               -filter_types $itk_option(-file_types) \
               -button_4 "Browse" \
               -cmd_4 [code $this browse_file_ $dir $pattern]
         }
         wm transient $itk_component(fileselect) [winfo toplevel $w_]
      } else {

         #  Now a transient of this window, not one that created it.
         wm transient $itk_component(fileselect) [winfo toplevel $w_]

         #  Also deiconfy and raise in case previous parent is iconised.
         wm deiconify $itk_component(fileselect)
         raise $itk_component(fileselect)
      }
      if {[$itk_component(fileselect) activate]} {
         return [$itk_component(fileselect) get]
      }
   }

   #  Browse the content of the file selected in the dialog.
   #  Use to look for HDUs.
   protected method browse_file_ {dir pattern} {

      #  Release the file selection window.
      set file [$itk_component(fileselect) get]
      if { [::file exists $file] && [::file isfile $file] } {
         wm withdraw $itk_component(fileselect)
         utilReUseWidget gaia::GaiaHduBrowser $w_.browser \
            -file $file \
            -transient 1 \
            -open_cmd [code $this browsed_open_] \
            -cancel_cmd [code $this get_file_ $dir $pattern] \
            -shorthelpwin $itk_option(-shorthelpwin)
      } else {
         #  Ignore, no such file.
         warning_dialog "Not a disk filename ($file)" $w_
         get_file_ $dir $pattern
      }
   }

   #  Handle an open request from the file browser. Could be opening
   #  and image, cube or table.
   protected method browsed_open_ {type name {naxes 0}} {
      if { $type == "image" } {
         #  If a cube, send this to the cube toolbox.
         if { $naxes >= 3 } {
            open_cube_ $name
         } else {
            open_image_ $name
         }
      } elseif { $type == "table" } {
         #  Set the catalog config entry from the $catinfo table if this
         #  isn't just a simple filename (note this still may cause problems
         #  updating the catalog info from the headers).
         if { ! [::file exists $name] } {
            if { [catch "$astrocat_ entry get $name"] } {
               if { "[string index $name 0]" != "/"} {
                  set fname [pwd]/$name
               } else {
                  set fname $name
               }
               $astrocat_ entry add \
                  [list "serv_type local" "long_name $fname" \
                      "short_name $name" "url $fname"]
            }
         }

         #  Display the catalogue.
         gaia::GaiaSearch::new_local_catalog $name $image_ ::gaia::GaiaSearch
      }
   }

   #  Open a known cube in the cube toolbox.
   protected method open_cube_ {name} {
      make_opencube_toolbox
      set msg {}
      set result [catch {$itk_component(opencube) configure -cube $name} msg]
      if { $result != 0 } {
         maybe_release_cube_
         $itk_component(opencube) close
         if { $msg != {} } {
            info_dialog "$msg" $w_
         }
      }
   }

   #  Get the cube toolbox to open a CUPID catalogue.
   protected method open_cupid_cat_ {name} {
      if { [info exists itk_component(opencube)] } {
         $itk_component(opencube) import_cupid_cat $name
      }
   }

   #  Open an image, handling the setting of the HDU number if part
   #  of the specification.
   protected method open_image_ {name} {
      set namer [gaia::GaiaImageName \#auto -imagename $name]
      $image_ configure -hdu [$namer fitshdunum]
      $image_ configure -file [$namer fullname 0]
      ::delete object $namer
   }

   #  Open a new file without a filebrowser, or return the name of the
   #  displayed file. Always use an absolute name (for matching etc.).
   #  FITS extensions are enabled if needed by using the "fullname 0"
   #  switch.
   public method open {args} {

      if { "$args" != "" } {
         set imagename [lindex $args 0]
         set namer [gaia::GaiaImageName \#auto -imagename $imagename]
         if { [$namer exists] } {

            #  Release any cubes before proceeding, otherwise this holds
            #  dataset open when we'd like to reopen if needed here (will
            #  return to file_loaded_ after reading file).
            if { $itk_option(-check_for_cubes) } {
               maybe_release_cube_
            }

            $namer absolute
            set fullname [$namer fullname 0]
            set hdunum [$namer fitshdunum]
            configure -hdu $hdunum
            configure -file $fullname
            $image_ configure -hdu $hdunum
            $image_ configure -file $fullname

            #  Lower the image on the canvas so that any existing
            #  graphics are revealed.
            [$image_ get_canvas] lower [$image_ get_image]
         } else {
            error_dialog "There is no file named '$imagename'" $w_
         }
         delete object $namer
      } else {
         return $itk_option(-file)
      }
   }

   #  Make a new main window with the given name, or the next in
   #  sequence name. This version can stop the TopLevelWidget::start
   #  command from blocking (stopping the possibility of remotely
   #  determining when the clone has been created). It also provides
   #  the ability to specify the file name directly (thus replacing
   #  the command-line version) and to gain access to an existing
   #  clone (by number). Used for demo/remote control. Note that
   #  a clone number of -1 generates a clone number.
   public method noblock_clone {number {file ""} {block 0} args} {
      global ::argv ::argc ::gaia_usage

      #  Append any new args.
      set argv [concat $argv $args]
      set argc [llength $argv]

      #  If given the file replaces the one in the command-line args or
      #  is added to the list.
      if { $file != "" } {
         replace_image_ $file
      }

      #  If a clone number was given construct the related name.
      if { $number != "" } {
         if { $number == -1 } {
            set number [expr $clone_cnt_ + 1]
         }
         set name ".gaia$number"
      } else {
         set name ""
      }

      #  If named window already exists, just configure args and file
      #  and return.
      if { [winfo exists $name] } {
          if { $args != "" } {
              eval $name configure $args
          }
          if { $file != "" } {
              $name open $file
          }
          return $name
      }

      #  If name is "" then create a new name.
      if { $name == "" } {
         set name "$prefix_[expr $clone_cnt_+1]"
         while { [winfo exists $name] } {
            set name "$prefix_[incr clone_cnt_]"
         }
      }

      #  Start a new clone, do not wait for application to exit, when not
      #  blocking.
      util::TopLevelWidget::start gaia::Gaia "-file" "$gaia_usage" \
         "$name" $block

      #  Actually we only arrive here if not blocking, except when
      #  application is exiting, so clone number is wrong.
      if { ! $block } {
         tkwait visibility $name
         if { $number != {} } {

            #  Number given, so update the title and make sure next
            #  clone doesn't try to have same name.
            $name configure -number $number
            $name.image update_title
            catch {set clone_cnt_ [max $clone_cnt_ $number]}
         }
      }
      return $name
   }

   #  Make a window clone, but display a new image.
   public method newimage_clone {filename args} {
      global ::argv ::argc ::gaia_usage
      if { $args != {} } {
         set argv [concat $argv $args]
         set argc [llength $argv]
      }

      #  Add the image name to replace the existing one.
      replace_image_ $filename

      #  And create the new clone.
      after 0 [code util::TopLevelWidget::start gaia::Gaia "-file" "$gaia_usage" ]
      return $prefix_[expr $clone_cnt_+1]
   }

   #  Standard clone method. Make a new main window, named either the
   #  next in sequence or using a given name.
   public method clone {args} {
      global ::argv ::argc ::gaia_usage
      if { $args != {} } {
         set argv [concat $argv $args]
         set argc [llength $argv]
      }

      #  Use the -noop option to avoid reloading the main image (part
      #  of $argv list).
      after 0 [code util::TopLevelWidget::start gaia::Gaia "-noop" "$gaia_usage"]
      return $prefix_[expr $clone_cnt_+1]
   }

   #  Replace the existing image in the default args lists.
   protected method replace_image_ {filename} {
      global ::argv ::argc
      set index [lsearch -exact $argv "-file"]
      if { $index == -1 } {

         #  Filename may be unpaired with -file, so look for it.
         set newargv ""
         set seenfile 0
         for {set i 0} {$i < $argc} {incr i} {
            set opt [lindex $argv $i]
            if {"[string index $opt 0]" == "-" && "$opt" != "-"} {
               set arg [lindex $argv [incr i]]
            } else {
               set seenfile 1
               set arg "$filename"
               set opt "-file"
            }
            lappend newargv $opt $arg
         }
         if { ! $seenfile } {

            #  No -file and no unpaired options.
            lappend newargv "-file" "$filename"
         }
         set argv $newargv
         set argc [llength $argv]
      } else {

         #  Has "-file" so just replace associated value.
         incr index
         set argv [lreplace $argv $index $index $filename]
      }
   }

   #  Re-open the current file. Remaps the data and informs any toolboxes
   #  with direct access (cube).
   public method reopen {} {
      lassign [maybe_release_cube_] cube_open cube_name

      #  Don't open cube slices, they have just been released.
      if { ! [[$image_ get_image] volatile] } {
         $image_ reopen
      } else {
         #  Instead fully re-open the cube.
         if { [info exists itk_component(opencube)] } {
            $itk_component(opencube) configure -cube $cube_name
         } else {
            #  No cube toolbox, so a volatile image.
            $image_ reopen
         }
      }
   }

   #  If a cube is currently loaded release it. Returns if release
   #  happened and the name of the cube.
   protected method maybe_release_cube_ {} {
      set cube_open 0
      set cube_name {}
      if { [info exists itk_component(opencube) ] &&
           [winfo exists $itk_component(opencube) ] } {
         set cube_name [$itk_component(opencube) cget -cube]
         set cube_open [$itk_component(opencube) release]
         $itk_component(opencube) halt
      }
      return [list $cube_open $cube_name]
   }

   #  Return the name of the GaiaImageCtrl so that other external
   #  routines may talk to it.
   public method get_image {} { return $image_ }

   #  Set the colour of the main canvas background.
   public method set_image_background {{colour {}}} {
      if { $colour == {} } {
         set colour $itk_option(-image_background)
      }
      [$image_ get_canvas] configure -background $colour
   }

   #  Set the colour of the any blank pixels.
   public method set_blankcolour {{colour {}}} {
      if { $colour == {} } {
         set colour $itk_option(-blank_color)
      }
      [$image_ get_image] blankcolor $colour
   }

   #  Start a process to load the ESO config file. Need this in case
   #  local version becomes horribly out of date.
   public method load_eso_config {} {

      #  Attempt to get the default config file using a batch process
      #  to avoid nasty blocking.
      util::Batch $w_.bg_proc -command [code $this loaded_eso_config_]
      blt::busy hold $w_
      $w_.bg_proc bg_eval [code $this get_eso_config_]

      #  Query times out after 10 seconds.
      set after_id_ [after 10000 [code $this loaded_eso_config_ 1 "timed out"]]
   }

   #  Start the application with the above class as the main window.
   #  This proc is called from tkAppInit.c when we are running the single
   #  binary version.
   #
   #  Note that the binary version doesn't need to set auto_path or look for
   #  Tcl sources or colormaps at run-time, since they are already loaded in
   #  the binary.
   public proc startGaia {} {
      global ::gaia_usage ::tk_strictMotif ::tcl_precision \
         ::argv0 ::argv ::argc ::env

      #  Where to look for catalog config file:
      #    use CATLIB_CONFIG, if set (assume this is deliberate)
      #    next use ~/.skycat/skycat.cfg if it exists (this contains
      #    the user's preferences), finally use $SKYCAT_CONFIG if set
      #    (note native implementation ignores SKYCAT_CONFIG as this
      #    may be set by CURSA, which is bad).
      if { ! [info exists env(CATLIB_CONFIG)] } {

         #  Make sure ~/.skycat exists.
         set config_file [utilGetConfigFilename .skycat skycat.cfg]
         if {[file exists $config_file]} {
            set env(CATLIB_CONFIG) "file:$config_file"
            check_config_file $config_file
         } elseif {[info exists env(SKYCAT_CONFIG)]  && ! $native} {
            set env(CATLIB_CONFIG) $env(SKYCAT_CONFIG)
         } else {
            copy_default_config_file_ $config_file
            set env(CATLIB_CONFIG) "file:$config_file"
         }
      }

      #  Initialise any proxy server.
      cat::AstroCat::check_proxies

      #  Set some application options
      tk appname GAIA
      set tk_strictMotif 1
      set tcl_precision 15

      #  Insert some default options
      set argv [linsert $argv 0 -disp_image_icon 1]
      set argc [llength $argv]

      #  Start the application
      util::TopLevelWidget::start gaia::Gaia "-file" "$gaia_usage"
   }

   #  Copy the default config file to another file. If the target file
   #  already exists a backup copy is made.
   protected proc copy_default_config_file_ { config_file } {
      global ::gaia_library
      if { [file exists $gaia_library/skycat2.0.cfg] } {
         set backupname ""
         set today ""
         if { [file exists $config_file] } {
            set today [clock format [clock seconds] -format "%d-%b-%Y"]
            set backupname ${config_file}_${today}
            file copy -force $config_file ${backupname}
         }
         file copy -force $gaia_library/skycat2.0.cfg $config_file

         #  Make a directory entry that accesses the old configs.
         if { $backupname != "" } {
            ::astrocat tmpcat
            tmpcat load ${backupname} "Configuration of $today"
            ::cat::CatalogInfo::save {} {}
            destroy tmpcat
         }
      }
   }

   #  If user has a local config file then this may need to be updated
   #  from time to time as features are added to the default file.
   #  The match string should be set to something new in the
   #  default file.
   public proc check_config_file { config_file } {
      set newmatch {*APASS*}

      #  Search the file for the string match.
      set ok 0
      set fileid [::open $config_file "r"]
      while { [gets $fileid line] >= 0 } {
         if { [string match $newmatch $line] } {
            set ok 1
            break;
         }
      }
      ::close $fileid
      if { !$ok } {
         set msg \
            "Your local catalogue configuration file '$config_file'
is out of date. Do you want to update it?"
         set choice [choice_dialog $msg {OK Details Cancel} {OK}]

         if { $choice == "Details" } {
            set detailsmsg \
               "The local catalogue configuration file '$config_file'
contains a description of catalogues that are shown in the
Data-Servers menus. It appears that this file is now out of date with
respect to the system default version \(which may contain new
catalogues and image servers\) and you should probably allow it to be
updated.
When you open local catalogues of your own, or have ones created for
you locally \(the object detection toolbox does this\), or apply
configuration changes \(such as changing the colour of the overlay
markers\) these preferences are recorded in this configuration
file. Since you may not want to loose these changes a copy of your
existing configuration file will be made \(stamped with todays date\)
and added as a directory to the list of catalogue directories before
applying the update. Using the \"Browse Catalog Directories...\"
window gives you access to this."
            info_dialog $detailsmsg

            # After details re-choose.
            set choice [choice_dialog $msg {OK Cancel} {OK}]
         }
         if { $choice == "OK" } {
            copy_default_config_file_ $config_file
         }
      }
   }

   #  Retrieve the ESO config file, returning its content as the result.
   protected method get_eso_config_ {} {
      return [gaiautils::urlget $itk_option(-eso_config_file)]
   }

   #  Invoked an attempt to load ESO config file is completed. If
   #  successful overwrite the local ~/.skycat/skycat.cfg file and
   #  force a local reload.
   protected method loaded_eso_config_ {status msg} {
      blt::busy release $w_
      if { $status } {
         error_dialog "Failed to get ESO config file: $msg"
      } else {

         #  msg is file contents, need to make this the local file.
         global ::env
         set file $env(HOME)/.skycat/skycat.cfg
         if {[file exists $file]} {
            if {[catch {::file rename -force $file $file.BAK} error]} {
               error_dialog $error
               return
            }
         }
         set fd [::open $file w]
         puts $fd $msg
         ::close $fd

         #  Finally force this to be loaded.
         cat::AstroCat::reload_config_file $w_
      }
      delete object $w_.bg_proc
      if { $after_id_ != {} } {
         catch [after cancel $after_id_]
         set after_id_ {}
      }
   }

   #  Print a copy of the colorramp to postscript.
   private method print_ramp_ {} {
      utilReUseWidget gaia::GaiaRampPrint $w_.printramp \
         -image $image_.colorramp.image \
         -show_footer 0 \
         -whole_canvas 1 \
         -transient 1 \
         -mainimage [$image_ get_image] \
         -maincanvas [$image_ get_canvas]
   }

   #  Import a text file as a catalogue.
   protected method import_catalogue_ {} {
      #  Start import dialog. The output file is fixed and the user
      #  chooses the input file. The format of the output file is a
      #  TAB table.
      utilReUseWidget gaia::GaiaTextImport $w_.importer \
         -title "Import text file to catalogue ($itk_option(-number))" \
         -format tab \
         -show_infile 1 \
         -show_outfile 1

      #  Wait for import to complete and get the catalogue name.
      lassign [$w_.importer activate] outfile
      if { $outfile != {} && [file exists $outfile] } {
         cat::AstroCat::open_catalog_window \
            $outfile \
            [code $image_] \
            ::gaia::GaiaSearch \
            0 $this
      }
   }

   #  Import a CUPID catalogue.
   protected method import_cupid_cat_ {} {
      utilReUseWidget gaia::GaiaCupidImporter $w_.cupidimp \
         -title "Import CUPID catalogue ($itk_option(-number))" \
         -gaia $this
   }

   #  Apply the autoscale value. Need to also manage autofill, which
   #  requires autoscale to be true.
   protected method autoscale_ {} {
      global ::$w_.autoscale
      if { ! [set $w_.autoscale] } {
         if { $itk_option(-autofill) } {
            configure -autofill 0
            autofill_ 0
         }
      }
      $image_ autoscale $w_.autoscale
   }

   #  Apply the autofit value.
   protected method autofit_ {} {
      $image_ configure -autofit $itk_option(-autofit)
   }

   #  Apply the autofill value, also make sure autoscale is set or unset
   #  as needed.
   protected method autofill_ {autoscale} {
      if { $autoscale } {
         global ::$w_.autoscale
         if { $itk_option(-autofill) } {
            if { ! [set $w_.autoscale] } {
               set $w_.autoscale 1
               autoscale_
            }
         } else {
            if { [set $w_.autoscale] } {
               set $w_.autoscale 0
               autoscale_
            }
         }
      }
      $image_ configure -autofill $itk_option(-autofill)
   }

   #  Set the location of the history file.
   public proc set_history_catalog {catalog} {
      gaia::GaiaSearch::set_history_catalog "$catalog"
      gaia::GaiaHistory::set_history_catalog "$catalog"
      gaia::GaiaCubeHistory::set_history_catalog "$catalog"
   }

   #  SAMP support
   #  ------------

   #  Add a menubutton for SAMP activities.
   public method add_interop_menu {} {

      set interopmenu_ [add_menubutton Interop]
      set m $interopmenu_
      configure_menubutton Interop -underline 6
      add_short_help $itk_component(menubar).interop \
         {Interop menu: control application interoperability using SAMP}

      add_menuitem $m command "Register" \
         {Register GAIA with a running SAMP hub} \
         -command [code start_samp_]
      add_menuitem $m command "Unregister" \
         {Unregister GAIA with the SAMP hub} \
         -command [code stop_samp_]

      $m add separator

      set samp_send_image_menu_ [menu $m.send_image]
      add_menuitem $m command "Broadcast Image" \
         {Send the current image to all SAMP-registered applications} \
         -command [code $this samp_send_image_ {}]
      add_menuitem $m cascade "Send Image" \
         {Send the current image to a selected SAMP-registered application} \
         -menu $samp_send_image_menu_
   }

   #  Sends the currently displayed image via SAMP to other subscribed
   #  clients.  If no recipient is specified, it will be broadcast to all.
   public method samp_send_image_ {recipient_id} {
      if {[catch {
         {*}$samp_sender_ send_image $image_ $recipient_id
      } msg]} {
         error_dialog "$msg"
      }
   }

   #  Ensures that a SAMP client is in place for this class.
   #  If no client currently exists, create one.  If a hub is
   #  apparently running, try connecting to it.
   #  We could in principle have multiple SAMP clients, one for each
   #  GAIA window, but for now use a common one for the whole application.
   protected proc init_samp_ {} {
      if { $samp_client_ == "" } {

         #  Construct and configure the client object.
         set meta(samp.name) "gaia"
         set meta(samp.description.text) \
                "Graphical Astronomy and Image Analysis tool"
         set meta(samp.documentation.url) \
                "http://astro.dur.ac.uk/~pdraper/gaia/gaia.html"
         set meta(samp.icon.url) \
                "http://astro.dur.ac.uk/~pdraper/gaia/gaiaicon.gif"
         set samp_agent_ [gaia::GaiaSampAgent \#auto]
         lappend agents [code $samp_agent_]
         lappend agents [code [samp::UtilAgent \#auto]]
       # lappend agents [code [samp::TestAgent \#auto]]
         set client [samp::SampClient \#auto -agents $agents \
                                             -metadata [array get meta]]

         #  If a hub appears to be running, have a go at registering with it.
         if { [samp::SampHub::is_hub_running] } {
            if { [catch {
               $client register
            } msg] } {
               puts stderr "Failed to register with a SAMP hub: $msg"
            }
         }

         #  Construct a sender object which works in tandem with the
         #  SampClient to make outgoing calls.
         set samp_sender_ \
             [code [gaia::SampSender \#auto -samp_client [code $client]]]

         #  Store the client object in a common variable.
         set samp_client_ $client
      }
   }

   #  Attempts to ensure that we are connected to a SAMP hub.
   protected proc start_samp_ {} {
      if { $samp_client_ == "" || ! [$samp_client_ is_registered] } {
         if {[catch {
            $samp_client_ register
         } msg]} {
            info_dialog "Failed to register with a SAMP hub: \n$msg"
         }
      }
   }

   #  Attempts to terminate any existing connection with a SAMP hub.
   protected proc stop_samp_ {} {
      if {$samp_client_ != "" && [$samp_client_ is_registered]} {
         $samp_client_ unregister
      }
   }

   #  Invoked when we register or unregister with the SAMP hub.
   protected method samp_reg_changed_ {} {
      if {$samp_client_ != ""} {
         set is_reg [$samp_client_ is_registered]
      } else {
         set is_reg 0
      }
      set when_reg [expr {$is_reg ? "normal" : "disabled"}]
      set when_unreg [expr {$is_reg ? "disabled" : "normal"}]
      $interopmenu_ entryconfigure Register -state $when_unreg
      $interopmenu_ entryconfigure Unregister -state $when_reg
      $interopmenu_ entryconfigure {Broadcast Image} -state $when_reg
      $interopmenu_ entryconfigure {Send Image} -state $when_reg
   }

   #  Invoked when the state (registration or configuration) of some
   #  other client in the hub changes.
   protected method samp_client_changed_ {} {

      #  Rebuild the Send Image submenu so that it contains an up-to-date
      #  list of all the applications that are prepared to receive images.
      set m $samp_send_image_menu_
      $m delete 0 last
      if {[$samp_client_ is_registered]} {
         set send_mtype "image.load.fits"
         set tracker [$samp_client_ cget -client_tracker]
         foreach client_id [{*}$tracker get_subscribed_clients $send_mtype] {
            set client_name [{*}$tracker get_name $client_id]
            add_menuitem $m command "Send to $client_name" \
               "Send the current image to $client_id" \
               -command "$this samp_send_image_ $client_id"
         }
      }
   }

   #  Position the point of interest graphics marker. Used by remote
   #  applications. The ra and dec should be qualified by a units string,
   #  this should be "wcs equinox", "deg equinox", "image" etc. as
   #  required by the rtdimage convert command. The default equinox is J2000.
   public method position_of_interest {ra dec units} {

      #  If this is the same position remove marker.
      if { "$ra,$dec" == $last_position_of_interest_ } {
         if { $position_of_interest_ != {} } {
            set canvas [$image_ get_canvas]
            $canvas delete $position_of_interest_
            set position_of_interest_ {}
         }
         set last_position_of_interest_ {}

      } else {
         set canvas [$image_ get_canvas]
         set image [$image_ get_image]
         lassign [$image scale] xs ys
         if { $position_of_interest_ == {} } {
            set position_of_interest_ [$canvas create rtd_mark 0 0 \
                                          -type circle -scale $xs -fixscale 0 \
                                          -minscale 1 -size 11 \
                                          -outline "green"]
         }

         #  Transform to canvas coordinates and move the marker to that
         #  position.
         if { ! [catch {$image convert coords $ra $dec $units \
                           cx cy canvas} msg ] } {
            #  Check for AST__BAD returns.
            if { [expr abs($cx)] < 1.0E20 } {
               $canvas coords $position_of_interest_ $cx $cy
               set last_position_of_interest_ "$ra,$dec"

               #  Make sure the position is visible, cannot succeed when the
               #  image is zoomed and the position is off image.
               set dw [$image dispwidth]
               set dh [$image dispheight]
               set cw [winfo width $canvas]
               set ch [winfo height $canvas]
               if { $cw != 1 && $dw && $dh } {
                  set px [expr ($cx+0.0)/$dw]
                  set py [expr ($cy+0.0)/$dh]
                  set xrange [$canvas xview]
                  set yrange [$canvas yview]

                  #  Only move if the position is not currently visible, and
                  #  the image is larger than the window in at least one
                  #  dimension.
                  if { $dw > $cw || $dh > $ch } {
                     if { $px < [lindex $xrange 0] || $px > [lindex $xrange 1] ||
                          $py < [lindex $yrange 0] || $py > [lindex $yrange 1] } {
                        $canvas xview moveto [expr (($cx-$cw/2.0)/$dw)]
                        $canvas yview moveto [expr (($cy-$ch/2.0)/$dh)]
                     }
                  }
               }
            } else {
               error "Failed to set interest position: bad coordinates"
            }
         } else {
            error "Failed to set interest position: $msg"
         }
      }
   }

   #  Returns the SAMP sender object, if there is one.
   public proc get_samp_sender {} {
      return [code $samp_sender_]
   }

   #  Returns the SAMP client object, if there is one.
   public proc get_samp_client {} {
      return [code $samp_client_]
   }

   #  VO support
   #  ----------

   #  Open a dialog for querying SIAP services for any images. There
   #  are two flavours, one queries a list of servers for images and
   #  the other allows each server to be queried individual.
   public method vo_siap_query {use_list} {
      if { [gaia::GaiaVOTableAccess::check_for_gaiavo] } {

         #  Find and open the current list of servers.
         set siap_file [vo_config_file_ GaiaSIAPServers.vot]

         #  Open the appropriate dialog.
         if { $use_list } {
            utilReUseWidget gaiavo::GaiaVOCatsSIAP $w_.siapquery \
               -siap_catalog $siap_file \
               -gaia [scope $this] \
               -title "Query current SIAP list" \
               -help_file siap \
               -blacklist [gaiavo::GaiaVOBlacklist::get_instance SIAP]
         } else {
            utilReUseWidget gaiavo::GaiaVOCatRegistry $w_.siapregistry \
               -catalog $siap_file \
               -service SIAP \
               -title "SIAP searches" \
               -show_cols {shortName title} \
               -activate_cmd [code $this vo_query_siap_] \
               -whole_operation 0 \
               -help_file siapregistry \
               -blacklist [gaiavo::GaiaVOBlacklist::get_instance SIAP]
         }
      } else {
         error_dialog "No GAIA-VO extension is available" $w_
      }
   }

   #  Open a dialog for querying a SIAP server.
   protected method vo_query_siap_ {headers row} {
      if { [gaia::GaiaVOTableAccess::check_for_gaiavo] } {

         #  See if the given headers and row data specify a SIAP server.
         #  Need a accessURL field for that.
         set accessURL [gaiavo::GaiaVOCat::getAccessURL $headers $row]
         if { $accessURL != {} } {
            set name [gaiavo::GaiaVOCat::getName $headers $row]
            set id [gaiavo::GaiaVOCat::getIdentifier $headers $row]
            gaiavo::GaiaVOCatSIAP $w_.siapquery\#auto \
               -accessURL $accessURL \
               -identifier $id \
               -shortname $name \
               -gaia $this \
               -title "$name SIAP server" \
               -help_file siapserver \
               -blacklist [gaiavo::GaiaVOBlacklist::get_instance SIAP] \
               -blacklist_cmd [code $w_.siapregistry update_content]
         } else {
            warning_dialog "SIAP service does not specify an accessURL" $w_
         }
      }
   }

   #  Open a dialog for querying Cone Search services for any catalogues.
   public method vo_find_cone {} {
      if { [gaia::GaiaVOTableAccess::check_for_gaiavo] } {

         #  Find and open the current list of servers.
         set cone_file [vo_config_file_ GaiaConeServers.vot]

         utilReUseWidget gaiavo::GaiaVOCatRegistry $w_.voregistrycone \
            -catalog $cone_file \
            -service CONE \
            -title "Catalogue Cone search" \
            -show_cols {shortName title} \
            -activate_cmd [code $this vo_query_cone_] \
            -whole_operation 0 \
            -help_file cone \
            -blacklist [gaiavo::GaiaVOBlacklist::get_instance Cone]
      } else {
         error_dialog "No GAIA-VO extension is available" $w_
      }
   }

   #  Open a dialog for querying a Cone Search server.
   protected method vo_query_cone_ {headers row} {
      if { [gaia::GaiaVOTableAccess::check_for_gaiavo] } {

         #  See if the given headers and row data specify a Cone
         #  server. Need a accessURL field for that.
         set accessURL [gaiavo::GaiaVOCat::getAccessURL $headers $row]
         if { $accessURL != {} } {
            set name [gaiavo::GaiaVOCat::getName $headers $row]
            gaiavo::GaiaVOCatCone $w_.conequery\#auto \
               -accessURL $accessURL \
               -shortname $name \
               -gaia $this \
               -title "$name Cone Search service" \
               -help_file conesearch
         } else {
            warning_dialog "Cone service does not specify an accessURL" $w_
         }
      }
   }

   #  Open a dialog for querying TAP services.
   public method vo_find_tap {} {
      if { [gaia::GaiaVOTableAccess::check_for_gaiavo] } {

         #  Find and open the current list of servers.
         set tap_file [vo_config_file_ GaiaTAPServers.vot]

         utilReUseWidget gaiavo::GaiaVOCatRegistry $w_.voregistrytap \
            -catalog $tap_file \
            -service TAP \
            -title "TAP server queries" \
            -activate_cmd [code $this vo_query_tap_] \
            -whole_operation 0 \
            -help_file tap
      } else {
         error_dialog "No GAIA-VO extension is available" $w_
      }
   }

   #  Open a dialog for querying a TAP server.
   protected method vo_query_tap_ {headers row} {
      if { [gaia::GaiaVOTableAccess::check_for_gaiavo] } {

         #  See if the given headers and row data specify a TAP
         #  service. Need a URL resource for that.
         set accessURL [gaiavo::GaiaVOCat::getAccessURL $headers $row]
         if { $accessURL != {} } {
            set name [gaiavo::GaiaVOCat::getName $headers $row]
            gaiavo::GaiaVOTAP $w_.tap\#auto \
               -accessURL $accessURL \
               -shortname $name \
               -gaia $this \
               -title "$name TAP service" \
               -help_file tapquery
         } else {
            warning_dialog "TAP service does not specify a resource URL" $w_
         }
      }
   }

   #  Get a cached configuration file. If not present use builtin list.
   protected method vo_config_file_ {name} {
      set config_file [utilGetConfigFilename .skycat $name]
      if { ! [::file exists $config_file] } {
         #  Use builtin defaults. If any.
         if { [::file exists $::gaiavo_library/$name ] } {
            ::file copy -force $::gaiavo_library/$name $config_file
         } else {
            puts stderr "Warning: no $name configuation file located"
         }
      }
      return $config_file
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Is this controlled from the tabbed interface?
   itk_option define -tabbedgaia tabbedgaia TabbedGaia 0

   #  Command invoked when window is closed.
   itk_option define -on_close_cmd on_close_cmd On_Close_Cmd {}

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

   #  Whether the spectral plot is transient (iconize with main window).
   itk_option define -transient_spectralplot transient_spectralplot \
              Transient_SpectralPlot 1

   #  The known file types.
   itk_option define -file_types file_types File_Types {{any *}}

   #  Whether to reveal the GAIA menu or not.
   itk_option define -gaia gaia Gaia 1

   #  Whether to reveal the filters menu or not.
   itk_option define -filters filters Filters 0

   #  Whether to reveal the interoperability menu or not.
   itk_option define -interop_menu interop_menu Interop_Menu 1

   #  Redefine scrollbars to be true.
   itk_option define -scrollbars scrollbars Scrollbars 1

   #  The NDF component to display.
   itk_option define -component component Component data {
      if { [info exists image_] } {
         if { [winfo exists $image_] } {
            $image_ configure -component $itk_option(-component)
         }
      }
   }

   #  Switch on demo mode (this makes an additional class available
   #  for running GAIA and making it show off/test some of its
   #  capabilities).
   itk_option define -demo_mode demo_mode Demo_Mode 1

   #  Name of ESO catalogue config file.
   itk_option define -eso_config_file eso_config_file Eso_config_file \
      {http://archive.eso.org/skycat/skycat2.0.cfg}

   #  Redefine panel_layout to GAIA default
   itk_option define -panel_layout panel_layout Panel_layout reverse

   #  Redefine panel_orient to GAIA default
   itk_option define -panel_orient panel_orient Panel_orient horizontal

   #  Set focus following policy (can only set once, then stuck with it).
   itk_option define -focus_follows_mouse focus_follows_mouse \
      Focus_Follows_Mouse 0 {
         if { $itk_option(-focus_follows_mouse) } {
            tk_focusFollowsMouse
         }
      }

   #  Option to inhibit the display of the exit application dialog.
   #  UKIRT requested this for 14,000 ft anoxia cases!
   itk_option define -quiet_exit quiet_exit Quiet_Exit 1

   #  Set the HDU that is displayed initially.
   itk_option define -hdu hdu Hdu 0

   #  Whether to enable the UKIRT quick look parts of the interface.
   itk_option define -ukirt_ql ukirt_ql UKIRT_QL 0 {
      if { $itk_option(-ukirt_ql) } {
         configure -rtd 1
         configure -subsample 0
         set appname_ "UKIRT::Quick Look"
      }
   }

   #  Whether to enable the UKIRT quick look xy profile changes of the
   #  interface. These are later developments to the ATC ones. Not
   #  sure if there is a relationship or not.
   itk_option define -ukirt_xy ukirt_xy UKIRT_XY 0 {
      if { $itk_option(-ukirt_xy) } {
         configure -rtd 1
         configure -subsample 0
         set appname_ "UKIRT::Quick Look"
      }
   }

   #  Whether to display pixel indices as the readout X,Y values.
   itk_option define -pixel_indices pixel_indices Pixel_Indices 0

   #  Whether to display coordinates using extended precision. This
   #  displays at milli arc-second resolution.
   itk_option define -extended_precision extended_precision \
      Extended_Precision 0

   #  Whether CAR projections should be interpreted as a linear mapping.
   itk_option define -linear_cartesian linear_cartesian Linear_Cartesian 1

   #  Whether to force the display of degrees (instead of HMS).
   itk_option define -force_degrees force_degrees Force_Degrees 0

   #  Whether primary headers should always be merged with extension.
   itk_option define -always_merge always_merge Always_Merge 0

   #  Whether to attempt to show and control the HDU chooser. If 0
   #  then control is only attempted when the HDU already exists.
   itk_option define -show_hdu_chooser show_hdu_chooser Show_Hdu_Chooser 1

   #  The default percentage cut used for new files.
   itk_option define -default_cut default_cut Default_Cut 100.0

   #  Check any images that are opened to see if they are cubes.
   itk_option define -check_for_cubes check_for_cubes Check_For_Cubes 1

   #  Open a CUPID catalogue if accompanied by a cube.
   itk_option define -cupidcat cupidcat CupidCat {}

   #  Whether to autocut realtime images to the default cut.
   itk_option define -rtd_autocut rtd_autocut Rtd_AutoCut 0

   #  Search box size when centroiding.
   itk_option define -isize isize Isize 9 {
      if { [info exists itk_component(astreference)] } {
         $itk_component(astreference) configure -isize $itk_option(-isize)
      }
      if { [info exists itk_component(astrefine)] } {
         $itk_component(astrefine) configure -isize $itk_option(-isize)
      }
      if { [info exists itk_component(positions)] } {
         $itk_component(positions) configure -isize $itk_option(-isize)
      }
   }

   #  Maximum shift when centroiding.
   itk_option define -maxshift maxshift Maxshift 5.5 {
      if { [info exists itk_component(astreference)] } {
         $itk_component(astreference) configure -maxshift $itk_option(-maxshift)
      }
      if { [info exists itk_component(astrefine)] } {
         $itk_component(astrefine) configure -maxshift $itk_option(-maxshift)
      }
      if { [info exists itk_component(positions)] } {
         $itk_component(positions) configure -maxshift $itk_option(-maxshift)
      }
   }

   #  Autoscale images to fit main window (keeps aspect ratio disables zoom).
   #  Messy as need to use variable of the menu item that controls this
   #  option. Also needs to be applied after image_ is realized first time.
   itk_option define -autoscale autoscale AutoScale 0 {
       global ::$w_.autoscale
       set $w_.autoscale $itk_option(-autoscale)
       if { [info exists image_] } {
          $image_ autoscale $w_.autoscale
       }
   }

   #  Autofit new images to fit main window. Like autoscale except only
   #  applies when image is first loaded and zoom remain enabled.
   itk_option define -autofit autofit AutoFit 0 {
       if { [info exists image_] } {
          autofit_
       }
   }

   #  Autofill new images to fit main window. Like autoscale except
   #  independent scaling in the two axes. Dangerous as not all
   #  graphics items etc. will function correctly.
   itk_option define -autofill autofill AutoFill 0 {
       if { [info exists image_] } {
          autofill_ 1
       }
   }

   #  A font used for labels.
   itk_option define -labelfont labelfont LabelFont TkDefaultFont {
      set ::gaia_fonts(labelfont) $itk_option(-labelfont)
   }

   #  A font used for fixed width text.
   itk_option define -textfont textfont TextFont TkFixedFont {
      set ::gaia_fonts(textfont) $itk_option(-textfont)
   }

   #  A font used for labels that require symbols (alpha & delta).
   itk_option define -wcsfont wcsfont WcsFont TkDefaultFont {
      set ::gaia_fonts(wcsfont) $itk_option(-wcsfont)
   }

   #  Zoom factor used in the pick object window.
   itk_option define -pick_zoom_factor pick_zoom_factor Pick_Zoom_Factor 10

   #  Additional text for title bar (expected use is identifying amongst
   # instances).
   itk_option define -ident ident Ident {} {
       if { [info exists image_] } {
          $image_ configure -ident $itk_option(-ident)
          $image_ update_title
       }
   }

   #  Colour for blank pixels. Usually black.
   itk_option define -blank_color blank_color Blank_Colour black

   #  Colour for image background. Usually black.
   itk_option define -image_background image_background Image_Background black

   #  Font scale factor. See tk scaling command.
   itk_option define -font_scale font_scale Font_Scale 0.0 {
      if { $itk_option(-font_scale) > 0.0 } {
         tk scaling -displayof $w_ $itk_option(-font_scale)
      }
   }

   #  Use unicode in the RA Dec labels.
   itk_option define -unicoderadec unicoderadec UnicodeRaDec 1 {

   }

   #  Whether to search NDF extension for additional related NDFs.
   itk_option define -deep_search deep_search Deep_Search 1 {
      gaia::GaiaNDAccess::set_deep_search $itk_option(-deep_search)
   }

   #  Create debugging log.
   itk_option define -debuglog debuglog DebugLog 0

   #  If in developer mode allow control of debug logging.
   itk_option define -developer developer Developer 0

   #  Protected variables: (available to instance)
   #  --------------------

   #  Application name.
   protected variable appname_ "Starlink GAIA::Skycat"

   #  Clone number of this window.
   protected variable clone_ 0

   #  Initialization progress count.
   protected variable sofar_ 0

   #  Initialization windows.
   protected variable Init_
   protected variable Progress_

   #  Offered colours of the main background and blank pixels.
   protected variable colours_ {
      white
      grey90 grey80 grey70 grey60 grey50 grey40 grey30 grey20 grey10
      black
      red green blue cyan magenta yellow
   }

   #  Id of after command for reading ESO config file.
   protected variable after_id_ {}

   #  Number for creating toolbox clones.
   protected variable tool_clones_ 0

   #  Unique identifier for items to be ignored when drawn on canvas.
   protected variable ast_tag_ ast_element

   #  Control re-creation of the help menu (gets called from Rtd and SkyCat).
   protected variable help_menu_done_ 0

   #  Name of menu with toolboxes.
   protected variable toolmenu_ {}

   #  Name of menu for application interoperability.
   protected variable interopmenu_ {}

   #  Name of submenu for sending images view SAMP.
   protected variable samp_send_image_menu_

   #  Canvas identifier of the position of interest.
   protected variable position_of_interest_ {}

   #  Last world coordinates of position of interest.
   protected variable last_position_of_interest_ {}

   #  Whether or not debugging is already started.
   protected variable debug_started_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Maximum clone number so far
   common clone_max_ 0

   #  Prefix to use to create new main windows.
   common prefix_ ".gaia"

   #  Handler for catalogues.
   common astrocat_ [astrocat ::cat::.astrocat]

   #  SAMP client; takes care of communication with the hub.
   common samp_client_ {}

   #  SAMP sender; sends message via the hub to other clients.
   common samp_sender_ {}

   #  SAMP agent; handles GAIA-specific incoming SAMP messages.
   common samp_agent_ {}

#  End of class definition.
}

#  Need to override this proc so we use the browser version of the open dialog
#  for local catalogues (and we need it here so that it is used in preference
#  to the autoload version).
itcl::body ::cat::AstroCat::local_catalog {{id ""} {classname AstroCat} {debug 0} {w ""}} {
   gaia::GaiaSearch::get_local_catalog $id $w
}

#  Re-direct this proc so that we can add VO features. The Data-Servers menu
#  is erased and no hooks exist to restore the VO menus.
itcl::body ::cat::AstroCat::add_catalog_menu {w {id {} } {classname AstroCat} {debug 0}} {
   ::gaia::GaiaSearch::add_catalog_menu $w $id $classname $debug
}
