#+
#  Name:
#     GaiaVOSIAPSearchs

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Class for querying a set of SIAP servers.

#  Description:
#     Provides controls for constructing and doing a query of a set of SIAP
#     version 1 servers. The servers are specified when a call to query is
#     made, this will be qualified by a position and size on the sky.

#  Invocations:
#
#        GaiaVOSIAPSearchs object_name [configuration options]
#
#     This creates an instance of a GaiaSIAPSearch object. The return is
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

#  Copyright:
#     Copyright (C) 2008-2009 Science and Technology Facilities Council
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
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     25-NOV-2008 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaVOSIAPSearchs {}

itcl::class gaiavo::GaiaVOSIAPSearchs {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Handler for temporary files.
      set tempcats_ [gaia::GaiaTempName \#auto -prefix GaiaTempSIAP \
                        -exists 0 -type ".TAB"]
      set lwidth 10
      set vwidth 50

      #  Get the position from an object name lookup.
      itk_component add object {
         util::LabelEntry $w_.object \
            -text "Object name:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -command [code $this object_query_]
      }
      pack $itk_component(object) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(object) \
         {Name of an object to lookup, press <Return> to activate}

      #  Get the position on the sky, an RA and a Dec.
      itk_component add ra {
         util::LabelEntry $w_.ra \
            -text "RA:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -value $ra_ \
            -textvariable [scope ra_]
      }
      pack $itk_component(ra) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(ra) {RA centre of images, degrees or HH:MM:SS}

      itk_component add dec {
         util::LabelEntry $w_.dec \
            -text "Dec:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -value $dec_ \
            -textvariable [scope dec_]
      }
      pack $itk_component(dec) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(dec) {Dec centre of images, degrees or DD:MM:SS}

      itk_component add size1 {
         util::LabelEntry $w_.size1 \
            -text "Width:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -value $size1_ \
            -textvariable [scope size1_]
      }
      pack $itk_component(size1) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(size1) {Width of images, arcminutes}

      itk_component add size2 {
         util::LabelEntry $w_.size2 \
            -text "Height:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -value $size2_ \
            -textvariable [scope size2_]
      }
      pack $itk_component(size2) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(size2) {Height of images, arcminutes}

      #  Additional options.
      itk_component add options {
         frame $w_.options
      }

      #  Open registry query dialog to change the list of services.
      itk_component add registry {
         button $itk_component(options).registry \
            -text "View image servers..." \
            -command [code $this registry_query_]
      }
      pack $itk_component(registry) -side right -padx 1m -pady 2m
      add_short_help $itk_component(registry) \
         {View or change the VO image services that will be queried}

      #  Set search region from the displayed image, mark region by dragging.
      itk_component add setfromimg {
         button $itk_component(options).setfromimg \
            -text "Set area from image" \
            -command [code $this set_from_image]
      }
      pack $itk_component(setfromimg) -side right -padx 1m -pady 2m
      add_short_help $itk_component(setfromimg) \
         {Set search region to match currently displayed image}

      itk_component add selectarea {
         button $itk_component(options).selectarea \
            -text "Select area..." \
            -command [code $this select_area]
      }
      pack $itk_component(selectarea) -side right -padx 1m -pady 2m
      add_short_help $itk_component(selectarea) \
         {Select region of image with mouse button 1}

      pack $itk_component(options) -fill x -expand 1
   }

   #  Destructor:
   #  -----------
   destructor  {
      $tempcats_ clear
   }

   #  Methods:
   #  --------

   #  Complete interface.
   public method init {} {
      util::FrameWidget::init
      set_from_image
   }

   #  Do a query on the given access URL.
   public method query { url } {

      #  Query starts, so might want to do something.
      if { $itk_option(-feedbackcommand) != {} } {
         eval $itk_option(-feedbackcommand) on
      }

      #  Transform RA and Dec values into degrees, if needed. The search sizes
      #  are in arcminutes, transform those.
      lassign [skycat::.wcs hmstod $ra_ $dec_] ra dec
      set size {}
      if { $size1_ != {} } {
         set size "[expr $size1_/60.0]"
      }
      if { $size2_ != {} } {
         set size "$size,[expr $size2_/60.0]"
      }

      #  Establish object to run the query script.
      if { $querytask_ == {} } {
         set querytask_ [gaia::GaiaForeignExec \#auto \
                            -application $::gaia_dir/querysiap \
                            -notify [code $this query_done_]]
      }

      #  Do the query.
      set interrupted_ 0
      set votable_ [$tempcats_ get_typed_name ".vot"]
      $querytask_ runwith $url $ra $dec "$size" $votable_
   }

   #  Interrupt the queries.
   public method interrupt {} {
      if { $querytask_ != {} } {
         set interrupted_ 1
         catch {$querytask_ delete_now}
         set querytask_ {}
      }

      #  Do the user command (always done as something may be waiting
      #  for the query to complete, tempname may be an error message).
      if { $itk_option(-command) != {} } {
         eval $itk_option(-command) 0 interrupted
      }
      if { $itk_option(-feedbackcommand) != {} } {
         eval $itk_option(-feedbackcommand) off
      }
   }

   #  Called when a query completes.
   protected method query_done_ {} {

      #  Immediate notification we're finished for now.
      if { $itk_option(-feedbackcommand) != {} } {
         eval $itk_option(-feedbackcommand) off
      }

      if { $interrupted_ } {
         info_dialog "Query interrupted"
         return
      }

      #  Check file exists.
      if { ! [::file exists $votable_] } {
         warning_dialog "Failed querying SIAP servers"
         return
      }

      #  Convert to TST and do the command to display etc.
      read_query $votable_
   }

   #  Save the result of a query to an external VOTable.
   public method save_query {filename} {
      if { $votable_ != {} && [::file exists $votable_] } {
         ::file copy -force $votable_ $filename
      }
   }

   #  Read the query directly from an existing file.
   public method read_query {filename} {

      #  Convert to a TST file so we can open it up as usual.
      if { [catch {set vot [gaiavotable::open $filename]} msg] } {
         set status 0
         set tempname "$msg ($filename)"
      } else {

         #  Check the STATUS return.
         lassign [gaiavotable::info $vot "QUERY_STATUS"] query_status errmsg
         if { $query_status != "ERROR" } {
            set status 1
            set tempname [$tempcats_ get_name]
            set tst [gaiavotable::save $vot 0 $tempname]
            gaiavotable::close $vot

            #  This is the current VOTable now.
            set votable_ $filename

         } else {
            set status 0
            set tempname "Query returned an error ($query_status: $errmsg)"
         }
      }

      #  Do the user command (always done as something may be waiting
      #  for the query to complete, tempname may be an error message).
      if { $itk_option(-command) != {} } {
         eval $itk_option(-command) \$status \$tempname
      }
   }

   #  Perform a query for the position of an object.
   protected method object_query_ {args} {
      set name [$itk_component(object) get]
      if { $name != {} } {
         if { $batch_ == {} } {
            set batch_ [util::Batch $w_.batch \
                           -command [code $this name_query_done_]]
         }
         blt::busy hold $w_
         $batch_ bg_eval \
            [code $this do_object_query_ $itk_option(-namesvr) $name]
      }
   }

   #  Do the name server query.
   protected method do_object_query_ {namesvr name} {
      return [$itk_option(-astrocat) namesvr $itk_option(-namesvr) $name]
   }

   #  Called when a name server query completes.
   protected method name_query_done_ {status args} {
      blt::busy release $w_
      if { $status } {
         error_dialog $args
      } else {
         eval lassign $args ra_ dec_
      }
   }

   #  Define the region to query so that it matches the displayed image.
   public method set_from_image {} {
      set list [get_image_center_radius]
      if { [llength $list] > 2 } {
         lassign $list ra_ dec_ size1_

         #  Many servers don't like a width & height, so avoid that.
         set size2_ {}
      }
   }

   #  Return a list of values indicating the center coordinates and radius
   #  of the current image.
   public method get_image_center_radius {} {
      set image [$itk_option(-gaiactrl) get_image]
      if { ! [$image isclear] } {

         #  Coordinates have to be in J2000, so make sure image is set to
         #  that. Catch this so that any problems are not fatal.
         if { [$image astcelestial] } {
            catch {
               set oldsystem [$image astget "System"]
               $image astset "System" "FK5"
            }
            catch {
               set oldequinox [$image astget "Equinox"]
               $image astset "Equinox" "2000"
            }
         }

         set center [$image wcscenter]

         if { [$image astcelestial] } {
            catch {
               $image astset "System" $oldsystem
            }
            catch {
               $image astset "Equinox" $oldequinox
            }
         }

         if { [llength $center] >= 2 } {
            lassign $center ra dec equinox
            set radius [format "%.2f" [$image wcsradius]]
            if { $radius } {
               return [list $ra $dec $radius]
            }
         }
      }
      return
   }

   #  Select area on the image for the query region.
   public method select_area {} {
      set list [select_image_area]
      if { [llength $list] > 2 } {
         lassign $list ra_ dec_ size1_
         set size2_ $size1_
      }
   }

   #  Ask the user to select an area of the image by dragging out a region on
   #  the image return the resulting center pos and radius as a list of
   #  {ra dec radius-in-arcmin}.
   public method select_image_area {} {
      set image [$itk_option(-gaiactrl) get_image]
      if { [$image isclear] } {
         error_dialog "No image is currently loaded"
         return
      }

      #  Get canvas coords of selected area.
      set list [$itk_option(-gaiactrl) select_area]
      if { [llength $list] != 4 } {
         return
      }
      lassign $list x0 y0 x1 y1

      #  Get center and radius in canvas coords.
      set x [expr ($x0+$x1)/2.]
      set y [expr ($y0+$y1)/2.]

      if { [catch {$image convert coords $x $y canvas ra dec "wcs J2000"} msg] } {
         error_dialog \
            "error converting canvas ($x, $y) to world coordinates: $msg" $w_
         return
      }

      set radius [expr [$image wcsdist $x0 $y0 $x $y]/60.]
      return [list $ra $dec $radius]
   }

   #  Realise the registry dialog, we need this for service functions as well
   #  as when we want to change the list of SIAP servers, so use local method
   #  rather than utils. XXX clearly this should all be refactored so thst
   #  registry is a model and we just have various views.
   protected method create_registry_ { withdraw } {
      if { [winfo exists $w_.voregistry] } {
         $w_.voregistry config \
            -catalog [$itk_option(-astrocat) longname] \
            -service SIAP \
            -activate_cmd [code $this changed_registry_] \
            -blacklist $itk_option(-blacklist)
      } else {
         gaia::GaiaVOCatRegistry $w_.voregistry \
            -catalog [$itk_option(-astrocat) longname] \
            -service SIAP \
            -activate_cmd [code $this changed_registry_] \
            -blacklist $itk_option(-blacklist)
         #  Wait for realisation to complete...
         tkwait visibility $w_.voregistry
      }

      if { ! $withdraw } {
         utilRaiseWindow $w_.voregistry
      } else {
         wm withdraw $w_.voregistry
      }
   }


   #  Activate the registry dialog.
   protected method registry_query_ {} {
      blt::busy hold [winfo toplevel $w_]
      create_registry_ 0
   }

   #  Registry has been changed and maybe accepted.
   protected method changed_registry_ {accepted} {
      blt::busy release [winfo toplevel $w_]
      $itk_option(-astrocat) open [$w_.voregistry cget -catalog]
   }

   #  Provide access to GaiaVOCatRegistry specialisations for different
   #  registry types.
   public method get_identifier {headings row} {
      create_registry_ 1
      return [$w_.voregistry get_identifier "$headings" "$row"]
   }
   public method get_access_url {headings row} {
      create_registry_ 1
      return [$w_.voregistry get_access_url "$headings" "$row"]
   }
   public method get_name {headings row} {
      create_registry_ 1
      return [$w_.voregistry get_name "$headings" "$row"]
   }


   #  Configuration options: (public variables)
   #  ----------------------

   #  Command to execute when a list of images is accepted. The command
   #  will be trailed by a status flag (1 for OK) and either a filename
   #  or an error message.
   itk_option define -command command Command {}

   #  Command to execute when batch jobs starts and stops.
   itk_option define -feedbackcommand feedbackcommand FeedBackCommand {}

   #  An astrocat instance that contains the registry catalogue.
   itk_option define -astrocat astrocat AstroCat {}

   #  Nameserver for looking up object coordinates.
   itk_option define -namesvr namesvr NameSvr ned@eso

   #  GaiaImageCtrl instance.
   itk_option define -gaiactrl gaiactrl GaiaCtrl {}

   #  GaiaVOBlacklist instance that manages the blacklist for SIAP services.
   itk_option define -blacklist blacklist Blacklist {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Temporary files.
   protected variable tempcats_ {}

   #  Name of the VOTable from query.
   protected variable votable_ {}

   #  Task controlling querys.
   protected variable querytask_ {}

   #  Set true when a query is being interrupted.
   protected variable interrupted_ 0

   #  SIAP RA, Dec and sizes.
   protected variable ra_ 00:00:00
   protected variable dec_ 00:00:00
   protected variable size1_ 10
   protected variable size2_ {}

   #  Batch job handler.
   protected variable batch_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
