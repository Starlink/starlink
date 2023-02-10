#+
#  Name:
#     GaiaVOSIAPSearch

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Class for querying a VO SIAP server.

#  Description:
#     Provides controls for constructing and doing a query of a SIAP version
#     1 server. The server is defined by an accessURL, which will be qualified
#     by a position and sizes on the sky.

#  Invocations:
#
#        GaiaVOSIAPSearch object_name [configuration options]
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
#     08-AUG-2008 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaVOSIAPSearch {}

itcl::class gaiavo::GaiaVOSIAPSearch {

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

      #  Display the SIAP shortname and/or access URL.
      set lwidth 10
      set vwidth 50
      if { $itk_option(-shortname) != {} } {
         set name "$itk_option(-shortname) ($itk_option(-accessURL))"
      } else {
         set name $itk_option(-accessURL)
      }
      itk_component add server {
         util::LabelValue $w_.server \
            -text "Server:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -value $name
      }
      pack $itk_component(server) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(server) "The SIAP server"

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

      #  Additional options. Set search region from the displayed
      #  image, mark region by dragging.
      itk_component add options {
         frame $w_.options
      }

      itk_component add setfromimg {
         button $itk_component(options).setfromimg \
            -text "Set from image" \
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
      FrameWidget::init
      set_from_image
   }

   #  Do the query as a background job.
   public method query {} {

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
      set votable_ [$tempcats_ get_typed_name ".vot"]
      set interrupted_ 0
      $querytask_ runwith $itk_option(-accessURL) $ra $dec "$size" $votable_
   }

   #  Interrupt the query.
   public method interrupt {} {
      if { $querytask_ != {} } {
         set interrupted_ 1
         catch {$querytask_ delete_now}
         set querytask_ {}
      }
      if { $itk_option(-feedbackcommand) != {} } {
         eval $itk_option(-feedbackcommand) off
      }
   }

   #  Called when the query completes.
   protected method query_done_ {} {

      #  Immediate notification we're finished.
      if { $itk_option(-feedbackcommand) != {} } {
         eval $itk_option(-feedbackcommand) off
      }

      if { $interrupted_ } {
         info_dialog "Query interrupted"
         return
      }

      #  Check file exists.
      if { ! [::file exists $votable_] } {
         warning_dialog "Failed to query SIAP server"
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
         $batch_ bg_eval [code $this do_object_query_ $itk_option(-namesvr) $name]
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

         #  Don't set second size. Not all servers seem to support that.
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

   #  Configuration options: (public variables)
   #  ----------------------

   #  The shortname of the service.
   itk_option define -shortname shortname ShortName {}

   #  The SIAP accessURL.
   itk_option define -accessURL accessURL AccessURL {}

   #  Command to execute when a list of images is accepted. The command
   #  will be trailed by a status flag (1 for OK) and either a filename
   #  or an error message.
   itk_option define -command command Command {}

   #  Command to execute when batch jobs starts and stops.
   itk_option define -feedbackcommand feedbackcommand FeedBackCommand {}

   #  An astrocat instance for handling the result as a TST.
   itk_option define -astrocat astrocat AstroCat {}

   #  Nameserver for looking up object coordinates.
   itk_option define -namesvr namesvr NameSvr ned@eso

   #  GaiaImageCtrl instance.
   itk_option define -gaiactrl gaiactrl GaiaCtrl {}

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
