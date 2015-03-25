#+
#  Name:
#     GaiaVOTAPQuery

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Class for querying a VO TAP service.

#  Description:
#     Provides controls for constructing and doing a query of a TAP version
#     1 service. The service is defined by an URL representing the service
#     resource root. Note just uses /sync, future versions may consider
#     an async service, but that could be overkill for GAIA.

#  Invocations:
#
#        GaiaVOTAPQuery object_name [configuration options]
#
#     This creates an instance of a GaiaTAPQuery object. The return is
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
#     Copyright (C) 2014 Science and Technology Facilities Council
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
#     28-MAR-2014 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaVOTAPQuery {}

itcl::class gaiavo::GaiaVOTAPQuery {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Handler for temporary files.
      set tempcats_ [gaia::GaiaTempName \#auto -prefix GaiaTempTAP \
                        -exists 0 -type ".TAB"]

      #  Display the TAP resource name and/or resource URL.
      set lwidth 10
      set vwidth 50
      if { $itk_option(-shortname) != {} } {
         set name "$itk_option(-shortname) ($itk_option(-accessURL))"
      } else {
         set name $itk_option(-accessURL)
      }

      itk_component add actionframe {
         frame $w_.actionframe
      }
      pack $itk_component(actionframe) -side top -fill x -ipadx 1m -ipady 1m

      itk_component add servicename {
         LabelValue $itk_component(actionframe).servicename \
            -text "Service:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -value $itk_option(-shortname)
      }
      pack $itk_component(servicename) -side top -fill x -expand 1
      add_short_help $itk_component(servicename) "The TAP service name"

      itk_component add serviceurl {
         LabelEntry $itk_component(actionframe).serviceurl \
            -text "        " \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -value $itk_option(-accessURL) \
            -textvariable [scope itk_option(-accessURL)]
      }
      pack $itk_component(serviceurl) -side top -fill x -expand 1
      add_short_help $itk_component(serviceurl) "The TAP service endpoint"

      #  Additional query for discovering information about the tables
      #  on offer.
      itk_component add queryservice {
         button $itk_component(actionframe).queryservice \
            -text "Tables..." \
            -command [code $this query_tables_]
      }
      pack $itk_component(queryservice) -side right -fill none -expand 0 -padx 1m
      add_short_help $itk_component(queryservice) \
         {Query the service for information about the tables available}

      #  Insert query with range of displayed image.
      itk_component add imagerange {
         button $itk_component(actionframe).imagerange \
            -text "Insert area fragment..." \
            -command [code $this insert_image_range_]
      }
      pack $itk_component(imagerange) -side right -fill none -expand 0 -padx 1m
      add_short_help $itk_component(imagerange) \
         {Insert query fragment with displayed image range in RA and Dec}

      #  Get the ADQL query.
      itk_component add adqlframe {
         iwidgets::scrolledtext $w_.adql \
            -labeltext "ADQL Query" \
            -wrap none \
            -vscrollmode static \
            -hscrollmode dynamic
      }

      pack $itk_component(adqlframe) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(adqlframe) {ADQL query, press Query button to activate}
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
   }

   #  Do the query as a background job.
   public method query {} {

      #  Query starts, so might want to do something.
      if { $itk_option(-feedbackcommand) != {} } {
         eval $itk_option(-feedbackcommand) on
      }

      #  Establish object to run the query script.
      if { $querytask_ == {} } {
         set querytask_ [gaia::GaiaForeignExec \#auto \
                            -application $::gaia_dir/querytap \
                            -notify [code $this query_done_]]
      }
      set votable_ [$tempcats_ get_typed_name ".vot"]
      set interrupted_ 0
      set query [$itk_component(adqlframe) get 1.0 end]
      $querytask_ runwith $itk_option(-accessURL) $query $votable_
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
         warning_dialog "Failed to query TAP service"
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

   #  Query the TAP service about the tables that it provides and display some
   #  of the metadata. Only done once, or when accessURL is changed.
   protected method query_tables_ {} {
      set query "$itk_option(-accessURL)/tables"
      if { $tables_doc_ == {} || $query != $last_tables_query_ } {
         if { $querytaptask_ == {} } {
            set querytaptask_ [gaia::GaiaForeignExec \#auto \
                                  -application $::gaia_dir/querytapthings \
                                  -preprocess [code $this query_tables_output_] \
                                  -notify [code $this query_tables_done_]]
         }

         blt::busy hold $w_
         set tables_doc_ {}
         set last_tables_query_ $query
         $querytaptask_ runwith $itk_option(-accessURL) "tables"
      } else {
         #  Just make sure the results window can be viewed.
         utilRaiseWindow $w_.tableset
      }
   }

   #  Handle output from query_tables task.
   protected method query_tables_output_ {msg} {
      append tables_doc_ $msg
   }

   #  Query table completed, so pass on result to viewer.
   protected method query_tables_done_ {} {
      blt::busy release $w_
      if { $tables_doc_ != {} } {
         utilReUseWidget gaiavo::GaiaVOTableSet $w_.tableset -xml $tables_doc_
         ::update idletasks
         $w_.tableset update
      }
   }

   #  Insert a symbolic fragment of text that shows the range of the
   #  displayed image in RA and Dec.
   protected method insert_image_range_ {} {
      set range [get_image_area]
      if { [llength $range] == 4 } {
         lassign $range ramin ramax decmin decmax
         $itk_component(adqlframe) insert end "\
<RA> BETWEEN $ramin AND $ramax \n\
AND \n\
<DEC> BETWEEN $decmin AND $decmax"
      } else {
         info_dialog "Displayed image does not have an RA/Dec range"
      }
   }

   #  Return a list of RA/Dec values covering the area of the image.
   #  Not going to work near the poles, also if RA/Dec are swapped.
   public method get_image_area {} {
      set image [$itk_option(-gaiactrl) get_image]

      #  Use the corners of the image, so make sure these are available.
      set width [$image width]
      set height [$image height]
      if { ! [$image isclear] && $width > 0 && $height > 0 } {

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

         #  Get cordinates of corners.
         lassign [$image astpix2wcs 1 1 1] ramin decmin
         lassign [$image astpix2wcs $width $height 1] ramax decmax

         if { [$image astcelestial] } {
            catch {
               $image astset "System" $oldsystem
            }
            catch {
               $image astset "Equinox" $oldequinox
            }
         }

         return [list [min $ramin $ramax] [max $ramin $ramax] \
                    [min $decmin $decmax] [max $decmin $decmax]]
      }
      return {}
   }




   #  Configuration options: (public variables)
   #  ----------------------

   #  The shortname of the service.
   itk_option define -shortname shortname ShortName {}

   #  The TAP resource URL.
   itk_option define -accessURL accessURL AccessURL {}

   #  Command to execute when a list of images is accepted. The command
   #  will be trailed by a status flag (1 for OK) and either a filename
   #  or an error message.
   itk_option define -command command Command {}

   #  Command to execute when batch jobs starts and stops.
   itk_option define -feedbackcommand feedbackcommand FeedBackCommand {}

   #  An astrocat instance for handling the result as a TST.
   itk_option define -astrocat astrocat AstroCat {}

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

   #  Task to query the TAP service about it's tables.
   protected variable querytaptask_ {}

   #  Output from query tables task, an XML document.
   protected variable tables_doc_ {}

   #  Last query tables task query.
   protected variable last_tables_query_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
