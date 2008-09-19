#+
#  Name:
#     GaiaVORegistrySearch

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Class for querying a VO registry.

#  Description:
#     This class defines the access methods for querying the NVO version 1
#     registry. This should probably be generalised to offer the list of
#     registry of registries held by the IVOA at http://rofr.ivoa.net and
#     work with standard methods and any registry, but currently it is
#     fixed. XXX review this when version 1 stabilizes.
#
#     The query is defined using a service type like SimpleImageAccess, and an
#     ADQL predicate to refine the search and the results is returned as 
#     a VOTable (rather than the more generalised XML of a registry response).

#  Invocations:
#
#        GaiaVORegistrySearch object_name [configuration options]
#
#     This creates an instance of a GaiavoVolume object. The return is
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
#     Copyright (C) 2008 Science and Technology Facilities Council
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     22-JUL-2008 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaVORegistrySearch {}

itcl::class gaiavo::GaiaVORegistrySearch {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Handler for temporary files.
      set tempcats_ [gaia::GaiaTempName \#auto -prefix GaiaVORegistry \
                        -exists 0 -type ".TAB"]

      #  Display the registry. XXX Can edit during testing.
      set lwidth 10
      set vwidth 50
      itk_component add registry {
         LabelEntry $w_.registry \
            -text "Registry:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -value $itk_option(-registry) \
            -textvariable [scope itk_option(-registry)]
      }
      pack $itk_component(registry) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(registry) {VO Registry}

      #  Display the type of service. Can edit during testing.
      itk_component add service {
         LabelEntry $w_.service \
            -text "Service:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -value $service_ \
            -textvariable [scope service_]
      }
      pack $itk_component(service) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(service) {Service type}

      #  Simple predicate.
      itk_component add predicate {
         LabelEntry $w_.predicate \
            -text "Predicate:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -value $itk_option(-predicate) \
            -textvariable [scope itk_option(-predicate)] \
            -command [code $this start_query_]
      }
      pack $itk_component(predicate) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(predicate)  \
         {Simple predicate to qualify query, e.g. "title LIKE '%%galex%%'"}
   }

   #  Destructor:
   #  -----------
   destructor  {
      $tempcats_ clear
   }

   #  Methods:
   #  --------

   #  Start query. If a query_cmd is registered then this will be performed
   #  when <Return> is pressed in the predicate field.
   protected method start_query_ {args} {
      if { $itk_option(-query_cmd) != {} } {
         eval $itk_option(-query_cmd)
      }
   }

   #  Do the query as a batch job.
   public method query {} {

      #  Query starts, so might want to do something.
      if { $itk_option(-feedbackcommand) != {} } {
         eval $itk_option(-feedbackcommand) on
      }

      #  Establish object to run the query script.
      if { $querytask_ == {} } {
         set querytask_ [gaia::GaiaForeignExec \#auto \
                            -application $::gaia_dir/queryvoreg \
                            -notify [code $this query_done_]]
      }
      set votable_ [$tempcats_ get_typed_name ".vot"]
      set interrupted_ 0
      $querytask_ runwith $itk_option(-registry) $service_ \
         $itk_option(-predicate) $itk_option(-endmethod) $votable_
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
         warning_dialog "Failed to query registry"
         return
      }

      #  Convert to TST and do the command to display etc.
      read_query $votable_
   }

   #  Translate a service type to its full description.
   protected method set_service_ {} {
      if { [info exists services_($itk_option(-service))] } {
         set service_ $services_($itk_option(-service))
      }
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
      set vot [gaiavotable::open $filename]
      set tempname [$tempcats_ get_name]
      set tst [gaiavotable::save $vot 0 $tempname]
      gaiavotable::close $vot

      #  This is the current VOTable now.
      set votable_ $filename

      #  Do command so that something happens.
      if { $itk_option(-command) != {} } {
         eval $itk_option(-command) $tempname
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The VO registry to query. End point that returns the WSDL.
   itk_option define -registry registry Registry \
      "http://nvo.stsci.edu/vor10/NVORegInt.asmx?WSDL"

   #  The method of the registry to use.
   itk_option define -endmethod endmethod EndMethod "VOTCapabilityPredicate"

   #  The type of query, SIAP, SSAP or CONE.
   itk_option define -service service Service SIAP {
      set_service_
   }

   #  The query predicate.
   itk_option define -predicate predicate Predicate {}

   #  Command to execute when a list of servers is accepted.
   itk_option define -command command Command {}

   #  Command to execute when batch jobs starts and stops.
   itk_option define -feedbackcommand feedbackcommand FeedBackCommand {}

   #  An astrocat instance for handling the result as a TST.
   itk_option define -astrocat astrocat AstroCat {}

   #  Command to execute to inititate a query externally (that's use this to
   #  do the same job as the "Query" button). Issued when return is pressed in
   #  the predicate entry.
   itk_option define -query_cmd query_cmd Query_Cmd {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  The full name of the service.
   protected variable service_ {}

   #  Temporary files.
   protected variable tempcats_ {}

   #  Name of the VOTable from query.
   protected variable votable_ {}

   #  Task controlling querys.
   protected variable querytask_ {}

   #  Set true when a query is being interrupted.
   protected variable interrupted_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Mapping for short to full names of services.
   protected common services_
   set services_(SIAP) "SimpleImageAccess"
   set services_(SSAP) "SimpleSpectralAccess"
   set services_(CONE) "conesearch"

#  End of class definition.
}
