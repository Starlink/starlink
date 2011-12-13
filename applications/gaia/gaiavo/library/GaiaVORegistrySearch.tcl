#+
#  Name:
#     GaiaVORegistrySearch

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Class for querying a VO registry.

#  Description:
#     This class defines the access methods for querying the NVO
#     registry and astrogrid registries.
#
#     The basic query to a service type (like sia:SimpleImageAccess) can be
#     refined by adding a ADQL where clause that looks for a specified
#     substring in one of the known columns (these must match the meta-data
#     definitions for a VOResource).

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
      set tempcats_ [gaia::GaiaTempName \#auto -prefix GaiaTempReg \
                        -exists 0 -type ".TAB"]

      #  Display the registry. Offer two symbolic types. NVO and AstroGrid.
      set lwidth 10
      set vwidth 50
      itk_component add registry {
         LabelMenu $w_.registry \
            -text "Registry:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth
      }
      pack $itk_component(registry) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(registry) {VO Registry}

      foreach {name value} [array get registries_] {
         $itk_component(registry) add \
            -command [code $this set_registry_ $name] \
            -label $name \
            -value $name
      }
      $itk_component(registry) configure -value $itk_option(-registry)

      #  Display the type of service. This is fixed.
      itk_component add service {
         LabelValue $w_.service \
            -text "Service:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -value $services_($itk_option(-service))
      }
      pack $itk_component(service) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(service) {Type of service being queried}

      #  Simple column and substring to form predicate.
      #  XXX enumerate some column names (xpath form for AQDL query, bit much
      #  for the typical end-user).
      itk_component add column {
         LabelEntry $w_.column \
            -text "Column:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -value $itk_option(-column) \
            -textvariable [scope itk_option(-column)] \
            -command [code $this start_query_]
      }
      pack $itk_component(column) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(column)  {Registry column to qualify query}

      itk_component add substring {
         LabelEntry $w_.substring \
            -text "Substring:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -value $itk_option(-substring) \
            -textvariable [scope itk_option(-substring)] \
            -command [code $this start_query_]
      }
      pack $itk_component(substring) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(substring)  {Substring to search in column value}
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

      #  Establish object to run the query scripts.
      if { $querytask_ == {} } {
         set querytask_ [gaia::GaiaForeignExec \#auto \
                               -application $::gaia_dir/queryreg \
                               -notify [code $this query_done_]]
      }

      set votable_ [$tempcats_ get_typed_name ".vot"]
      set interrupted_ 0

      if { $itk_option(-column) != {} && $itk_option(-substring) != {} } {
         $querytask_ runwith [get_registry_] [get_service_] \
            "$itk_option(-column)" "$itk_option(-substring)" "$votable_"
      } else {
         $querytask_ runwith [get_registry_] [get_service_] \
            "" "" "$votable_"
      }
   }

   #  Interrupt the query for the current task.
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

   #  Set the registry.
   protected method set_registry_ {registry} {
      configure -registry $registry
   }

   #  Get the registry endpoint.
   protected method get_registry_ {} {
      return $registries_($itk_option(-registry))
   }

   #  Set the service.
   protected method set_service_ {service} {
      configure -service $service
   }

   #  Translate a service type to its full description or standard ID.
   protected method get_service_ {} {
      return $standardIDs_($itk_option(-service))
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
         set tempname \
            "Failed to open registry query result ($query_status: $errmsg)"
      }

      #  Do command so that something happens.
      if { $itk_option(-command) != {} } {
         eval $itk_option(-command) \$status \$tempname
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The type of VO registry to query, NVO or AstroGrid.
   itk_option define -registry registry Registry "NVO"

   #  The type of query, SIAP, SSAP or CONE.
   itk_option define -service service Service "SIAP"

   #  Command to execute when a list of servers is accepted.
   itk_option define -command command Command {}

   #  Command to execute when batch jobs starts and stops.
   itk_option define -feedbackcommand feedbackcommand FeedBackCommand {}

   #  Command to execute to inititate a query externally (that's use this to
   #  do the same job as the "Query" button). Issued when return is pressed in
   #  the substring entry.
   itk_option define -query_cmd query_cmd Query_Cmd {}

   #  Name of a column to qualify query.
   itk_option define -column column Column {title}

   #  Substring to search for in column.
   itk_option define -substring substring Substring {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Temporary files.
   protected variable tempcats_ {}

   #  Name of the VOTable from query.
   protected variable votable_ {}

   #  Task controlling queries.
   protected variable querytask_ {}

   #  Set true when a query is being interrupted.
   protected variable interrupted_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

   #  The known registries.
   protected common registries_
   set registries_(NVO) "http://nvo.stsci.edu/vor10/ristandardservice.asmx"
   set registries_(AstroGrid) \
      "http://registry.astrogrid.org/astrogrid-registry/services/RegistryQueryv1_0"

   #  Mapping for short to full names of services.
   protected common services_
   set services_(SIAP) "SimpleImageAccess"
   set services_(SSAP) "SimpleSpectralAccess"
   set services_(CONE) "ConeSearch"

   #  Mapping of short service names to their standard ids.
   protected common standardIDs_
   set standardIDs_(SIAP) "ivo://ivoa.net/std/SIA"
   set standardIDs_(SSAP) "ivo://ivoa.net/std/SSA"
   set standardIDs_(CONE) "ivo://ivoa.net/std/ConeSearch"

   #  Possible columns for adding a predicate.
   protected common columns_ "title shortName"


#  End of class definition.
}
