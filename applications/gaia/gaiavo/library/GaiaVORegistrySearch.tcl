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
      set tempcats_ [gaia::GaiaTempName \#auto \
                        -prefix GaiaVORegistry \
                        -type ".TAB"]

      set lwidth 10
      set vwidth 50

      #  Display the registry. XXX Can edit during testing.
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
            -textvariable [scope itk_option(-predicate)]
      }
      pack $itk_component(predicate) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(predicate)  \
         {Simple predicate to qualify query, e.g. "title like '%galex%'"}

      #  Create an object for running interruptable queries.
      Batch $w_.batch \
         -command [code $this query_done_] \
         -debug $itk_option(-debug)
   }

   #  Destructor:
   #  -----------
   destructor  {
      $tempcats_ clear
   }

   #  Methods:
   #  --------

   #  Do the query as a batch job.
   public method query {} {
      if { $itk_option(-feedbackcommand) != {} } {
         eval $itk_option(-feedbackcommand) on
      }
      
      #cmdtrace on noeval notruncate

      $w_.batch bg_eval [code $this do_query_]
   }

   #  Interrupt the query.
   public method interrupt {} {
      $w_.batch interrupt
   }

   #  Query the registry.
   protected method do_query_ {} {

      #  Using http from Tcl core, so fix to use the standard proxy settings.
      #  Do this each time to access new settings.
      set_proxy_
      
      #  Access the service and get its WSDL description.
      set def [::WS::Client::GetAndParseWsdl $itk_option(-registry) \
                  {} VORegistry]
      
      #  Set query dictionary.
      set inputs [list "predicate" "$itk_option(-predicate)" \
                     "capability" "$service_"]
      
      #  Do the query.
      set results [::WS::Client::DoRawCall VORegistry \
                      $itk_option(-endmethod) $inputs]
      
      #  Parse results and access the SOAP message body.
      ::dom parse -keepEmpties $results doc
      $doc documentElement top
      set xns {
         ENV "http://schemas.xmlsoap.org/soap/envelope/"
         xsi "http://www.w3.org/2001/XMLSchema-instance"
         xs "http://www.w3.org/2001/XMLSchema"
      }
      $doc selectNodesNamespaces $xns
      set body [$top selectNodes ENV:Body]
      
      #  Locate RESOURCE and save in a skeletal VOTable as a string.
      set resource [$body getElementsByTagName RESOURCE]
      if { $resource != {} } {
         set votstr {<VOTABLE version="1.1"
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xsi:schemaLocation="http://www.ivoa.net/xml/VOTable/v1.1 http://www.ivoa.net/xml/VOTable/v1.1"
            xmlns="http://www.ivoa.net/xml/VOTable/v1.1">
            <!--
            !  VOTable written by GAIA
            !-->
         }
         append votstr [$resource asXML]
         append votstr {</VOTABLE>}
         
         #  Read into a VOTable.
         set votable [gaiavotable::read $votstr]
         
         #  Convert to a TST file. 
         set filename [$tempcats_ get_name]
         set tst [gaiavotable::save $votable 0 $filename]
         gaiavotable::close $votable
         
         return $filename
      }
      return {}
   }

   #  Called when the query completes.
   protected method query_done_ {status args} {

      puts "status = $status, args = $args"

      if { $itk_option(-feedbackcommand) != {} } {
         eval $itk_option(-feedbackcommand) off
      }
      if { $itk_option(-command) != {} } {
         eval $itk_option(-command) $status $args
      }
   }

   #  Translate a service type to its full description.
   protected method set_service_ {} {
      if { [info exists services_($itk_option(-service))] } {
         set service_ $services_($itk_option(-service))
      }
   }

   #  Set the proxy server, if needed. Uses the http_proxy environment
   #  variable.
   protected method set_proxy_ {} {
      set proxy {}
      set port {}
      if { [info exists ::env(http_proxy)] } {
         if { [scan $::env(http_proxy) {http://%[^:/]:%d} proxy port] != 2 } {
            scan $::env(http_proxy) {http://%[^:/]} proxy
         }
      }
      ::http::config -proxyhost $proxy
      ::http::config -proxyport $port
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

   #  Whether to run batch command in foreground for debugging.
   itk_option define -debug debug Debug 0

   #  An astrocat instance for handling the result as a TST.
   itk_option define -astrocat astrocat AstroCat {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  The full name of the service.
   protected variable service_ {}

   #  Temporary files.
   protected variable tempcats_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Mapping for short to full names of services.
   protected common services_
   set services_(SIAP) "SimpleImageAccess"
   set services_(SSAP) "SimpleSpectralAccess"
   set services_(CONE) "conesearch"

#  End of class definition.
}
