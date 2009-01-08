#+
#  Name:
#     GaiaVOCatsSIAP

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Query a set of SIAP servers for the images on a region of sky.

#  Description:
#     Extends the GaiaVOCats class to query a set of SIAP servers for any
#     images they have on a region of sky. The images may then be downloaded
#     and displayed. The list of SIAP servers can be modified using the
#     associated query dialog.

#  Invocations:
#
#        GaiaVOCatsSIAP object_name [configuration options]
#
#     This creates an instance of a GaiaVOCatsSIAP object. The return is
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
#     25-NOV-2008 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaVOCatsSIAP {}

itcl::class gaiavo::GaiaVOCatsSIAP {

   #  Inheritances:
   #  -------------
   inherit gaiavo::GaiaVOCats

   #  Constructor:
   #  ------------
   constructor {args} {
      wm title $w_ "Query VO Simple Image Access servers"
      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor {
   }

   #  Methods:
   #  --------

   #  Add additional menu items. Nameserver.
   public method init {} {
      GaiaVOCats::init

      set m [get_menu "Options"]
      set ns_menu [menu $m.ns]
      add_menuitem $m cascade "Set Name Server" \
         {Select the name server used to resolve astronomical object names} \
         -menu $ns_menu

      if { [catch {set list [$w_.cat info namesvr]} msg] } {
         error_dialog $msg $w_
         return
      }

      foreach namesvr $list {
         $ns_menu add radiobutton \
            -label $namesvr \
            -command [code $this set_namesvr $namesvr] \
            -variable [scope itk_option(-namesvr)]
      }

      if { ![info exists namesvr] } {
         error_dialog "No default name server found for astronomical objects"
         return
      }

      #  Set default name server.
      set_namesvr $namesvr

      #  Initialise the catalog of SIAP servers.
      init_servers_

      #  And the blacklist.
      if { ![info exists blacklist_] } {
         init_blacklist_
      }
   }

   #  Start the queries, calls query_ for each SIAP server.
   protected method start_queries_ {} {
      foreach {url name} [get_access_details] {
         if { ! $interrupted_ } {
            query_ $url $name
         }
      }
   }

   #  Set the name server used, pass to other components.
   public method set_namesvr {name} {
      configure -namesvr $name
      if { [info exists itk_component(siap)] } {
         $itk_component(siap) configure -namesvr $name
      } else {
         puts "skipped component(siap)"
      }
   }

   #  Add the component that will control the queries.
   protected method add_query_component_ {} {
      itk_component add siap {
         gaiavo::GaiaVOSIAPSearchs $w_.siap \
            -astrocat [code $w_.cat] \
            -feedbackcommand  [code $this set_feedback] \
            -command [code $this query_done] \
            -namesvr $itk_option(-namesvr) \
            -gaiactrl [$itk_option(-gaia) get_image]
      }
      pack $itk_component(siap) -side top -fill x
      add_short_help $itk_component(siap) {Controls for querying SIAP services}

      #  Needed for superclass.
      set query_component_ $itk_component(siap)
   }

   #  Initialise the SIAP servers from the catalogue.
   protected method init_servers_ {} {

      if { $itk_option(-siap_catalog) != {} && $query_component_ != {} } {

         #  Open the catalogue.
         if { [catch {$w_.cat open $itk_option(-siap_catalog)} msg] } {
            error_dialog $msg $w_
            return -code error
         }

         #  Display catalogue name in header.
         wm title $w_ "[$w_.cat longname $itk_option(-siap_catalog)] ($itk_option(-number))"
      }
   }

   #  Open a service, "args" is a list of values from a row of the
   #  current table.
   protected method open_service_ {args} {

      #  Need to locate the VOX:Image_AccessReference field to get the URL for
      #  downloading the image.
      set ucds [$w_.cat$current_ ucd]
      set n 0
      foreach ucd $ucds {
         if { [string match -nocase "*accessref*" $ucd] } {
            break
         }
         incr n
      }
      set accessref [eval lindex $args $n]

      if { $itk_option(-gaia) != {} } {
         if { $urlget_ == {} } {
            set urlget_ [gaia::GaiaUrlGet .\#auto \
                            -notify_cmd [code $this display_image_]]
            blt::busy hold $w_
            $urlget_ get $accessref
         }
      }
   }

   #  Display an image, if it exists.
   protected method display_image_ {filename type} {
      blt::busy release $w_
      if { [::file exists $filename] } {
         $itk_option(-gaia) open $filename
      }
      if { $urlget_ != {} } {
         catch {delete object $urlget_}
      }
      set urlget_ {}
   }

   #  Return a list of access URLs and titles for all the SIAP servers in
   #  the local catalogue. Skips servers without an accessURL and those in the
   #  blacklist (servers known to be test services).
   public method get_access_details {} {
      set headings [$w_.cat headings]
      set result {}
      foreach row [$w_.cat content] {
         eval lassign \$row $headings
         if { ! [blacklisted_ $identifier] && $accessURL != {} } {
            lappend result $accessURL $title
         }
      }
      return $result
   }

   #  Check if an identifier is blacklisted. These are URNs of the form
   #  ivo://... that should be unique for each service.
   protected method blacklisted_ {identifier} {
      return [info exists blacklist_($identifier)]
   }

   #  Initialise the blacklist. Just a simple text file with comment lines
   #  starting with # in the first column and other lines being the
   #  identifiers of the blacklisted services.
   protected method init_blacklist_ {} {
      set blacklist [utilGetConfigFilename .skycat GaiaSIAPBlacklist]
      if { ! [::file exists $blacklist] } {

         #  Use builtin defaults.
         ::file copy -force $::gaiavo_library/GaiaSIAPBlacklist $blacklist
      }
      set fid [::open $blacklist]
      while { [gets $fid line] >= 0 } {
         if { [string range $line 0 0] != "\#" } {
            set blacklist_($line) 1
         }
      }
      ::close $fid
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The SIAP catalogue. Contains XML response from a SIAP query to a
   #  a registry.
   itk_option define -siap_catalog siap_catalog Siap_Catalog {} {
      if { $itk_option(-siap_catalog) != {} } {
         init_servers_
      }
   }

   #  Instance of GAIA to display an image.
   itk_option define -gaia gaia Gaia {}

   #  The name server.
   itk_option define -namesvr namesvr NameSvr {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Active getter for downloading an image.
   protected variable urlget_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  The blacklist of identifiers. Array for quick hashing.
   common blacklist_
}
