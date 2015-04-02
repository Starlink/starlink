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

itk::usual GaiaVOCatsSIAP {}

itcl::class gaiavo::GaiaVOCatsSIAP {

   #  Inheritances:
   #  -------------
   inherit gaiavo::GaiaVOCats

   #  Constructor:
   #  ------------
   constructor {args} {
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

      wm title $w_ "Query VO Simple Image Access servers"

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

      #  Add additional commands to display the selected images in new
      #  windows. Re-uses "Open" as Display.
      $itk_component(open) configure -text "Display"
      add_short_help $itk_component(open) "Download and display selected image"

      itk_component add displaynew {
         button $itk_component(buttons).displaynew \
            -text "Display in new" \
            -command [code $this display_in_new]
      }
      pack $itk_component(displaynew) -before $itk_component(close) \
         -side left -expand 1 -pady 2m
      add_short_help $itk_component(displaynew) \
         {Download and display selected image in new windows}

      #  Set default name server.
      set_namesvr $namesvr

      #  Initialise the catalog of SIAP servers.
      init_servers_
   }

   #  Start the queries, calls query_ for each SIAP server.
   protected method start_queries_ {} {
      foreach {url name identifier} [get_access_details] {
         if { ! $interrupted_ } {
            query_ $url $name $identifier
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
            -gaiactrl [$itk_option(-gaia) get_image] \
            -blacklist $itk_option(-blacklist)
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
      }
   }

   #  Display the selected image in a new window.
   public method display_in_new {} {
      set new_window_ 1
      foreach row [$itk_component(results$current_) get_selected] {
         $this open_service_ $row
         break
      }
      set new_window_ 0
   }

   #  Open a service, "args" is a list of values from a row of the
   #  current table.
   protected method open_service_ {args} {

      #  Need to locate the VOX:Image_AccessReference field to get the URL for
      #  downloading the image.
      set ucds [$w_.cat$current_ ucd]
      set n 0
      set accessref {}
      foreach ucd $ucds {
         if { [string match -nocase "*accessref*" $ucd] } {
            set accessref [eval lindex $args $n]
            break
         }
         incr n
      }

      if { $itk_option(-gaia) != {} && $accessref != {} } {
         if { $urlget_ == {} } {
            set urlget_ [gaia::GaiaUrlGet .\#auto \
                            -notify_cmd \
                            [code $this display_image_ $new_window_]]
         }
         blt::busy hold $w_
         $urlget_ get $accessref
      }
   }

   #  Display an image, if it exists.
   protected method display_image_ {new_window filename type} {
      blt::busy release $w_
      if { [::file exists $filename] } {
         if { $new_window } {
            $itk_option(-gaia) newimage_clone $filename
         } else {
            $itk_option(-gaia) open $filename
         }
      }
      if { $urlget_ != {} } {
         catch {delete object $urlget_}
      }
      set urlget_ {}
   }

   #  Use specialised warning dialog that offers to blacklist the current
   #  server.
   protected method warning_dialog_ {message} {

      #  Truncate message, these can be very long.
      set message [string range $message 0 200]

      set choice [choice_dialog "$message" "OK Blacklist" "OK" $w_]
      if { $choice != "OK" } {
         $itk_option(-blacklist) blacklist $ids_($current_)
      }
   }

   #  Use specialised error dialog that offers to blacklist the current
   #  server.
   protected method error_dialog_ {message} {
      #  Truncate message, these can be very long.
      set message [string range $message 0 200]

      set choice [choice_dialog "$message" "OK Blacklist" "OK" $w_]
      if { $choice != "OK" } {
         $itk_option(-blacklist) blacklist $ids_($current_)
      }
   }

   #  Return a list of access URLs and titles for all the SIAP servers in
   #  the local catalogue. Skips servers without an accessURL and those in the
   #  blacklist.
   public method get_access_details {} {
      set headings [$w_.cat headings]
      set result {}
      foreach row [$w_.cat content] {
         set identifier [$query_component_ get_identifier "$headings" "$row"]
         set accessURL [$query_component_ get_access_url "$headings" "$row"]
         set title [$query_component_ get_name "$headings" "$row"]
         if { ! [$itk_option(-blacklist) blacklisted $identifier] && $accessURL != {} } {
            lappend result $accessURL $title $identifier
         }
      }
      return $result
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

   #  Whether downloaded images should be displayed in new windows.
   protected variable new_window_ 0

   #  The blacklist object. Usual shared between all SIAP widgets.
   itk_option define -blacklist blacklist BlackList {}

   #  Common variables: (shared by all instances)
   #  -----------------

}
