#+
#  Name:
#     GaiaVOCatSIAP

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Query a SIAP server for images.

#  Description:
#     Extends the GaiaVOCat class to query a given SIAP server for any images.
#     The images may then be downloaded and displayed.

#  Invocations:
#
#        GaiaVOCatSIAP object_name [configuration options]
#
#     This creates an instance of a GaiaVOCatSIAP object. The return is
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     08-AUG-2008 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaVOCatSIAP {}

itcl::class gaiavo::GaiaVOCatSIAP {

   #  Inheritances:
   #  -------------
   inherit gaiavo::GaiaVOCat

   #  Constructor:
   #  ------------
   constructor {args} {
      wm title $w_ "Query VO Simple Image Access server"
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
      GaiaVOCat::init

      set m [add_menubutton Options "Options menu"]

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

   #  Add the component that will control the query.
   protected method add_query_component_ {} {

      itk_component add siap {
         gaiavo::GaiaVOSIAPSearch $w_.siap \
            -accessURL $itk_option(-accessURL) \
            -shortname $itk_option(-shortname) \
            -feedbackcommand  [code $this set_feedback] \
            -astrocat [code $w_.cat] \
            -command [code $this query_done] \
            -namesvr $itk_option(-namesvr) \
            -gaiactrl [$itk_option(-gaia) get_image]
      }
      pack $itk_component(siap) -side top -fill x
      add_short_help $itk_component(siap) {Controls for querying SIAP server}

      set query_component_ $itk_component(siap)
   }

   #  Open a service, "args" is a list of values from a row of the current
   #  table. 
   protected method open_service_ {args} {

      #  Need to locate the VOX:Image_AccessReference field to get the URL for
      #  downloading the image.
      set ucds [$w_.cat ucd]
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

   #  Extract the accessURL for the SIAP service from a list of headers
   #  and the associated data row.
   public proc getAccessURL {headers row} {
      eval lassign "$row" $headers
      if { [info exists accessURL] } {
         return $accessURL
      }
      return {}
   }

   #  Extract a name for SIAP service from a list of headers
   #  and the associated data row.
   public proc getName {headers row} {
      eval lassign "$row" $headers
      if { [info exists shortName] && $shortName != {} } {
         return $shortName
      }
      if { [info exists title] } {
         return $title
      }
      return {}
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The shortname of the service.
   itk_option define -shortname shortname ShortName {}

   #  The accessURL for the SIAP server.
   itk_option define -accessURL accessURL AccessURL {}

   #  Instance of GAIA to display the image.
   itk_option define -gaia gaia Gaia {}

   #  The name server.
   itk_option define -namesvr namesvr NameSvr {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Active getter for downloading an image.
   protected variable urlget_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

}
