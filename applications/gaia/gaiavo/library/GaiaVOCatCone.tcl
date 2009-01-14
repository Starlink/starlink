#+
#  Name:
#     GaiaVOCatCone

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Query a Cone Search server for a catalogue.

#  Description:
#     Extends the GaiaVOCat class to query a given Cone Search server for 
#     any objects it contains in a given region of sky. The objects are
#     described as a row in a catalogue.

#  Invocations:
#
#        GaiaVOCatCone object_name [configuration options]
#
#     This creates an instance of a GaiaVOCatCone object. The return is
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
#     Copyright (C) 2009 Science and Technology Facilities Council
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
#     13-JAN-2009 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaVOCatCone {}

itcl::class gaiavo::GaiaVOCatCone {

   #  Inheritances:
   #  -------------
   inherit gaiavo::GaiaVOCat

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
      GaiaVOCat::init

      #  Override title.
      wm title $w_ "Query VO Cone Search server"

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

      #  Plot button.
      itk_component add plot {
         button $itk_component(buttons).plot \
            -text "Plot" \
            -command [code $this plot]
      }

      pack $itk_component(plot) -side left -expand 1 -pady 2m
      add_short_help $itk_component(plot) {Plot positions over image}
   }

   #  Set the name server used, pass to other components.
   public method set_namesvr {name} {
      configure -namesvr $name
      if { [info exists itk_component(cone)] } {
         $itk_component(cone) configure -namesvr $name
      } else {
         puts "skipped component(cone)"
      }
   }

   #  Add the component that will control the query.
   protected method add_query_component_ {} {

      itk_component add cone {
         gaiavo::GaiaVOConeSearch $w_.cone \
            -accessURL $itk_option(-accessURL) \
            -shortname $itk_option(-shortname) \
            -feedbackcommand  [code $this set_feedback] \
            -astrocat [code $w_.cat] \
            -command [code $this query_done] \
            -namesvr $itk_option(-namesvr) \
            -gaiactrl [$itk_option(-gaia) get_image]
      }
      pack $itk_component(cone) -side top -fill x
      add_short_help $itk_component(cone) \
         {Controls for querying Cone Search server}

      set query_component_ $itk_component(cone)
   }

   #  Open a service, "args" is a list of values from a row of the current
   #  table. 
   protected method open_service_ {args} {
      # Hmm, what to do with this? Offer extended view of the row?
      puts "nothing done with open_service_"
   }

   #  Extract the accessURL for the Cone Search service from a list of headers
   #  and the associated data row.
   public proc getAccessURL {headers row} {
      eval lassign "$row" $headers
      if { [info exists accessURL] } {
         return $accessURL
      }
      return {}
   }

   #  Extract a name for Cone Search service from a list of headers
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

   #  Plot the RA and Dec positions on the image.
   public method plot {} {
      set rtdctrl [$itk_option(-gaia) get_image]
      set rtdimage [$rtdctrl get_image]
      set equinox "J2000"; # Must be true for all catalogues.

      #  Do the plot... 
      #  XXX uses the plotting symbols set in the astrocat for this catalogue,
      #  need to set a value. 
      set symbol [list {} [list circle red {} {} {} {}] [list {4.0} {}]]
      $w_.cat symbol $symbol

      if {[catch {$w_.cat imgplot $rtdimage $info_ $equinox $headings_} msg]} {
         error_dialog $msg
      }
   }



   #  Configuration options: (public variables)
   #  ----------------------

   #  The shortname of the service.
   itk_option define -shortname shortname ShortName {}

   #  The accessURL for the Cone Search server.
   itk_option define -accessURL accessURL AccessURL {}

   #  Instance of GAIA to display the catalogue.
   itk_option define -gaia gaia Gaia {}

   #  The name server.
   itk_option define -namesvr namesvr NameSvr {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

}
