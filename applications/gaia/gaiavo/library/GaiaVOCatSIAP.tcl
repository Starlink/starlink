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

itk::usual GaiaVOCatSIAP {}

itcl::class gaiavo::GaiaVOCatSIAP {

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
      gaiavo::GaiaVOCat::init

      wm title $w_ "Query VO Simple Image Access server"

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

   #  Display the selected image in a new window.
   public method display_in_new {} {
      set new_window_ 1
      foreach row [$itk_component(results) get_selected] {
         open_service_ $row
      }
      set new_window_ 0
   }

   #  Open a service, "args" is a list of values from a row of the current
   #  table.
   protected method open_service_ {args} {

      #  Need to locate the VOX:Image_AccessReference field to get the URL for
      #  downloading the image.
      set ucds [$w_.cat ucd]
      set n 0
      set accessref {}
      foreach ucd $ucds {
         if { [string match -nocase "*accessref*" $ucd] } {
            set accessref [eval lindex $args $n]
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
      set choice [choice_dialog "$message" "OK Blacklist" "OK" $w_]
      if { $choice != "OK" } {
         $itk_option(-blacklist) blacklist $itk_option(-identifier)
         if { $itk_option(-blacklist_cmd) != {} } {
            eval $itk_option(-blacklist_cmd)
         }
         close
      }
   }

   #  Use specialised error dialog that offers to blacklist the current
   #  server.
   protected method error_dialog_ {message} {
      set choice [choice_dialog "$message" "OK Blacklist" "OK" $w_]
      if { $choice != "OK" } {
         $itk_option(-blacklist) blacklist $itk_option(-identifier)
         if { $itk_option(-blacklist_cmd) } {
            eval $itk_option(-blacklist_cmd)
         }
         close
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The identifier for the SIAP server.
   itk_option define -identifier identifier Identifier {}

   #  Instance of GAIA to display the image.
   itk_option define -gaia gaia Gaia {}

   #  The name server.
   itk_option define -namesvr namesvr NameSvr {}

   #  The blacklist object.
   itk_option define -blacklist blacklist BlackList {}

   #  Command to execute if this server is blacklisted. Better
   #  arrange to remove it from any registry displays.
   itk_option define -blacklist_cmd blacklist_cmd Blacklist_Cmd {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Active getter for downloading an image.
   protected variable urlget_ {}

   #  Whether downloaded images should be displayed in new windows.
   protected variable new_window_ 0

   #  Common variables: (shared by all instances)
   #  -----------------
}
