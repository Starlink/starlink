#+
#  Name:
#     GaiaVOCatRegistry

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Query the VO registry for services.

#  Description:
#     Extends the GaiaVOCat class to query the supported registries
#     for services of a given capability.

#  Invocations:
#
#        GaiaVOCatRegistry object_name [configuration options]
#
#     This creates an instance of a GaiaVOCatRegistry object. The return is
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

itk::usual GaiaVOCatRegistry {}

itcl::class gaiavo::GaiaVOCatRegistry {

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

   #  Make the interface conform to our usage. The "Open" button should
   #  be "Accept" and "Close" should be "Cancel" when we're offering a
   #  whole catalog service, otherwise we keep "Open" and "Close" and
   #  deal with row-based actions.
   public method init {} {
      gaiavo::GaiaVOCat::init

      wm title $w_ "Query VO Registry for services"

      if { $itk_option(-whole_operation) } {
         $itk_component(open) configure -text "Accept"
         add_short_help $itk_component(open) {Accept list of services}

         $itk_component(close) configure -text "Cancel"
         add_short_help $itk_component(close) {Cancel changes and close window}
      }

      #  Bindings on the listbox showing catalogue content.
      set lbox $itk_component(results).listbox

      #  Allow focus in the listbox, for keyboard control of selection
      #  and delete of selected rows.
      bind $lbox <1> [code $this focus_listbox_]

      #  Double click is same as "Open", unless this is a whole_operation
      #  in which case it is "Edit".
      if { $itk_option(-whole_operation) } {
         bind $lbox <Double-1> [code $this edit_selected_object]
      } else {
         bind $lbox <Double-1> [code $this open]
      }

      #  Delete removes the selected rows.
      bind $lbox <Delete> [code $this remove_selected]

      #  Read the registry catalogue, if given.
      if { $itk_option(-catalog) != {} &&
           [::file exists $itk_option(-catalog)] } {
         $itk_component(registry) read_query $itk_option(-catalog)

         #  Keep this name, the catalogue used may change but we want any
         #  permanent changes to be kept in the original.
         set initial_catalogue_ $itk_option(-catalog)
         $w_.cat open $itk_option(-catalog)

         #  Apply the blacklist.
         update_content
      }

      #  Add a menu option to apply the blacklist.
      set m [get_menu Options]
      add_menuitem $m checkbutton "Apply blacklist" \
         {Filter list to remove those in the blacklist} \
         -variable [scope itk_option(-apply_blacklist)] \
         -onvalue 1 -offvalue 0 \
         -command [code $this update_content]
   }

   #  User pressed the accept button. Override to not necessarily require
   #  the selected row and close window.
   public method open {} {
      if { $itk_option(-whole_operation) } {
         open_service_ 1
         close
      } else {
         gaiavo::GaiaVOCat::open
      }
   }

   #  Close the window, activated in response to a cancel. Only difference to
   #  accept is what argument is used to qualify activate_cmd.
   public method close {} {
      if { $itk_option(-whole_operation) } {
         open_service_ 0
      }
      gaiavo::GaiaVOCat::close
   }

   #  Add the component that will control the registry query.
   protected method add_query_component_ {} {

      #  Registry searches for now.
      itk_component add registry {
         gaiavo::GaiaVORegistrySearch $w_.registry \
            -feedbackcommand  [code $this set_feedback] \
            -command [code $this query_done] \
            -query_cmd [code $this query] \
            -show_cols_cmd [code $this set_show_cols] \
            -service $itk_option(-service)
      }
      pack $itk_component(registry) -side top -fill x
      add_short_help $itk_component(registry) {Controls to set registry query}

      set query_component_ $itk_component(registry)
   }

   #  Open a service. In this case it means accept the whole catalogue which
   #  could be modified and in a new catalogue, so we need to attempt to
   #  update the initial catalogue with this new content. Note we change the
   #  meaning of activate_cmd when operating on a whole catalogue.
   protected method open_service_ {args} {
      if { $itk_option(-activate_cmd) != {} } {
         $w_.cat save $initial_catalogue_
         if { $itk_option(-whole_operation) } {
            eval $itk_option(-activate_cmd) $args
         } else {
            #  Need headers and selected row.
            set headings [$itk_component(results) get_headings]
            eval $itk_option(-activate_cmd) \$headings "$args"
         }
      }
   }

   #  Set focus into the listbox.
   protected method focus_listbox_ {} {
      catch {::focus $itk_component(results).listbox}
   }

   #  Override update_content so we can apply the blacklist.
   public method update_content {} {
      set info_ [$w_.cat content]
      if { $itk_option(-blacklist) != {} && $itk_option(-apply_blacklist) } {
         set headings [$w_.cat headings]
         set result {}
         foreach row [$w_.cat content] {
            set identifier [$query_component_ get_identifier "$headings" "$row"]
            if { ! [$itk_option(-blacklist) blacklisted $identifier] } {
               lappend result $row
            }
         }
         set info_ $result
      }
      $itk_component(results) config -info $info_
   }

   #  Provide access to query_component_ specialisations for different
   #  registry types.
   public method get_identifier {headings row} {
      return [$query_component_ get_identifier "$headings" "$row"]
   }
   public method get_access_url {headings row} {
      return [$query_component_ get_access_url "$headings" "$row"]
   }
   public method get_name {headings row} {
      return [$query_component_ get_name "$headings" "$row"]
   }

   #  Add more help if this is just a simple registry query.
   protected method add_help_menu_ {} {
      if { $itk_option(-help_file) == {} } {
         add_help_button registry "On Window..." {Help on this window}
      }
      gaiavo::GaiaVOCat::add_help_menu_
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The type of operation, whole or row.
   itk_option define -whole_operation whole_operation Whole_Operation 1

   #  The type of services to query for - SSAP, SIAP, ConeSearch, TAP.
   itk_option define -service service Service SIAP

   #  GaiaVOBlacklist instance that manages the blacklist for the services.
   itk_option define -blacklist blacklist Blacklist {}

   #  Whether to apply the blacklist to filter the list.
   itk_option define -apply_blacklist apply_blacklist Apply_Blacklist 1

   #  Protected variables: (available to instance)
   #  --------------------

   #  Name of the initial catalogue.
   protected variable initial_catalogue_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

}
