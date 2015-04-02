#+
#  Name:
#     GaiaVOCat

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Query a VO service and display the result.

#  Description:
#     This class defines a standard basic UI for containing the query
#     elements specific to a particular VO service. It controls the query and
#     displays the resultant VOTable. Subclasses should be created that
#     specialise the query itself and what actions to take when activating
#     a response.

#  Invocations:
#
#        GaiaVOCat object_name [configuration options]
#
#     This creates an instance of a GaiaVOCat object. The return is
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
#     31-JUL-2008 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaVOCat {}

itcl::class gaiavo::GaiaVOCat {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {
      eval itk_initialize $args

      #  Catalogue handler.
      astrocat $w_.cat

      #  Add a short help window.
      make_short_help

      #  Add menubar.
      add_menubar

      #  File menu.
      set m [add_menubutton File]

      #  Read old query from disk.
      add_menuitem $m command "Open..." \
         {Read previous query results from a VOTable} \
         -command [code $this read_query]

      #  Save query to disk.
      add_menuitem $m command "Save as..." \
         {Save query results to a VOTable} \
         -command [code $this save_query]

      add_menuitem $m command "Close" \
         {Close this window} \
         -command [code $this close]

      #  Edit menu.
      set m [add_menubutton Edit]

      add_menuitem $m command "Remove selected" \
         {Remove selected rows} \
         -command [code $this remove_selected]

      add_menuitem $m command "Keep selected" \
         {Keep the selected rows} \
         -command [code $this save_selected]

      add_menuitem $m command "Add new row..." \
         {Enter the data for a new row of results} \
         -command [code $this enter_new_object]

      add_menuitem $m command "Edit selected row..." \
         {Edit the data for the selected row} \
         -command [code $this edit_selected_object]

      #  Options menu.
      set m [add_menubutton Options]

      add_menuitem $m command "Set Sort Columns..." \
         {Set options for sorting the query results} \
         -command [code $this sort_dialog]

      add_menuitem $m command "Hide/Show Columns..." \
         {Set options for displaying columns of the query results} \
         -command [code $this select_columns]

      add_menuitem $m command "Proxies..."  \
         "Define an HTTP proxy server for use with a firewall." \
         -command [code cat::AstroCat::proxies]

      #  Do the initial check for proxies.
      check_proxies
   }

   #  Destructor:
   #  -----------
   destructor {
      if { [::file exists $newfile_] } {
         catch { ::file delete -force $newfile_ }
      }
   }

   #  Methods:
   #  --------

   #  Finish building the UI. Done later as can rely on configuration
   #  options in sub-classes (add_query_component_ is defined there).
   public method init {} {

      #  Help menu.
      add_help_menu_

      #  Add the query component. Usually has some controls
      #  for establishing the query context and initiating it.
      add_query_component_

      #  Controls for the query.
      itk_component add querybuttons {
         frame $w_.querybuttons -borderwidth 2 -relief raised
      }
      pack $itk_component(querybuttons) -side top -fill x

      itk_component add query {
         button $itk_component(querybuttons).query \
            -text "Query" \
            -command [code $this query]
      }
      pack $itk_component(query) -side left -expand 1 -pady 2m
      add_short_help $itk_component(query) \
         {Start query}

      itk_component add interrupt {
         button $itk_component(querybuttons).interrupt \
            -text "Interrupt" \
            -state disabled \
            -command [code $this interrupt]
      }
      pack $itk_component(interrupt) -side left -expand 1 -pady 2m
      add_short_help $itk_component(interrupt) \
         {Interrupt the query}

      #  Add the table for displaying the query results.
      itk_component add results {
         GaiaQueryResult $w_.results \
            -astrocat [code $w_.cat] \
            -title "Query Results" \
            -hscroll 1 \
            -height 12 \
            -sortcommand [code $this set_sort_cols] \
            -layoutcommand [code $this set_show_cols] \
            -selectmode extended \
            -exportselection 0
      } {
      }
      pack $itk_component(results) -side top -fill both -expand 1
      add_short_help $itk_component(results) {Results of query to repository}

      #  Double click is same as "Open".
      bind $itk_component(results).listbox <Double-1> [code $this open]

      #  Add the dialog button frame and the action buttons.
      itk_component add buttons {
         frame $w_.buttons -borderwidth 2 -relief raised
      }
      pack  $itk_component(buttons) -side top -fill x

      itk_component add open {
         button $itk_component(buttons).open \
            -text "Open" \
            -command [code $this open]
      }
      pack $itk_component(open) -side left -expand 1 -pady 2m
      add_short_help $itk_component(open) {Open window to query selected service}

      itk_component add close {
         button $itk_component(buttons).close \
            -text "Close" \
            -command [code $this close]
      }
      pack $itk_component(close) -side left -expand 1 -pady 2m
      add_short_help $itk_component(close) {{bitmap b1} = close this window}

      #  Add the ProgressBar, looks busy during query.
      itk_component add progressbar {
         ProgressBar $w_.progress
      }
      pack $itk_component(progressbar) -side top -fill x
      add_short_help itk_component(progressbar) \
         {Progress bar: displays status of work in progress}

      #  Display the catalogue, if set.
      open_catalog
   }

   #  Close this window.
   public method close {} {
      wm withdraw $w_
   }

   #  Save query to a VOTable.
   public method save_query {} {
      set w [FileSelect .\#auto -title "Save query to a local file"]
      if {[$w activate]} {
         set filename [$w get]
         if {[file exists $filename]} {
            if {! [confirm_dialog \
                      "File `[file tail $filename]' exists. Overwrite it?" $w_]} {
               return
            }
         }
         $query_component_ save_query $filename
      }
   }

   #  Read query from a VOTable.
   public method read_query {} {
      set w [FileSelect .\#auto -title "Read old query from a local file"]
      if { [$w activate] } {
         set filename [$w get]
         if {[file exists $filename]} {
            $query_component_ read_query [$w get]
         } else {
            warning_dialog "File '$filename' doesn't exist" $w_
         }
      }
   }

   #  Open services for the selected rows. Depends on the type of service query
   #  as to what will happen. For SIAP we should open a window to download an
   #  image.
   public method open {} {
      foreach row [$itk_component(results) get_selected] {
         open_service_ $row
      }
   }

   #  Pop up a dialog to sort the list.
   public method sort_dialog {} {
      $itk_component(results) sort_dialog
   }

   #  Called when the user has selected columns to sort the results by.
   #  The first arg is the sort columns, the second arg is the order
   #  (increasing, decreasing).
   public method set_sort_cols {sort_cols sort_order} {
      if { "[$w_.cat sortcols]" != "$sort_cols" \
              || "[$w_.cat sortorder]" != "$sort_order"} {
         $w_.cat sortcols $sort_cols
         $w_.cat sortorder $sort_order
         cat::CatalogInfo::save {} $w_ 0
         $itk_component(results) config -sort_cols $sort_cols \
            -sort_order $sort_order
         query
      }
   }

   #  Pop up a dialog to select table columns to display.
   public method select_columns {} {
      $itk_component(results) select_columns
   }

   #  Called when the user has selected columns to show.
   public method set_show_cols {cols} {
      if { [$w_.cat open] } {
         set show [$w_.cat showcols]
         if { "$show" == {} } {
            set show [$itk_component(results) get_headings]
         }
         if { "$show" != "$cols" } {
            $w_.cat showcols $cols
            cat::CatalogInfo::save {} $w_ 0
         }
      }
      configure -show_cols $cols
   }

   #  Clear the table listing.
   public method clear {} {
      catch {$itk_component(results) clear}
   }

   #  Remove the currently selected rows.
   public method remove_selected {} {
      $itk_component(results) remove_selected
      refresh_
   }

   #  Remove all but the selected rows.
   public method save_selected {} {
      $itk_component(results) save_selected
      refresh_
   }

   #  Re-read catalogue to refresh. Note there is a wart here. For VOTables
   #  only the backing store TST has been updated. So we need to take a copy
   #  of the backing file before opening it as it will be deleted when the
   #  astrocat sees that the file name has changed (from the VOTable name).
   protected method refresh_ {} {

      #  Prepare to delete last catalogue file (could be open, so defer).
      set oldfile $newfile_

      #  Copy backing file to new file.
      set tempname [$w_.cat url]
      set newfile_ [gaia::GaiaTempName::make_name "GaiaTempCat" $count_ ".TAB"]
      incr count_
      ::file copy -force $tempname $newfile_

      #  Now delete old catalogue.
      if { [::file exists $oldfile] } {
         catch { ::file delete -force $oldfile }
      }

      #   Finally open catalogue, read and update display.
      $w_.cat open $newfile_
      $w_.cat query
      update_content
   }

   #  Update the display from the catalogue content.
   public method update_content {} {
      set info_ [$w_.cat content]
      $itk_component(results) config -info $info_
   }

   #  Pop up a dialog to enter the data for a new row.
   public method enter_new_object {} {
      $itk_component(results) enter_new_object [code $this refresh_]
   }

   #  Pop up a window so that the user can edit the selected object(s).
   public method edit_selected_object {} {
      $itk_component(results) edit_selected_object [code $this refresh_]
   }

   #  Open the catalogue for this window.
   public method open_catalog {} {

      #  Normally -catalog should be specified when creating this widget
      #  if not, choose a default...
      if { $itk_option(-catalog) == {} } {
         return
      }

      #  Open the catalogue.
      set name $itk_option(-catalog)
      if { [catch {$w_.cat open $name $itk_option(-catalogdir)} msg] } {
         error_dialog $msg $w_
         return -code error
      }

      #  Display catalogue name in header and icon.
      wm title $w_ "[$w_.cat longname $name $itk_option(-catalogdir)] ($itk_option(-number))"
      wm iconname $w_ [$w_.cat shortname $name $itk_option(-catalogdir)]
   }

   #  Interrupt the current query.
   public method interrupt {} {
      $query_component_ interrupt
      set_feedback off
      catch {$itk_component(results) config -title "Query Results"}
      set_state normal
   }

   #  Set/reset widget states while busy.
   public method set_state {state} {
      if { $state == "normal" } {
         $itk_component(query) config -state normal
         $itk_component(interrupt) config -state disabled
      } else {
         $itk_component(query) config -state disabled
         $itk_component(interrupt) config -state normal
      }
      update idletasks
      $itk_component(progressbar) reset
   }

   # This method is called when the background query is done.
   public method query_done {status result} {

      set_state normal

      #  If status is bad result is a message, otherwise it should be
      #  the name of a VOTable file.
      if { ! $status } {
         warning_dialog_ $result
         after 0 [list $itk_component(progressbar) config -text $result]
      } elseif { ! [file exists $result] } {
         error_dialog_ $result
         after 0 [list $itk_component(progressbar) config -text $result]
      } else {

         #  Got a VOTable.
         busy {
            #  Open and read.
            $w_.cat open $result
            $w_.cat query

            #  Remember this result for any updates to catalogue.
            set last_result_ $result

            #  Need to update GaiaQueryResult with content.
            #  Update table headings.
            set prev_headings $headings_
            set headings_ [$w_.cat headings]
            $itk_component(results) config -headings $headings_

            #  Initial show columns.
            if { $itk_option(-show_cols) != {} } {
               $itk_component(results) \
                  set_options $headings_ Show 0
               $itk_component(results) \
                  set_options $itk_option(-show_cols) Show 1
            }

            if { "$prev_headings" != "$headings_" } {
               $itk_component(results) update_options
            }

            #  Update content.
            update_content
            $itk_component(results) config \
               -title "Returned [$itk_component(results) total_rows] rows"

         }
      }
   }

   #  Popup the warning dialog. Override for specialised behaviour.
   protected method warning_dialog_ {message} {
      warning_dialog "$message" $w_
   }

   #  Popup the error dialog. Override for specialised behaviour.
   protected method error_dialog_ {message} {
      error_dialog "$message" $w_
   }

   #  Start the catalogue query based on the current query options and
   #  display the results in the table.
   public method query {args} {
      #  Start the query in the background.
      catch {
         $itk_component(results) config -title "Querying..."
         $itk_component(results) clear
      }

      set_state disabled
      $itk_component(progressbar) config -text "Querying..."
      $itk_component(progressbar) look_busy

      $query_component_ query
   }

   #  Respond to feedback about query progress (stop and start).
   protected method set_feedback {onoff} {
      if { $onoff == "off" } {
         $itk_component(progressbar) reset
      }
   }

   #  Add the component that will control the query parameters.
   protected method add_query_component_ {} {
      puts stderr "Must implement an add_query_component_ method"
   }

   #  Open a service, "args" is a list of values from a row of the
   #  current table.
   protected method open_service_ {args} {
      puts stderr "Must implemenent an open_service_ method"
   }

   #  Add and populate the help menu. Has the VO overview by default
   #  plus an additional topic defined by help_file and help_label.
   protected method add_help_menu_ {} {
      if { $itk_option(-help_file) != {} && $itk_option(-help_label) != {} } {
         add_help_button $itk_option(-help_file) $itk_option(-help_label) \
            {Help on this window...}
      }
      add_help_button vo "About VO services..." {Help on VO in GAIA}
   }

   #  Extract an access URL from the given row, given the names of the values
   #  in headers. Tricky as we have two possible names access_url and
   #  accessURL from the two registry types.
   public proc getAccessURL {headers row} {

      #  Nasty external knowledge required as to what kind of registry the row
      #  came from, so try all known handlers.
      foreach handler $reghandlers_ {
         set accessurl [$handler get_access_url $headers $row]
         if { $accessurl != {} } {
            return $accessurl
         }
      }
      return {}
   }

   #  Extract a name for TAP service from a list of headers and the associated
   #  data row.
   public proc getName {headers row} {

      #  Nasty external knowledge required as to what kind of registry the row
      #  came from, so try all known handlers.
      foreach handler $reghandlers_ {
         set name [$handler get_name $headers $row]
         if { $name != {} } {
            return $name
         }
      }
      return {}
   }

   #  Extract the IVOA identifier for the service from a list of headers
   #  and the associated data row.
   public proc getIdentifier {headers row} {

      #  Nasty external knowledge required as to what kind of registry the row
      #  came from, so try all known handlers.
      foreach handler $reghandlers_ {
         set id [$handler get_identifier $headers $row]
         if { $id != {} } {
            return $id
         }
      }
      return {}
   }

   #  Check for a file ~/.skycat/proxies, once each session, and use it to
   #  initialize environment variables for a proxy server.
   public proc check_proxies {} {
      if { $checked_proxies_ } {
         return
      }
      cat::ProxyDialog::check_proxies $::env(HOME)/.skycat/proxies
      set checked_proxies_ 1
   }

   #  Pop up a dialog to set or change the HTTP proxy server.
   public proc proxies {} {
      utilReUseWidget ProxyDialog .proxy \
         -configfile $::env(HOME)/.skycat/proxies
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of catalogue.
   itk_option define -catalog catalog Catalog {}

   #  Name of catalogue directory, or a tcl list forming the path to it (empty
   #  means root).
   itk_option define -catalogdir catalogDir CatalogDir {}

   #  Columns to show. Defined once during initialisation.
   itk_option define -show_cols show_cols Show_Cols {}

   #  Command to execute when a response is activated. The command will
   #  be trailed by the accessURL value and the full row content. Normally
   #  this will open a window to do the actual service query for images etc.
   itk_option define -activate_cmd activate_cmd Activate_Cmd {}

   #  Define a help file and label for the Help menu.
   itk_option define -help_file help_file Help_File {}
   itk_option define -help_label help_label Help_Label "On Window..."

   #  The shortname of the service.
   itk_option define -shortname shortname ShortName {}

   #  The URL for the service.
   itk_option define -accessURL accessURL AccessURL {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  List of catalogue column headings (from results of most recent query).
   protected variable headings_ {}

   #  Result from most recent query (list of rows).
   protected variable info_ {}

   #  Component that will perform the query. Must be defined in add_query_component_.
   protected variable query_component_ {}

   #  Temporary file for modified table contents.
   protected variable newfile_ {}

   #  Name of last resultt catalogue.
   protected variable last_result_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Counter for temp names.
   protected common count_ 0

   #  Flag: set to 1 after we checked for a proxy server.
   protected common checked_proxies_ 0

   #  Handlers for different registry types.
   protected common regtap_ [::gaiavo::GaiaVORegTapQuery::instance]
   protected common regv1_ [::gaiavo::GaiaVORegV1Query::instance]
   protected common reghandlers_ "$regtap_ $regv1_"
}
