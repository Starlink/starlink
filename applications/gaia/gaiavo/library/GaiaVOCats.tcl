#+
#  Name:
#     GaiaVOCats

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Query set of VO services and display the results.

#  Description:
#     This class defines a standard basic UI for containing queries to a set
#     of VO services. It controls the queries and displays the resultant
#     VOTables. Subclasses should be created that specialise the queries (to
#     the same class) and what actions to take when activating a response.

#  Invocations:
#
#        GaiaVOCats object_name [configuration options]
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
#     Copyright (C) 2008-2014 Science and Technology Facilities Council
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
#     17-NOV-2008 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaVOCats {}

itcl::class gaiavo::GaiaVOCats {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {
      eval itk_initialize $args

      #  Catalog server for general use and for opening the list of servers.
      astrocat $w_.cat

      #  Add a short help window.
      make_short_help

      #  Add menubar.
      add_menubar

      #  File menu.
      set m [add_menubutton File]

      #  XXX need save and restore functions for current service responses.

      add_menuitem $m command "Close" \
         {Close this window} \
         -command [code $this close]

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
      #  Delete all instances of astrocat.
      $w_.cat delete
      for { set i 0 } { $i < $npages_ } { incr i } {
         $w_.cat$i delete
      }
   }

   #  Methods:
   #  --------

   #  Finish building the UI. Done later as can rely on configuration
   #  options in sub-classes (add_query_component_ is defined there).
   public method init {} {

      #  Add help menu.
      add_help_menu_

      #  Add the query component. Usually has some controls
      #  for establishing the query context (SIAP/Registry etc.) and
      #  initiating it.
      add_query_component_

      #  Controls for the query.
      itk_component add querybuttons {
         frame $w_.querybuttons -borderwidth 2 -relief raised
      }
      pack $itk_component(querybuttons) -side top -fill x

      itk_component add query {
         button $itk_component(querybuttons).query \
            -text "Query" \
            -command [code $this start_queries]
      }
      pack $itk_component(query) -side left -expand 1 -pady 2m
      add_short_help $itk_component(query) \
         {Start queries to the current servers}

      itk_component add interrupt {
         button $itk_component(querybuttons).interrupt \
            -text "Interrupt" \
            -state disabled \
            -command [code $this interrupt]
      }
      pack $itk_component(interrupt) -side left -expand 1 -pady 2m
      add_short_help $itk_component(interrupt) \
         {Interrupt the queries}

      #  Menu for switching between the various results pages in the
      #  notebook. We do not use a tabnotebook as that is limited to one
      #  row of tabs.
      itk_component add bookmenu {
         util::LabelMenu $w_.bookmenu \
            -text "SIAP server results:" \
            -labelwidth 18 \
            -valuewidth 30 \
            -valueanchor w
      }
      pack $itk_component(bookmenu) -side top -fill none -pady 2m

      #  Notebook for displaying the results.
      itk_component add notebook {
         iwidgets::notebook $w_.book -height 250
      }
      pack $itk_component(notebook) -side top -fill both -expand 1

      #  Add the dialog button frame and the action buttons.
      itk_component add buttons {
         frame $w_.buttons -borderwidth 2 -relief raised
      }
      pack $itk_component(buttons) -side top -fill x

      itk_component add open {
         button $itk_component(buttons).open \
            -text "Open" \
            -command [code $this open]
      }
      pack $itk_component(open) -side left -expand 1 -pady 2m
      add_short_help $itk_component(open) {Open selected service}

      itk_component add close {
         button $itk_component(buttons).close \
            -text "Close" \
            -command [code $this close]
      }
      pack $itk_component(close) -side left -expand 1 -pady 2m
      add_short_help $itk_component(close) {{bitmap b1} = close this window}

      #  Add the ProgressBar, looks busy during query.
      itk_component add progressbar {
         util::ProgressBar $w_.progress
      }
      pack $itk_component(progressbar) -side top -fill x
      add_short_help itk_component(progressbar) \
         {Progress bar: displays status of work in progress}
   }

   #  Close this window.
   public method close {} {
      wm withdraw $w_
   }

   #  Open service for the the selected row. Depends on the type of service
   #  query as to what will happen. For SIAP we should open a window to
   #  download and display an image.
   public method open {} {
      foreach row [$itk_component(results$current_) get_selected] {
         open_service_ $row
         break
      }
   }

   #  Switch to the page selected in the "Query Results" option menu.
   protected method view_page_ {page} {
      $itk_component(notebook) view $page
      set current_ $page
   }

   #  Pop up a dialog to sort the all the lists of results.
   public method sort_dialog {} {
      $itk_component(results$current_) sort_dialog
   }

   #  Called when the user has selected columns to sort the results by.
   #  The first arg is the sort columns, the second arg is the order
   #  (increasing, decreasing).
   public method set_sort_cols {sort_cols sort_order} {
      if { "[$w_.cat$current_ sortcols]" != "$sort_cols" ||
           "[$w_.cat$current_ sortorder]" != "$sort_order" } {
         $w_.cat$current_ sortcols $sort_cols
         $w_.cat$current_ sortorder $sort_order
         cat::CatalogInfo::save {} $w_ 0
         $itk_component(results$current_) config -sort_cols $sort_cols \
            -sort_order $sort_order

         #  Update the table.
         $itk_component(results$current_) new_info
      }
   }

   #  Pop up a dialog to select table columns to display.
   public method select_columns {} {
      $itk_component(results$current_) select_columns
   }

   #  Called when the user has selected columns to show.
   public method set_show_cols {cols} {
      set show [$w_.cat$current_ showcols]
      if { "$show" == {} } {
         set show [$itk_component(results$current_) get_headings]
      }
      if { "$show" != "$cols" } {
         $w_.cat$current_ showcols $cols
         cat::CatalogInfo::save {} $w_ 0
      }
   }

   #  Clear the current table listing.
   public method clear {} {
      $itk_component(results$current_) clear
   }

   #  Interrupt the current query.
   public method interrupt {} {
      set interrupted_ 1
      $query_component_ interrupt
      set_feedback off
      catch {$itk_component(results$current_) config -title "Query Results"}
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

   #  This method is called when a background query is done. Should
   #  be the current query.
   public method query_done {status result} {

      set_state normal

      #  If status is bad result is a message, otherwise it should be
      #  the name of a VOTable file.
      if { $status && [file exists $result] } {

         #  Got a table response.
         busy {
            $w_.cat$current_ open $result

            #  If empty or have problems reading it, do away with it.
            set info_ {}
            if { [catch {set info_ [$w_.cat$current_ content]} msg] != 0 } {
               warning_dialog_ "$names_($current_): $msg"
               remove_current_ "$msg"
            } else {
               if { $info_ == {} } {
                  remove_current_ "No images in $names_($current_)"
               } else {

                  #  Need to update GaiaQueryResult with content.
                  #  Update table headings.
                  set prev_headings $headings_
                  set headings_ [$w_.cat$current_ headings]
                  $itk_component(results$current_) config -headings $headings_

                  #  Initial show columns.
                  if { $itk_option(-show_cols) != {} } {
                     $itk_component(results$current_) \
                        set_options $headings_ Show 0
                     $itk_component(results$current_) \
                        set_options $itk_option(-show_cols) Show 1
                  }

                  if { "$prev_headings" != "$headings_" } {
                     $itk_component(results$current_) update_options
                  }

                  #  Update content.
                  $itk_component(results$current_) config -info $info_

                  $itk_component(results$current_) config -title \
                     "Returned [$itk_component(results$current_) total_rows] rows"

                  #  Make sure results are visible.
                  $itk_component(bookmenu) configure -value $current_
                  view_page_ $current_
               }
            }
         }
      } else {
         #  Something went wrong, remove this service from consideration and
         #  make a report. Note a bad status is less bad than no file and
         #  if the result is "Interrupted" with a bad status we just handle
         #  the issue silently.
         if { $result != "interrupted" } {
            if { ! $status } {
               warning_dialog_ "$names_($current_): $result"
            } else {
               error_dialog_ "$names_($current_): $result"
            }
         }
         remove_current_ $result
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

   #  Remove the current query result page and associated. Text is something
   #  set as the progressbar value (usually the error that caused this
   #  removal).
   protected method remove_current_ {text} {
      after 0 [list $itk_component(progressbar) config -text $text]

      #  Seems to fail with some visibility timer, so handle that ungracefully.
      catch {
         $itk_component(notebook) delete $current_
         $itk_component(bookmenu) delete $current_
         $itk_component(bookmenu) update_menubutton
         incr current_ -1
         incr npages_ -1
         incr ncolumn_ -1
      }
   }

   #  Start the queries.
   public method start_queries {args} {

      #  Clear any existing queries.
      clear_query_results_

      #  Now invoke method to start the queries in sub-classes.
      set interrupted_ 0
      start_queries_
   }

   #  Start the queries. The implementation of this method should call query
   #  once for each server.
   protected method start_queries_ {} {
      puts stderr "you must implement a start_queries_ method"
   }

   #  Clear all the existing queries so that a new set can be done.
   protected method clear_query_results_ {} {
      for { set i 0 } { $i < $npages_ } { incr i } {
         if { [info exists itk_component(results$i)] } {
            delete object $itk_component(results$i)
         }
      }
      $itk_component(bookmenu) clear
      $itk_component(bookmenu) update_menubutton
      if { $current_ > -1 } {
         $itk_component(notebook) delete 0 end
      }
      set npages_ 0
      set current_ -1
   }

   #  Start the catalogue query based on the current query options and
   #  display the results in a page of the results book. The url will
   #  be passed to the query component and the name used in a menu.
   #  The identifier is needed so that this may be blacklisted.
   protected method query_ {url name identifier} {

      #  Create a page for the query results.
      add_query_result_ $name $identifier

      #  Start the query in the background.
      catch {
         $itk_component(results$current_) config -title "Querying: $name"
         $itk_component(results$current_) clear
      }

      set_state disabled
      $itk_component(progressbar) config -text "Querying: $url"
      $itk_component(progressbar) look_busy

      $query_component_ query $url
   }

   #  Add a GaiaQueryResult to the notebook.
   protected method add_query_result_ {title identifier} {

      #  Create a new page and make it current.
      set site [$itk_component(notebook) add -label $title]
      set current_ $npages_
      set names_($current_) $title
      set ids_($current_) $identifier
      incr npages_
      incr ncolumn_

      if { ![ info exists $w_.cat$current_] } {
         astrocat $w_.cat$current_
      }

      itk_component add results$current_ {
         gaia::GaiaQueryResult $site.results \
            -astrocat [code $w_.cat$current_] \
            -title "Query Results" \
            -hscroll 1 \
            -height 12 \
            -sortcommand [code $this set_sort_cols] \
            -layoutcommand [code $this set_show_cols] \
            -selectmode extended \
            -exportselection 0
      } {
      }
      pack $itk_component(results$current_) -side top -fill both -expand 1
      add_short_help $itk_component(results$current_) \
         {Results of query to repository}

      #  Double click is same as "Open".
      bind $itk_component(results$current_).listbox <Double-1> \
         [code $this open]

      set cbreak 0
      if { $ncolumn_  > 25 } {
         set cbreak 1
         set ncolumn_ 0
      }

      #  Add to "Query Results:" menu.
      $itk_component(bookmenu) add \
         -command [code $this view_page_ $current_] \
         -label $title \
         -value $current_ \
         -columnbreak $cbreak
   }

   #  Respond to feedback about query progress (stop and start).
   protected method set_feedback {onoff} {
      if { $onoff == "off" } {
         $itk_component(progressbar) reset
      }
   }

   #  Add the component that will control the query parameters.
   protected method add_query_component_ {} {
      puts stderr "you must implement an add_query_component_ method"
   }

   #  Open a service, "args" is a list of values from a row of the
   #  current table.
   protected method open_service_ {args} {
      puts stderr "you must implemenent an open_service_ method"
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
      utilReUseWidget cat::ProxyDialog .proxy \
         -configfile $::env(HOME)/.skycat/proxies
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Columns to show. Defined once during initialisation.
   itk_option define -show_cols show_cols Show_Cols {}

   #  Command to execute when a response is activated. The command will
   #  be trailed by the accessURL value and the full row content. Normally
   #  this will open a window to do the actual service query for images etc.
   itk_option define -activate_cmd activate_cmd Activate_Cmd {}

   #  Define a help file and label for the Help menu.
   itk_option define -help_file help_file Help_File {}
   itk_option define -help_label help_label Help_Label "On Window..."

   #  Protected variables: (available to instance)
   #  --------------------

   #  List of catalogue column headings (from results of most recent query).
   protected variable headings_ {}

   #  Result from most recent query (list of rows).
   protected variable info_ {}

   #  Component that will perform the query.
   #  Must be defined in add_query_component_.
   protected variable query_component_ {}

   #  Number of pages in notebook.
   protected variable npages_ 0

   #  Current page of notebook.
   protected variable current_ -1

   #  Number of entries in the bookmenu column.
   protected variable ncolumn_ 0

   #  Names of pages for error dialogs etc.
   protected variable names_

   #  Identifiers of services.
   protected variable ids_

   #  True then interrupted.
   protected variable interrupted_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Flag: set to 1 after we checked for a proxy server.
   protected common checked_proxies_ 0
}
