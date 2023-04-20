#+

#  Name:
#     SampSearch

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Displays a table that originated from a SAMP message.

#  Description:
#     This class extends GaiaSearch to provide methods required for
#     subsequent SAMP control of a catalogue which has been acquired
#     through the SAMP system.

#  Inheritance:
#     gaia::GaiaSearch

#  Copyright:
#     Copyright (C) 2009 Science and Technology Facilities Council.
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
#     MBT: Mark Taylor (Bristol University)
#     {enter_new_authors_here}

#  History:
#     24-JUN-2009 (MBT):
#        Original version.
#     {enter_further_changes_here}

#-

itk::usual SampSearch {}

itcl::class gaia::SampSearch {
   inherit gaia::GaiaSearch

   #  Constructor.
   constructor {args} {
      eval itk_initialize $args
   }

   #  Initialise widget.
   public method init {} {
      gaia::GaiaSearch::init

      set samp_client_ [gaia::Gaia::get_samp_client]
      if {$samp_client_ != ""} {

         # Add a new menu for SAMP-related activities.
         set interopmenu_ [add_menubutton Interop]
         set m $interopmenu_
         configure_menubutton Interop -underline 6
         add_short_help $itk_component(menubar).interop \
            {Interop menu: catalogue-related tool interoperability using SAMP}

         $m add checkbutton -label "Selection broadcasts row index" \
            -variable [scope send_rows]
         $m add checkbutton -label "Selection broadcasts row position" \
            -variable [scope send_radecs]

         $m add separator

         set send_info_menu [menu $m.send_info]
         add_menuitem $m command "Broadcast current search" \
            {Send all visible rows to SAMP applications} \
            -command [code $this transmit_info {}]
         add_menuitem $m cascade "Send current search" \
            {Send all visible rows to a selected SAMP application} \
            -menu $send_info_menu

         set send_selection_menu [menu $m.send_selection]
         add_menuitem $m command "Broadcast current selection" \
            {Send current row selection to SAMP applications} \
            -command [code $this transmit_selection {}]
         add_menuitem $m cascade "Send current selection" \
            {Send current row selection to a selected SAMP application} \
            -menu $send_selection_menu

         #  Arrange for the menu to be kept up to date with the current
         #  state of the SAMP connection.
         $samp_client_ reg_change_command [code $this samp_reg_changed_]
         samp_reg_changed_
         set tracker [$samp_client_ cget -client_tracker]
         {*}$tracker client_change_command [code $this samp_client_changed_]
         samp_client_changed_
      }

      #  Store the initial set of rows in two ways: first as a normal
      #  list of the row contents, and second as an array of row indices
      #  keyed by the row contents.  Both of these may be required for
      #  efficient operations later on.  Although this set of rows may
      #  not continue to match the set currently displayed by this widget,
      #  it is the set with reference to which SAMP messages relating
      #  to the table contained here must be understood.
      set idx 0
      set all_rows_ [$w_.cat query]
      foreach row $all_rows_ {
         set all_row_idxs_($row) $idx
         incr idx
      }
   }

   #  Selects the rows identified by a given list of integer values.
   public method select_indices {idx_list} {
      busy {
         set info {}
         foreach idx $idx_list {
            lappend info [lindex $all_rows_ $idx]
         }
         set_info $headings_ $info
      }
   }

   #  Highlights one of the rows in the table represented by this object.
   #  The given idx value is the index of the row in the full table as
   #  originally loaded.
   public method highlight_index {idx} {

      #  Work out the index into the current search result.  This isn't
      #  very efficient (search through all rows for an identical match)
      #  but I don't see how else it can be done using the existing data
      #  structures available to this class.
      set row [lindex $all_rows_ $idx]
      set info_idx [lsearch $info_ $row]
      if {$info_idx >= 0} {

         #  Highlight the chosen row in the table viewer widget.
         $results_ select_row $info_idx

         #  Highlight the associated symbol on the image widget.
         set id_col [$w_.cat id_col]
         if {$id_col >= 0} {
            set id [lindex $row $id_col]
            if {$id != ""} {
               select_symbol cat$id 0 $info_idx
            }
         }
      }
   }

   #  Override select_result_row to perform additional SAMP-related
   #  row highlighting activities.
   public method select_result_row {} {
      skycat::SkySearch::select_result_row
      activate_selected_row
   }

   #  Override select_symbol to perform additional SAMP-related
   #  row highlighting activities.
   public method select_symbol {id toggle {rownum -1}} {
      skycat::SkySearch::select_symbol $id $toggle $rownum
      activate_selected_row
   }

   #  Transmits the current selection to other SAMP clients.
   public method transmit_selection {recipient_id} {
      transmit_rows [$results_ get_selected] $recipient_id
   }

   #  Transmits the current search result to other SAMP clients.
   public method transmit_info {recipient_id} {
      transmit_rows $info_ $recipient_id
   }

   #  Transmits a list of rows as a list of indices to be highlighted
   #  in other SAMP clients.
   public method transmit_rows {rows recipient_id} {
      if {[catch {
         set sender [gaia::Gaia::get_samp_sender]
         if {$sender != "" && $table_id != ""} {
            set idx_list {}
            busy {
               foreach row $rows {
                  if {[info exists all_row_idxs_($row)]} {
                     lappend idx_list $all_row_idxs_($row)
                  }
               }
               $sender send_selection $table_id $idx_list $recipient_id
            }
         }
      } msg]} {
         puts "selection send error: $msg"
      }
   }

   #  Invoked when the row selection may have changed.  According to the
   #  current state of this object, it may transmit messages to the
   #  SAMP system concerning the new row selection.
   public method activate_selected_row {} {

      #  Send the row index via SAMP if required.
      if {$is_reg_ && $send_rows} {
         if {[catch {
            set sender [gaia::Gaia::get_samp_sender]
            if {$sender != "" && $table_id != ""} {
               set rows [$results_ get_selected]
               if {[llength $rows] == 1} {
                  set row [lindex $rows 0]
                  if {[info exists all_row_idxs_($row)]} {
                     $sender send_row $table_id $all_row_idxs_($row) {}
                  }
               }
            }
         } msg]} {
            puts "row activation error: $msg"
         }
      }

      #  Send the row sky position via SAMP if required.
      if {$is_reg_ && $send_radecs} {
         if {[catch {
            set sender [gaia::Gaia::get_samp_sender]
            set ra_col [$w_.cat ra_col]
            set dec_col [$w_.cat dec_col]
            if {$sender != "" && $table_id != "" &&
                $ra_col >= 0 && $dec_col >= 0} {
                set rows [$results_ get_selected]
                if {[llength $rows] == 1} {
                   set row [lindex $rows 0]
                   set ra  [lindex $row $ra_col]
                   set dec [lindex $row $dec_col]
                   $sender send_radec $ra $dec {}
                }
            }
         } msg]} {
            puts "row activation error: $msg"
         }
      }
   }

   #  Called when the SAMP connection goes up or down.
   protected method samp_reg_changed_ {} {

      #  Configure the menu items all enabled/disabled according to whether
      #  there is a SAMP connection.
      if {$samp_client_ != ""} {
         set is_reg_ [$samp_client_ is_registered]
      } else {
         set is_reg_ 0
      }
      set when_reg [expr {$is_reg_ ? "normal" : "disabled"}]
      set nitem [$interopmenu_ index last]
      for {set item 0} {$item <= $nitem} {incr item} {
         catch {$interopmenu_ entryconfigure $item -state $when_reg}
      }
   }

   #  Called when external clients register or unregister with SAMP or
   #  change their metadata or subscriptions.
   protected method samp_client_changed_ {} {

      #  Configure some of the cascade menus so that they have one entry
      #  for each of the currently subscribed SAMP clients.
      set selection_menu $interopmenu_.send_selection
      set info_menu $interopmenu_.send_info
      $info_menu delete 0 last
      $selection_menu delete 0 last
      if {[$samp_client_ is_registered]} {
         set tracker [$samp_client_ cget -client_tracker]
         set mtype "table.select.rowList"
         set subscribed [{*}$tracker get_subscribed_clients $mtype]
         foreach client_id $subscribed {
            set client_name [{*}$tracker get_name $client_id]
            add_menuitem $info_menu command "Send to $client_name" \
               "Send all visible rows to $client_name" \
               -command "[code $this transmit_info $client_id]"
            add_menuitem $selection_menu command "Send to $client_name" \
               "Send current row selection to $client_name" \
               -command "[code $this transmit_selection $client_id]"
         }
      }
   }

   #  ID of the table loaded into this widget.  This is the ID used by
   #  SAMP messages to identify the table.
   public variable table_id {}

   #  Whether to broadcast each row index when it is highlighted.
   public variable send_rows 1

   #  Whether to broadcast each row sky position when it is highlighted.
   public variable send_radecs 0

   #  List of all the rows in the initial table
   protected variable all_rows_

   #  Array with each row in the initial table as keys and row index as values.
   protected variable all_row_idxs_

   #  SampClient object used for SAMP communications.
   protected variable samp_client_

   #  Whether we are currently registered with the SAMP hub.
   protected variable is_reg_ 0

   #  Interoperability menu.
   protected variable interopmenu_
}
