#+
#  Name:
#     GaiaVOTableSet

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Viewer for VODataService XML documents.

#  Description:
#     Displays a VODataService XML document as a series of tables
#     found within a <tableset> element. These are associated with
#     various schema, but we just offer all <table>'s that are found.
#     Tables have the interesting properties of <column> name, plus
#     units and ucd, utypes etc.

#  Invocations:
#
#        GaiaVOTableSet object_name [configuration options]
#
#     This creates an instance of a GaiaVOTableSet object. The return is
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

#  Configuration options:
#     See itk_option define statements.

#  Methods:
#     See method definitions below.

#  Inheritance:
#     TopLevelWidget

#  Copyright:
#     Copyright (C) 2014 Science and Technology Research Council.
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
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     07-AUG-2014 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaVOTableSet {}

itcl::class gaiavo::GaiaVOTableSet {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

       #  Evaluate any options.
       eval itk_initialize $args

       #  Add a short help window.
       make_short_help

       #  Add menubar.
       add_menubar

       #  File menu.
       set m [add_menubutton File]

       add_menuitem $m command "Close" \
          {Close this window} \
          -command [code $this close]

   }

   #  Destructor:
   #  -----------
   destructor  {

   }

   #  Methods:
   #  --------

   #  Complete interface construction.
   public method init {} {

      #  Help menu.
      add_help_menu_

      #  Add the table for displaying all the tables.
      itk_component add tables {
         TableList $w_.tables \
            -title "Tables" \
            -hscroll 1 \
            -selectmode $selectmode_ \
            -exportselection $exportselection_ \
            -headings {name description} \
            -height 5 \
            -width 80
      }
      add_short_help $itk_component(tables) {Tables available}

      #  Set table binding edit the entries.
      bind $itk_component(tables).listbox <1> [code $this view_selected_]
      bind $itk_component(tables).listbox <<ListboxSelect>> \
         [code $this view_selected_]

      #  Add the table for displaying the current table.
      itk_component add table {
         TableList $w_.table \
            -title "Current table" \
            -hscroll 1 \
            -selectmode $selectmode_ \
            -exportselection $exportselection_ \
            -headings {column description datatype units ucd utype xtype} \
            -height 15 \
            -width 80
      }
      add_short_help $itk_component(table) \
         {Columns and descriptions in the current table}

      #  Add frames for holding table action buttons.
      itk_component add actionbar {
         frame $w_.actionbar
      }

      #  Close window.
      itk_component add close {
         button $itk_component(actionbar).close \
            -text "Close" \
            -command [code $this close]
      }
      pack $itk_component(close) -side left -expand 1 -pady 2m
      add_short_help $itk_component(close) {Close window}

      #  Pack all widgets into place.
      pack $itk_component(tables) -side top -fill both -expand 1
      pack $itk_component(table) -side top -fill both -expand 1
      pack $itk_component(actionbar) -side top -fill x -pady 3 -padx 3

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

   #  Close this window.
   public method close {} {
      wm withdraw $w_
   }

   #  Return number of rows.
   public method total_rows {main} {
      if { $main } {
         return [$itk_component(tables) total_rows]
      } else {
         return [$itk_component(table) total_rows]
      }
   }

   #  Return contents of table.
   public method get_contents {main} {
      if { $main } {
         return [$itk_component(tables) get_contents]
      } else {
         return [$itk_component(table) get_contents]
      }
   }

   #  Return selected contents of table. Only one row allowed.
   public method get_selected {main} {
      if { $main } {
         return [lindex [$itk_component(tables) get_selected] 0]
      } else {
         return [lindex [$itk_component(table) get_selected] 0]
      }
   }

   #  Set the contents of the table.
   public method set_contents { main rows } {
      if { $main } {
         $itk_component(tables) append_rows $rows
      } else {
         $itk_component(table) append_rows $rows
      }
   }

   #  Append a new row to the table. Call new_info when completed.
   public method append_row {main row} {
      if { $main } {
         $itk_component(tables) append_row $row
      } else {
         $itk_component(table) append_row $row
      }
   }
   public method new_info {main} {
      if { $main } {
         $itk_component(tables) new_info
      } else {
         $itk_component(table) new_info
      }
   }

   #  Select the row with the given id.
   public method select_row {main id} {
      if { $main } {
         $itk_component(tables) search id $id
      } else {
         $itk_component(table) search id $id
      }
   }

   #  Clear the tables.
   public method clear_table {main} {
      if { $main } {
         $itk_component(tables) clear
      } else {
         $itk_component(table) clear
      }
   }
   public method clear_tables {} {
      $itk_component(tables) clear
      $itk_component(table) clear
   }

   #  Write a copy of the table to an ordinary file.
   public method write_to_file {} {
      set w [FileSelect .\#auto -title "Write positions to file"]
      if {[$w activate]} {
         set filename [$w get]
         if { [file exists $filename] } {
            if { ! [ confirm_dialog \
                     "$filename exists - do you want to overwrite?"] } {
               destroy $w
               return
            }
         }
         save_positions $filename
      }
      destroy $w
   }

   #  Write table contents to a named file.
   public method save_positions {filename} {
      set fid [::open $filename w]
      set nrows [$itk_component(table) total_rows]
      set contents [$itk_component(table) get_contents]
      for { set i 0 } { $i < $nrows } { incr i } {
         puts $fid [lindex $contents $i]
      }
      ::close $fid
   }

   #  XML handling.

   #  Parse the document after updating the XML.
   public method update {} {
      clear_tables
      if { $itk_option(-xml) != {} } {
         parse_doc_
      }
   }

   #  Parse the XML into lists of tables and the table's columns.
   protected method parse_doc_ {} {
      if { [ catch { set doc [dom parse $itk_option(-xml)] } msg ] } {
         error_dialog $msg
         return
      }

      #  Now find all the <table>s.
      set root [$doc documentElement]

      #  Don't do this, some responses are not correct, but still usable.
      #if { ! [string match -nocase "*tableset" [$root nodeName]] } {
      #   error_dialog "root of document not a tableset: [$root nodeName]"
      #   return
      #}
      set tables [$root selectNodes "//table"]

      #  Gather names and descriptions.
      set info {}
      array unset tablelist_
      foreach table $tables {
         set name [get_node_text_ $table "name"]

         #  We require a name.
         if { $name != {} } {
            set desc [get_node_text_ $table "description"]
            set desc [clean_ $desc]
            lappend info [list $name $desc]

            #  Keep indexed list of table nodes so we can pick out content as
            #  needed.
            set tablelist_($name) $table
         }
      }

      set_contents 1 $info
      new_info 1

      #  Select first row.
      $itk_component(tables) select_row 1
      ::update idletasks
      view_selected_
   }

   #  View the properties of the selected table.
   protected method view_selected_ {} {
      set selected [get_selected 1]
      set name [lindex $selected 0]
      if { [info exists tablelist_($name)] } {
         parse_table_ $name
      }
   }

   #  Extract and view the properties of the given table.
   protected method parse_table_ {name} {
      clear_table 0

      set table $tablelist_($name)

      #  Get all the columns and create rows describing each.
      set info {}
      set columns [$table selectNode "column"]
      foreach column $columns {
         catch {
            set name [get_node_text_ $column "name"]
            set desc [get_node_text_ $column "description"]
            set desc [clean_ $desc]
            set datatype [get_node_text_ $column "dataType"]
            set units [get_node_text_ $column "unit"]
            set ucd [get_node_text_ $column "ucd"]
            set utype [get_node_text_ $column "utype"]
            set xtype [get_node_text_ $column "xtype"]

            lappend info [list $name $desc $datatype $units $ucd $utype $xtype]
         }
      }
      set_contents 0 $info
      new_info 0
   }

   #  Get all text from a named child.
   protected method get_node_text_ {parent name} {
      set node [$parent selectNode $name]
      if { $node != {} } {
         return [$node text]
      }
      return {}
   }


   #  Clean a string, removing funny characters and trimming.
   protected method clean_ {str} {
      regsub -all {[^\w\d\s\.]} $str {} str
      return [string trim $str]
   }


   #  Configuration options
   #  =====================

   #  The XML document. Use update if changed after creation.
   itk_option define -xml xml Xml {}

   #  Define a help file and label for the Help menu.
   itk_option define -help_file help_file Help_File {}
   itk_option define -help_label help_label Help_Label "On Window..."

   #  Protected variables: (available to instance)
   #  --------------------

   #  Table elements of the XML document, indexed by the table name.
   protected variable tablelist_

   #  Table configuration.
   protected variable selectmode_ browse
   protected variable exportselection_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
