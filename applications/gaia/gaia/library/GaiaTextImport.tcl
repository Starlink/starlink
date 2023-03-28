#+
#  Name:
#     GaiaTextImport

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Import a miscellaneous format text file as a TAB Table or
#     GaiaAstTable (and derived classes) text file.

#  Description:
#     This class opens a text file and interprets its contents
#     according to a series of configurable rules. The configuration
#     covers what columns names to use, what delimiters are used and
#     which columns are coordinates.
#
#     The result is a new file which has either the format of a TAB
#     table, or that used by the GaiaAstTable class (id ra dec x y).

#  Invocations:
#
#        GaiaTextImport object_name [configuration options]
#
#     This creates an instance of a GaiaTextImport object. The return is
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
#     See itk_option definitions below.

#  Methods:
#     See individual method declarations below.

#  Inheritance:
#     util::TopLevelWidget

#  Copyright:
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
#     29-MAR-2000 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaTextImport {}

itcl::class gaia::GaiaTextImport {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Set any defaults.
      set_defaults_

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      #  Add short help window
      make_short_help

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0

      #  Add the cancel menu item.
      $File add command -label Cancel \
         -command [code $this cancel] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this cancel]

      #  Add the accept menu item.
      $File add command -label Accept \
         -command [code $this accept] \
         -accelerator {Control-x}
      bind $w_ <Control-x> [code $this accept]

      #  Add window help.
      add_help_button importtext "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      #  Name of input file.
      if { $itk_option(-show_infile) } {
         itk_component add infile {
            gaia::LabelFileChooser $w_.infile \
               -labelwidth 9 \
               -text "Input file:" \
               -textvariable [scope itk_option(-infile)] \
               -command [code $this process_]
         }
         pack $itk_component(infile) -side top -fill x -ipadx 1m -ipady 2m
         add_short_help $itk_component(infile) \
            {Name of input text file}
      }

      #  Name of output file.
      if { $itk_option(-show_outfile) } {
         itk_component add outfile {
            gaia::LabelFileChooser $w_.outfile \
               -labelwidth 9 \
               -text "Output file:" \
               -textvariable [scope itk_option(-outfile)] \
               -command [code $this save_file_]
         }
         pack $itk_component(outfile) -side top -fill x -ipadx 1m -ipady 2m
         add_short_help $itk_component(outfile) \
            {Name of output catalogue}
      }

      #  If the result is a TAB table then we can specify the
      #  system and equinox of the values.
      if { $itk_option(-format) == "tab" } {

         itk_component add system {
            util::LabelMenu $w_.system \
               -text "System:" \
               -relief raised \
               -labelwidth 9
         }
         foreach system {fk5 fk4 fk4-no-e gappt ecliptic galactic
            supergalactic} {
            $itk_component(system) add \
               -label $system \
               -value $system
         }
         pack $itk_component(system) -side top -fill x -ipadx 1m -ipady 2m
         add_short_help $itk_component(system) {Celestial coordinate system}

         itk_component add equinox {
            gaia::LabelEntryMenu $w_.equinox \
               -text "Equinox:" \
               -labelwidth 9
         }
         foreach equinox {2000 1950} {
            $itk_component(equinox) add \
               -label $equinox \
               -value $equinox
         }
         pack $itk_component(equinox) -side top -fill x -ipadx 1m -ipady 2m
         add_short_help $itk_component(equinox) \
            {Equinox of imported coordinates (decimal years)}
      }

      #  Add notebook to contain the various controls.
      itk_component add notebook {
         ::iwidgets::tabnotebook $w_.notebook -tabpos w -width 410 -height 300
      }
      pack $itk_component(notebook) -side top -fill both -expand 1 \
         -ipadx 1m -ipady 1m

      #  Add a page for the structural format.
      $itk_component(notebook) add -label Structure
      set child [$itk_component(notebook) childsite 0]
      add_struct_controls_ $child

      #  Add a page for the column meanings/headings
      $itk_component(notebook) add -label Headings
      set child [$itk_component(notebook) childsite 1]
      add_heading_controls_ $child

      #  Add a page for showing the interpreted table.
      $itk_component(notebook) add -label Import
      set child [$itk_component(notebook) childsite 2]
      add_table_controls_ $child

      #  Add action buttons.
      itk_component add actions {
         frame $w_.actions
      }
      pack $itk_component(actions) -side top -fill x

      #  Update displayed table contents.
      itk_component add update {
         button $itk_component(actions).update -text Update \
            -command [code $this process_]
      }
      add_short_help $itk_component(update) \
         {Update table to reflect current state}
      pack $itk_component(update) -side left -expand 1 -pady 3 -padx 3

      #  Reset window to defaults.
      itk_component add reset {
         button $itk_component(actions).reset -text Reset \
            -command [code $this reset]
      }
      add_short_help $itk_component(reset) \
         {Reset window to builtin defaults}
      pack $itk_component(reset) -side left -expand 1 -pady 3 -padx 3

      #  Cancel window returning no new file.
      itk_component add cancel {
         button $itk_component(actions).cancel -text Cancel \
            -command [code $this cancel]
      }
      add_short_help $itk_component(cancel) \
         {Close window cancelling import}
      pack $itk_component(cancel) -side left -expand 1 -pady 3 -padx 3

      #  Close window accepting import.
      itk_component add accept {
         button $itk_component(actions).accept -text Accept \
            -command [code $this accept]
      }
      add_short_help $itk_component(accept) \
         {Close window accepting import}
      pack $itk_component(accept) -side left -expand 1 -pady 3 -padx 3

      #  View the first page.
      $itk_component(notebook) view 0

      #  Run-time initialisations
      set_separated_
      set values_(id) {}
      set values_(ra) {}
      set values_(dec) {}
      set values_(x) {}
      set values_(y) {}
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Set defaults for widget states.
   protected method set_defaults_ {} {
      set values_(separated) 1
      set values_(delimiter) " "
      set values_(comment) "\#"
      set values_(fixwidths) {10 20}
      set values_(skip) 0
   }

   #  Close window writing output file.
   public method accept {} {
      if { [save_file_] } {
         wm withdraw $w_
         incr saved_
      }
   }

   #  Close window without writing file.
   public method cancel {} {
      wm withdraw $w_
      incr saved_
   }

   #  Activate interface, waiting until window is closed. Return the
   #  filename and which coordinates where found.
   public method activate {} {
      wm deiconify $w_
      tkwait variable [scope saved_]
      return [list $itk_option(-outfile) $raout_ $decout_ $xout_ $yout_]
   }

   #  Reset window to defaults.
   public method reset {} {
      set_defaults_
      clear_headings_
      $itk_component(table) clear
   }

   #  Add input file structure controls.
   protected method add_struct_controls_ {parent} {
      set lwidth 12

      #  Delimitered or fixed format.
      itk_component add separated {
         gaia::StarLabelCheck $parent.separated \
            -text "Use delimiter (otherwise fixed width):" \
            -onvalue 1 \
            -offvalue 0 \
            -variable [scope values_(separated)] \
            -command [code $this set_separated_]
      }
      pack $itk_component(separated) -side top -fill x -pady 3 -padx 3
      add_short_help $itk_component(separated) \
         {Use a delimiter between fields, fixed width otherwise}

      #  Set the delimiter.
      itk_component add delimiter {
         util::LabelMenu $parent.delimiter \
            -labelwidth $lwidth \
            -text "Delimiter:" \
            -variable [scope values_(delimiter)]
      }
      pack $itk_component(delimiter) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(delimiter) \
         {The field delimiter character(s)}
      foreach {sname value} {space " " comma "," tab "\t" semicolon ";"} {
         $itk_component(delimiter) add \
            -label $sname \
            -value $value \
            -command [code $this set_delimiter_ $value]
      }
      set_delimiter_ { }

      #  Comment delimiter.
      itk_component add comment {
         util::LabelEntry $parent.comment \
            -labelwidth $lwidth \
            -text "Comment:" \
            -textvariable [scope values_(comment)] \
            -value $values_(comment)
      }
      pack $itk_component(comment) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(comment) \
         {The comment indicator character(s)}

      #  Number of lines to skip at beginning of file.
      itk_component add skip {
         util::LabelEntry $parent.skip \
            -labelwidth $lwidth \
            -text "Header lines:" \
            -textvariable [scope values_(skip)] \
            -value $values_(skip) \
            -validate integer
      }
      pack $itk_component(skip) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(skip) \
         {Number of lines to skip at head of file (includes comments)}

      #  Fixed width control.
      itk_component add fixwidths {
         util::LabelEntry $parent.fixwidths \
            -labelwidth $lwidth \
            -text "Field positions:" \
            -textvariable [scope values_(fixwidths)] \
            -value $values_(fixwidths) \
            -command [code $this update_fixed_ to]
      }
      pack $itk_component(fixwidths) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(fixwidths) \
         {Positions of field separation: position1 position2 ... }

      #  Interactive tab positioning widget.
      itk_component add tabstop {
         gaia::GaiaTabStops $parent.tabstop \
            -change_cmd [code $this update_fixed_ from] \
            -text "No information, press update"
      }
      $itk_component(tabstop) setindices $values_(fixwidths)
      pack $itk_component(tabstop) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(tabstop) \
         {Fixed width positions (can interactively adjust/create/delete)}
   }


   #  Toggle the state of the delimiter fields.
   protected method set_separated_ {} {
      if { $values_(separated) } {
         #  Using delimiter
         $itk_component(delimiter) configure -state normal
         $itk_component(fixwidths) configure -state disabled
         $itk_component(tabstop) configure -state disabled
      } else {
         #  Using fixed format
         $itk_component(delimiter) configure -state disabled
         $itk_component(fixwidths) configure -state normal
         $itk_component(tabstop) configure -state normal
      }
   }

   #  Set/reset the fixed widths.
   protected method update_fixed_ {action value} {
      if { $action == "from" } {
         set values_(fixwidths) $value
      } else {
         $itk_component(tabstop) setindices \
            [$itk_component(fixwidths) get]
      }
   }

   #  Set the delimiter symbol.
   protected method set_delimiter_ {value} {
      set values_(delimiter) $value
   }

   #  Heading names and types controls.
   protected method add_heading_controls_ {parent} {

      #  Create a scrollable region for showing all columns.
      itk_component add headregion {
         ::iwidgets::scrolledframe $parent.headregion -width 75 -height 400
      }
      pack $itk_component(headregion) -fill both -expand 1
      set headparent_ [$itk_component(headregion) childsite]
   }

   #  Add field for each column name. Also add radio buttons to select
   #  amongst RA/Dec and X/Y. These all exist in a table.
   protected method add_headings_ {} {

      itk_component add headtable {
         frame $headparent_.headtable
      }
      set parent $itk_component(headtable)
      itk_component add head0 {
         label $parent.head0 -text "Column name"
      }
      itk_component add head1 {
         label $parent.head1 -text "ID"
      }
      itk_component add head2 {
         label $parent.head2 -text "RA"
      }
      itk_component add head3 {
         label $parent.head3 -text "Dec"
      }
      itk_component add head4 {
         label $parent.head4 -text "X"
      }
      itk_component add head5 {
         label $parent.head5 -text "Y"
      }
      blt::blttable $itk_component(headtable) \
         $itk_component(head0)  0,0 \
         $itk_component(head1)  0,1 \
         $itk_component(head2)  0,2 \
         $itk_component(head3)  0,3 \
         $itk_component(head4)  0,4 \
         $itk_component(head5)  0,5

      #  Add the entry fields and radiobuttons.
      for {set i 0;set j 1} {$i < $ncolumns_} {incr i; incr j} {

         #  Keep old column names if any exist.
         if { ! [info exists values_(heading$i)] } {
            set values_(heading$i) "Col$i"
         }
         itk_component add col$i {
            util::LabelEntry $parent.col$i \
               -labelwidth 2 \
               -text "$i:" \
               -textvariable [scope values_(heading$i)] \
               -value $values_(heading$i)
         }
         itk_component add id$i {
            radiobutton $parent.id$i \
               -variable [scope values_(id)] \
               -value $i
         }
         itk_component add ra$i {
            radiobutton $parent.ra$i \
               -variable [scope values_(ra)] \
               -value $i
         }
         itk_component add dec$i {
            radiobutton $parent.dec$i \
               -variable [scope values_(dec)] \
               -value $i
         }
         itk_component add x$i {
            radiobutton $parent.x$i \
               -variable [scope values_(x)] \
               -value $i
         }
         itk_component add y$i {
            radiobutton $parent.y$i \
               -variable [scope values_(y)] \
               -value $i
         }

         blt::blttable $itk_component(headtable) \
            $itk_component(col$i) $j,0 -fill x -pady 3 -padx 3 \
            $itk_component(id$i) $j,1 -fill x -pady 3 -padx 3 \
            $itk_component(ra$i) $j,2 -fill x -pady 3 -padx 3 \
            $itk_component(dec$i) $j,3 -fill x -pady 3 -padx 3 \
            $itk_component(x$i) $j,4 -fill x -pady 3 -padx 3 \
            $itk_component(y$i) $j,5 -fill x -pady 3 -padx 3
      }
      pack $itk_component(headtable) -fill both -expand 1
      add_short_help $itk_component(headtable) \
         {Set column names and identify the RA/Dec, X/Y coordinates}

      #  Make the <2> button clear a radiobutton column (no other way
      #  of backing out of pressing one).
      bind Radiobutton <2> {%W deselect}
   }

   #  Remove existing headings controls.
   protected method clear_headings_ {} {
      if { [info exists itk_component(headtable)] &&
           [winfo exists $itk_component(headtable)] } {
         catch {destroy $itk_component(headtable)}
      }
   }

   #  Add a TableList object to display the decoded text file.
   protected method add_table_controls_ {parent} {
      itk_component add table {
         util::TableList $parent.table \
            -title "Input file data" \
            -hscroll 1
      }
      add_short_help $itk_component(table) \
         {Data read from input file}
      pack $itk_component(table) -side top -fill both -expand 1
   }

   #  Update the table by decoding the input file.
   protected method update_table_ {} {

      #  Remove existing contents
      $itk_component(table) clear

      #  Set the table headings.
      set headings ""
      for {set i 0} {$i < $ncolumns_} {incr i} {
         append headings "$values_(heading$i) "
      }
      $itk_component(table) configure -headings $headings

      #  Add each decoded line. Open the file.
      set fid [::open $itk_option(-infile) r]
      set count 0
      set first 1
      while { 1 } {
         incr count
         set llen [gets $fid line]
         if { $count > $values_(skip) } {
            if { $llen > 0 } {
               if { ! [string match "$values_(comment)*" $line] } {
                  if { $values_(separated) } {
                     parseline_ $line
                  } else {
                     splitline_ $line
                  }
                  $itk_component(table) append_row $wordlist_
                  if { $first } {
                     $itk_component(tabstop) configure -text $line
                     set first 0
                  }
               }
            } elseif { $llen < 0 } {
               break
            }
         }
      }
      ::close $fid
      $itk_component(table) new_info
   }

   #  Run the decode cycle. Opens input file, reads first line and
   #  decodes it (to count words), if using a delimiter. Otherwise
   #  just counts the fixed width words.
   protected method process_ {args} {

      #  Remove old controls.
      clear_headings_

      #  If using a delimiter.
      if { $values_(separated) } {

         #  Open the file.
         if { $itk_option(-infile) != "" } {
            set fid [::open $itk_option(-infile) r]
            set count 0
            while { 1 } {
               incr count
               set llen [gets $fid line]
               if { $count > $values_(skip) } {
                  if { $llen > 0 } {
                     if { ! [string match "$values_(comment)*" $line] } {
                        break
                     }
                  }
               }
            }
            set ncolumns_ [parseline_ $line]
            ::close $fid
         } else {
            info_dialog "You need to select an input file"
            return
         }
      } else {

         #  Fixed width entries.
         set fixwidths [split $values_(fixwidths) " "]
         set ncolumns_ [llength $fixwidths]
      }
      add_headings_
      update_table_
   }

   #  Parse a string into delimitered words, returning a count of the
   #  number of words located. The results are stored as a list
   #  "wordlist_".
   protected method parseline_ {line} {
      set wordlist_ ""
      if { $line != {} } {
         set ncol 0
         while { 1 } {
            set word [ctoken line $values_(delimiter)]
            if { $word != {} } {
               incr ncol
               lappend wordlist_ $word
            } else {
               break
            }
         }
      } else {
         set ncol 0
      }
      return $ncol
   }

   #  Split a line into words using the fixed width position
   #  boundaries.
   protected method splitline_ {line} {
      set wordlist_ {}
      if { $line != {} } {
         set last 0
         foreach pos $values_(fixwidths) {
            incr pos -1
            lappend wordlist_ [string range $line $last $pos]
            set last [expr $pos+1]
         }
      }
   }

   #  Save the converted contents to a TAB catalogue.
   protected method save_file_ {args} {
      if { $itk_option(-outfile) != {} && $itk_option(-infile) != {} } {
         set outid [::open $itk_option(-outfile) w]
         set inid [::open $itk_option(-infile) r]
         set raout_ -1
         set decout_ -1
         set xout_ -1
         set yout_ -1
         if { $itk_option(-format) == "tab" } {

            #  Write a TAB table.
            puts $outid "QueryResult"
            puts $outid "serv_type: catalog"
            puts $outid "long_name: conversion of text file: $itk_option(-infile)"
            puts $outid "short_name: convert@text"
            puts $outid "url: $itk_option(-outfile)"
            if { $values_(id) != {} } {
               puts $outid "id_col: $values_(id)"
            }
            if { $values_(ra) != {} } {
               puts $outid "ra_col: $values_(ra)"
               set raout_ 1
            }
            if { $values_(dec) != {} } {
               puts $outid "dec_col: $values_(dec)"
               set decout_ 1
            }
            if { $values_(ra) != {} || $values_(dec) != {} } {
               puts $outid "equinox: [$itk_component(equinox) get]"
               puts $outid "system: [$itk_component(system) get]"
            }
            if { $values_(x) != {} } {
               puts $outid "x_col: $values_(x)"
               set xout_ 1
            }
            if { $values_(y) != {} } {
               puts $outid "y_col: $values_(y)"
               set yout_ 1
            }
            set headings ""
            for {set i 0} {$i < $ncolumns_} {incr i} {
               append headings "$values_(heading$i)\t"
            }
            if { $values_(ra) != {} && $values_(dec) != {}
                 ||
                 $values_(x) != {} && $values_(y) != {} } {
               puts $outid "symbol: {} circle 4"
            }

            puts $outid "$headings"
            puts $outid "---------"
            set count 0
            while { 1 } {
               incr count
               set llen [gets $inid line]
               if { $count > $values_(skip) } {
                  if { $llen > 0 } {
                     if { ! [string match \
                                "$values_(comment)*" $line]  } {
                        if { $values_(separated) } {
                           parseline_ $line
                        } else {
                           splitline_ $line
                        }
                        puts $outid [join $wordlist_ "\t"]
                     }
                  } elseif { $llen < 0 } {
                     break
                  }
               }
            }
            puts $outid "EOD"
         } else {

            #  Write an astrometry table. This needs the coordinates
            #  to be identified, otherwise we use defaults.
            set id -1
            set ra -1
            set dec -1
            set x -1
            set y -1
            if { $values_(id) != {} } {
               set id $values_(id)
            }
            if { $values_(ra) != {} } {
               set raout_ $values_(ra)
            }
            if { $values_(dec) != {} } {
               set decout_ $values_(dec)
            }
            if { $values_(x) != {} } {
               set xout_ $values_(x)
            }
            if { $values_(y) != {} } {
               set yout_ $values_(y)
            }

            #  Set defaults, if RA/DEC etc. not selected. Note these
            #  are only used if not values are set. If any are set
            #  already then obviously the user doesn't have any other
            #  columns.
            if { $ncolumns_ >= 5 } {

               #  Five columns or more. Defaults are id, ra, dec, x, y.
               if { $id == -1 && $raout_ == -1 && $decout_ == -1 &&
                    $xout_ == -1 && $yout_ == -1} {
                  set values_(id) 0
                  set id 0
                  set values_(ra) 1
                  set raout_ 1
                  set values_(dec) 2
                  set decout_ 2
                  set values_(x) 3
                  set xout_ 3
                  set values_(y) 4
                  set yout_ 4
               }
            } elseif { $ncolumns_ == 4 } {

               #  Only four columns. Defaults are ra, dec, x, y.
               if { $id == -1 && $raout_ == -1 && $decout_ == -1 &&
                    $xout_ == -1 && $yout_ == -1} {
                  set values_(ra) 0
                  set raout_ 0
                  set values_(dec) 1
                  set decout_ 1
                  set values_(x) 2
                  set xout_ 2
                  set values_(y) 3
                  set yout_ 3
               }
            } elseif { $ncolumns_ == 3 } {

               #  Only three columns. Defaults are id, ra, dec.
               if { $id == -1 && $raout_ == -1 && $decout_ == -1 &&
                    $xout_ == -1 && $yout_ == -1} {
                  set values_(id) 0
                  set id 0
                  set values_(ra) 1
                  set raout_ 1
                  set values_(dec) 2
                  set decout_ 2
               }
            }

            #  Extract/generate values.
            set count 0
            while { 1 } {
               incr count
               set llen [gets $inid line]
               if { $count > $values_(skip) } {
                  if { $llen > 0 } {
                     if { ! [string match "$values_(comment)*" $line] } {
                        if { $values_(separated) } {
                           parseline_ $line
                        } else {
                           splitline_ $line
                        }
                        if { $id != -1 } {
                           set vid [lindex $wordlist_ $id]
                        } else {
                           set vid $count
                        }
                        if { $raout_ != -1 } {
                           set vra [lindex $wordlist_ $raout_]
                        } else {
                           set vra "00:00:00"
                        }
                        if { $decout_ != -1 } {
                           set vdec [lindex $wordlist_ $decout_]
                        } else {
                           set vdec "00:00:00"
                        }
                        if { $xout_ != -1 } {
                           set vx [lindex $wordlist_ $xout_]
                        } else {
                           set vx 0.0
                        }
                        if { $yout_ != -1 } {
                           set vy [lindex $wordlist_ $yout_]
                        } else {
                           set vy 0.0
                        }
                        puts $outid "$vid $vra $vdec $vx $vy"
                     }
                  } elseif { $llen < 0 } {
                     break
                  }
               }
            }
         }
         ::close $inid
         ::close $outid
      } else {
         if { $itk_option(-infile) == {} } {
            error_dialog "You must supply an input file name"
         } else {
            error_dialog "You must supply an output file name"
         }
         return 0
      }
      return 1
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Output file format. This is either a TAB table or an astrometry
   #  table. Valid values are: "tab" or "ast".
   itk_option define -format format Format {tab} {
      if { $itk_option(-format) != "tab" &&
           $itk_option(-format) != "ast" } {
         set itk_option(-format) "tab"
      }
   }

   #  Name of the input text file.
   itk_option define -infile infile Infile {}

   #  Whether input file selection is visible.
   itk_option define -show_infile show_infile Show_infile 1

   #  Name of the output catalogue.
   itk_option define -outfile outfile Outfile {}

   #  Whether output file selection is visible.
   itk_option define -show_outfile show_outfile Show_outfile 1

   #  Protected variables: (available to instance)
   #  --------------------

   #  Number columns in or expected in the input file.
   protected variable ncolumns_ -1

   #  Parent of headings controls (scrolled region).
   protected variable headparent_ {}

   #  Current list of parsed words.
   protected variable wordlist_

   #  The coordinates (ra,dec,x,y) found when the output file
   #  was written.
   protected variable raout_ -1
   protected variable decout_ -1
   protected variable xout_ -1
   protected variable yout_ -1

   #  Variable to trace when output file is written.
   protected variable saved_ 0

   #  Values shared by widgets -- indexed by (fieldname).
   protected variable values_

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
