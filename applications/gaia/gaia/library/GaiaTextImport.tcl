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
#     covers what columns names to use, what delimeters are used and
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

      #  Set title of window.
      wm title $w_ "GAIA: Text file import ($itk_option(-number))"

      #  Add short help window
      make_short_help

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File" left]
      configure_menubutton File -underline 0

      #  Add the cancel menu item.
      $File add command -label Cancel \
         -command [code $this cancel] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this cancel]

      #  Add the close menu item.
      $File add command -label Close \
         -command [code $this close] \
         -accelerator {Control-x}
      bind $w_ <Control-x> [code $this close]

      #  Name of input file.
      if { $itk_option(-show_infile) } {
         itk_component add infile {
            LabelFileChooser $w_.infile \
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
            LabelFileChooser $w_.outfile \
               -labelwidth 9 \
               -text "Output file:" \
               -textvariable [scope itk_option(-outfile)] \
               -command [code $this save_file_]
         }
         pack $itk_component(outfile) -side top -fill x -ipadx 1m -ipady 2m
         add_short_help $itk_component(outfile) \
            {Name of output catalogue}
      }

      #  Add notebook to contain the various controls.
      itk_component add notebook {
         tabnotebook $w_.notebook -tabpos w -width 410 -height 300
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
      itk_component add close {
         button $itk_component(actions).close -text Close \
            -command [code $this close]
      }
      add_short_help $itk_component(close) \
         {Close window accepting import}
      pack $itk_component(close) -side left -expand 1 -pady 3 -padx 3

      #  View the first page.
      $itk_component(notebook) view 0

      #  Run-time initialisations
      set_separated_
      set values_($this,id) {}
      set values_($this,ra) {}
      set values_($this,dec) {}
      set values_($this,x) {}
      set values_($this,y) {}
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Set defaults for widget states.
   protected method set_defaults_ {} {
      set values_($this,separated) 1
      set values_($this,delimeter) ","
      set values_($this,comment) "\#"
      set values_($this,fixwidths) {10,10,10,10,10}
      set values_($this,skip) 0
   }

   #  Close window writing output file.
   public method close {} {
      if { [save_file_] } {
         wm withdraw $w_
         configure -outfile $itk_option(-outfile)
      }
   }

   #  Close window without writing file.
   public method cancel {} {
      wm withdraw $w_
      configure -outfile {}
   }

   #  Activate interface, waiting until window is closed.
   public method activate {} {
      wm deiconify $w_
      tkwait variable itk_option(-outfile)
      return $itk_option(-outfile)
   }

   #  Reset window to defaults.
   public method reset {} {
      set_defaults_
      clear_headings_
      $itk_component(table) clear
   }

   #  Add input file structure controls.
   protected method add_struct_controls_ {parent} {
      set lwidth 10

      #  Delimetered or fixed format.
      itk_component add separated {
         StarLabelCheck $parent.separated \
            -text "Use delimeter (otherwise fixed width):" \
            -onvalue 1 \
            -offvalue 0 \
            -variable [scope values_($this,separated)] \
            -command [code $this set_separated_]
      }
      pack $itk_component(separated) -side top -fill x -pady 3 -padx 3
      add_short_help $itk_component(separated) \
         {Use a delimeter between fields, fixed width otherwise}

      #  Set the delimeter.
      itk_component add delimeter {
         LabelMenu $parent.delimeter \
            -labelwidth $lwidth \
            -text "Delimeter:" \
            -variable [scope values_($this,delimeter)]
      }
      pack $itk_component(delimeter) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(delimeter) \
         {The field delimeter character(s)}
      foreach {sname value} {space " " comma "," tab "\t" semicolon ";"} {
         $itk_component(delimeter) add \
            -label $sname \
            -value $value \
            -command [code $this set_delimeter_ $value]
      }
      set_delimeter_ { }

      #  Comment delimeter.
      itk_component add comment {
         LabelEntry $parent.comment \
            -labelwidth $lwidth \
            -text "Comment:" \
            -textvariable [scope values_($this,comment)] \
            -value $values_($this,comment)
      }
      pack $itk_component(comment) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(comment) \
         {The comment indicator character(s)}

      #  Number of lines to skip at beginning of file.
      itk_component add skip {
         LabelEntry $parent.skip \
            -labelwidth $lwidth \
            -text "Header lines:" \
            -textvariable [scope values_($this,skip)] \
            -value $values_($this,skip) \
            -validate integer
      }
      pack $itk_component(skip) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(skip) \
         {Number of lines to skip at head of file (includes comments)}

      #  Fixed width control.
      itk_component add fixwidths {
         LabelEntry $parent.fixwidths \
            -labelwidth $lwidth \
            -text "Field widths:" \
            -textvariable [scope values_($this,fixwidths)] \
            -value $values_($this,fixwidths)
      }
      pack $itk_component(fixwidths) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(fixwidths) \
         {Widths of fields: width1,width2,...}

      #  Interactive tab positioning widget.
      itk_component add tabstop {
         GaiaTabStops $parent.tabstop \
            -change_cmd [code $this update_fixed_] \
            -text "No information, press update"
      }
      pack $itk_component(tabstop) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(tabstop) \
         {Fixed width positions (interactive adjustment)}
   }


   #  Toggle the state of the delimeter fields.
   protected method set_separated_ {} {
      if { $values_($this,separated) } {
         #  Using delimeter
         $itk_component(delimeter) configure -state normal
         $itk_component(fixwidths) configure -state disabled
         $itk_component(tabstop) configure -state disabled
      } else {
         #  Using fixed format
         $itk_component(delimeter) configure -state disabled
         $itk_component(fixwidths) configure -state normal
         $itk_component(tabstop) configure -state normal
      }
   }

   #  Set the fixed widths.
   protected method update_fixed_ {value} {
      set values_($this,fixwidths) $value
   }

   #  Set the delimeter symbol.
   protected method set_delimeter_ {value} {
      set values_($this,delimeter) $value
   }

   #  Heading names and types controls.
   protected method add_heading_controls_ {parent} {

      #  Create a scrollable region for showing all columns.
      itk_component add headregion {
         scrolledframe $parent.headregion -width 75 -height 400
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
      table $itk_component(headtable) \
         $itk_component(head0)  0,0 \
         $itk_component(head1)  0,1 \
         $itk_component(head2)  0,2 \
         $itk_component(head3)  0,3 \
         $itk_component(head4)  0,4 \
         $itk_component(head5)  0,5


      #  Add the entry fields and radiobuttons.
      for {set i 0;set j 1} {$i < $ncolumns_} {incr i; incr j} {

         #  Keep old column names if any exist.
         if { ! [info exists values_($this,heading$i)] } {
            set values_($this,heading$i) "Col$i"
         }
         itk_component add col$i {
            LabelEntry $parent.col$i \
               -labelwidth 2 \
               -text "$i:" \
               -textvariable [scope values_($this,heading$i)] \
               -value $values_($this,heading$i)
         }
         itk_component add id$i {
            radiobutton $parent.id$i \
               -variable [scope values_($this,id)] \
               -value $i
         }
         itk_component add ra$i {
            radiobutton $parent.ra$i \
               -variable [scope values_($this,ra)] \
               -value $i
         }
         itk_component add dec$i {
            radiobutton $parent.dec$i \
               -variable [scope values_($this,dec)] \
               -value $i
         }
         itk_component add x$i {
            radiobutton $parent.x$i \
               -variable [scope values_($this,x)] \
               -value $i
         }
         itk_component add y$i {
            radiobutton $parent.y$i \
               -variable [scope values_($this,y)] \
               -value $i
         }

         table $itk_component(headtable) \
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
         TableList $parent.table \
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
         append headings "$values_($this,heading$i) "
      }
      $itk_component(table) configure -headings $headings

      #  Add each decoded line. Open the file.
      set fid [::open $itk_option(-infile) r]
      set count 0
      set first 1
      while { 1 } {
         incr count
         set llen [gets $fid line]
         if { $count > $values_($this,skip) } {
            if { $llen > 0 } {
               if { ! [string match "$values_($this,comment)*" $line] } {
                  set ncol [parseline_ $line]
                  $itk_component(table) append_row $wordlist_
                  if { $first } {
                     $itk_component(tabstop) configure -text $line
                     set $first 0
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
   #  decodes it (to count words), if using a delimeter. Otherwise
   #  just counts the fixed width words.
   protected method process_ {args} {

      #  Remove old controls.
      clear_headings_

      #  If using a delimeter.
      if { $values_($this,separated) } {

         #  Open the file.
         if { $itk_option(-infile) != "" } {
            set fid [::open $itk_option(-infile) r]
            set count 0
            while { 1 } {
               incr count
               set llen [gets $fid line]
               if { $count > $values_($this,skip) } {
                  if { $llen > 0 } {
                     if { ! [string match "$values_($this,comment)*" $line] } {
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
         set fixwidths [split $values_($this,fixwidths) ","]
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
            set word [ctoken line $values_($this,delimeter)]
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

   #  Save the converted contents to a TAB catalogue.
   protected method save_file_ {args} {
      if { $itk_option(-outfile) != {} } {
         set outid [::open $itk_option(-outfile) w]
         set inid [::open $itk_option(-infile) r]
         if { $itk_option(-format) == "tab" } {

            #  Write a TAB table.
            puts $outid "QueryResult"
            puts $outid "serv_type: catalog"
            puts $outid "long_name: conversion of text file: $itk_option(-infile)"
            puts $outid "short_name: convert@text"
            puts $outid "url: $itk_option(-outfile)"
            if { $values_($this,id) != {} } {
               puts $outid "id_col: $values_($this,id)"
            }
            if { $values_($this,ra) != {} } {
               puts $outid "ra_col: $values_($this,ra)"
            }
            if { $values_($this,dec) != {} } {
               puts $outid "dec_col: $values_($this,dec)"
            }
            if { $values_($this,x) != {} } {
               puts $outid "x_col: $values_($this,x)"
            }
            if { $values_($this,y) != {} } {
               puts $outid "y_col: $values_($this,y)"
            }
            set headings ""
            for {set i 0} {$i < $ncolumns_} {incr i} {
               append headings "$values_($this,heading$i)\t"
            }
            puts $outid "$headings"
            puts $outid "---------"
            set count 0
            while { 1 } {
               incr count
               set llen [gets $inid line]
               if { $count > $values_($this,skip) } {
                  if { $llen > 0 } {
                     if { ! [string match "$values_($this,comment)*" $line] } {
                        set ncol [parseline_ $line]
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
            if { $values_($this,id) != {} } {
               set id $values_($this,id)
            }
            if { $values_($this,ra) != {} } {
               set ra $values_($this,ra)
            }
            if { $values_($this,dec) != {} } {
               set dec $values_($this,dec)
            }
            if { $values_($this,x) != {} } {
               set x $values_($this,x)
            }
            if { $values_($this,y) != {} } {
               set y $values_($this,y)
            }

            #  Set defaults, if RA/DEC etc. not selected. Note these
            #  are only used if not values are set. If any are set
            #  already then obviously the user doesn't have any other
            #  columns.
            if { $ncolumns_ >= 5 } {

               #  Five columns or more. Defaults are id, ra, dec, x, y.
               if { $id == -1 && $ra == -1 && $dec == -1 &&
                    $x == -1 && $y == -1} {
                  set values_($this,id) 0
                  set id 0
                  set values_($this,ra) 1
                  set ra 1
                  set values_($this,dec) 2
                  set dec 2
                  set values_($this,x) 3
                  set x 3
                  set values_($this,y) 4
                  set y 4
               }
            } elseif { $ncolumns_ == 4 } {

               #  Only four columns. Defaults are ra, dec, x, y.
               if { $id == -1 && $ra == -1 && $dec == -1 &&
                    $x == -1 && $y == -1} {
                  set values_($this,ra) 0
                  set ra 0
                  set values_($this,dec) 1
                  set dec 1
                  set values_($this,x) 2
                  set x 2
                  set values_($this,y) 3
                  set y 3
               }
            } elseif { $ncolumns_ == 3 } {

               #  Only three columns. Defaults are id, ra, dec.
               if { $id == -1 && $ra == -1 && $dec == -1 &&
                    $x == -1 && $y == -1} {
                  set values_($this,id) 0
                  set id 0
                  set values_($this,ra) 1
                  set ra 1
                  set values_($this,dec) 2
                  set dec 2
               }
            }

            #  Extract/generate values.
            set count 0
            while { 1 } {
               incr count
               set llen [gets $inid line]
               if { $count > $values_($this,skip) } {
                  if { $llen > 0 } {
                     if { ! [string match "$values_($this,comment)*" $line] } {
                        set ncol [parseline_ $line]
                        if { $id != -1 } {
                           set vid [lindex $wordlist_ $id]
                        } else {
                           set vid $count
                        }
                        if { $ra != -1 } {
                           set vra [lindex $wordlist_ $ra]
                        } else {
                           set vra "00:00:00"
                        }
                        if { $dec != -1 } {
                           set vdec [lindex $wordlist_ $dec]
                        } else {
                           set vdec "00:00:00"
                        }
                        if { $x != -1 } {
                           set vx [lindex $wordlist_ $x]
                        } else {
                           set vx 0.0
                        }
                        if { $y != -1 } {
                           set vy [lindex $wordlist_ $y]
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
         error_dialog "You must supply an output file name"
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

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Values shared by widgets -- indexed by ($this,fieldname).
   common values_

#  End of class definition.
}
