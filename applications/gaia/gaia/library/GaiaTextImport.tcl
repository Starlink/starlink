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
#     which columns are coordinates. It also provides the means to
#     convert these coordinates into those of a displayed rtdimage (if
#     possible).
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

      #  Add short help window
      make_short_help

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      #  Name of input file.
      itk_component add infile {
         LabelFileChooser $w_.infile \
            -text "Input file:" \
            -textvariable [scope values_($this,infile)] \
            -command [code $this start_process_]
      }
      pack $itk_component(infile) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(infile) \
         {Name of input text file}

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

      #  Add a page for astrometric conversion.
      $itk_component(notebook) add -label Astrometry
      #add_astrometry_controls_

      #  Add a page for showing the interpreted table.
      $itk_component(notebook) add -label Import
      #add_table_controls_

      #  Add action buttons.
      itk_component add actions {
         frame $w_.actions
      }

      #  Update displayed table contents.
      itk_component add update {
         button $itk_component(actions).update -text Update \
            -command [code $this update_table]
      }
      add_short_help $itk_component(update) \
         {Update table to reflect current state}
      pack $itk_component(actions) -side top -fill x 
      pack $itk_component(update) -side left -expand 1 -pady 3 -padx 3

      #  View the first page.
      $itk_component(notebook) view 0

      #  Run-time initialisations
      set_separated_
      set values_($this,ra) -1
      set values_($this,dec) -1
      set values_($this,x) -1
      set values_($this,y) -1
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

      #  How many lines to skip?

   }


   #  Toggle the state of the delimeter fields.
   protected method set_separated_ {} {
      if { $values_($this,separated) } {
         #  Using delimeter
         $itk_component(delimeter) configure -state normal
         $itk_component(fixwidths) configure -state disabled
      } else {
         #  Using fixed format
         $itk_component(delimeter) configure -state disabled
         $itk_component(fixwidths) configure -state normal
      }
   }

   #  Set the delimeter symbol.
   protected method set_delimeter_ {value} {
      set values_($this,delimeter) $value
      puts "set delimeter to: '$value'"
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
         label $parent.head1 -text "RA"
      }
      itk_component add head2 {
         label $parent.head2 -text "Dec"
      }
      itk_component add head3 {
         label $parent.head3 -text "X"
      }
      itk_component add head4 {
         label $parent.head4 -text "Y"
      }
      table $itk_component(headtable) \
         $itk_component(head0)  0,0 \
         $itk_component(head1)  0,1 \
         $itk_component(head2)  0,2 \
         $itk_component(head3)  0,3 \
         $itk_component(head4)  0,4


      for {set i 0;set j 1} {$i < $ncolumns_} {incr i; incr j} {
         set values_($this,heading$i) "Col$i"
         itk_component add col$i {
            LabelEntry $parent.col$i \
               -labelwidth 2 \
               -text "$i:" \
               -textvariable [scope values_($this,heading$i)] \
               -value $values_($this,heading$i)
         }
         #add_short_help $itk_component(col$i) \
         #   "Name for data in column $i"
         
         itk_component add ra$i {
            radiobutton $parent.ra$i \
               -variable [scope values_($this,ra)] \
               -value $i
         }
         #add_short_help $itk_component(ra$i) \
         #   "Column contains RA"

         itk_component add dec$i {
            radiobutton $parent.dec$i \
               -variable [scope values_($this,dec)] \
               -value $i
         }
         #add_short_help $itk_component(dec$i) \
         #   "Column contains Dec"

         itk_component add x$i {
            radiobutton $parent.x$i \
               -variable [scope values_($this,x)] \
               -value $i
         }
         #add_short_help $itk_component(x$i) \
         #   "Column contains X coordinates"

         itk_component add y$i {
            radiobutton $parent.y$i \
               -variable [scope values_($this,y)] \
               -value $i
         }
         #add_short_help $itk_component(y$i) \
         #   "Column contains Y coordinates"

         table $itk_component(headtable) \
            $itk_component(col$i) $j,0 -fill x -pady 3 -padx 3 \
            $itk_component(ra$i) $j,1 -fill x -pady 3 -padx 3 \
            $itk_component(dec$i) $j,2 -fill x -pady 3 -padx 3 \
            $itk_component(x$i) $j,3 -fill x -pady 3 -padx 3 \
            $itk_component(y$i) $j,4 -fill x -pady 3 -padx 3
      }
      pack $itk_component(headtable) -fill both -expand 1
      add_short_help $itk_component(headtable) \
         {Set column names and identify the RA/Dec, X/Y coordinates}
   }

   #  Remove existing headings controls.
   protected method clear_headings_ {} {
      if { [info exists itk_component(headtable)] && 
           [winfo exists $itk_component(headtable)] } {
         catch {destroy $itk_component(headtable)}
      }
   }

   #  Start the decode cycle. Opens input file, reads first line and
   #  decodes it (to count words), if using a delimeter. Otherwise
   #  just counts the fixed width words.
   protected method start_process_ {args} {

      #  Remove old controls.
      clear_headings_

      #  If using a delimeter.
      if { $values_($this,separated) } {

         #  Open the file.
         set fid [open $values_($this,infile) r]
         set ok 1
         set first_line_ {}
         while { $ok  } {
            set llen [gets $fid line]
            puts "line: $line"
            if { $llen > 0 } {
               if { ! [string match "$values_($this,comment)*" $line] } {
                  set first_line_ $line
                  break
               }
            }
         }
         puts "first_line_: $first_line_"
         set ncolumns_ [parseline_ $line]
         close $fid
      } else {

         #  Fixed width entries.
         set fixwidths [split $values_($this,fixwidths) ","]
         set ncolumns_ [llength $fixwidths]
      }
      add_headings_
   }

   #  Parse a string into delimitered words, returning a count of the
   #  number of words located. The results are stored in the words_ array.
   protected method parseline_ {line} {
      catch {unset words_}
      if { $line != {} } {
         set ok 1
         set ncol 0
         while { $ok } {
            set word [ctoken line $values_($this,delimeter)]
            if { $word != {} } {
               incr ncol
               set words_($ncol) $word
            } else {
               set ok 0
            }
         }
      } else {
         set ncol 0
      }
      return $ncol
   }
   
   #  Configuration options: (public variables)
   #  ----------------------
   
   #  Protected variables: (available to instance)
   #  --------------------
   
   #  Number columns in or expected in the input file.
   protected variable ncolumns_ -1
   
   #  Parent of headings controls (scrolled region).
   protected variable headparent_ {}
   
   #  Current list of parsed words.
   protected variable words_
   
   #  Common variables: (shared by all instances)
   #  -----------------

   #  Values shared by widgets -- indexed by ($this,fieldname).
   common values_

#  End of class definition.
}
