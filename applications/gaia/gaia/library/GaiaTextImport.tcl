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
#     covers what columns names to use, what separators are used and
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

      $itk_component(notebook) view 0
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
      set values_($this,separator) ","
      set values_($this,comment) "\#"
      set values_($this,fixwidths) {10,10,10,10,10}
   }


   #  Add input file structure controls.
   protected method add_struct_controls_ {parent} {
      set lwidth 17

      #  Separator or fixed format.
      itk_component add separated {
         StarLabelCheck $parent.separated \
            -text "Use separator (otherwise fixed width):" \
            -onvalue 1 \
            -offvalue 0 \
            -variable [scope values_($this,separated)] \
            -command [code $this set_separated_]
      }
      pack $itk_component(separated) -side top -fill x -pady 3 -padx 3
      add_short_help $itk_component(separated) \
         {Use a separator between fields, fixed width otherwise}

      #  The separator.
      itk_component add separator {
         LabelEntry $parent.separator \
            -labelwidth $lwidth \
            -text "Separator:" \
            -textvariable [scope values_($this,separator)] \
            -value $values_($this,separator)
      }
      pack $itk_component(separator) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(separator) \
         {The field separator character(s)}


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
   }


   #  Toggle the state of the separator fields.
   protected method set_separated_ {} {
      if { $values_($this,separated) } {
         #  Using separator
         $itk_component(separator) configure -state normal
         $itk_component(fixwidths) configure -state disabled
      } else {
         #  Using fixed format
         $itk_component(separator) configure -state disabled
         $itk_component(fixwidths) configure -state normal
      }
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

   #  Add field for each column. Also add radio buttons to select
   #  amongst RA/Dec and X/Y.
   protected method add_headings_ {} {
      for {set i 0} {$i < $ncolumns_} {incr i} {
         set values_($this,heading$i) "Col$i"
         itk_component add col$i {
            LabelEntry $headparent_.col$i \
               -labelwidth 17 \
               -text "Name for column $i:" \
               -textvariable [scope values_($this,heading$i)] \
               -value $values_($this,heading$i)
         }
         pack $itk_component(col$i) -side top -fill x -ipadx 1m -ipady 1m
         add_short_help $itk_component(col$i) \
            "Name for data in column $i"
      }
   }

   #  Remove existing headings controls.
   protected method clear_headings_ {} {
      for {set i 0} {$i < $ncolumns_} {incr i} {
         delete object $itk_component(col$i)
      }
   }

   #  Start the decode cycle. Opens input file, reads first line and
   #  decodes it (to count words), if using a separator. Otherwise
   #  just counts the fixed width words.
   protected method start_process_ {args} {

      #  Remove old controls.
      clear_headings_

      #  If using a separator.
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
            set word [ctoken line $values_($this,separator)]
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
