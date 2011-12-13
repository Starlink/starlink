#!/usr/bin/wish
#+
#  Name:
#     tkast

#  Purpose:
#     Browse an AST Object.

#  Language:
#     TCL

#  Invocation:
#     tkast <file>

#  Command Line Arguments:
#     file - A text file containing descriptions of one or more AST Objects
#            in the format used by a simple AST "Channel".

#  Copyright:
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

# Set up global constants.
   set MENUBACK "#b0b0b0"
   set COMPBACK "#e0e0d0"
   set ERROR "!!!"
   set CH 4i

# Procedure definitions...
#---------------------------------------------------------------

proc Exit {} {
   global TITLE
   if { [info exists TITLE] } {
      destroy .
   } {
      exit
   }
}

proc MakeData {top} {
#+
#  Name:
#     MakeData

#  Purpose:
#     Create a Tcl description of an AST data item. The AST description of
#     the data item is obtained by reading records sequentially from the
#     supplied file (opened with descriptor $FD), starting at the current
#     file position.
#
#     The returned Tcl description of a data item consists of a list of 5
#     elements, in the following order:
#
#     0 - The name of the data item.
#     1 - The type of the data item. This will be either:
#           - The Object class name if the data item is an Object.
#           - "IsA" if the data item marks the end of a parent Object.
#           - Blank if the data item is a primitive (i.e. integer, float,
#             string). Note, all primitives are treated as string values.
#     2 - A comment describing the item.
#     3 - A list containing the values of all the components of the data item.
#         If the data item is an Object, the list will contain a data item
#         description for each component of the Object structure.
#         If the data item is a primitive, then the list will contain only
#         one element, and that element will consist of a single item holding
#         the string value.
#     4 - A flag indicating if the item was commented out in the supplied
#         file. A value of 1 is stored if it was not commented out (i.e. if
#         it is "required"), and a value of zero is stored if it was
#         commented out.

#  Language:
#     TCL

#  Arguments:
#     top - Should be set to 1 by the calling procedure to indicate a
#           top-level entry. It will be set to zero when calling this
#           procedure recursively.

#  Returned Value:
#     A 5 element list describing the data item read from the input file.
#     If an error occurs while in a recursive entry to this procedure, then
#     $ERROR is returned. If an eror occurs in a top-level entry, then an
#     error message is reported and the applications is aborted.

#  Notes:
#     -  "IsA" data items are stored with a blank name, and a value consisting
#     of a single string component holding the class name of the parent Object
#     just completed.

#  Copyright:
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global FD
   global FILE
   global PNAME
   global PCOMM
   global ERROR
   global TITLE

   set ret ""

# If this is a top-levl entry, initialise the number of objects in the
# supplied file.
   if { $top } {
      set nobj 0
   }

# Loop reading records from the input file.
   while { 1 } {

# If this is a top-level entry, decide on the name for the next Object.
      if { $top } {
         incr nobj
         set PNAME "Object_$nobj"
      }

# Get the next record form the input file. If the end-of-file is reached
# then return $ERROR unless this is a top-level entry.
      set nc [gets $FD line]
      if { $nc == -1 } {
         if { !$top } { set ret $ERROR }
         break
      }

# Extract the required items from the current record.
      if { [SplitLine $line name type comm value req] } {

# Convert the data item type to lower case.
         set lctype [string tolower $type]

# The first line at the top level must be a "Begin" statement.
         if { $lctype != "begin" && $top } {
            puts "File \"$FILE\" does not start with a \"Begin\" statement."
            close $FD
            exit 1

# Break out of the loop and return, when an Object description ends.
         } elseif { $lctype == "end" } {
            break

# If a new Object is begining, construct a description of it. Call this
# procedure recursively to a description of the Object value.
         } elseif { $lctype == "begin" } {
            set name $PNAME
            set type $value
            if { !$top } {
               set comm $PCOMM
            }

            set value [MakeData 0]
            if { $value == $ERROR } {
               set mess "tkast: Error reading "

               if { $type != "" } {
                  if { [regexp -nocase {^A|^E|^I|^O|^U} $type] } {
                     append mess "an $type "
                  } {
                     append mess "a $type "
                  }
               } {
                  append mess "an item "
               }

               if { $name != "" } {
                  append mess "called $name "
               }

               puts "${mess}from file \"$FILE\"."
               close $FD
               exit 1
            }

         } elseif { [lindex $value 0] == "" } {
            set PNAME $name
            set PCOMM $comm
            continue

         } {
            set value [list $value]
         }

         lappend ret [list $name $type $comm $value $req]

      }

   }

   return $ret

}

proc Save {object title} {
#+
#  Name:
#     Save

#  Purpose:
#     Write an object out to the text file with name ${title}.ast in
#     a format which can be read by a standard AST Channel.

#  Language:
#     TCL

#  Arguments:
#     object - the Tcl list defining the object to be dumped.
#     title - A string giving the name of the object.

#  Copyright:
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global FD
   regsub -all {\.} $title "_" file
   append file ".ast"

   set FD [open $file w]
   WriteObject $object " " 1
   close $FD

   puts "Object $title saved in file $file"

}

proc WriteObject {object indent top} {
#+
#  Name:
#     WriteObject

#  Purpose:
#     Write an object out to the text file opened on unit $FD, in
#     a format which can be read by a standard AST Channel.

#  Language:
#     TCL

#  Arguments:
#     object - the Tcl list defining the object to be dumped.
#     indext - A string of spaces giving the current indentation.
#     top - Should be set to 1 by the calling procedure to indicate a
#           top-level entry. It will be set to zero when calling this
#           procedure recursively.

#  Copyright:
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global FD

   set name [lindex $object 0]
   set type [lindex $object 1]
   set comm [lindex $object 2]
   set value [lindex $object 3]
   set comp0 [lindex $value 0]
   set req [lindex $object 4]

   if { $req } {
      set line " "
   } {
      set line "#"
   }

   append line $indent

   if { $type == "IsA" } {
      append line "IsA [lindex $comp0 0]	# $comm"
      puts $FD $line

   } elseif {$type == "" } {
      set val [lindex $comp0 0]
      if { [scan $val "%g %s" fval sval] == 1 } {
         append line "   $name = $val	# $comm"
      } {
         append line "   $name = \"$val\"	# $comm"
      }
      puts $FD $line

   } {
      if { !$top } {
         append line "$name = 	# $comm"
         puts $FD $line
         append indent "   "
      }

      if { $req } {
         set line " "
      } {
         set line "#"
      }

      append line "${indent}Begin $type"
      puts $FD $line

      foreach  comp $value {
         WriteObject $comp $indent 0
      }

      if { $req } {
         set line " "
      } {
         set line "#"
      }
      append line "${indent}End $type"
      puts $FD $line

   }

}

proc SplitLine {line namen typen commn valuen reqn } {
   global FILE
   global FD

   upvar $namen name
   upvar $commn comm
   upvar $typen type
   upvar $valuen value
   upvar $reqn req

# Initialise the returned items.
   set comm ""
   set name ""
   set type ""
   set value ""
   set req 0

# Save the original line.
   set oline $line

# Remove leading and trailing spaces. If this leaves nothing, return 0.
   set line [string trim $line]
   if { $line == "" } { return 0 }

# Check for and remove any leading "#" character.
   if { [regsub "^#" $line "" line] } {

# If a leading "#" was found, check that there is another "#" in the
# line. If so, indicate that the item is not a "required" item, and
# remove any leading spaces. If not, the whole line is discarded as a
# comment line. This is indicated by returning zero.
      if { [string first "#" $line] == -1 } {
         return 0
      } {
         set req 0
         set line [string trim $line]
      }

# If no leading "#" was found indicate that the item is "required".
   } {
      set req 1
   }

# Find the start of the comment (i.e. the first "#" character). If a
# comment character is found, extract everything following it.
   set ic [string first "#" $line]
   if { $ic != -1 } {
      set comm [string range $line [expr $ic + 2] end]
      set line [string range $line 0 [expr $ic - 1] ]
   }

# Remove leading and trailing spaces. If this leaves nothing, return 0.
   set line [string trim $line]
   if { $line == "" } { return 0 }

# Extract the remaining items. First try a {name = "value"} format (for
# string primitives).
   if { [regexp  {^([^ ]+) += +\"([^\"]*)\" *$} $line match name value] } {
      set ret 1

# Now try a {name = value} format (for non-string primitives).
   } elseif { [regexp  {^([^ ]+) += +([^ ]+) *$} $line match name value] } {
      set ret 1

# Now try a {name = } format (for Objects).
   } elseif { [regexp  {^([^ ]+) += *$} $line match name] } {
      set ret 1

# Now try a {type value} format (eg for Begin, End and Isa).
   } elseif { [regexp  {^([^ ]+) +([^ ]+) *$} $line match type value] } {
      set ret 1

# Abort if any other items are found.
   } {
      puts "tkast: The following line of file \"FILE\" has an unrecognised format:\n$oline"
      close $FD
      exit 1
   }

# Ensure the returned value is a valid list.
   set value [list $value]

   return $ret
}

proc ShowObject {w object parent} {
   global MENUBACK
   global COMPBACK
   global FREEZE
   global CLASS
   global CMPLST
   global FILE
   global CH
   global XDEF
   global TITLE

# Check the object has the correct number of elements.
   set olen [llength $object]
   if { $olen != 5 && $olen != 0 } {
      puts "A: Input data file \"$FILE\" contains an inconsistent Object description!"
      puts "$object"
      exit
   }

# Extract the items from the object description.
   if { $olen == 5 } {
      set name [lindex $object 0]
      set type [lindex $object 1]
      set comm [lindex $object 2]
      set value [lindex $object 3]
      set req [lindex $object 4]
   } {
      set name ""
      set type ""
      set comm ""
      set value ""
      set req 0
   }

# Ensure the window exists.
   if { $w != "." } {
      catch {destroy $w}
      toplevel $w -class TkAst
      wm transient $w [winfo toplevel [winfo parent $w]]
   }

# Indicate that the new window is not frozen, and that the parent window
# is frozen.
   set FREEZE($w) 0
   set FREEZE([winfo parent $w]) 1

# Get the full path to the Object. This will be used as the window title.
   if { $parent != "" && $parent != "Contents" } {
      set path $parent
      append path "."
      append path $name
   } {
      set path $name
   }

# Set up the window.
   if { $parent != "" || ![info exists TITLE] } {
      set ttl $path
   } {
      set ttl $TITLE
   }
   wm title $w $ttl

   if { [string trim $ttl] == "" } { set ttl $name }
   if { [string trim $ttl] == "" } { set ttl "object" }


   wm iconname $w $name
   wm protocol $w WM_DELETE_WINDOW "Close $w"

# Create an all-encompassing frame.
   set aw [frame $w.all -relief raised -bd 2]
   pack $aw -padx 2m -pady 2m -expand 1 -fill both

# Divide up the window into four parts.
   set menubar [frame $aw.menubar -relief raised -bd 2 -background $MENUBACK ]
   pack $menubar -side top -anchor n -fill x

   set top [frame $aw.top]
   pack $top -side top -anchor n -fill x

   set headers [frame $top.headers]
   pack $headers -side left -fill x -anchor nw -expand 1 -padx 5m -pady 3m

   set ancestors [frame $top.ancestors -relief groove -bd 2]
   pack $ancestors  -side left -fill x -anchor nw -expand 1 -padx 5m -pady 3m

   set cframe [frame $aw.cframe -relief sunken -bd 3 ]
   pack $cframe -side bottom -fill both -expand 1 -padx 5m -pady 5m

   set yscroll $cframe.yscroll

   set canvas [canvas $cframe.canvas -height $CH -width 0i -background $COMPBACK \
                    -yscrollcommand "$yscroll set"]
   pack $canvas -side left -fill both -expand 1

   scrollbar $yscroll -command "$canvas yview"
   pack $yscroll -side left -fill y

   set complist [frame $canvas.complist]
   set CMPLST $complist
   $canvas create window 0 0 -window $complist -anchor nw -tag clist

# Build the menu bar.
   set file [menubutton $menubar.file -text File -menu $menubar.file.menu \
                                      -background $MENUBACK -underline 0]
   set filemenu [menu $file.menu]

   set opts [menubutton $menubar.opts -text Options -menu $menubar.opts.menu \
                                      -background $MENUBACK -underline 0]
   set optsmenu [menu $opts.menu]
   pack $file $opts -side left

# Build the "File" menu.
   $filemenu add command -label "Close  " -command "Close $w" -underline 0 -accelerator "Ctrl+c"
   bind $w <Control-c> "Close $w"
   $filemenu add command -label "Exit   " -command Exit -underline 0 -accelerator "Ctrl+e"
   bind $w <Control-e> Exit
   $filemenu add command -label "Save   " -command "Save {$object} $ttl" -underline 0 -accelerator "Ctrl+s"
   bind $w <Control-s> "Save {$object} $ttl"

# Build the "Options" menu.
   if { $w == "." } {
      set XDEF($w) 0
   } {
      set XDEF($w) $XDEF([winfo parent $w])
   }
   $optsmenu add checkbutton -label "Exclude defaults   " -variable XDEF($w) \
                      -underline 1 -accelerator "Ctrl+x" \
                      -command "ShowClass $canvas $complist [list $type] [list $value]  [list $type] [list $path] 1"
   bind $w <Control-x> "if { \$XDEF($w) } {
                           set XDEF($w) 0
                        } {
                           set XDEF($w) 1
                        }
                        ShowClass $canvas $complist [list $type] [list $value] \
                                            [list $type] [list $path] 1"

# Display the descriptive headers.
   if { $name == "Contents" } {
      pack [label $headers.l1 -text "Index of file contents"] -side top \
           -anchor w
   } {
      pack [label $headers.l1 -text "Name  :  $path"] \
           [label $headers.l2 -text "Class  :  $type"] \
           [label $headers.l3 -text "Comment  :  $comm"] -side top -anchor w
   }

# Display the attributes of the Object in the component list panel.
   ShowClass $canvas $complist $type $value $type $path

# Create an array of radio buttons in the top panel, listing the
# ancestors of the supplied Object.
   pack [label $ancestors.title -text "View as..." ] -side top -pady 2m -expand 1 -anchor nw
   set rbclass $type
   set i [llength $value]
   set CLASS($ancestors) $type
   while { $rbclass != "" } {
      set rbname [string tolower "${ancestors}.${rbclass}"]
      pack [radiobutton $rbname -text $rbclass -variable CLASS($ancestors)  -value $rbclass \
                        -command "ShowClass $canvas $complist $type [list $value] $rbclass $path" \
                        -anchor w] -side top -anchor w

      set rbclass ""
      while {$i > 0} {
         incr i -1
         set comp [lindex $value $i]

         if { [llength $comp] != 5 } {
            puts "B: Input data file \"$FILE\" contains an inconsistent Object description!"
            puts "$comp"
            exit
         }

         if { [lindex $comp 1] == "IsA" } {
            set rbclass [lindex $comp 3]
            break
         }
      }
   }

# If creating a new TopLevel, set a grab and claim the focus too.
   if { $w != "." } {
      tkwait visibility $w
      grab $w
      focus $w
   }
}

proc Close {w} {
   global FREEZE
   global SEL
   global CMPLST
   global CURCLASS

   if { [info exists CMPLST] && [info exists CURCLASS($CMPLST)] } {
      unset CURCLASS($CMPLST)
   }

# If closing the main application window, just do an exit.
   if { $w == "." } { exit }

# Otherwise, get the name of the previous window.
   set w0 [winfo parent $w]

# Indicate that the previous window is no longer frozen.
   set FREEZE($w0) 0

# Cancel any item selection in the previous window.
    if { [info exists SEL($w0) ] } {
       foreach widget $SEL($w0) {
          $widget  configure -relief flat
       }
    }

# Otherwise, reinstate the previous window's focus and grab.
   catch {focus $w0}

# It's possible that the window has already been destroyed,
# hence this "catch".  Delete the Destroy handler so that
# tkPriv(button) doesn't get reset by it.
   catch {
       bind $w <Destroy> {}
       destroy $w
   }

   grab $w0

}

proc ShowClass {canvas complist type value class path args} {
   global FILE
   global CURCLASS
   global COMPBACK
   global CH

# If this object has already been displayed...
   if { [info exists CURCLASS($complist)] } {

# If the options "args" argument has been supplied, then re-display the
# currently displayed class.
      if { $args != "" } {
         set class $CURCLASS($complist)

# Otherwise, return without action if the requested class is already
# displayed.
      } {
         if { $CURCLASS($complist) == $class } { return }
      }
   }

# Save the new class type.
   set CURCLASS($complist) $class

# Create 4 vertical columns in the components list panel. Destroy any
# existing columns first.
   set c1 "${complist}.c1"
   set c2 "${complist}.c2"
   set c3 "${complist}.c3"

   if { [winfo exists $c1] } { destroy $c1 }
   if { [winfo exists $c2] } { destroy $c2 }
   if { [winfo exists $c3] } { destroy $c3 }

   pack [frame $c1 -relief ridge -bd 1 -background $COMPBACK ] \
        [frame $c2 -relief ridge -bd 1 -background $COMPBACK ] \
        [frame $c3 -relief ridge -bd 1 -background $COMPBACK ] \
        -side left -ipadx 2m -ipady 1m -fill x -expand 1

# Find the start of the required class data.
   set i [llength $value]
   while { $type != $class } {

      while {$i > 0} {
         incr i -1
         set comp [lindex $value $i]

         if { [llength $comp] != 5 } {
            puts "C: Input data file \"$FILE\" contains an inconsistent Object description!"
            puts "$comp"
            exit
         }

         if { [lindex $comp 1] == "IsA" } {
            set type [lindex $comp 3]
            break
         }
      }
   }

# Now display the required class data, stopping when the next (parent) class
# is encountered.
   while {$i > 0} {
      incr i -1

# Get the next component, and check it has 5 elements.
      set comp [lindex $value $i]
      if { [llength $comp] != 5 } {
         puts "D: Input data file \"$FILE\" contains an inconsistent Object description!"
         puts "$comp"
         exit
      }

      if { [lindex $comp 1] == "IsA" } {
         break
      } {
         ShowComp $comp $c1 $c2 $c3 $path $i
      }
   }

# Ensure that the canvas is as wide as the components list frame.
   update idletasks
   $canvas configure -width [winfo width $complist] \
                     -scrollregion [$canvas bbox clist]

# Ensure that the canvas is as high as the components list frame, unless this
# is more than the original height of the canvas.
   update idletasks
   set h1 [winfo height $complist]
   if { $h1 < [winfo pixels $canvas $CH] } {
      $canvas configure -height $h1
   } {
      $canvas configure -height $CH
   }


}

proc ShowComp {comp c1 c2 c3 path count} {
   global FILE
   global XDEF
   global COMPBACK

# Extract the items from the component description.
   set cname [lindex $comp 0]
   set ctype [lindex $comp 1]
   set ccomm [lindex $comp 2]
   set cvalue [lindex $comp 3]
   set creq [lindex $comp 4]

# Find the name of the current toplevel window.
   set top [winfo toplevel $c1]

# Return without action if the supplied object is a default and defaults
# are not to be displayed.
   if { !$creq && $XDEF($top) } { return }

# See if the component is a primitive or an Object.
   if { $ctype == "" } {
      set isaobject 0
   } {
      set isaobject 1
   }

# Get the value text to display.
   if { $isaobject } {
      set cvalue "<${ctype}>"
   } {
      set cvalue [lindex [lindex $cvalue 0] 0]
   }

# Create the labels.
   set lab [string tolower $cname]
   append lab $count
   set lname [label $c1.$lab -text $cname -background $COMPBACK ]
   set lvalue [label $c2.$lab -text $cvalue -background $COMPBACK ]
   set lcomm [label $c3.$lab -text $ccomm -background $COMPBACK ]

# Pack them.
   pack $lname -side top -fill x
   pack $lvalue -side top -fill x
   pack $lcomm -side top -fill x

# If the component is an object...
   if { $isaobject } {

# Create bindings to sink the whole row when the cursor enters any
# of the associated labels. Raise the row when the cursor leaves
# so long as the current toplevel window is still grabbing X events.
      bindtags $lname $lab
      bindtags $lvalue $lab
      bindtags $lcomm $lab

      bind $lab <Enter> "if { !\$FREEZE($top) } {
                            $lname configure -relief sunken
                            $lvalue configure -relief sunken
                            $lcomm configure -relief sunken
                            set SEL($top) \[list $lname $lvalue $lcomm\]
                         }"
      bind $lab <Leave> "if { !\$FREEZE($top) } {
                            $lname configure -relief flat
                            $lvalue configure -relief flat
                            $lcomm configure -relief flat
                            if { \[info exists SEL($top)\] } {
                               unset SEL($top)
                            }
                         }"

# Create a binding to display the component if the cursor is clicked over
# any of the labels.
      if { $top == "." } {
         set w0 ".${lab}"
      } {
         set w0 "${top}.${lab}"
      }

      bind $lab <Button> "ShowObject $w0 [list $comp] [list $path]"
   }
}




# Main entry point
#---------------------------------------------------------------

# Get the data file containing the output from an AST TkChan, and source
# it.
   if { ![info exists FILE] } {
      if { $argc != 1 } {
         puts "tkast: No input file supplied."
         puts "tkast: usage - \"tkast <file>\""
         exit 1
      } {
         set FILE [lindex $argv 0]
      }
   }

   if { $FILE == "" } {
      puts "tkast: Blank input file supplied."
      exit 1
   } elseif { ![file readable $FILE] } {
      puts "tkast: Cannot read input file \"$FILE\"."
      exit 1
   } {
      set FD [open $FILE]
      set data [MakeData 1]
      close $FD

      if { $data == "" } {
         puts "test: Cannot read data from input file \"$FILE\"."
         exit 1

      } elseif { [llength $data] == 0 } {
         puts "tkast: Input file \"$FILE\" does not contain any AST data."
         exit 1
      } {
         set AST_DATA [list {} {} {Index of supplied Objects} $data 1]
      }
   }

# If the supplied data file contains more than 1 object, reverse the
# order of the Objects (to get Object 1 first), and show the list of
# Objects in the data file.
   set comps [lindex $AST_DATA 3]
   set ncomps [llength $comps]

   if { $ncomps > 1 } {
      incr ncomps -1
      for {set i $ncomps} {$i >= 0} {incr i -1} {
         lappend new_comps [lindex $comps $i]
      }

      set AST_DATA [lreplace $AST_DATA 3 3 $new_comps]
      ShowObject . $AST_DATA ""

   } {
     ShowObject . [lindex $comps 0] ""
   }


