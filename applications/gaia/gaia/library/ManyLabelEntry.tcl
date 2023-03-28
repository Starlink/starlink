#+
#  Name:
#     ManyLabelEntry

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines the class of labelled entry widgets with many entry fields.

#  Description:
#     This class creates a labelled widget with a number of associated
#     entry fields. The number of entries is defined when the widget is
#     created and these can be written to using a complete list (see
#     the setvals method; note any missing values are set to {}). The
#     values stored in the entries are returned as a list (see the get
#     method) and may be all validated as various types (real, alphabetic
#     alphanumeric, hexidecimal, integer and numeric).

#  Configuration options:
#     See itk_option definitions below and in base classes.

#  Methods:
#     See method definitions below and in base classes.

#  Inheritance:
#     This class inherits util::FrameWidget.

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
#     28-AUG-1998 (PWD):
#     	 Original version, based on Allan Brighton's LabelEntry.
#     {enter_changes_here}

#-

itk::usual ManyLabelEntry {}

itcl::class gaia::ManyLabelEntry {

   # Constructor: create a new ManyLabelEntry widget.
   inherit util::FrameWidget

   constructor {args} {

      #  Create a frame to hold the label
      itk_component add lframe {
         frame $w_.lframe -bd 0
      }

      #  Create a frame to hold the entries.
      itk_component add eframe {
         frame $w_.eframe -bd 0
      }
      pack $itk_component(lframe) -side left -fill x
      pack $itk_component(eframe) -side left -fill x -expand 1

      #  Create the label.
      itk_component add label {
         label $itk_component(lframe).label
      } {
         keep -text -background -foreground -anchor
         rename -width -labelwidth labelWidth LabelWidth
         rename -font -labelfont labelFont LabelFont
      }

      #  Evaluate the arguments, creating all the entry widgets.
      eval itk_initialize $args
   }


   #  Return a list of all the entry values.
   public method get {} {
      for {set i 0} {$i < $itk_option(-nentry)} {incr i} {
         lappend value  [$itk_component(entry$i) get]
      }
      return $value
   }

   #  Set the values of the entry widgets. Sets up to either the
   #  number of values given, or the maximum number of entries. Extra
   #  entries are cleared.
   public method setvals {args} {
      if { $args != {} } {
         set nargs [llength $args]
         set n [expr ($itk_option(-nentry) < $nargs) ? $itk_option(-nentry) : $nargs]
         for {set i 0} {$i < $n} {incr i} {
            set prev_state [$itk_component(entry$i) cget -state]
            $itk_component(entry$i) configure -state normal
            $itk_component(entry$i) delete 0 end
            $itk_component(entry$i) insert 0 [lindex $args $i]
            if {"$itk_option(-justify)" == "right"} {
               $itk_component(entry) icursor end
               $itk_component(entry) xview moveto 1
            }
            $itk_component(entry$i) configure -state $prev_state
         }

         #  Clear any remaining fields.
         for {set i $n} {$i < $itk_option(-nentry)} {incr i} {
            set prev_state [$itk_component(entry$i) cget -state]
            $itk_component(entry$i) configure -state normal
            $itk_component(entry$i) delete 0 end
            $itk_component(entry$i) configure -state $prev_state
         }
      }
   }

   #  Called for return or keypress in entry, calls command proc
   #  with new value.
   method command_proc_ {cmd} {
      lappend cmd [get]
      eval $cmd
   }

   #  The peek procedure returns the value of the entry with the char
   #  inserted at the insert position. (ripped from iwidgets::entryfield).
   method peek_ {char index} {
      set str [$itk_component(entry$index) get]
      set insertPos [$itk_component(entry$index) index insert]
      set firstPart [string range $str 0 [expr $insertPos - 1]]
      set lastPart [string range $str $insertPos end]
      append rtnVal $firstPart $char $lastPart
      return $rtnVal
   }

   #  Called for keypress events when validation is on (based on code
   #  from iwidgets).
   method validate_ {char sym index} {
      set cmd $validate_cmd_

      if {"$cmd" == "" || "$itk_option(-validate)" == ""} {
         return
      }

      #  Pass these on to other bindings...
      if {$sym == "Return" ||
          $sym == "Tab" ||
          $sym == "BackSpace" ||
          $sym == "Delete" ||
          $char == ""} {
         return
      }
      regsub -all "%W" $cmd $itk_component(hull) cmd
      regsub -all "%P" $cmd [peek_ $char $index] cmd
      regsub -all "%S" $cmd [$itk_component(entry$index) get] cmd

      if {$char == "\\"} {
         regsub -all "%c" $cmd {\\\\} cmd
      } elseif {$char == "&"} {
         regsub -all "%c" $cmd {\&} cmd
      } else {
         regsub "\"|\\\[|\\\]|{|}| " $char {\\\0} char
         regsub -all "%c" $cmd $char cmd
      }

      set valid [eval ManyLabelEntry::$cmd]

      if {($valid == "") || (! $valid)} {
         eval $itk_option(-invalid)
         return -code break
      }
   }

   #  Validation methods used for -validate option (from
   #  iwidgets::entryfield class).
   proc numeric {char} {
      return [regexp {[0-9]} $char]
   }
   proc integer {string} {
      return [regexp {^[-+]?[0-9]*$} $string]
   }
   proc alphanumeric {char} {
      return [regexp -nocase {[0-9a-z]} $char]
   }
   proc alphabetic {char} {
      return [regexp -nocase {[a-z]} $char]
   }
   proc hexidecimal {string} {
      return [regexp {^(0x)?[0-9a-fA-F]*$} $string]
   }
   proc real {string} {
      return [regexp -nocase {^[-+]?[0-9]*\.?[0-9]*([0-9]\.?e[-+]?[0-9]*)?$} $string]
   }

   #  Update the global textvariable to that of each entry widget.
   protected method set_textvar_ {args} {
      set $itk_option(-textvariable) [get]
   }

   # -- Options --

   #  Number of entry widgets to create, also creates them.
   itk_option define -nentry nentry Nentry 2 {
      if { $nentry_ < $itk_option(-nentry) } {

         #  Need a few more.
         for {set i $nentry_} {$i < $itk_option(-nentry)} {incr i} {
            itk_component add entry$i {
               entry $itk_component(eframe).entry$i \
                  -textvariable [scope tracevars_($i)]
            } {
               keep -relief -borderwidth -state
               rename -font -valuefont valueFont ValueFont
            }
            trace variable [scope tracevars_($i)] w [code $this set_textvar_]
         }
         set started_ 1
         configure -orient $itk_option(-orient)
      } elseif {$nentry_ > $itk_option(-nentry) } {
         #  More than we need.
         for {set i $itk_option(-nentry)} {$i < $nentry_} {incr i} {
            destroy $itk_component(entry$i)
         }
      }
      set nentry_ $itk_option(-nentry)
   }

   #  Set the values displayed in the entry fields.
   itk_option define -value value Value {} {
      eval setvals $itk_option(-value)
   }

   #  Set the width of the value displayed.
   itk_option define -valuewidth valueWidth ValueWidth {15} {
      if { $itk_option(-valuewidth) != {} } {
         for {set i 0} {$i < $itk_option(-nentry)} {incr i} {
            $itk_component(entry$i) config -width $itk_option(-valuewidth)
         }
      }
   }

   #  Set the state to normal or disabled (greyed out).
   itk_option define -state state State normal {
      for {set i 0} {$i < $itk_option(-nentry)} {incr i} {
         $itk_component(entry$i) config -state $itk_option(-state)
         if {"$itk_option(-state)" == "normal"} {
            $itk_component(entry$i) config -foreground $itk_option(-foreground)
            $itk_component(label) config -foreground $itk_option(-foreground)
         } else {
            $itk_component(entry$i) config -foreground $itk_option(-disabledforeground)
            $itk_component(label) config -foreground $itk_option(-disabledforeground)
         }
      }
   }

   #  The command for <Return> in any entry, called with the new values.
   itk_option define -command command Command {} {
      if {"$itk_option(-command)" != ""} {
         for {set i 0} {$i < $itk_option(-nentry)} {incr i} {
            bind $itk_component(entry$i) \
               <Return> [code $this command_proc_ $itk_option(-command)]
         }
      }
   }

   #  Alternative name for use when -command is already used by a
   #  derived class.
   itk_option define -entrycommand entryCommand EntryCommand {} {
      if {"$itk_option(-entrycommand)" != ""} {
         for {set i 0} {$i < $itk_option(-nentry)} {incr i} {
            bind $itk_component(entry$i) \
               <Return> [code $this command_proc_ $itk_option(-entrycommand)]
         }
      }
   }

   #  Set to "right" to make sure the end of the entry is visible.
   itk_option define -justify justify Justify {left}

   #  Commands to evaluate whenever the entry values change.
   itk_option define -changecmd changecmd Changecmd {} {
      if {"$itk_option(-changecmd)" != ""} {
         if {"$itk_option(-textvariable)" == ""} {
            config -textvariable $w_
         }
         set var $itk_option(-textvariable)
         global ::$var
         trace variable $var w [code $this trace_]
      }
   }

   #  XXX Contents of entry widgets as global variable
   itk_option define -textvariable textvariable Textvariable {}

   #  Widget orientation: horizontal or vertical
   itk_option define -orient orient Orient {horizontal} {
      if { $started_ } {
         if { $itk_option(-orient) == "horizontal" } {
            set side left
         } else {
            set side top
         }
         if {"$itk_option(-text)" != ""} {
            pack $itk_component(label) -side left -anchor w -ipadx 3
         }
         for {set i 0} {$i < $itk_option(-nentry)} {incr i} {
            pack $itk_component(entry$i) -side $side -fill x -padx 1m -ipadx 1m
         }
      }
   }

   #  Validation of entry fields (based on iwidgets entryfield class)
   #  numeric, alphabetic, integer, hexidecimal, real, and alphanumeric.
   itk_option define -validate validate Validate {""} {
      if {"$itk_option(-validate)" != ""} {
         set validate_cmd_ ""
         switch $itk_option(-validate) {
            numeric {
               set validate_cmd_ "numeric %c"
            }
            integer {
               set validate_cmd_ "integer %P"
            }
            hexidecimal {
               set validate_cmd_ "hexidecimal %P"
            }
            real {
               set validate_cmd_ "real %P"
            }
            alphabetic {
               set validate_cmd_ "alphabetic %c"
            }
            alphanumeric {
               set validate_cmd_ "alphanumeric %c"
            }
         }
         if {"$validate_cmd_" != ""} {
            for {set i 0} {$i < $itk_option(-nentry)} {incr i} {
               bind $itk_component(entry$i) \
                  <KeyPress> [code $this validate_ %A %K $i]
            }
         }
      }
   }

   #  For compat with iwidgets entryfield: action for invalid char
   #  with -validate
   itk_option define -invalid invalid Command {bell}

   #  Disabled foreground color
   itk_option define -disabledforeground disabledforeground DisabledForeground {grey}

   # -- protected vars --

   #  This flag is set to 1 to avoid tracing when changing the entry's value
   protected variable notrace_ 0

   #  Command to validate entry contents
   protected variable validate_cmd_ ""

   #  Number of entry widgets that exist.
   protected variable nentry_ 0

   #  Are the entries created (first time).
   protected variable started_ 0

   #  Variable for tracing changes in all entry fields.
   protected variable tracevars_
}
