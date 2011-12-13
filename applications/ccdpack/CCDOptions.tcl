#+
#  Name:
#     CCDOptions

#  Purpose:
#     Sets the default look for a CCDPACK Tcl/Tk application.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk commands

#  Invocation:
#     source CCDOptions

#  Description:
#     This file sets up the options for controlling the default look of
#     a CCDPACK Tcl/Tk application. This sets things like the default
#     padding and reliefs of the various widgets. To override these
#     values use explicit widget commands or set using X defaults or
#     enter suitable commands in a $HOME/.ccdpack file.
#
#     The colour scheme is the default widget one, but this may be changed
#     using the CCDprefs(scheme_colour) value.
#
#     The priority of the options is set up userDefault, should
#     really be startupFile to allow .Xdefaults to override, but these
#     generally look horrid.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995, 2000 Central Laboratory of the Research
#     Councils. Copyright (C) 2006 Particle Physics & Astronomy
#     Research Council. All Rights Reserved.

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
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     11-MAR-1994 (PDRAPER):
#        Original version. Colours based on Tix grey schemes by Ioi Kim Lam.
#     9-MAY-1994 (PDRAPER):
#        Added option to read application startup file ~/.ccdpack
#     5-MAY-1995 (PDRAPER):
#        Started to tune for Tk4. Default colours may be gray soon in
#        which case this file will become mostly redundant.
#     9-MAY-1995 (PDRAPER):
#        Added defaults for takeFocus and highlightThickness. These
#        are switched off by default and enabled for those widgets
#        capable of dealing with the focus sensibly. Added the
#        tk_focusFollowsMouse option.
#     26-MAY-1995 (PDRAPER):
#        Removed strict Motif directive.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     31-JAN-2006 (PDRAPER):
#        Finally removed all colour defaults. Standard set are no
#        longer bisque, so may as well use them.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.
      global CCDprefs
      global env
#.

#  Set the priority level for the options. Use userDefault so that user
#  cannot override these in their .Xdefaults.
     set CCDprefs(priority) userDefault

#  Make a guess for a good point size for any fonts based on the display
#  resolution.
      set display_x [ winfo vrootwidth . ]
      set display_y [ winfo vrootheight . ]
      if {  $display_x > 800  &&  $display_y > 800  } {
         set font_size_default 14
      } else {
         set font_size_default 12
      }
      if { ! [ info exists CCDprefs(font_size) ] } {
         set CCDprefs(font_size) $font_size_default
      } else {
         if { $CCDprefs(font_size) != 12  && \
              $CCDprefs(font_size) != 14  } {
            set CCDprefs(font_size) $font_size_default
         }
      }

#  Miscellaneous defaults.
      set CCDprefs(anchor)           c
      set CCDprefs(border)           2
      set CCDprefs(click_for_focus)  1

#  Read in user preferences which may override the "intrinsic" defaults.
#  These are found in a file $HOME/.ccdpack by default.
      if { [ file exists $env(HOME)/.ccdpack ] } {
         source $env(HOME)/.ccdpack
      }

#  Are we to override the colours defaults?
      if { [info exists CCDprefs(scheme_colour)] } {
         tk_setPalette $CCDprefs(scheme_colour)
      }

#  Set default fonts using the point size.
      set CCDprefs(font)         -*-helvetica-medium-r-normal-*-$CCDprefs(font_size)-*-*-*-*-*-*-*
      set CCDprefs(bold_font)    -*-helvetica-bold-r-normal-*-$CCDprefs(font_size)-*-*-*-*-*-*-*
      set CCDprefs(menu_font)    -*-helvetica-bold-o-normal-*-$CCDprefs(font_size)-*-*-*-*-*-*-*
      set CCDprefs(fixed_font)   -*-courier-medium-r-*-*-$CCDprefs(font_size)-*-*-*-*-*-*-*

#  Set defaults for screen attributes of widgets.
      set prior $CCDprefs(priority)
      option add *Font                 $CCDprefs(font)                   $prior
      option add *font                 $CCDprefs(font)                   $prior
      option add *activeBorderWidth    $CCDprefs(border)                 $prior
      option add *anchor               $CCDprefs(anchor)                 $prior
      option add *borderWidth          $CCDprefs(border)                 $prior
      option add *highlightThickness   0                                 $prior
      option add *takeFocus            0                                 $prior

#  Buttons:
      option add *Button.borderWidth        $CCDprefs(border)            $prior
      option add *Button.anchor             $CCDprefs(anchor)            $prior
      option add *Button.highlightThickness $CCDprefs(border)            $prior
      option add *Button.takeFocus          {}                           $prior

#  Checkbuttons:
      option add *Checkbutton.relief       flat                          $prior
      option add *Checkbutton.highlightThickness $CCDprefs(border)       $prior
      option add *Checkbutton.takeFocus    {}                            $prior

#  Radiobuttons:
      option add *Radiobutton.relief       flat                          $prior
      option add *Radiobutton.highlightThickness $CCDprefs(border)       $prior
      option add *Radiobutton.takeFocus          {}                      $prior

#  Entries:
      option add *Entry.relief           ridge                           $prior
      option add *Entry.highlightThickness $CCDprefs(border)             $prior
      option add *Entry.takeFocus        {}                              $prior
      option add *Entry.font             $CCDprefs(fixed_font)           $prior

#  Frames:
      option add *Frame.relief             groove                        $prior
      option add *Frame.borderWidth        3                             $prior
      option add *Frame.takeFocus          0                             $prior

#  Labels:
      option add *Label.anchor          w                                $prior
      option add *Label.borderWidth     0                                $prior
      option add *Label.relief          flat                             $prior
      option add *Label.font            $CCDprefs(bold_font)             $prior
      option add *Label.takeFocus       0                                $prior

#  Listboxes:
      option add *Listbox.selectmode    browse                           $prior
      option add *Listbox.relief        sunken                           $prior
      option add *Listbox.font          $CCDprefs(fixed_font)            $prior
      option add *Listbox.width         20                               $prior
      option add *Listbox.setGrid       1                                $prior
      option add *Listbox.highlightThickness $CCDprefs(border)           $prior
      option add *Listbox.selectBorderWidth  $CCDprefs(border)           $prior
      option add *Listbox.takeFocus     {}                               $prior

#  Ccd::tables:
      option add *Ccd*table.relief      sunken                           $prior

#  Menus:
      option add *Menu*font             $CCDprefs(menu_font)             $prior
      option add *Menu*highlightThickness $CCDprefs(border)              $prior
      option add *Menu*takeFocus          {}                             $prior

#  Menubuttons:
      option add *Menubutton*font        $CCDprefs(menu_font)            $prior
      option add *Menubutton.relief      flat                            $prior
      option add *Menubutton.highlightThickness $CCDprefs(border)        $prior
      option add *Menubutton.takeFocus   {}                              $prior

#  Scales:
      option add *Scale.highlightThickness $CCDprefs(border)             $prior
      option add *Scale.takeFocus          {}                            $prior

#  Scrollbars:
      option add *Scrollbar.width            13                          $prior
      option add *Scrollbar.relief           ridge                       $prior
      option add *Scrollbar.highlightThickness $CCDprefs(border)         $prior
      option add *Scrollbar.takeFocus          {}                        $prior

#  Texts:
      option add *Text.relief        sunken                              $prior
      option add *Text.font          $CCDprefs(fixed_font)               $prior
      option add *Text.width         80                                  $prior
      option add *Text.height        20                                  $prior
      option add *Text.setGrid       true                                $prior
      option add *Text.highlightThickness $CCDprefs(border)              $prior
      option add *Text.takeFocus          {}                             $prior

#  Messages:
      option add *Message*font            $CCDprefs(bold_font)           $prior
      option add *Message*relief          flat                           $prior

#  Canvas:
      option add *Canvas                  {}                             $prior

#  Focus follows mouse.
      if { ! $CCDprefs(click_for_focus) } {
         tk_focusFollowsMouse
      }

# $Id$
