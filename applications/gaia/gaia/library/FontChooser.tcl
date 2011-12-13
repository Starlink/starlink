#+
#  Name:
#     FontChooser

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Chooser dialog for fonts.

#  Description:
#     Display a list of the available fonts, styles and a selection
#     of sizes. The window is a dialog so will block other interactions
#     while in operation if modal is set 1.
#
#     FontChooser .fc -title "Font chooser"
#
#     if {[.fc activate]} {
#        puts stdout "OK >>==> [.fc get]"
#     } else {
#        puts stdout "Cancel"
#     }
#     .fc destroy

#  Invocations:
#
#        FontChooser object_name [configuration options]
#
#     This creates an instance of a FontChooser object. The return is
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
#     See method definitions below.

#  Inheritance:
#     util::TopLevelWidget

#  Copyright:
#     Copyright (C) 2008-2009 Science and Technology Facilities Council.
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
#     PWD: Peter Draper (JAC - Durham University)
#     {enter_new_authors_here}

#  History:
#     07-MAR-2008 (PWD):
#        Original version.
#     {enter_further_changes_here}
#-

#.

itk::usual FontChooser {}

itcl::class gaia::FontChooser {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Default title.
      configure -title "Font Chooser"

      #  Platform fonts.
      set fonts_ [lsort -dictionary [font families]]

      #  Pick a default font, style and size based on the TkDefaultFont.
      set_default_font ""

      #  Evaluate any options.
      eval itk_initialize $args

      #  If asked restrict font selected to fixed width.
      if { $itk_option(-fixed_width) } {
         set fixed_fonts ""
         foreach f $fonts_ {
            set fa [font create -family "$f"]
            if { [font metrics $fa -fixed] } {
               lappend fixed_fonts $f
            }
         }
         if { $fixed_fonts != {} } {
            set fonts_ $fixed_fonts
         }
      }
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Construct the interface.
   public method init {} {

      #  Close window is like cancel.
      wm protocol $w_ WM_DELETE_WINDOW [code $this done_ 0]

      #  Three frames for the basic top to bottom layout.
      itk_component add topf {
         frame $w_.top
      }
      itk_component add midf {
         frame $w_.middle
      }
      itk_component add botf {
         frame $w_.bottom
      }

      pack $itk_component(topf) -side top -fill both -expand 1
      pack $itk_component(midf) -side top -fill x
      pack $itk_component(botf) -side top -fill x

      #  Three columns, font, style and size, like a file chooser.
      itk_component add fontframe {
         frame $itk_component(topf).fontf
      }
      itk_component add styleframe {
         frame $itk_component(topf).stylef
      }
      itk_component add sizeframe {
         frame $itk_component(topf).sizef
      }

      pack $itk_component(fontframe) -side left -fill both -expand 1
      pack $itk_component(styleframe) -side left -fill both -expand 1
      pack $itk_component(sizeframe) -side left -fill both -expand 1

      #  Labelled values.
      itk_component add fontlabel {
         labelframe $itk_component(fontframe).label -text "Font:"
      } {}
      itk_component add fontvalue {
         entry $itk_component(fontlabel).value -textvariable [scope font_]
      }

      itk_component add stylelabel {
         labelframe $itk_component(styleframe).label -text "Style:"
      } {}
      itk_component add stylevalue {
         entry $itk_component(stylelabel).value -textvariable [scope style_]
      }

      itk_component add sizelabel {
         labelframe $itk_component(sizeframe).label -text "Size:"
      } {}
      itk_component add sizevalue {
         entry $itk_component(sizelabel).value -textvariable [scope size_]
      }

      pack $itk_component(fontlabel) -side top -fill both -expand 1 -padx 4 -ipady 1m
      pack $itk_component(stylelabel) -side top -fill both -expand 1 -padx 4 -ipady 1m
      pack $itk_component(sizelabel) -side top -fill both -expand 1 -padx 4 -ipady 1m

      pack $itk_component(fontvalue) -side top -fill both -padx 4 -ipady 1m
      pack $itk_component(stylevalue) -side top -fill both -padx 4 -ipady 1m
      pack $itk_component(sizevalue) -side top -fill both -padx 4 -ipady 1m

      #  Listboxes of selectable options.
      itk_component add fontlist {
         gaia::Scrollbox $itk_component(fontlabel).list \
            -listvariable [scope fonts_] \
            -scrollbarplaces right \
            -height 7 \
            -exportselection 0 \
            -singleselect 1
      }
      set index [lsearch -exact $fonts_ $font_]
      $itk_component(fontlist) select set $index
      $itk_component(fontlist) see $index

      itk_component add stylelist {
         gaia::Scrollbox $itk_component(stylelabel).list \
            -listvariable [scope styles_] \
            -scrollbarplaces right \
            -height 7 \
            -exportselection 0 \
            -singleselect 1
      }
      set index [lsearch -exact $styles_ $style_]
      $itk_component(stylelist) select set $index
      $itk_component(stylelist) see $index

      itk_component add sizelist {
         gaia::Scrollbox $itk_component(sizelabel).list \
            -listvariable [scope sizes_] \
            -scrollbarplaces right \
            -height 7 \
            -exportselection 0 \
            -singleselect 1
      }
      set index [lsearch -exact $sizes_ $size_]
      $itk_component(sizelist) select set $index
      $itk_component(sizelist) see $index

      pack $itk_component(fontlist) -side top -fill both -expand 1 -padx 4 -ipadx 1m
      pack $itk_component(stylelist) -side top -fill both -expand 1 -padx 4 -ipadx 1m
      pack $itk_component(sizelist) -side top -fill both -expand 1 -padx 4 -ipadx 1m

      #  Clicking on listbox updates font display.
      set listname [$itk_component(fontlist) listname]
      bind $listname <<ListboxSelect>> [code $this update_font_]

      set listname [$itk_component(stylelist) listname]
      bind $listname <<ListboxSelect>> [code $this update_style_]

      set listname [$itk_component(sizelist) listname]
      bind $listname <<ListboxSelect>> [code $this update_size_]

      #  Sample of our current font. Use label within label to avoid
      #  resizing as font changes and add padding to allow the view
      #  to show bigger fonts.
      itk_component add sampleframe {
         labelframe $itk_component(midf).sampleframe -text "Sample"
      } {}
      itk_component add sampleouter {
         label $itk_component(sampleframe).sample \
            -bd 2 \
            -pady 10 \
            -relief sunken
      }
      itk_component add sample {
         entry $itk_component(sampleouter).sample \
            -bd 2 \
            -relief flat \
            -width 30 \
            -justify c \
            -textvariable [scope sampletext_]
      }
      pack $itk_component(sampleframe) -side top -fill x -expand 1 -padx 4 -ipadx 5m
      pack $itk_component(sampleouter) -side top -fill both -expand 1 -padx 10 -pady 10
      pack $itk_component(sample) -side top -fill both -expand 1 -ipady 100
      pack propagate $itk_component(sampleouter) 0

      #  Buttons to accept and cancel.
      itk_component add ok {
         button $itk_component(botf).ok \
            -text OK \
            -command [code $this done_ 1]
      }
      itk_component add cancel {
         button $itk_component(botf).cancel \
            -text Cancel \
            -command [code $this done_ 0]
      }
      pack $itk_component(ok) -side left -expand 1 -padx 4 -ipadx 1m
      pack $itk_component(cancel) -side left -expand 1 -padx 4 -ipadx 1m

      #  Initialise.
      update_
   }

   #  Update after new font is selected.
   protected method update_font_ {} {
      set s [$itk_component(fontlist) curselection]
      if { $s != {} } {
         set font_ [$itk_component(fontlist) get $s]
         update_
      }
   }

   #  Update after new style is selected.
   protected method update_style_ {} {
      set s [$itk_component(stylelist) curselection]
      if { $s != {} } {
         set style_ [$itk_component(stylelist) get $s]
         update_
      }
   }

   #  Update after size is changed.
   protected method update_size_ {} {
      set s [$itk_component(sizelist) curselection]
      if { $s != {} } {
         set size_ [$itk_component(sizelist) get $s]
         update_
      }
   }

   #  Change the display of the font sample to reflect the current
   #  values.
   protected method update_ {} {
      $itk_component(sample) configure -font [get]
   }

   #  Activate the dialog and wait for the user interaction to end.
   #  Returns 1 if OK was pressed.
   public method activate {} {
      wm deiconify $w_
      if { $itk_option(-modal) } {
         grab $w_
      }
      tkwait variable [scope result_]
      wm withdraw $w_
      return $result_
   }

   #  Get the current font.
   public method get {} {

      #  Font name can have several words so put into a list.
      set font [list $font_]
      lappend font $size_
      lappend font $style_

      return "$font"
   }

   #  Handle OK or Cancel.
   protected method done_ {OK} {
      if { $itk_option(-modal) } {
         grab release $w_
      }
      set result_ $OK
   }

   #  Set the interface to use a given font. If this is the empty string then
   #  the default will be set to that displayed in an entry widget.
   public method set_default_font {font} {
      if { $font == {} } {
         set font TkDefaultFont
      }
      array set default_font [font actual $font]

      set font_ $default_font(-family)
      set size_ $default_font(-size)

      if { $default_font(-weight) == "bold" && $default_font(-slant) == "italic" } {
         set style_ "bold italic"
      } elseif { $default_font(-weight) == "bold" } {
         set style_ "bold"
      } elseif { $default_font(-slant) == "italic" } {
         set style_ "italic"
      } else {
         set style_ "normal"
      }
   }


   #  Configuration options: (public variables)
   #  ----------------------

   #  flag: if true, grab the screen
   itk_option define -modal modal Modal 0

   #  flag: if true then only fixed width fonts will be shown.
   itk_option define -fixed_width fixed_width Fixed_Width 0

   #  Protected variables: (available to instance)
   #  --------------------

   #  Result of interaction, OK == 1 or Cancel == 0.
   protected variable result_ 0

   #  Selected font.
   protected variable font_ {}

   #  Selected style.
   protected variable style_ normal

   #  Selected size.
   protected variable size_ 12

   #  Currently available fonts.
   protected variable fonts_ {}

   #  Font styles.
   protected variable styles_ {normal italic bold "bold italic"}

   #  Default sizes.
   protected variable sizes_ {8 9 10 11 12 14 16 18 20 22 24 26 28 36 48}

   #  Strike through.
   protected variable strikethrough_ 0

   #  Underline.
   protected variable underline_ 0

   #  Sample text.
   protected variable sampletext_ "AaBbYyZz\u03b1\u03b2\u03b3\u03b4"

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
