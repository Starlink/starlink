#+
#  Name:
#     GaiaMOC

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class for drawing IVOA MOC regions over the image.

#  Description:
#     This class provides a toolbox for drawing IVOA MOC regions over an
#     image. MOCs are defined as part of the VO:
#
#        http://www.ivoa.net/documents/MOC/
#
#     The regions are defined as HEALpix index tables, so we just
#     provide the ability to read these from a FITS file.

#  Invocations:
#
#        GaiaMOC object_name [configuration options]
#
#     This creates an instance of a GaiaMOC object. The
#     return is the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#     See the "itk_option define" declarations below.

#  Methods:
#     See the method declarations below.

#  Inheritance:
#     TopLevelWidget

#  Copyright:
#     Copyright (C) 2018 Peter W. Draper.
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
#     PWD: Peter Draper (Durham University)
#     {enter_new_authors_here}

#  History:
#     11-DEC-2018 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaMOC {}

itcl::class gaia::GaiaMOC {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: HEALPix Multi-Order Coverage map ($itk_option(-number))"

      #  Add short help window.
      make_short_help

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File" left]
      configure_menubutton File -underline 0

      #  Add the options menu
      set Options [add_menubutton "Options" left]
      configure_menubutton Options -underline 0

      #  Add window help.
      add_help_button stcs "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      #  Add option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      $short_help_win_ add_menu_short_help $File \
         {New window} {Create a new toolbox}

      #  Save description to a file.
      #$File add command \
      #   -label {Save description...} \
      #  -command [code $this write_file] \
      #   -accelerator {Control-s}
      #bind $w_ <Control-s> [code $this write_file]
      #$short_help_win_ add_menu_short_help $File \
      #   {Save description...}\
      #   {Write the current description to a text file}

      #  Read MOC from a file.
      $File add command \
         -label {MOC FITS file...} \
         -command [code $this read_file] \
         -accelerator {Control-r}
      bind $w_ <Control-r> [code $this read_file]
      $short_help_win_ add_menu_short_help $File \
         {MOC FITS file...}\
         {Read a MOC from a table in a FITS file}

      #  Set the exit menu item.
      $File add command -label Exit \
         -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]

      #  Option menu for selecting the colour.
      $Options add cascade -label "Colour" -menu [menu $Options.colour]
      gaia::ColourMenu \#auto $Options.colour \
         -change_cmd [code $this set_colour_]

      #  Add menu for selecting the width.
      $Options add cascade -label "Width" -menu [menu $Options.width]
      foreach i {1 2 3 4} {
         $Options.width add radiobutton \
            -value width$i \
            -bitmap width$i \
            -command [code $this set_width_ $i]
      }

      #  Text area for the FITS filename. XXX not needed.
      itk_component add mainrule {
         LabelRule $w_.astrule \
            -text "MOC FITS file:"
      }
      pack $itk_component(mainrule) -side top -fill x -expand 1

      itk_component add maintext {
         gaia::ScrollText $w_.text
      }
      pack $itk_component(maintext) -side top -fill both -expand 1
      set lwidth 21

      #  Create the button bar
      itk_component add actionframe {frame $w_.action}

      #  Add a button to close window.
      itk_component add close {
         button $itk_component(actionframe).close -text Close \
            -command [code $this close]
      }
      add_short_help $itk_component(close) {Close window}

      #  Draw the MOCs.
      itk_component add draw {
         button $itk_component(actionframe).draw -text {Draw MOCs} \
            -command [code $this draw 1]
      }
      add_short_help $itk_component(draw) {Draw MOCs}

      #  Pack all the components into place.
      pack $itk_component(actionframe) -side bottom -fill x -pady 5 -padx 5
      pack $itk_component(mainrule) -side top -fill x -expand 1
      pack $itk_component(maintext) -side top -fill both -expand 1
      pack $itk_component(close) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(draw) -side left -expand 1 -pady 3 -padx 3

      #  Initialise the graphics tag.
      set regiontag_ "ckey[incr unique_]"
      set drawn_ 0
   }

   #  Destructor:
   #  -----------
   destructor  {
      #  Do nothing.
   }

   #  Methods:
   #  --------

   #  Create a new instance of this object.
   protected method clone_me_ {} {
      if { $itk_option(-clone_cmd) != {} } {
         eval $itk_option(-clone_cmd)
      }
   }

   #  Close this window, kill it if needed, otherwise withdraw. Also
   #  remove the contours.
   public method close {} {

      #  Remove the contours.
      catch {clear_regions}

      if { $itk_option(-really_die) } {
         delete object $this
      } else {
         wm withdraw $w_
      }
   }

   #  Public redraw method. Only used externally to this class, which
   #  means we can decline to draw the regions unless they have already
   #  been drawn using the button.
   public method redraw { {override 0} } {
      if { $drawn_ || $override } {
         draw 1
      }
   }

   #  Save the current description to a file. XXX not used.
   public method write_file {} {
      set w [FileSelect .\#auto -title "Save description to a file"]
      if {[$w activate]} {
         save_description [$w get]
      }
      destroy $w
   }

   #  Read MOC from a FITS file. XXX use an entry dialog with file selector.
   public method read_file {} {
      set w [FileSelect .\#auto -title "Read MOC from a FITS file"]
      if {[$w activate]} {
         read_description [$w get]
      }
      destroy $w
   }

   #  Set the MOC filename.
   public method read_description {filename} {
      set filename_ $filename
   }

   #  Draw the regions.
   public method draw {{all 1} args} {
      busy {

         #  Clear existing regions.
         clear_regions

         #  Set the tag used to control clear etc.
         $itk_option(-rtdimage) configure -ast_tag \
            "$itk_option(-ast_tag) $regiontag_"

         #  Gather attributes (colour and width), note width is of
         #  canvas item so we do that directly.
         set attributes "colour(border)=$colour_"

         #  And draw...
         set moc [gaiautils::fitsmoc $filename_]
         $itk_option(-rtdimage) mocplot $moc $attributes
         gaiautils::astannul $moc
         update
         set_width_ $width_
      }

      #  Some regions are now drawn.
      set drawn_ 1
   }

   #  Clear the regions.
   public method clear_regions {} {
      $itk_option(-canvas) delete $regiontag_

      #  No regions are now drawn.
      set drawn_ 0
   }

   #  Set the colour.
   protected method set_colour_ {index} {
      if { $index != $colour_ } {
         set colour_ $index
         draw 1
      }
   }

   #  Set the width.
   protected method set_width_ {width} {
      set width_ $width
      $itk_option(-canvas) itemconfigure $regiontag_ -width $width
   }


   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of canvas.
   itk_option define -canvas canvas Canvas {} {}

   #  Name of rtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {} {}

   #  Name of RtdImageCtrl widget or a derived class.
   itk_option define -image image Image {} {}

   #  Name of CanvasDraw widget.
   itk_option define -canvasdraw canvasdraw CanvasDraw {} {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  If this is a clone, then it should die rather than be withdrawn.
   itk_option define -really_die really_die Really_Die 0

   #  Global tag used to control redraws etc. 
   itk_option define -ast_tag ast_tag Ast_Tag {} {
      if { $itk_option(-ast_tag) == {} } {
         set itk_option(-ast_tag) "ast_element"
      }
   }

   #  Maximum width of contour line (as multiple of 0.005).
   itk_option define -maxwidth maxwidth Maxwidth 10

   #  Protected variables: (available to instance)
   #  --------------------

   #  Name of MOC FITS file.
   protected variable filename_ {}

   #  Whether regions have been drawn.
   protected variable drawn_

   #  Width of lines.
   protected variable width_ 1

   #  Current colour.
   protected variable colour_ 2

   #  Tag for canvas items created by this instance.
   protected variable regiontag_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Unique number across all instances of this class.
   common unique_ 0


#  End of class definition.
}
