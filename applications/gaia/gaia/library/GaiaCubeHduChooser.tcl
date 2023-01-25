#+
#  Name:
#     GaiaCubeHduChooser

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Chooser for HDUs in FITS or HDS container files.

#  Description:
#     This class provides an interface for browsing all the extensions
#     available in a FITS MEF file or NDFs in an HDS container file. The
#     "HDUs" can currently only be opened if a cube or a table containing
#     a compressed cube (RICE encoded).
#
#     Note that this class isn't related to GaiaHduChooser, that only offers
#     Skycat functionality which requires support of an rtdimage, this class
#     is based on more direct access without any image bias.

#  Invocations:
#
#        GaiaCubeHduChooser object_name [configuration options]
#
#     This creates an instance of a GaiaCubeHduChooser object. The return is
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
#     Copyright (C) 2008 Science and Technology Facilities Council.
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
#     31-JAN_2008 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaCubeHduChooser {}

itcl::class gaia::GaiaCubeHduChooser {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Decoration.
      wm title $w_ "FITS HDUs/NDFs ($itk_option(-number))"
      wm iconname $w_ "FITS HDUs/NDFs ($itk_option(-number))"

      #  Close always destroys the window.
      wm protocol $w_ WM_DELETE_WINDOW [code $this quit]
   }

   #  Destructor:
   #  -----------
   destructor  {
      if { $namer_ != {} } {
         catch {::delete object $namer_}
      }
   }

   #  Methods:
   #  --------

   #  Quit this widget
   public method quit { } {
      destroy $w_
   }

   #  This method is called after the options have been evaluated.
   protected method init {} {

      set headings [[get_accessor] hdu listheadings]

      #  TableList(n) widget for displaying the list of HDUs.
      itk_component add table {
         util::TableList $w_.table \
            -title {FITS HDUs/NDFs} \
            -headings $headings \
            -width [string length $headings]
      }
      add_short_help $itk_component(table) \
         {Table: Click to select HDU, double-click to open}
      pack $itk_component(table) -side top -fill both -expand 1

      #  Double-click opens HDU.
      bind $itk_component(table).listbox <Double-ButtonPress-1> [code $this select_hdu]

      #  Add a row of buttons at bottom.
      itk_component add buttons {
         frame $w_.buttons -borderwidth 2
      }
      pack $itk_component(buttons) -side top -fill x

      itk_component add open {
         button $itk_component(buttons).open \
            -text Open \
            -command [code $this select_hdu]
      }
      add_short_help $itk_component(open) {Open the selected HDU}
      pack $itk_component(open) -side left -expand 1 -fill x -padx 1m -ipadx 1m

      itk_component add close {
         button $itk_component(buttons).close \
            -text Close \
            -command [code $this quit]
      }
      add_short_help $itk_component(close) {Close the window}
      pack $itk_component(close) -side left -expand 1 -fill x -padx 1m -ipadx 1m

      #  Populate.
      show_hdu_list
   }

   #  Get the current accessor being used by the GaiaCube instance.
   public method get_accessor {} {
      return [$itk_option(-gaiacube) get_cubeaccessor]
   }

   #  If the current FITS table is a compressed image or cube, uncompress
   #  it and display.
   protected method set_table {name hdu} {

      #  Get name of FITS file.
      set file [[get_accessor] cget -dataset]

      #  May be a compressed cube masquerading as a table. Check for that.
      if { [string first "COMPRESSED_IMAGE" $name] > -1 } {

         #  Arrange for decompression of this extension and then load it
         #  as required.
         decompress_inline_ $file $hdu
         return
      }
   }

   #  Decompress a cube stored in an extension (RICE format for instance)
   #  and display it. Will reuse a decompressed extension, if already
   #  processed. Decompression is supported by the StarFITSIO write method, so
   #  we just move to the HDU and save it to disk.
   protected method decompress_inline_ {file hdu} {

      #  If this extension is already done, and the file still exists,
      #  reuse it.
      if { [info exists tempfiles_($file,$hdu)] &&
           [::file exists $tempfiles_($file,$hdu)] } {
         set converted_file $tempfiles_($file,$hdu)
      } else {

         #  Perform the conversion.
         incr count_
         set converted_file [gaia::GaiaTempName::make_name \
                                "GaiaCubeHduImg" $count_ ".fits"]
         if { [file exists $converted_file] } {
            ::file delete -force $converted_file
         }

         #  Write the HDU out to disk.
         set accessor [get_accessor]
         set objname "[$accessor cget -dataset]+\[${hdu}\]"
         $accessor hdu get $hdu $converted_file $objname

         #  Success so cache for next time.
         set tempfiles_($file,$hdu) $converted_file
      }

      #  Display the cube as an image, need to do this as we it could be an
      #  image really. Note this is not marked temporary as we reuse these
      #  files and they do not represent a processed result of any kind.
      set gaia [$itk_option(-gaiacube) cget -gaia]
      $gaia open $converted_file

      #  Close this window as now out of date (the extracted cube isn't part
      #  of this MEF).
      quit
   }

   #  Remove all the temporary files we have created. Needs to be called
   #  sometime before the application exits. Not done by the destructor as
   #  that would stop any chance of caching and reusing converted files.
   public proc release_temporary_files {} {
      if { [info exists tempfiles_] } {
         foreach f [array names tempfiles_] {
            if { [::file exists $tempfiles_($f)] } {
               ::file delete -force $tempfiles_($f)
               set tempfiles_($f) {}
            }
         }
      }
   }

   #  Handle the selected HDU.
   protected method set_hdu {hdu} {
      catch {
         $itk_component(table) select_row [expr $hdu-1]
      }

      #   Pick out the HDU description from the full list.
      set accessor [get_accessor]
      set list [$accessor hdu list]
      set hdulist [lindex $list [expr $hdu-1]]

      #   Assign parts to the names of the headers.
      set headings [$accessor hdu listheadings]
      eval lassign [list $hdulist] $headings

      #   Display only a real image or cube (not a dummy primary).
      if { ( "$Type" == "image" || "$Type" == "NDF" ) && "$NAXIS" < 1 } {
         return
      }

      #  Move to the HDU if an image or cube..
      if { ( "$Type" == "image" || "$Type" == "NDF" ) } {

         #  Update name with this HDU.
         set dataset [$accessor cget -dataset]
         if { $namer_ == {} } {
            set namer_ [gaia::GaiaImageName \#auto]
         }
         $namer_ configure -imagename $dataset

         if { "$Type" == "NDF" } {
            if { $ExtName != "." } {
               $namer_ setpath "${ExtName}"
            }
         } else {
            $namer_ setfitshdunum $hdu
         }

         if { "$NAXIS" >= 3 } {
            $itk_option(-gaiacube) configure -cube [$namer_ fullname]
         } else {
            set gaia [$itk_option(-gaiacube) cget -gaia]
            $gaia open [$namer_ fullname]
         }

         #  Close as need to re-visit to populate for new MEF/NDF.
         quit
      }
   }

   #  Update the list of HDUs. This should be invoked when the accessor data
   #  is changed.
   public method show_hdu_list {} {

      #  Update the table listing
      set accessor [get_accessor]
      set hdu_list [$accessor hdu list]
      $itk_component(table) clear
      $itk_component(table) config -height [llength $hdu_list] -info $hdu_list

      #  And table title.
      $itk_component(table) configure -title "  [$accessor cget -dataset]  "
   }

   #  Set the HDU to display. Makes the currently selected HDU the current HDU
   protected method select_hdu {} {
      set sel [$itk_component(table) get_selected]
      if {[llength $sel]} {
         lassign [lindex $sel 0] hdu type name
         if { "$type" == "image" || "$type" == "NDF" } {
            set_hdu $hdu
         } elseif { "$type" == "ascii" || "$type" == "binary" } {
            set_table $name $hdu
         }
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  GaiaCube instance. Must be defined.
   itk_option define -gaiacube gaiacube GaiaCube {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  GaiaImageName instance to create HDU qualified names.
   protected variable namer_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Counter for temporary names.
   common count_ 0

   #  Array of temporary files. These are indexed by the filename and
   #  extension and may be reused. Shared by all instances.
   common tempfiles_

   #  Instance of astrocat for handling Skycat catalogues.
   common astrocat_ [astrocat ::cat::.astrocat]

#  End of class definition.
}
