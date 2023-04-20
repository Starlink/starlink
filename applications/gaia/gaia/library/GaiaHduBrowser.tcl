#+
#  Name:
#     GaiaHduBrowser

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Browser for HDUs in FITS, HDS and VOTable container files.

#  Description:
#     This class provides an interface for browsing all the extensions
#     available in a FITS MEF file, NDFs in an HDS container file,
#     or TABLEs in a VOTable.
#
#     When selected for opening an open command will be executed. This
#     will be appended by the type of data selected (image or table), a
#     fully qualified name and the number of dimensions (if an image).

#  Invocations:
#
#        GaiaHduBrowser object_name [configuration options]
#
#     This creates an instance of a GaiaHduBrowser object. The return is
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
#     05-FEB-2008 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaHduBrowser {}

itcl::class gaia::GaiaHduBrowser {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Decoration.
      wm title $w_ "FITS HDUs/NDFs/VOTables ($itk_option(-number))"
      wm iconname $w_ "FITS HDUs/NDFs/VOTables ($itk_option(-number))"

      #  Close always destroys the window.
      wm protocol $w_ WM_DELETE_WINDOW [code $this quit]
   }

   #  Destructor:
   #  -----------
   destructor  {
      if { $namer_ != {} } {
         catch {::delete object $namer_}
      }
      if { $accessor_ != {} } {
         catch {::delete object $accessor_}
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
      if { $accessor_ == {} } {
         return
      }

      set headings [$accessor_ hdu listheadings]
      set nc [expr min([string length $headings],80)]

      #  TableList(n) widget for displaying the list of HDUs.
      itk_component add table {
         util::TableList $w_.table \
            -title {FITS HDUs/NDFs/VOTables} \
            -headings $headings \
            -hscroll 1 \
            -width $nc \
            -height 50
      }
      add_short_help $itk_component(table) \
         {Table: Click to select HDU, double-click to open}

      #  Double-click opens HDU.
      bind $itk_component(table).listbox <Double-ButtonPress-1> \
         [code $this select_hdu_]

      #  Add a row of buttons at bottom.
      itk_component add buttons {
         frame $w_.buttons -borderwidth 2
      }

      #  Open selected HDU.
      itk_component add open {
         button $itk_component(buttons).open \
            -text Open \
            -command [code $this select_hdu_]
      }
      add_short_help $itk_component(open) {Open the selected HDU}

      #  Add a Cancel button if required.
      if { $itk_option(-cancel_cmd) != {} } {
         itk_component add cancel {
            button $itk_component(buttons).cancel \
               -text Cancel \
               -command [code $this cancel]
         }
         add_short_help $itk_component(cancel) \
            {Close the window, cancelling action}
      }

      itk_component add close {
         button $itk_component(buttons).close \
            -text Close \
            -command [code $this quit]
      }
      add_short_help $itk_component(close) {Just close the window}

      #  Packing. Make sure buttons are always visible by packing first.
      pack $itk_component(buttons) -side bottom -fill x -expand 1
      pack $itk_component(table) -side top -fill both -expand 1
      pack $itk_component(open) -side left -expand 1 -padx 1m -ipadx 1m
      if { $itk_option(-cancel_cmd) != {} } {
         pack $itk_component(cancel) -side left -expand 1 -padx 1m -ipadx 1m
      }
      pack $itk_component(close) -side left -expand 1 -padx 1m -ipadx 1m

      #  Populate.
      show_hdu_list
   }

   #  Update the list of HDUs. This should be invoked when the accessor data
   #  is changed.
   public method show_hdu_list {} {

      #  Update the table listing
      set hdu_list [$accessor_ hdu list]
      $itk_component(table) clear
      $itk_component(table) config \
         -width 50 \
         -height [llength $hdu_list] \
         -info $hdu_list
      $itk_component(table) select_row 0

      #  And table title.
      $itk_component(table) configure -title "  [$accessor_ cget -dataset]  "
   }

   #  HDU is selected. Need to fire the open_cmd if set.
   protected method select_hdu_ {} {
      if { $itk_option(-open_cmd) != {} } {
         set sel [$itk_component(table) get_selected]
         if {[llength $sel]} {
            if { $isvotable_ } {
               lassign [lindex $sel 0] hdu title
               open_table_ $itk_option(-file) $hdu
            } else {
               lassign [lindex $sel 0] hdu type name
               if { "$type" == "image" || "$type" == "NDF" } {
                  open_image_ $name $hdu
               } elseif { "$type" == "ascii" || "$type" == "binary" } {
                  open_table_ $name $hdu
               }
            }
            #  Close window after open.
            quit
         } else {
            info_dialog "No row is currently selected" $w_
         }
      }
   }

   #  Close the window, cancelling it in some way. Always destroys but might
   #  execute a command first.
   public method cancel {} {
      if { $itk_option(-cancel_cmd) != {} } {
         #  Start close now as next command may block (by starting
         #  up a dialog window).
         after idle [code $this quit]
         eval $itk_option(-cancel_cmd)
      }
   }

   #  If the current FITS table is a compressed image or cube, uncompress
   #  it and display.
   protected method open_table_ {name hdu} {

      #  Get name of FITS file.
      set file [$accessor_ cget -dataset]

      #  May be a compressed cube masquerading as a table. Check for that.
      if { [string first "COMPRESSED_IMAGE" $name] > -1 } {

         #  Arrange for decompression of this extension and then load it
         #  as required.
         decompress_inline_ $file $hdu
         return
      }

      #  Table, construct the fullname and send the open event.
      if { $hdu > 1 } {
         set catalogue "$file\{$hdu\}"
      } else {
         set catalogue "$file"
      }
      eval $itk_option(-open_cmd) table "$catalogue" 0
   }

   #  Decompress a cube stored in an extension (RICE format for instance) and
   #  send a open event for the resulting image or cube.  Will reuse a
   #  decompressed extension, if already processed. Decompression is supported
   #  by the StarFITSIO write method, so we just move to the HDU and save it to
   #  disk.
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
                                "GaiaBrowserHduImg" $count_ ".fits"]
         if { [file exists $converted_file] } {
            ::file delete -force $converted_file
         }

         #  Write the HDU out to disk.
         set objname "[$accessor_ cget -dataset]+\[${hdu}\]"
         $accessor_ hdu get $hdu $converted_file $objname

         #  Success so cache for next time.
         set tempfiles_($file,$hdu) $converted_file
      }

      eval $itk_option(-open_cmd) image "$converted_file" 0
   }

   #  Handle request to open an image or cube.
   protected method open_image_ {name hdu} {
      catch {
         $itk_component(table) select_row [expr $hdu-1]
      }

      #   Pick out the HDU description from the full list.
      set list [$accessor_ hdu list]
      set hdulist [lindex $list [expr $hdu-1]]

      #   Assign parts to the names of the headers.
      set headings [$accessor_ hdu listheadings]
      eval lassign [list $hdulist] $headings

      #  Update name with this HDU and open it.
      set dataset [$accessor_ cget -dataset]
      if { $namer_ == {} } {
         set namer_ [gaia::GaiaImageName \#auto]
      }
      $namer_ configure -imagename $dataset

      if { "$Type" == "NDF" } {
         if { $ExtName != "" && $ExtName != "." } {
            $namer_ setpath "${ExtName}"
         }
      } else {
         $namer_ setfitshdunum $hdu
      }

      #   Cubes have a non-trivial last dimension, make sure we get
      #   that right (note we don't have access to dimensions higher
      #   than 3, so this could still be incorrect).
      if { $NAXIS3 != {} && $NAXIS3 == 1 } {
         set NAXIS 2
      }
      eval $itk_option(-open_cmd) image \[$namer_ fullname\] \$NAXIS
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

   #  Configuration options: (public variables)
   #  ----------------------

   #  The file we're browsing. Can be FITS, HDS or XML.
   itk_option define -file file File {} {
      set isvotable_ 0
      if { $itk_option(-file) != {} } {
         set ext [::file extension $itk_option(-file)]
         if { [gaia::GaiaVOTableAccess::check_for_gaiavo] &&
              ( $ext == ".xml" || $ext == ".vot" ) } {

            #  VOTable and support available.
            set isvotable_ 1
            if { $accessor_ != {} } {
               if { ! [$accessor_ isa GaiaVOTableAccess] } {
                  catch {::delete object $accessor_}
                  set accessor_ {}
               }
            }
            if { $accessor_ == {} } {
               set accessor_ [uplevel \#0 GaiaVOTableAccess \#auto]
            }
            $accessor_ configure -dataset $itk_option(-file)
            if { [info exists itk_component(table)] } {
               show_hdu_list
            }

         } else {

            #  NDF or FITS.
            if { $accessor_ != {} } {
               if { ! [$accessor_ isa gaia::GaiaNDAccess] } {
                  catch {::delete object $accessor_}
                  set accessor_ {}
               }
            }
            if { $accessor_ == {} } {
               set accessor_ [uplevel \#0 gaia::GaiaNDAccess \#auto]
            }
            $accessor_ configure -dataset $itk_option(-file)
            if { [$accessor_ exists "DATA"] } {
               if { [info exists itk_component(table)] } {
                  show_hdu_list
               }
            }
         }
      }
   }

   #  Command to execute to handle an open request. This will be trailed
   #  by three arguments, the type of HDU (image or table), the fully
   #  qualified extension name and the dimensionality (only if a obvious
   #  image, otherwise this will be 0).
   itk_option define -open_cmd open_cmd Open_Cmd {}

   #  Command to execute if the cancel button is pressed (restore associated
   #  file browser for instance).
   itk_option define -cancel_cmd cancel_cmd Cancel_Cmd {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  The accessor.
   protected variable accessor_ {}

   #  The filename handler.
   protected variable namer_ {}

   #  Whether this is a VOTable.
   protected variable isvotable_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Instances of this class.
   common instances_ 0

   #  Counter for temporary names.
   common count_ 0

   #  Array of temporary files. These are indexed by the filename and
   #  extension and may be reused. Shared by all instances.
   common tempfiles_

#  End of class definition.
}
