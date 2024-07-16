#+
#  Name:
#     GaiaSampAgent

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     GAIA-specific implementation of SAMP MType support.

#  Description:
#     An instance of this class does the actual work for responding to
#     SAMP messages.  It should be installed in a samp::SampClient,
#     and it should contain a method for each MType that it implements,
#     along with the standard get_subscribed_mtypes method listing them.
#     The name of each method is the actual MType name.
#
#     Some of the MTypes here are standard SAMP ones; a list and
#     descriptions can be found near http://www.ivoa.net/samp/.

#  Copyright:
#     Copyright (C) 2009 Science and Technology Facilities Council.
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
#     MBT: Mark Taylor
#     {enter_new_authors_here}

#  History:
#     23-JUN-2009 (MBT):
#        Original version, adapted from GaiaPlastic.tcl.

#-

#  So we can decode encoded characters in the URL.
package require ncgi

itcl::class gaia::GaiaSampAgent {

   destructor {
      if { $temp_files_ != {} } {
         $temp_files_ clear
      }
   }

   #  Standard method listing MTypes implemented by this class.
   public method get_subscribed_mtypes {} {
      return {
         image.load.fits
         coord.pointAt.sky
         coverage.load.moc.fits
         table.load.votable
         table.select.rowList
         table.highlight.row
      }
   }

   #  Load a FITS file specified as a URL into the display.
   public method image.load.fits {sender_id param_list} {
      array set params $param_list
      set img_url $params(url)
      set basegaia [get_gaia_]
      if { $basegaia != {} } {
         set fname [get_file_ $img_url]
         if { $fname != {} } {
            $basegaia open $fname
         } else {
            #  Remote file, arrange to download this in the background, if not
            #  already busy downloading...
            if { $urlget_ == {} } {
               set urlget_ \
                  [gaia::GaiaUrlGet .\#auto -notify_cmd [code $this display_file_]]
               $urlget_ get $img_url
            } else {
               error "Already downloading - can't do two at once"
            }
         }
      } else {
         error "No GAIA window found for display"
      }
   }

   #  Point at a coordinate by drawing an identifier graphic, ra and dec
   #  are both in decimal degrees.
   public method coord.pointAt.sky {sender_id param_list} {
      array set params $param_list
      set ra $params(ra)
      set dec $params(dec)
      set basegaia [get_gaia_]
      if { $basegaia != {} } {
         $basegaia position_of_interest $ra $dec "deg J2000"
      } else {
         error "No GAIA window found for display"
      }
   }

   #  Load a MOC map in FITS format.
   public method coverage.load.moc.fits {sender_id param_list} {
      array set params $param_list
      set moc_url $params(url)
      set basegaia [get_gaia_]
      if { $basegaia != {} } {
         set fname [get_file_ $moc_url]
         if { $fname != {} } {
            display_coverage_ $fname ""
         } else {
            #  Remote file, arrange to download this in the background, if not
            #  already busy downloading...
            if { $urlget_ == {} } {
               set urlget_ \
                  [gaia::GaiaUrlGet .\#auto -notify_cmd [code $this display_coverage_]]
               $urlget_ get $moc_url
            } else {
               error "Already downloading - can't do two at once"
            }
         }
      } else {
         error "No GAIA window found for display"
      }
   }

   #  Execute Tcl code within GAIA.  This is a non-standard MType specific
   #  to GAIA.  Note that the MD5 checksum business used in the PLASTIC
   #  implementation is no longer required, since the SAMP protocol,
   #  unlike PLASTIC, contains defences against unauthorised attempts to
   #  send messages to applications.
   #
   #  MType specification:
   #     MType:
   #        gaia.execute.tcl
   #     Parameters:
   #        script (string) - executable Tcl
   #     Return values:
   #        value (string) - return value of successfully executed script
   #
   #  If an error is encountered while executing the script, a SAMP error
   #  response will result.
   #
   #  Withdrawn as potential security risk, made worse by SAMP opening
   #  up to web-applications.
   #
   #public method gaia.execute.tcl {sender_id param_list} {
   #   array set params $param_list
   #   set script $params(script)
   #   set result(value) [eval $script]
   #   return [array get result]
   #}

   #  Load a VOTable as a catalogue.
   public method table.load.votable {sender_id param_list} {
      array set params $param_list
      set url $params(url)
      set table_id [get_param_ params table-id $url]
      set table_name [get_param_ params name ""]

      if { $temp_files_ == {} } {
         set temp_files_ [gaia::GaiaTempName \#auto \
                             -prefix "GaiaTempTst" \
                             -type ".TAB" -exists 0]
      }
      set tst_file [$temp_files_ get_name]

      #  Note don't use stderr output as error indicator.
      exec -ignorestderr $::gaia_dir/vot2tab $url 0 $tst_file
      if {! [file exists $tst_file]} {
         error "VOTable conversion failed"
      }

      set window [display_table_ $tst_file $table_id]
      set cat_windows_($table_id) $window
   }

   #  Display only a selection of the rows from a previously loaded catalogue.
   public method table.select.rowList {sender_id param_list} {
      array set params $param_list
      set rows $params(row-list)
      [get_cat_window_ params {table-id url}] select_indices $rows
   }

   #  Highlight a single row from a previously loaded catalogue.
   public method table.highlight.row {sender_id param_list} {
      array set params $param_list
      set row $params(row)
      [get_cat_window_ params {table-id url}] highlight_index $row
   }

   #  Protected methods:
   #  ------------------

   #  Download and display a file.
   protected method display_file_ {filename type} {
      set basegaia [get_gaia_]
      if { $basegaia != {} } {
         $basegaia open $filename
      }
      if { $urlget_ != {} } {
         catch {delete object $urlget_}
         set urlget_ {}
      }
   }

   # Display a coverage map (MOC in FITS format).
   protected method display_coverage_ {filename type} {
      set basegaia [get_gaia_]
      if { $basegaia != {} } {
        set toolbox [$basegaia get_toolbox moc]
        catch {$toolbox add_file $filename}
      }
      if { $urlget_ != {} } {
         catch {delete object $urlget_}
         set urlget_ {}
      }
   }

   #  Displays a catalogue in TST format.
   #  Returns the GaiaSearch widget which displays the catalogue.
   protected method display_table_ {filename table_id} {
      set images [skycat::SkyCat::get_skycat_images]
      set ctrlwidget [lindex $images 0]
      set gaia [winfo parent $ctrlwidget]
      set window [::cat::AstroCat::open_catalog_window \
                    $filename $ctrlwidget ::gaia::SampSearch 0 $gaia]
      $window configure -table_id $table_id

      #  Set symbol, after realization of window.
      set next_symbol [next_symbol_spec_]
      after idle "$window maybe_set_symbol $next_symbol"
      return $window
   }

   #  Returns a window associated with a previously received table.
   #  param_var is the name of an array, and key_list is a list of key
   #  names in it.  Any value of one of these keys in the array
   #  (in order of preference) will be used as an identifier for a
   #  previously received window.  The window is returned; if none can
   #  be found an error is thrown.
   private method get_cat_window_ {param_var key_list} {
      upvar $param_var params
      set keys {}
      set ident {}
      foreach k $key_list {
         if {[info exists params($k)]} {
            lappend keys $params($k)
            lappend ident "$k=$params($k)"
         }
      }
      if {[llength $keys] == 0} {
         error "No table identifier - none of $key_list"
      }
      foreach k $keys {
         if {[info exists cat_windows_($k)]} {
            return $cat_windows_($k)
         }
      }
      error "Unknown table $ident"
   }

   #  Utility procs:
   #  --------------

   #  Returns a named value from an array, or a default if the value is
   #  absent.
   private proc get_param_ {params_name key default} {
      upvar $params_name params
      if {[info exists params($key)]} {
         return $params($key)
      } {
         return $default
      }
   }

   #  Locate an instance of Gaia for displaying images.
   private proc get_gaia_ {} {
      foreach image [::skycat::SkyCat::get_skycat_images] {
         return [winfo parent $image]
      }
      return ""
   }

   #  Tries to turn a URL into a file name.  If the URL uses the file:
   #  protocol (in either its correct RFC1738 "file://host/..." or its
   #  incorrect but common "file:..." form) then the corresponding
   #  local filename is returned.  Otherwise an empty string is returned.
   #  Remember to decode any encoded characters (+, space etc.).
   private proc get_file_ {url} {
      if {[regsub ^file://(localhost|[info host]|)/ $url "" fname]} {
         return [::ncgi::decode "/${fname}"]
      } elseif {[regsub ^file: $url "" fname]} {
         return [::ncgi::decode $fname]
      }
      return {}
   }

   #  Provides a suitable value for the "symbol_id" column in a TST table.
   #  This is what determines how plotted symbols will appear on the
   #  image (unless changed).  This function endeavours to return a
   #  different symbol each time it is called (though may repeat eventually).
   protected proc next_symbol_spec_ {} {
      set shapes {circle square plus cross diamond}
      set colors {cyan yellow blue red grey50 green magenta}
      set shape [lindex $shapes [expr $symbol_idx_ % [llength $shapes]]]
      set color [lindex $colors [expr $symbol_idx_ % [llength $colors]]]
      set size 6
      incr symbol_idx_
      return [list {} \
                   [list $shape $color {} {} {} {}] \
                   [list $size {}]]
   }


   #  Instance variables:
   #  -------------------

   #  Name of the active instance of GaiaUrlGet.
   protected variable urlget_ {}

   private variable temp_files_ {}

   #  Class variables:
   #  ----------------

   #  Array of GaiaSearch windows which have been opened to display
   #  SAMP-acquired tables.  The array is indexed by table_id.
   protected common cat_windows_

   #  Index of the last new symbol type used.
   protected common symbol_idx_ 0
}
