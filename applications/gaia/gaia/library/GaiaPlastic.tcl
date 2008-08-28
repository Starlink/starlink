#+
#  Name:
#     GaiaPlastic

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     GAIA-specific implementation of PLASTIC messages.

#  Description:
#     An instance of this class does the actual work for responding to
#     PLASTIC messages.  It should be supplied to the constructor of
#     a plastic::PlasticApp, and it should contain a method for each
#     message that it implements.  The name of each method is the actual
#     PLASTIC message ID, and the argument list is the PLASTIC ID of
#     the sender application, followed by all the other arguments which
#     have been passed as message parameters.
#
#     See http://plastic.sourceforge.net/ for listing of the standard
#     PLASTIC messages, their argument lists, and their semantics.

#  Copyright:
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     MBT: Mark Taylor
#     PWD: Peter W. Draper
#     {enter_new_authors_here}

#  History:
#     13-JUL-2006 (MBT):
#        Original version.
#     18-AUG-2006 (PWD):
#        Modified to run the stilts process in the background.
#     09-JUL-2008 (PWD):
#        Removed stilts dependency, use GAIA-VO facilities instead.
#     {enter_further_changes_here}

#-

package require rpcvar
package require md5 1.4.4
namespace import -force rpcvar::*

itcl::class gaia::GaiaPlastic {

   #  PLASTIC message implementations.
   #  --------------------------------

   #  Echo text.
   public method ivo://votech.org/test/echo {sender_id text args} {
      return [rpcvar string $text]
   }

   #  Return application name.
   public method ivo://votech.org/info/getName {sender_id args} {
      return "gaia"
   }

   #  Return application description.
   public method ivo://votech.org/info/getDescription {sender_id args} {
      return "Graphical Astronomy and Image Analysis Tool"
   }

   #  Return version of PLASTIC supported.
   public method ivo://votech.org/info/getVersion {sender_id args} {
      return [rpcvar string 0.4]
   }

   #  Return URL of an icon representing this application.
   public method ivo://votech.org/info/getIconURL {sender_id args} {
      return "http://astro.dur.ac.uk/~pdraper/gaia/gaialogo.gif"
   }

   #  Load a FITS file specified as a URL into the display.
   public method ivo://votech.org/fits/image/loadFromURL {sender_id img_url
                                                          args} {
      set basegaia [get_gaia_]
      if { $basegaia != {} } {
         set fname [get_file_ $img_url]
         if { $fname != {} } {
            $basegaia open $fname
            return $TRUE
         } else {
            #  Remote file, arrange to download this in the background, if not
            #  already busy downloading...
            if { $urlget_ == {} } {
               set urlget_ \
                  [GaiaUrlGet .\#auto -notify_cmd [code $this display_file_]]
               $urlget_ get $img_url
               return $TRUE
            }
         }
      }
      return $FALSE
   }

   #  Point at a coordinate by drawing an identifier graphic, ra and dec
   #  are both in decimal degrees.
   public method ivo://votech.org/sky/pointAtCoords {sender_id ra dec args} {
      set basegaia [get_gaia_]
      if { $basegaia != {} } {
         if { ![catch {$basegaia position_of_interest $ra $dec "deg J2000"}]} {
            return $TRUE
         }
      }
      return $FALSE
   }

   #  Execute a GAIA command.
   #  The script argument contains the executable Tcl.
   #  The checksum argument contains the hexadecimal md5 hash of the
   #  content of ~/.gaia-cookie followed by the content of the script
   #  argument.  As a convenience, the cookie may be present either
   #  with or without a trailing newline.
   public method ivo://plastic.starlink.ac.uk/gaia/executeMd5 {sender_id script
                                                               checksum args} {
      set cookie [[gaia::GaiaCookie::get_instance] cget -cookie]
      set hash [string toupper $checksum]
      set hash1 [string toupper [md5::md5 "$cookie$script"]]
      set hash2 [string toupper [md5::md5 "$cookie\n$script"]]
      if {$hash == $hash1 || $hash == $hash2} {
         return [eval $script]
      } else {
         return "Request rejected - bad MD5 checksum"
      }
   }

   #  Load a VOTable as a catalogue.
   public method ivo://votech.org/votable/loadFromURL {sender_id url args} {

      set failure 0
      catch {
         #  Second argument, if present, is a tag for the table.  If not
         #  present, use the URL.
         if {$args == ""} {
            set table_id_ $url
         } else {
            set table_id_ [lindex $args 0]
         }

         #  Convert the VOTable to TST format and display it when the
         #  conversion is completed.
         set tst_file_ [get_temp_file_ .TAB]
         if { $vot2tab_ == {} } {
            set vot2tab_ [GaiaForeignExec \#auto \
                             -notify [code $this completed_] \
                             -application $::gaia_dir/vot2tab]
         }
         $vot2tab_ runwiths "$url 0 $tst_file_"

         #  Wait for long running conversion to complete as we need the status
         #  return.
         if { $tst_file_ != {} } {
            tkwait variable [scope tst_file_]
         }

         if { $tst_file_ == {} } {
            set msg "Conversion failed"
            set failure 1
         }
      } msg

      if { $msg != {} && $msg != 1 } {
         set failure 1
      }

      #  Return as appropriate.
      if { ! $failure } {
         return $TRUE
      } else {
         error_dialog "Failed to load catalogue from PLASTIC:\n$msg"
         return $FALSE
      }
   }

   #  Called when the conversion command completes. Only job is to display
   #  the table, if conversion was successful.
   protected method completed_ {} {
      if { $tst_file_ != {}  && [file exists $tst_file_] } {
         set window [display_table_ $tst_file_ $table_id_]
         set cat_windows_($table_id_) $window
         set tst_file_ $tst_file_
      } else {
         error "Failed to load catalogue from PLASTIC message"
         set tst_file_ {}
      }
   }

   #  Display only a selection of the rows from a previously loaded catalogue.
   public method ivo://votech.org/votable/showObjects {sender_id table_id
                                                       idx_list args} {
      if {[info exists cat_windows_($table_id)]} {
         $cat_windows_($table_id) select_indices $idx_list
         return $TRUE
      } else {
         return $FALSE
      }
   }

   #  Highlight a single row from a previously loaded catalaogue.
   public method ivo://votech.org/votable/highlightObject {sender_id table_id
                                                           idx args} {
      if {[info exists cat_windows_($table_id)]} {
         $cat_windows_($table_id) highlight_index $idx
         return $TRUE
      } else {
         return $FALSE
      }
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

   #  Displays a catalogue in TST format.
   #  Returns the GaiaSearch widget which displays the catalogue.
   protected method display_table_ {filename table_id} {
      set images [skycat::SkyCat::get_skycat_images]
      set ctrlwidget [lindex $images 0]
      set gaia [winfo parent $ctrlwidget]
      set window [::cat::AstroCat::open_catalog_window \
                    $filename $ctrlwidget ::gaia::PlasticSearch 0 $gaia]
      $window configure -table_id $table_id

      #  Set symbol, after realization of window.
      set next_symbol [next_symbol_spec_]
      after idle "$window maybe_set_symbol $next_symbol"
      return $window 
   }

   #  Utility procs:
   #  --------------

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
   private proc get_file_ {url} {
      if {[regsub ^file://(localhost|[info host]|)/ $url "" fname]} {
         return /$fname
      } elseif {[regsub ^file: $url "" fname]} {
         return $fname
      } else {
         return ""
      }
   }

   #  Get the name of a file it's OK to use for scratch space.
   #  The exten argument gives a file extension (e.g. ".fits").
   protected proc get_temp_file_ {exten} {
      set tmpdir ""
      if { [info exists ::env(GAIA_TEMP_DIR)] } {
         set trydirs "$::env(GAIA_TEMP_DIR) /tmp /var/tmp ."
      } else {
         set trydirs "/tmp /var/tmp ."
      }
      foreach trydir $trydirs {
         if {[file isdirectory $trydir] && [file writable $trydir]} {
            set tmpdir $trydir
            break
         }
      }
      if { $tmpdir == "" } {
         error "No temporary directory"
      }

      set basefile "${tmpdir}/gaia_temp_"
      for { set ix 1 } { $ix < 100 } { incr ix } {
         set tryfile "$tmpdir/gaia_temp_$ix$exten"
         if {! [file exists $tryfile] } {
            return $tryfile
         }
      }
      error "No free files with name like $tryfile"
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

   #  Command for converting VOTable to TST.
   protected variable vot2tab_ {}

   #  Name of the TST we're generating.
   protected variable tst_file_ {}

   #  Table identifier of the TST we're generating.
   protected variable table_id_ {}

   #  Class variables:
   #  ----------------

   #  Array of GaiaSearch windows which have been opened to display
   #  PLASTIC-acquired tables.  The array is indexed by table_id.
   protected common cat_windows_

   #  Constants for returning from boolean-declared XML-RPC methods.
   protected common FALSE [rpcvar boolean 0]
   protected common TRUE [rpcvar boolean 1]

   #  Index of the last new symbol type used.
   protected common symbol_idx_ 0
}
