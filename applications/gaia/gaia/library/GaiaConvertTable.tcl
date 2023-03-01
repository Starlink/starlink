#+
#  Name:
#     GaiaConvertTable

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Class for handling the import of foreign catalogue formats.

#  Description:
#     This class defines a object that controls a series of defined
#     filters for converting to and from FITS, other CAT and ASCII
#     supported formats and VOTable.

#  Invocations:
#
#        GaiaConvertTable object_name [configuration options]
#
#     This creates an instance of a GaiaConvertTable object. The return is
#     the name of the object.
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

#  Methods:

#  Inheritance:
#     This widget inherits no other classes.

#  Copyright:
#     Copyright (C) 1998-2001 Central Laboratory of the Research Councils
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council
#     Copyright (C) 2007-2008 Science and Technology Research Council
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
#     28-SEP-1998 (PWD):
#        Original version.
#     08-AUG-2001 (PWD):
#        Changed to map file types to lower case.
#     14-MAY-2007 (PWD):
#        Changed to handle import of FITS catalogues using native support
#        (CAT support for FITS is ageing, so 'K' format). Native support
#        is Skycat, plus some GAIA changes for backwards compatibility in
#        dealing with the meta-data. Export of FITS still uses CAT, but
#        that's quite lossy.
#     01-JUL-2008 (PWD):
#        Add VOTable support.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaConvertTable {

   #  Inheritances:
   #  -------------
   #  Nothing

   #  Constructor:
   #  ------------
   constructor  {args} {

       #  Set the names of the conversion filters.
       global gaia_dir
       foreach type $cattypes_ {
          set to_app_($type) "$gaia_dir/cat2tab"
          set from_app_($type) "$gaia_dir/tab2cat"
          set to_filter_($type) {}
          set from_filter_($type) {}
       }
       foreach type $asciitypes_ {
          set to_app_($type) "$gaia_dir/asc2tab"
          set from_app_($type) "$gaia_dir/tab2asc"
          set to_filter_($type) {}
          set from_filter_($type) {}
       }
       foreach type $votypes_ {
          set to_app_($type) "$gaia_dir/vot2tab"
          set from_app_($type) "$gaia_dir/tab2vot"
          set to_filter_($type) {}
          set from_filter_($type) {}
       }
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Convert from a CAT, ASCII or FITS catalogue to a tab table.
   #  Arguments in and out give the catalogue name and a name for the
   #  new catalogue.
   public method to {in out} {

      #  Get the file type.
      set type [get_type_ $in]

      if { $type == ".fits" || $type == ".fit" } {

         #  Use native support for FITS files.
         lassign [get_hdu_ $in] in hdu
         if { $hdu == -1 } {
            #  FITS table, so always pick first extension.
            set hdu 2
         }
         set rtdimage [::image create rtdimage -file "$in"]

         #  Move to the HDU.
         if { [catch {$rtdimage hdu set $hdu}] } {
            catch {::image delete $rtdimage}
            error_dialog "No catalog extension $hdu in $in"
            return 0
         }

         #  Make sure we revisit the disk file, otherwise uses cached version.
         $rtdimage update

         #  Get the catalog config entry.
         set entry [create_config_entry $rtdimage $in $hdu]

         #  Copy the FITS table to a local catalog.
         if { [catch {$rtdimage hdu get $hdu $out $entry} msg] } {
            catch {::image delete $rtdimage}
            error_dialog $msg
            return 0
         }
         catch {::image delete $rtdimage} msg

      } else {
         #  Start up external filter.
         if { $to_filter_($type) == {} } {
            global env
            set to_filter_($type) [gaia::GaiaForeignExec \#auto \
                                      -application $to_app_($type) \
                                      -show_output 0]
         }

         #  Now attempt the conversion. Note VOTables may contain more
         #  than one table, so a HDU concept is supported.
         if { $type == ".xml" || $type == ".vot" } {
            lassign [get_hdu_ $in] in hdu
            if { $hdu == -1 } {
               set hdu 0
            } else {
               incr hdu -1
            }
            set cmd "$in $hdu $out"
            catch {$to_filter_($type) runnows $cmd} msg
         } else {
            set cmd [format $to_cmd_ $in $out]
            catch {eval $to_filter_($type) runnow $cmd} msg
         }
         if { $msg == "1" || $msg == "0" } {
            set msg {}
         }
         if { $msg != {} } {
            return 0
         }
      }
      return 1
   }

   #  Convert a tab table to a CAT/ASCII catalogue. Handles FITS using
   #  foreign conversion as Skycat only supports saving FITS tables
   #  with an existing image, not to a new file. Downside is we loose
   #  some information, like 8 byte integer values (converted to doubles).
   public method from {in out} {

      #  Get the file type.
      set type [get_type_ $out]

      #  Start up external filter.
      if { $from_filter_($type) == {} } {
         global env
         set from_filter_($type) [gaia::GaiaForeignExec \#auto \
                                     -application $from_app_($type) \
                                     -show_output 0]
      }

      #  The signature for VOTables is slightly different.
      if { $type == ".xml" || $type == ".vot" } {
         set cmd "$in $out"
         set res [catch {$from_filter_($type) runnows $cmd} msg]
      } else {

         #  CAT will not overwrite existing files, so do this ourselves.
         if { [file exists $out] } {
            file delete $out
         }
         set cmd [format $from_cmd_ $in $out]
         set res [catch {eval $from_filter_($type) runnow $cmd} msg]
      }
      if { $msg == "1" || $msg == "0" } {
         set msg {}
      }
      if { $msg != {} || $res != 0 } {
         if { $msg == {} } {
            set msg "Failed to convert temporary file: $in back to $out"
         }
         error_dialog "$msg"
         return 0
      }
      return 1
   }

   #  Get the type of file. This should match one of our known types
   #  and conversion filters.
   protected method get_type_ {name} {

      #  Extract type from extension and map to lower case.
      set type [string tolower [file extension $name]]

      #  Some FITS types may reference extensions, so just check part
      #  of the extension.
      if { [string match {.fits*} "$type"] } {
         set type ".fits"
      } elseif { [string match {.fit*} "$type"] } {
         set type ".fit"
      } else {

         #  Same for VOTable.
         if { [string match {.xml*} "$type"] } {
            set type ".xml"
         } elseif { [string match {.vot*} "$type"] } {
            set type ".vot"
         }

      }
      return $type
   }

   #  Return the HDU for the given FITS or VOTable filename specification. The
   #  HDU is given as a number in {} or in []. The {} format supports backwards
   #  compatibility with CAT. The result is two values, the input name without
   #  the HDU specification and the hdu number (this defaults to -1 so that
   #  it's absence can be spotted).
   protected method get_hdu_ {in} {
      set hdu -1
      set i1 [string last "\[" $in]
      set i2 [string last "\]" $in]
      if { $i1 == -1 } {
         set i1 [string last "\{" $in]
         set i2 [string last "\}" $in]
      }
      if { $i1 > -1 && $i2 > -1 } {
         set hdu [string range $in [incr i1] [incr i2 -1]]
         set in [string range $in 0 [incr i1 -2]]
      }
      return [list $in $hdu]
   }

   #  Divine a configuration entry for this catalogue based on its
   #  properties. This is just a list of paired entries like:
   #  {{shortname shortname} {fullname filename} {symbol ...} ...}.
   protected method create_config_entry {rtdimage filename hdu} {

      set headings [$rtdimage hdu list]
      set headings [lindex $headings [expr $hdu-1]]

      set extname [lindex $headings 2]
      if { $extname == {} } {
         set extname "$filename"
      }

      set entry {}
      lappend entry [list serv_type local]
      lappend entry [list short_name $extname]
      lappend entry [list long_name "$filename"]
      lappend entry [list url "$filename"]

      #  Catalogue parameters.
      #
      #  Only interested in a fixed set of possible values. Symbol may span
      #  more than one line (and be suffixed by an integer from 1 to 9 to allow
      #  for long lines). No suffix is allowed. Note we gather the units.
      set symbol {}
      set headers [$rtdimage hdu fits $hdu]
      foreach line [split $headers "\n"] {
         set line [string trim $line]
         lassign [::gaia::GaiaFITSHeader::get_kvc $line] keyword value
         set keyword [string tolower $keyword]
         set value [regsub -all {'} $value {}]
         switch -glob -- $keyword {
            symbol* {
               append symbol $value
            }
            search_c* {
               lappend entry "search_cols $value"
            }
            sort_col {
               lappend entry "sort_col $value"
            }
            sort_ord* {
               lappend entry "sort_order $value"
            }
            show_col* {
               lappend entry "show_cols $value"
            }
            copyrigh* {
               lappend "copyright $value"
            }
            tunit* {
               set units($keyword) [string tolower $value]
            }
         }
      }

      #  Catalog columns. Try to use a knowledge of the various names
      #  to pick out the positional columns, use units picked in last pass.
      set racol -1
      set deccol -1
      set xcol -1
      set ycol -1
      set idcol -1
      set stccol -1
      set iscupid 0

      set headings [$rtdimage hdu headings $hdu]
      set nc [llength $headings]

      for { set i 0 } { $i < $nc } { incr i } {
         set name [string tolower [lindex $headings $i]]

         #  Check any units for special significance, radians or degrees are
         #  assumed to be possible sky coordinates.
         set unit {}
         set j [expr $i + 1]
         if { [info exists units(tunit$j)] } {
            set unit $units(tunit$j)
         }
         if { [string match "radian*" $unit] ||
              [string match "deg*" $unit] } {

            #  Assuming this is a column with angle data. This is either an RA
            #  or DEC. If qualified by {HOURS}, {HMSxxx} or the name is some
            #  variation of RA/Ra/r.a./Rightxxx/alphaxxx, then assume RA,
            #  otherwise it is a DEC. Note we need both of these to have a
            #  valid match, but we don't check for that.
            if { [string match "*hours*" $unit] ||
                 [string match "*hms*" $unit] } {
               if { $racol == -1 } {
                  set racol $i
               }
            } else {
               #  "daz" is AZEL for JAC, also DEl, but that should be a dec.
               #  "cen1" and "cen2" are support for CUPID. Both follow
               #   in definite columns (there can be others with degrees) so
               #   we assume one is followed by the other.
               switch -glob -- $name {
                  ra* -
                  right* -
                  r.a.* -
                  x_world* -
                  alpha* {
                     if { $racol == -1 } {
                        set racol $i
                     }
                  }
                  daz {
                     if { $racol == -1 } {
                        set racol $i
                        set deccol [expr $i+1]
                     }
                  }
                  cen1 {
                     set iscupid 1
                     if { $racol == -1 } {
                        set racol $i
                        set deccol [expr $i+1]
                     }
                  }
                  default {
                     #  Assume this is a declination, unless name starts with
                     #  "pos", which is most likely a position angle (for an
                     #  ellipse), and l or a b. Those are galactic coords.
                     switch -glob -- $name {
                        pos* -
                        l* -
                        b* {
                           #  Do nothing.
                        }
                        default {
                           if { $deccol == -1 } {
                              set deccol $i
                           }
                        }
                     }
                  }
               }
            }
         } else {

            #  Check for SExtractor specific names without units. SExtractor
            #  world coordinates are in degrees. Also allow X/Y and X_*/Y_*
            #  as generic X and Y columns and look for the CUPID shape column.
            switch -glob -- $name {
               x_world {
                  if { $racol == -1 } {
                     set racol $i
                  }
               }
               y_world {
                  if { $deccol == -1 } {
                     set deccol $i
                  }
               }
               x_image -
               x_* -
               x {
                  if { $xcol == -1 } {
                     set xcol $i
                  }
               }
               y_image -
               y_* -
               y {
                  if { $ycol == -1 } {
                     set ycol $i
                  }
               }
               id_col {
                  if { $idcol == -1 } {
                     set idcol $i
                  }
               }
               shape {
                  if { $stccol == -1 } {
                     set stccol $i
                  }
               }
            }
         }
      }

      #  OK, if we have located special columns then add these to the header
      #  section. Note that if world coordinates are not located then this is
      #  recorded (as -1). This is necessary as information about the presence
      #  of these coordinates may persist. If no id_col has been located then
      #  we will create a fake one, but only if the first column is positional
      #  (ra, dec, x or y), which isn't suitable.
      if { $idcol != - 1 } {
         lappend entry "id_col $idcol"
      } else {
         if { $racol != 0 && $deccol != 0 && $xcol != 0 && $ycol != 0 } {
            lappend entry "id_col 0"
         }
      }
      lappend entry "ra_col $racol"
      lappend entry "dec_col $deccol"
      if { $xcol != -1 && $ycol != -1 } {
         lappend entry "x_col $xcol"
         lappend entry "y_col $ycol"
      }

      #  If have a shape column, set the stc_col and an appropriate symbol.
      if { $iscupid && $stccol != -1 } {
         lappend entry "stc_col $stccol"
         if { $symbol == {} } {
            set symbol {{} {stcshape {} {} {} {} {}} {1 {deg 2000}}}
         }
      }

      #  If no symbol use a default.
      if { $symbol == {} } {
         set symbol {{} {circle {} {} {} {} {}} {4.0 {}}}
      }
      if { $symbol != {} } {
         lappend entry [list symbol $symbol]
      }
      return $entry
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Protected variables: (available to instance)
   #  --------------------

   #  Names of applications that form the input/output filters. These
   #  are indexed by the file types.
   protected variable to_app_
   protected variable from_app_

   #  Known file types. These are mapped to lower case.
   protected variable cattypes_ ".fits .fit .gsc .txt"
   protected variable asciitypes_ ".asc .lis"
   protected variable votypes_ ".xml .vot"

   #  Command strings to run the convert to/from a tab-table. This
   #  contains "format" specifiers for the input and output names.
   protected variable to_cmd_ "in=%s out=%s accept"
   protected variable from_cmd_ "in=%s out=%s accept"

   #  Names of the objects used to control the filters.
   protected variable to_filter_
   protected variable from_filter_

   #  Name of the FITS table containing catalog config info.
   protected variable catinfo_ "CATINFO"

   #  End of class definition.
}

