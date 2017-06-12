#+
#  Name:
#     GaiaPolData

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Describes data contained in a polpack catalogue.

#  Description:
#     A GaiaPolData object represents a Polpack catalogue, and cannot be
#     changed in any way once created. Except that... a GaiaPolData can
#     be "put to sleep" to reduce the memory required to store the data.
#     It can subsequently be re-awakened, at which time the data array is
#     re-created from a disk file.
#
#     The constructor uses the Polpack "polwrtcl" task to create the disk
#     file from the specified polpack catalogue. This disk file contains
#     a tcl code fragment which assigns values to properties of the
#     GaiaPolData (including the main data array).


#  Invocations:
#
#        GaiaPolData object_name disk-file
#
#     This creates an instance of a GaiaPolData object. The returned value
#     is the name of the object. Disk-file is the full path for the disk
#     file holding the Polpack catalogue.
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

#  Inheritance:
#     ::gaia::GaiaPolObject
#     (really it should inherited from GaiaPolData .....)

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
#     DSB: David S. Berry  (STARLINK)
#     {enter_new_authors_here}

#  History:
#     25-SEP-2000 (DSB):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaPolData {

#  Inheritances:
   inherit gaia::GaiaPolObject

#  Constructor:
#  ===========
#  $file is the name of the fil as supplied by the user. $w is the top
#  level window, and $tclfile is the name of a text file created by
#  polpack:polwrtcl containing the row/column data. If not supplied,
#  a new file is created.
   constructor { file w pbar {tclfile ""} } {

#  Now initialize the class data. If this constructor has been invoked
#  to construct the base class part of some super class, do not
#  initialize the data since this will be done as a consequence of
#  initializeing the super class data.
      if { [$this info class] == "::gaia::GaiaPolData" } {
         init $file $w $pbar $tclfile
      }

   }

#  Destructor:
#  ===========
#  The tcl file is not deleted in case the user wants to re-open the
#  polpack catalogue. All tcl files are deleted when the GaiaPol toolbox
#  is destroyed.
   destructor  {

#  Free any entry for $this in the list of usable GaiaPolData objects.
      set i [lsearch -exact $awake_ $this]
      if { $i > -1 } { set awake_ [lreplace $awake_ $i $i] }

#  Tcl files can be shared betwen GaiaPolData objects. If no objects are
#  left which refer to the tcl file associated with $this, then mark the
#  tcl file as "not currently in use" by adding it to the $oldfiles_ list.
#  Note, the file is not deleted in case the user chooses to re-open it
#  later. We only retain a limited number of unused tcl files to avoid
#  exessive disk space usage.
      if { [incr tclFileRefCount_($tclfile_) -1] == 0 } {

#  If this tclfile is already on the old files list, temporarily remove
#  it from the list.
         set i [lsearch -exact $oldfiles_ $tclfile_]
         if { $i != -1 } {
            set oldfiles_ [lreplace $oldfiles_ $i $i]

#  Otherwise, if the list is full delete the oldest unused tcl files.
         } elseif { [llength $oldfiles_] >= $maxold_ } {
            catch { file delete [lindex $oldfiles_ 0] }
            set oldfiles_ [lrange $oldfiles_ 1 end]
         }

#  Add the tcl file for $this to the end of the list of unused tcl files,
#  and unset the array element used to store its reference count.
         lappend oldfiles_ $tclfile_
         unset tclFileRefCount_($tclfile_)
      }
   }

#  Initialiser:
#  ============
#  Override the parent Init method to initialise the contents of the
#  memory allocated by the GaiaPolData constructor using a user-supplied
#  argument list.
   protected method init { file w pbar tclfile } {

#  First initialize the parent class data
      gaia::GaiaPolObject::init

#  Now initialize this class...
      set w_ $w
      set id_ [incr count_]
      set asleep_ 1
      set file_ $file
      set pbar_ $pbar
      set warned_ 0

#  Save the absolute path for the polpack disk file.
      set polfile_ [full_name $file]
      set desc_ "open $file"

#  If a tclfile was supplied, use it.
      if { $tclfile != "" } {
         set tclfile_ $tclfile
         catch { unset knownfiles_($polfile_) }

#  Otherwise we ay need to create one.
      } else {

#  If this polpack file has already been opened once, we may have a tcl file
#  already. See if the polpack file is included in the static array of
#  previously opened polpack files. If it does, the array element will
#  hold the name of the corresponding tcl file.
         if { [info exists knownfiles_($polfile_)] } {
            set tclfile_ $knownfiles_($polfile_)

#  Check the tcl file still exists.
            if { [file exists $tclfile_] } {

#  Remove the recorded tcl file from the list of unused files.
               set i [lsearch -exact $oldfiles_ $tclfile_]
               if { $i != -1 } { set oldfiles_ [lreplace $oldfiles_ $i $i] }

#  If the tcl file exists, check it is younger than the corresponding
#  polpack file. If the polpack file has been modified since the tcl
#  file was created, delete the tcl file.
               if { [file mtime $tclfile_] < [file mtime $polfile_] } {
                  catch { file delete $tclfile_}
                  unset knownfiles_($polfile_)
                  set tclfile_ ""
               }

            } else {
               unset knownfiles_($polfile_)
               set tclfile_ ""
            }

         } else {
            set tclfile_ ""
         }

#  If the is no existing tclfile, create a name for a new one using the
#  GaiaPolObject::tmpFile method. Save the name of the tclfile in the
#  static array of known files.
         if { $tclfile_ == "" } {
            set tclfile_ [tmpFile]
            set knownfiles_($polfile_) $tclfile_
         }
      }

#  Reset the class data members holding the catalogue description.
      sleep

#  Now set them by using polwrtcl to create the Tcl file and then sourcing
#  it. Report an error if it fails.
      set mess [wake 0]
      if { $asleep_ } {
         error "Failed to open catalogue $file: $mess"
      } else {
         if { [info exists tclFileRefCount_($tclfile_)] } {
            incr tclFileRefCount_($tclfile_)
         } else {
            set tclFileRefCount_($tclfile_) 1
         }
      }
   }

#  Public methods:
#  ===============
#  Calling any public method will "wake" the GaiaPolData. That is, if it
#  has been "put to sleep" to save memory, the tcl disk file describing the
#  catalogue will be re-sourced so that the data is available for use.

#  Convert between Z axis and Z column values. $type should be either
#  "zcval" (if $z is a Z column value), or "zaval" (if $z is a Z axis
#  value). A list of two values is returned holding the corresponding
#  Z column and Z axis values to use.
#  -------------------------------------------------------------------
   public method zConv { z type } {
      set ret [list "" ""]

#  Indicate what is happening
      $pbar_ config -text "Finding Z values to use ..."
      update idletasks

#  Ensure the values in the tclfile are available.
      wake

#  Ensure the POLPACK configuration file is appropriate for the column
#  names being used. If a file already exists, a temporary copy of it is
#  taken, and the name of the copy returned.
      set oldrc [makepolrc "X Y Z" ]
      if { $oldrc != "*" } {

#  Get the GaiaApp which provides access to the Polpack "polzconv" command.
         set polzconv [findGaiaApp "polzconv"]

#  Skip if the above failed.
         if { $polzconv != "" } {

#  Construct the polzconv parameter list.
            set plist "cat=$polfile_ "
            if { $type == "zaval" } {
               append plist "zcolval=! zaxval=$z "
            } else {
               append plist "zcolval=$z zaxval=! "
            }

#  Run polzconv
            run $polzconv $plist

#  Form the returned list holding the Z column and axis values calculated by
#  polzconv.
            set ret [list [getparam $polzconv zcoluse] [getparam $polzconv zaxuse]]
         }

#  Reinstate the original polpack config file.
         clearpolrc $oldrc
      }

#  Reset the progress bar.
      $pbar_ reset
      update idletasks

#  Return the answer.
      return $ret

   }

#  Create a polpack catalogue holding a binned copy of the supplied data
#  array, which is assumed to be a selection of rows from $this.
#  ---------------------------------------------------------------------
   public method bin {data box method debias minval sigmas {integ 0} {binfile ""} } {
      set ret ""

#  Indicate what is happening
      $pbar_ config -text "Binning vectors ..."
      update idletasks

#  Ensure the values in the tclfile are available.
      wake

#  Ensure the POLPACK configuration file is appropriate for the column
#  names being used. If a file already exists, a temporary copy of it is
#  taken, and the name of the copy returned.
      set oldrc [makepolrc "X Y I P ANG" ]
      if { $oldrc != "*" } {

#  If we are using the existing data array, we will bin the associated
#  polpack catalogue.
         if { $data == "" } {
            set file $polfile_

#  Otherwise, we have to create a new polpack catalogue containing the
#  supplied data.
         } else {

#  Decide on a name for the new polpack catalogue.
            set file [tmpFile]
            append file ".FIT"

#  Create a new Tclfile containing the supplied data.
            set tclfile [newTclFile $data]

#  Save the data as a polpack catalogue. This is done using polpack
#  application polrdtcl.
            tclSave $tclfile $polfile_ $file

         }

#  Check the catalogue was produced.
         if { [file exists $file] } {

#  Get the GaiaApp which provides access to the Polpack "polbin" command.
            set polbin [findGaiaApp "polbin"]

#  Skip if the above failed.
            if { $polbin != "" } {

#  Decide on a name for the binned catalogue.
               if { $binfile == "" } {
                  set binfile [tmpFile]
                  append binfile ".FIT"
               }

#  Convert the debias value to an PCS YES/NO value.
               if { $debias } {
                  set debias YES
               } else {
                  set debias NO
               }

#  Convert the integrate value to an PCS YES/NO value.
               if { $integ } {
                  set integ YES
               } else {
                  set integ NO
               }

#  Run polbin on the catalogue created above from $this.
               run $polbin "in=$file out=$binfile box=$box zbox=1 method=$method debias=$debias minval=$minval sigmas=$sigmas integrate=$integ radec=yes accept"

#  Check the binned catalogue was created.
               if { [file exists $binfile] } {
                  set ret $binfile
               }
            }
         }

#  Reinstate the original polpack config file.
         clearpolrc $oldrc

#  Delete any temporary files.
         if { $data != "" } {
            catch { file delete $file }
            catch { file delete $tclfile }
         }
      }

#  Reset the progress bar.
      $pbar_ reset
      update idletasks

      return $ret

   }

#  Save the data to a new polpack catalogue.
#  -----------------------------------------
   public method save { data file } {
      set mess ""
      if { $data == "" } {
         set err [catch {file copy -force $polfile_ $file} mess]
      } else {
         wake
         set tclfile [newTclFile $data]
         set err [catch {tclSave $tclfile $polfile_ $file} mess]
      }

      if { $err || ![file exists $file] } {
         error_dialog "Failed to save data to $file: $mess"
         set ret 0
      } else {
         set ret 1
      }
      return $ret
   }

#  Format for display
#  -------------------
   public method toString {} {
      wake
      set ret "GaiaPolData: $polfile_ "
      return $ret
   }

#  If $this refers to a different catalogue to $that, the string "redraw" is
#  returned. Otherwise a blank string is returned.
#  ----------------------------------------------------------------------
   public method changes {that} {
      wake
      if { [getTclfile] != [$that getTclfile] } {
         set ret "redraw"
      } else {
         set ret ""
      }
   }

#  Create a blank image with a Wcs system using the supplied rtdimage.
#  ----------------------------------------------------------------
   public method mkImage { rtdimage } {
      wake

#  First deal with cases where the catalogue has WCS info.
      if { $gotwcs_ && $ra_ != "" && $dec_ != "" && $equinox_ != "" } {

#  Create the blank image.
         $rtdimage clear -reuse 1 -width $nxpix_ -height $nypix_

#  Now set the correct WCS values.
         $rtdimage wcsset $ra_ $dec_ $secpix_ $xrefpix_ $yrefpix_ $nxpix_ \
                                        $nypix_ 0 $equinox_ 0 TAN

#  If no WCS, just create the image with no wcs.
      } else {
         $rtdimage clear -reuse 1 -width $nxpix_ -height $nypix_
      }
   }

#  Nullify the class members read from the tcl script produced by
#  polpack:polwrtcl. This may be done to save memory when the PolData is
#  not being used.
#  ----------------------------------------------------------------
   public method sleep {} {
      set gotwcs_   ""
      set uses_     ""
      set headings_ ""
      set xlo_      ""
      set ylo_      ""
      set zlo_      ""
      set xhi_      ""
      set yhi_      ""
      set zhi_      ""
      set ncol_     ""
      set nrow_     ""
      set ra_       ""
      set dec_      ""
      set equinox_  ""
      set epoch_    ""
      set xrefpix_  ""
      set yrefpix_  ""
      set nxpix_    ""
      set nypix_    ""
      set secpix_   ""
      set fmts_     ""
      set hfmts_    ""
      set zaunit_   ""
      set zcunit_   ""
      set data_     ""
      set refrot_   ""

#  Indicate the PolData is now asleep.
      set asleep_ 1
   }

#  Accessor methods...
#  --------------------
   public method getData {} { wake; return $data_ }
   public method setData {d} { wake; set data_ $d}
   public method gotWcs {} { wake; return $gotwcs_ }
   public method getXCol {} { wake; return [lsearch -exact $uses_ "X"] }
   public method getYCol {} { wake; return [lsearch -exact $uses_ "Y"] }
   public method getIdCol {} { wake; return [lsearch -exact $uses_ "ID"] }
   public method getRaCol {} { wake; return [lsearch -exact $uses_ "RA"] }
   public method getDecCol {} { wake; return [lsearch -exact $uses_ "DEC"] }
   public method getIDCol {} { wake; return [lsearch -exact $uses_ "ID"] }
   public method getCol {x} { wake; return [lsearch -exact $uses_ $x] }
   public method getHeadings {} { wake; return $headings_ }
   public method getNcol {} { wake; return $ncol_ }
   public method getNrow {} { wake; return $nrow_ }
   public method getEquinox {} { wake; return $equinox_ }
   public method getEpoch {} { wake; return $epoch_ }
   public method getId {} { wake; return "PCat$id_" }
   public method getFile {} { wake; return $polfile_ }
   public method getTclFile {} { wake; return $tclfile_ }
   public method getPixBounds {} { wake; return [list $xlo_ $ylo_ $xhi_ $yhi_] }
   public method getNpix {} { wake; return [expr $nxpix_*$nypix_] }
   public method getDesc {} { wake; return $desc_ }
   public method setDesc {desc} { wake; set desc_ "$desc" }
   public method getHfmts {} { wake; return $hfmts_ }
   public method getFmts {} { wake; return $fmts_ }
   public method getZlo {} { wake; return $zlo_ }
   public method getZhi {} { wake; return $zhi_ }
   public method getZcunit {} { wake; return $zcunit_ }
   public method getZaunit {} { wake; return $zaunit_ }
   public method getRefRot {} { wake; return $refrot_ }
   public method getWarned {} { wake; return $warned_}
   public method setWarned { {x 1} } { wake; set warned_ $x}

#  Return the heading of the column containing the quantity given by $q.
#  ---------------------------------------------------------------------
   public method getColNam {q} { wake;
      set icol [lsearch -exact $uses_ $q]
      if { $icol != -1 } {
         set ret [lindex $headings_ $icol]
      } else {
         set ret ""
      }
      return $ret
   }

#  Indicate that the column with heading $c stores the quantity given by
#  $q.
#  ---------------------------------------------------------------------
   public method setColNam {q c} { wake;

#  Find the index of the column with the supplied heading. Do nothing if
#  no column has this heading.
      set icol [lsearch -exact $headings_ $c]
      if { $icol != -1 } {

#  Indicate that the new column now stores this quantity.
         set uses_ [lreplace $uses_ $icol $icol $q]

      }
   }

#  Protected methods:
#  ==================

#  Creates a new disk file holding tcl code defining the contents of
#  a catalogue. The name of the disk file is returned. The description
#  stored in the disk file is a copy of $this except that the supplied
#  data array is used instead of the data array associated with $this.
#  ---------------------------------------------------------------------
   public method newTclFile { data } {

#  Ensure this PolData is usable.
      wake

#  Get the number of rows in the supplied data array.
      set nrow [llength $data]

#  Create a name for the new file.
      set newfile [tmpFile]

#  Open this new file.
      set fd [open $newfile w]

#  Write out all the attributes of this PolData to the file.
      puts $fd "set gotwcs_   \"[blanks $gotwcs_]\""
      puts $fd "set uses_     \"[blanks $uses_]\""
      puts $fd "set headings_ \"[blanks $headings_]\""
      puts $fd "set xlo_      \"[blanks $xlo_]\""
      puts $fd "set ylo_      \"[blanks $ylo_]\""
      puts $fd "set zlo_      \"[blanks $zlo_]\""
      puts $fd "set xhi_      \"[blanks $xhi_]\""
      puts $fd "set yhi_      \"[blanks $yhi_]\""
      puts $fd "set zhi_      \"[blanks $zhi_]\""
      puts $fd "set ncol_     \"[blanks $ncol_]\""
      puts $fd "set nrow_     \"[blanks $nrow]\""
      puts $fd "set ra_       \"[blanks $ra_]\""
      puts $fd "set dec_      \"[blanks $dec_]\""
      puts $fd "set equinox_  \"[blanks $equinox_]\""
      puts $fd "set epoch_    \"[blanks $epoch_]\""
      puts $fd "set xrefpix_  \"[blanks $xrefpix_]\""
      puts $fd "set yrefpix_  \"[blanks $yrefpix_]\""
      puts $fd "set nxpix_    \"[blanks $nxpix_]\""
      puts $fd "set nypix_    \"[blanks $nypix_]\""
      puts $fd "set secpix_   \"[blanks $secpix_]\""
      puts $fd "set fmts_     \"[blanks $fmts_]\""
      puts $fd "set hfmts_    \"[blanks $hfmts_]\""
      puts $fd "set zcunit_   \"[blanks $zcunit_]\""
      puts $fd "set zaunit_   \"[blanks $zaunit_]\""
      puts $fd "set refrot_   \"[blanks $refrot_]\""

#  Write out the supplied data array
      puts $fd "set data_ \{ \\"
      foreach row $data {
         puts $fd "\{ $row \} \\"
      }
      puts $fd "\}"

#  Close the file
      close $fd

      return  $newfile
   }

#  Get a GaiaApp which provides access to a specified polpack command.
#  ------------------------------------------------------------------
   protected method findGaiaApp {command} {

#  Get any existing GaiaApp for the specified command. These are stored
#  as static data members of the GaiaPolObject class.
      set app [GetGaiaApp $command]

#  If not already done, create a GaiaApp to provide access to the command.
      if { $app == "" } {
         global ::env
         if { [info exists env(POLPACK_DIR)] } {
            set app [::gaia::GaiaApp \#auto  \
                      -application $env(POLPACK_DIR)/$command ]

         } else {
            if { [file isdirectory "/star/bin/polpack"] }  {
               set app [::gaia::GaiaApp \#auto \
                      -application /star/bin/polpack/$command ]
            } else {
               info_dialog "Cannot locate the POLPACK directory. \
               Define the environment variable POLPACK_DIR and restart."
            }
         }

#  Save this GaiaApp as a static data member of the GaiaPolObject class (the
#  parent class of this class), so that it is available to all instances
#  of this clas. The GaiaPolObject class will take care of destroying the
#  GaiaApp when the last GaiaPolObject is destroyed.
         SetGaiaApp $command $app
      }

#  If succesful, configure the GaiaApp object so that it can be used with
#  this GaiaPolData.
      if { $app != "" } {
         $app configure -notify [code $this completed]
         $app configure -parnotify [code $this gotparam]
      }

#  Return the GaiaApp
      return $app
   }

#  Find the name to use for the polpack configuration file.
#  ---------------------------------------------------------
   protected method rcfile {} {
      global ::env
      if { [info exists env(POLPACKRC)] } {
         set rcfile $env(POLPACKRC)
      } else {
         set rcfile "$env(HOME)/.polpackrc"
      }
   }

#  Saves a copy of any existing polpack configuration file, and then
#  creates a new one holding the column definitions for $this. The name of
#  the file holding the copy of the original configuarion file is returned.
#  $req is a list of required quanities. Reports an error and returns "*" if
#  any required quantity is not available.
#  ------------------------------------------------------------------------
   protected method makepolrc { req } {

#  Get the name to use for the polpack config file.
      set rcfile [rcfile]

#  If it exists save a copy of it.
      if { [file exists $rcfile] } {
         set oldrc [tmpFile]
         file copy -force $rcfile $oldrc
      } else {
         set oldrc ""
      }

#  Open the config for for appending.
      set fd [open $rcfile "a+"]

#  Modify the config file by appending the column definitions for $this
#  to the end of the original rc file, overriding any earlier in the file.
      set ok 1
      foreach q "X Y Z RA DEC I Q U V DI DQ DU DV PI P DP ANG DANG DPI" {
         set colnam [getColNam $q]

#  If no column is available for this qunatity, report an error and abort
#  if the quantity is one of the required quantities.
         if { $colnam == "" } {
            if { [lsearch -exact $req $q] != -1 } {
               error_dialog "Please use the \"Column Names\" panel to indicate which column holds $q values."
               set ok 0
               break
            }
         }
         puts $fd "Column $q $colnam"
      }

#  Close the new config file.
      close $fd

#  If any required columns were missing, delete the new config file and
#  return "*".
      if { !$ok } {
         clearpolrc $oldrc
         set oldrc "*"
      }

#  Return the name of the original file (if any).
      return $oldrc
   }

#  Reinstate the original polpack config file.
#  -------------------------------------------
   protected method clearpolrc {oldrc} {
      if { $oldrc != "" } {
         file copy -force $oldrc [rcfile]
         catch { file delete $oldrc }
      } else {
         file delete [rcfile]
      }
   }

#  Save a given tcl file as a new polpack catalogue.
#  ----------------------------------------------
   protected method tclSave { tclfile refcat newcat } {
      global ::env

#  Get the GaiaApp which provides access to the Polpack "polrdtcl" command.
      set polrdtcl [findGaiaApp "polrdtcl"]

#  Skip if the above failed.
      if { $polrdtcl != "" } {

#  Run polrdtcl on the supplied tcl file, propagating meta-data from the
#  supplied catalogue.
         run $polrdtcl "in=$tclfile out=$newcat ref=$refcat accept"

      }

   }

#  Run a GaiaApp and wait for it to finish.
#  ----------------------------------------
   protected method run {gaiaapp plist} {
      set done_ 0
      blt::busy hold $w_
      $gaiaapp runwiths $plist
      while { $done_ == 0 } {
         after 500 {set a 1}
         update idletasks
         tkwait variable a
      }
   }

#  Executed when a GaiaApp "runwiths" completes.
#  ----------------------------------------
   protected method completed {} {
      blt::busy release $w_
      set done_ 1
   }

#  Executed when a GaiaApp "getparam" completes. Remove enclosing single
#  quotes which are added by startcl.
#  ---------------------------------------------------------------------
   protected method gotparam {param val} {
      if { ![regexp {^ *'(.*)' *$} $val match parval_] } {
         set parval_ $val
      }
      set done_ 1
   }

#  Get a parameter value from a GaiaApp.
#  ------------------------------------
   protected method getparam {gaiaapp param} {
      set done_ 0
      $gaiaapp getparam $param
      while { $done_ == 0 } {
         after 500 {set a 1}
         update idletasks
         tkwait variable a
      }
      return $parval_
   }

#  Set the class members by sourcing the tcl script produced by
#  polpack:polwrtcl.
#  ----------------------------------------------------------------
   protected method wake {{report 1}} {
      set msg " "

#  Do nothing if this GaiaPolData is already usable (not asleep), or if the
#  name of the Tcl file used to communicate with polpack:polwrtcl is unknown.
      if { $tclfile_ != "" && $asleep_ } {

#  If the Tcl file doesn't exist, use polwrtcl to re-create it.
         if { ![file exists $tclfile_] } {
            $pbar_ config -text "Reading $polfile_ ..."
            catch {tclLoad $polfile_ $tclfile_} msg
         } else {
            $pbar_ config -text "Re-reading $polfile_ ..."
         }
         update idletasks

#  If the Tcl file still doesn't exist, give up.
         if { [file exists $tclfile_] } {

#  Attempt to source the Tcl file, catching errors.
            if { ![catch {source $tclfile_} msg] } {

#  Check that critical class data members now have non-null values.
               if { $gotwcs_ != "" && $uses_ != "" && $headings_ != ""
                    && $refrot_ != "" && $xlo_ != "" && $ylo_ != "" &&
                    $xhi_ != "" && $yhi_ != "" && $ncol_ != "" &&
                    $nrow_ != "" && $data_ != ""  } {

#  If the maximum number of GaiaPolData objects are already awake, we
#  need to put one to sleep so that we can wake up $this. Choose the one
#  which has been awake longest.
                  if { [llength $awake_] == $maxawake_ } {
                     eval [lindex $awake_ 0] sleep
                     set awake_ [lrange $awake_ 1 end]
                  }

#  Indicate the PolData can be used (i.e. it is not asleep).
                  lappend awake_ $this
                  set asleep_ 0
               }
            }
         }

         $pbar_ reset
         update idletasks

      }

      if { $asleep_ } {
         if { $report } {
            error_dialog "Failed to re-read data for $polfile_ from file $tclfile_ : $msg"
         }
         set ret $msg
      } else {
         set ret ""
      }

      return $ret
   }

#  Use polpack:polwrtcl to create a text file holding a Tcl script which
#  will (when sourced) assign values to the data members of this class
#  which describe the contents of the requested polpack catalogue.
#  ----------------------------------------------------------------
   protected method tclLoad { polfile tclfile } {
      global ::env

#  Get the GaiaApp which provides access to the Polpack "polwrtcl" command.
      set polwrtcl [findGaiaApp "polwrtcl"]

#  Skip if the above failed.
      if { $polwrtcl != "" } {

#  Run polwrtcl on the supplied catalogue, putting the results in the
#  Tcl file.
         run $polwrtcl "in=$polfile out=$tclfile accept"
      }
   }

#  Class procedures:
#  ================

#  Expand name to full path relative to current directory.
#  --------------------------------------------------------
   proc full_name {name} {
      if { "[string index $name 0]" != "/"} {
         set fname [pwd]/$name
      } else {
         set fname $name
      }
      return $fname
   }


#  Replace blank elements of a list with a string consisting of two
#  adjacent double quotes.
#  ----------------------------------------------------------------
   proc blanks { l } {
      if { [llength $l] < 2 } {
         set ret $l
      } else {
         set ret ""
         foreach el $l {
            if { $el == "" } {
               append ret \{\}
            } else {
               append ret $el
            }
            append ret " "
         }
      }
      return $ret
   }

#  Public data members:
#  ====================

#  Protected data members:
#  =======================
   protected {

#  The supplied filename containing the polpack catalogue.
      variable file_ ""

#  The absolute path for the file containing the polpack catalogue.
      variable polfile_ ""

#  The absolute path for the file containing the Tcl version of the
#  catalogue, created by polpack:polwrtcl.
      variable tclfile_ ""

#  A Tcl list of rows. Each row is itself a Tcl list of column values.
      variable data_ ""

#  Set to 1 if RA/DEC columns are available, or 0 if not.
      variable gotwcs_ ""

#  A Tcl list holding the column headings.
      variable headings_ ""

#  A Tcl list holding the quantities stored in each column. These are the
#  standard polpack names used for each quantity: (X, Y, Z, RA, DEC, I, Q,
#  U, V, DI, DQ, DU, DV, P, PI, ANG, DP, DPI, DANG, ID ). A null string
#  is stored if the quantity stored in a column is unknown.
      variable uses_ ""

#  A list of format specifiers for the column headings
      variable hfmts_

#  A list of format specifiers for the column values
      variable fmts_

#  The minimum X pixel index in the data
      variable xlo_ ""

#  The minimum Y pixel index in the data
      variable ylo_ ""

#  The minimum Z column value in the data
      variable zlo_ ""

#  The maximum X pixel index in the data
      variable xhi_ ""

#  The maximum Y pixel index in the data
      variable yhi_ ""

#  The maximum Z column value in the data
      variable zhi_ ""

#  The number of columns in the catalogue
      variable ncol_ ""

#  The number of rows in the catalogue
      variable nrow_ ""

#  The RA (h:m:s) and DEC (d:m:s) of the field centre in degrees (may be blank)
      variable ra_ ""
      variable dec_ ""

#  The pixel offset to ra_/dec_ from the bottom-left corner of the bounding box.
      variable xrefpix_ ""

#  The pixel offset to ra_/dec_ from the bottom-left corner of the bounding box.
      variable yrefpix_ ""

#  No of pixels in X in bounding box.
      variable nxpix_ ""

#  No of pixels in Y in bounding box.
      variable nypix_ ""

#  An estimate of the pixel size in arcseconds
      variable secpix_ ""

#  The equinox of RA/DEC values (eg "2000").
      variable equinox_ ""

#  The epoch as a decimal year (using the 1984 rule to determine B/J)
      variable epoch_ ""

#  The units of the Z column.
      variable zcunit_ ""

#  The units of the Z axis.
      variable zaunit_ ""

#  Top level window
      variable w_ ""

#  Progress bar.
      variable pbar_ ""

#  A description of the operation which created the catalogue.
      variable desc_ ""

#  The position angle of the reference direction (angle from Dec to ref,
#  through RA  in degs).
      variable refrot_ ""

   }

#  Private data members:
#  =====================
   private {

#  Set to 1 when polpack:polwrtcl completes.
      variable done_ 0

#  Set to the value of the most recently "got" parameter value
      variable parval_ ""

#  A sequence number used to identify the GaiaPolData.
      variable id_ ""

#  Indicates if the data_ array has been nullified to save memory.
      variable asleep_ 0

#  Has the user been warned about the fact that the displayed image does
#  not have WCS whereas this catalgue does?
      variable warned_ 0
   }

#  Common (i.e. static) data members:
#  ==================================

#  A list of the currently awake GaiaPolData objects
   common awake_ ""

#  A count of the number of GaiaPolDatas  created so far.
   common count_ 0

#  An array of tcl files indexed by the corresponding polpack disk file.
   common knownfiles_

#  The maximum allowed number of awake GaiaPolData objects
   common maxawake_ 3

#  The maximum allowed number of unused tcl files.
   common maxold_ 3

#  A list of currently unused tcl files.
   common oldfiles_ ""

#  An array of reference counts for each tcl file.
   common tclFileRefCount_

#  End of class definition.
}
