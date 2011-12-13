#+
#  Name:
#     GaiaPolObject

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Implements a base Object class for use by the GaiaPolarimetry class.

#  Description:
#     GaiaPolObjects are [incr Tcl] objects which have an associated
#     reference count, which enables the class to know when an object is
#     no longer used, and can thus be destroyed. This class also handles
#     the creation of directories for temporary files used by PolObjects,
#     and the naming of such files.

#  Invocations:
#
#        GaiaPolObject object_name [configuration options]
#
#     This creates an instance of a GaiaPolObject object. The returned value
#     is the name of the object.
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
#     (none)

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
#     15-JUN-2000 (DSB):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaPolObject {

#  Inheritances:
#  =============
#  (none)

#  Constructor:
#  ============
   constructor {} {

#  Ensure the directory for temporary files if this is the first PolObject
#  has been created.
      mkTempDir

#  Append this objects name to the end of the list of all currently
#  active PolObjects.
      lappend objects_ $this

#  Initialise the class data if we are instantiating this class
#  rather than a derived class.
      if { [$this info class] == "::gaia::GaiaPolObject" } {
         init
      }
   }

#  Destructor:
#  ===========
   destructor {

#  Locate the object name within the list of currently active
#  PolObjects.
      set iobj [lsearch -exact $objects_ $this]

#  If found, remove it from the list.
      if { $iobj > -1 } {
         set objects_ [lreplace $objects_ $iobj $iobj]

#  If there are no active objects left...
         if { $objects_ == "" } {

#  Remove the directory used to hold temporary files.
            rmDir
            set tempdir_ ""

#  Destroy all the GaiaApps stored as static variables.
            ClearGaiaApps
         }

#  Otherwise, issue a warning.
      } else {
         puts "GaiaPolObject destructor: PolObject $this is not included in the list of currently active PolObjects!!"
         puts "($this = [$this toString])"
      }
   }

#  Public methods:
#  ===============

#  Retrieve a named static GaiaApp.
#  ------------------------------
   public method GetGaiaApp {name} {
      if { [info exists gaiaapp_($name)] } {
         return $gaiaapp_($name)
      } else {
         return ""
      }
   }

#  Store a named GaiaApp as a static data member.
#  ---------------------------------------------
   public method SetGaiaApp {name value} {
      set gaiaapp_($name) $value
   }

#  Destroy all stored GaiaApps
#  ---------------------------
   public method ClearGaiaApps {} {
      foreach name [array names gaiaapp_] {
         if { $name != "" } {
            catch { $gaiaapp_($name) delete_sometime }
         }
      }
   }

#  Call this when a reference to the object has been finished with.
#  It reduces the reference count for the object by one and then
#  destroys the object if no references are left.
#  ---------------------------------------------------------------
   public method annull {} {

      incr refCount_ -1

      if { $refCount_ == 0 } {
         ::itcl::delete object $this
      }
      return ""
   }

#  Return the object reference count.
#  --------------------------------------------------------------------
   public method refCount {} {
      return $refCount_
   }

#  Return the object parent.
#  -------------------------
   public method getParent {} {
      return $parent_
   }

#  Return the head of the clan.
#  -------------------------
   public method getTop {} {
      return $top_
   }

#  Call this to create a clone of (i.e. a new handle for) a GaiaPolObject.
#  it increases the reference count for the object by one, and returns the
#  object name. Note, no new GaiaPolObject is created.
#  --------------------------------------------------------------------
   public method clone {} {
      incr refCount_
      return $this
   }

#  Format a PolObject for textual display.
#  ---------------------------------------
   public method toString {} {
      return "GaiaPolObject: $this"
   }

#  Annull all GaiaPolObjects with a given head of clan.
#  ---------------------------------------------------
   public proc annullAll {top} {
      foreach obj $objects_ {
         if { ![catch {set topper [$obj getTop]}] } {
            if { $topper == $top } {
               $obj annull
            }
         }
      }
   }

#  Report any currently active PolObjects.
#  ---------------------------------------
   public proc activeObjects {} {
      foreach obj $objects_ {
         puts "$obj : [$obj toString]\n"
      }
      return [llength $objects_]
   }

#  Delete all currently active PolObjects.
#  ---------------------------------------
   public proc deleteActiveObjects {} {
      foreach obj $objects_ {
         $obj annull
      }
   }

#  Remove the directory used to store temporary files, etc.
#  --------------------------------------------------------
   public proc rmDir {} {
      if { [file exists $tempdir_] } {
         if { [catch {exec rm -r -f $tempdir_} msg ] } {
            error_dialog $msg
            puts $msg
	 }
      }
   }

#  Remove all files in the directory used to store temporary files, etc.
#  --------------------------------------------------------------------
   public proc emptyDir {} {
      foreach file [glob -nocomplain "$tempdir_/*"] {
         file delete $file
      }
   }

#  Return a unique basename for a temporary file, in the form "tempxxx"
#  where "xxx" is the sequence number given by $ifile_
#  ---------------------------------------------------------------
   public method tmpName {} {
      return "tmp[incr ifile_]"
   }

#  Return a unique path for a temporary file, in the form "tempxxx" where
#  "xxx" is the sequence number given by $ifile_
#  ---------------------------------------------------------------
   public method tmpFile {} {
      return "[getTmpDir]/[tmpName]"
   }

#  Return the path to the directory to be used to store temporary files.
#  ---------------------------------------------------------------
   public method getTmpDir {} {
      return $tempdir_
   }

#  Protected methods:
#  ==================

#  Returns a list holding the names of all PolObjects with a given parent.
#  -----------------------------------------------------------------------
   public proc findChildren {top} {
      set ret ""
      if { $top != "" } {
         foreach obj $objects_ {
            set p [$obj getParent]
            if { $p == $top } {
                lappend ret $obj
                lappend ret [::gaia::GaiaPolObject::findChildren $obj]
            }
         }
      }
      return $ret
   }

#  Initialise the contents of the memory allocated by the
#  GaiaPolObject constructor using a user-supplied argument list.
#  -------------------------------------------------------
   protected method init {} {
      set refCount_ 1

#  Record the name of the object which is creating this GaiaPolObject
      set parent_ ""
      set n 1
      upvar this parent
      while { [info exists parent] } {
         if { $parent != $this } {
            set parent_ $parent
            break
         }
         upvar [incr n] this parent
      }

#  Find the first ancestor which is not a GaiaPolObject.
      set top_ $parent_
      while { [$top_ isa ::gaia::GaiaPolObject] } {
         set top_ [$top_ getTop]
      }

   }

#  Private methods:
#  ==================

#  Create a directory to store temporary files created by the
#  gaia polarimetry toolboxes.
#  -------------------------------------------------------
   private method mkTempDir {} {
      global ::env

#  Do nothing if this has already been done.
      if { $tempdir_ == "" } {
         global env

#  First option is to create a new subdirectory within $GAIA_TEMP_DIR.
#  Otherwise, create it within the current directory.
         if { [info exists env(GAIA_TEMP_DIR)] } {
            set dir $env(GAIA_TEMP_DIR)
         } {
            set dir [pwd]
         }

#  The subdirectory name includes the pid to make it unique.
         set tempdir_ "${dir}/GaiaPolarimetry_[pid]"

#  Make sure this new directory exists (delete any existing version).
         rmDir
         catch {exec mkdir -p $tempdir_}

#  Initialise the sequence number.
         set ifile_ 0

      }
   }


#  (none)

#  Public data members:
#  ====================
#  (none)

#  Protected data members:
#  =======================
   protected {

#  No. of references currently in use for this GaiaPolObject.
      variable refCount_ 1

#  The parent object
      variable parent_ ""

#  The first ancestor of $this which is not a PolObject.
      variable top_ ""

   }

#  Private data members:
#  =====================
#  (none)

#  Common (i.e. static) data members:
#  ==================================

#  A list of all currently active PolObjects.
   common objects_ ""

#  A directory for temporary files
   common tempdir_ ""

#  A sequence number to use for temporary files.
   common ifile_ 0

#  An array of gaiaapps
   common gaiaapp_

#  End of class definition.
}

