#+
#  Name:
#     gaia_util

#  Purpose:
#     Utility procs.

#  Type of Module:
#     Tcl/Tk script.

#  Description:
#     Utility procedures for GAIA. Includes procedure for 
#     parsing an NDF filename and a replacement for filename_dialog
#     (in tclutil).

#  Authors:
#     PDRAPER: Peter Draper (STARLINK)
#     {enter_new_authors_here}

#  History:
#     21-NOV-1996 (PDRAPER):
#        Original version
#     {enter_changes_here}

#-


#  Define a useful procedure for handling image names with slices.
proc fileName {image} {

   # return the proper filename of an image and any slice information.
   set i1 [string last {(} $image]
   set i2  [string last {)} $image]
   if { $i1 > -1 && $i2 > -1 } {
      set slice [string range $image $i1 $i2]
      incr i1 -1
      set image [string range $image 0 $i1]
   } else {
      set slice ""
   }
   set image2 "$image"
   if { [file extension $image] == "" } {
      set image "${image}.sdf"
   }
   return [list $image $slice]
}

#  Get a file name from the user and return it or the empty string.
#  The optional arguments are the directory (first time only), the
#  filter value, the parent widget and an optional list of file types
#  (suffixes) to display in a menu. Modified to check if default types
#  are changed, before resetting.
proc filename_dialog {{dir "."} {filter "*"} {parent ""} {types ""}} {
   set w .fs
   if {![winfo exists $w]} {
      FileSelect $w -dir $dir -filter $filter -transient 1 \
         -withdraw 1 -filter_types "$types"
   } else {

      #  Only reconfigure if default types are changed.
      set curtypes [$w cget -filter_types]
      if { "$curtypes" != "$types" } {
         $w config -filter $filter -filter_types "$types"
      }
   }
   if {"$parent" != ""} {
      wm transient $w [winfo toplevel $parent]
   }
   if {[$w activate]} {
      return [$w get]
   }
}
