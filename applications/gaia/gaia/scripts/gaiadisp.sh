#!/bin/sh    
# The next line is executed by /bin/sh, but not Tcl \
exec $GAIA_DIR/gaia_stcl $0 ${1+"$@"}
#+
#   Name:
#      gaiadisp
#
#   Purpose:
#      Displays an image in a GAIA tool.
#
#   Usage:
#      gaiadisp image [clone_number]
#
#   Description:
#      This command displays a given image in GAIA. The image can 
#      be directed into a specified "clone". If the clone or GAIA 
#      do not exist then they are created. 
#
#      Clones are specified by an integer number less than 1000.
#
#   Notes:
#
#   Authors:
#      Peter W. Draper (PDRAPER):
#
#   History:
#      25-NOV-1996 (PDRAPER):
#         Original version.
#      09-MAR-1998 (PDRAPER):
#         Now uses the remote control interface, rather than send
#         mechanism (less X security complaints).
#      18-MAY-1999 (PDRAPER):
#         Converted to use GAIA single binary gaia_stcl.
#-
#.

#  Check the command-line arguments.
set clone ""
if { $argc >= 1 } { 
   set image [lindex $argv 0]
   if { $argc >= 2 } {
      set clone [lindex $argv 1]
   }
} else {
   puts stderr {Usage: gaiadisp filename [clone_number]}
   exit
}

#  Define class for dealing with image names (included so will work
#  with single binaries).
itcl::class GaiaImageName {
   constructor {args} {
      eval configure $args
   }
   destructor  {
   }
   public method fullname {} {
      return $fullname_
   }
   public method diskfile {} {
      return $diskfile_
   }
   public method slice {} {
      return $slice_
   }
   public method path {} {
      return $path_
   }
   public method type {} {
      return $type_
   }
   public method exists {} {
      if { [file readable $diskfile_] && [file isfile $diskfile_] } {
	 return 1
      } else {
	 return 0
      }
   }
   public method absolute {} {
      if { ! [string match {/*} $imagename] } {
	 if { ! [catch {set here [pwd]}] } {
            if { [string range $here 0 7] == "/tmp_mnt" } {
               set here [string range $here 8 end]
            }
	    set imagename "$here/$imagename"
	    parse_name_
	 }
      }
   }
   protected method parse_name_ {} {
      reset_
      get_slice_
      get_type_
      if { ! [check_type_] } {
	 get_path_
      }
      get_diskfile_
      get_fullname_
   }
   protected method get_slice_ {} {
      set i1 [string last {(} $imagename]
      set i2  [string last {)} $imagename]
      if { $i1 > -1 && $i2 > -1 } {
	 set slice_ [string range $imagename $i1 $i2]
      } else {
	 set slice_ ""
      }
   }
   protected method get_type_ {} {
      set tail [file tail $imagename]
      set i1 [string first {.} $tail]
      if { $i1 > -1 } {
	 set type_ [string range $tail $i1 end]
      } else {
	 set type_ ".sdf"
      }
   }
   protected method check_type_ {} {
      if { [string match ".fit*" $type_] ||
	   [string match ".FIT*" $type_] ||
	   [string match ".sdf" $type_] } {
	 return 1
      }
      global env
      if { [info exists env(NDF_FORMATS_IN)] } {
	 if { [string first $type_ $env(NDF_FORMATS_IN)] > -1 } {
	    return 1
	 }
      }
      return 0
   }
   protected method get_diskfile_ {} {
      set i1 [string first $type_ $imagename]
      if { $i1 > -1 } {
	 incr i1 -1
	 set diskfile_ "[string range $imagename 0 $i1]$type_"
      } else {
	 set i1 [string first $path_ $imagename]
	 if { $i1 > -1 } {
	    incr i1 -1
	    set diskfile_ "[string range $imagename 0 $i1]$type_"
	 } else {
	    if { $slice_ != {} } {
	       set i2 [expr [string first $slice_ $imagename]-1]
	    } else {
	       set i2 end
	    }
	    set diskfile_ "[string range $imagename 0 $i2]$type_"
	 }
      }
   }
   protected method get_fullname_ {} {
      set fullname_ "$diskfile_$path_$slice_"
   }
   protected method get_path_ {} {
      set i1 [string first {.sdf} $type_]
      if { $i1 > -1 } {
	 set i1 [expr $i1+4]
	 if { $slice_ != {} } {
	    set i2 [expr [string first $slice_ $type_]-1]
	 } else {
	    set i2 end
	 }
	 set path_ [string range $type_ $i1 $i2]
	 
      } else {
	 if { $slice_ != {} } {
	    set i2 [expr [string first $slice_ $type_]-1]
	 } else {
	    set i2 end
	 }
	 set path_ [string range $type_ 0 $i2]
      }
      set type_ ".sdf"
   }
   protected method reset_ {} {
      set fullname_ {}
      set diskfile_ {}
      set slice_ {}
      set path_ {}
      set type_ {.sdf}
   }
   public variable imagename {} {
      if { $imagename != {} } {
	 parse_name_
      }
   }
   protected variable fullname_ {}
   protected variable diskfile_ {}
   protected variable slice_ {}
   protected variable path_ {}
   protected variable type_ {.sdf}
}

#  Now parse name.
set namer [GaiaImageName .namer -imagename $image]
if { ! [$namer exists] } { 
   puts stderr "Cannot read image: $image"
   exit 1
}

#  Make it absolute (also stripping off tmp_mnt, if present).
$namer absolute

#  Open a socket to a GAIA application and return the file descriptor
#  for remote commands. If a GAIA isn't found then start one up.
proc connect_to_gaia {} {
   global env

   #  Get the hostname and port info from the file ~/.rtd-remote,
   #  which is created by rtdimage when the remote subcommand is
   #  used.
   set tries 0
   while { 1 } {
      set needed 0

      #  Open the file containing the GAIA process information and read it.
      if {[catch {set fd [open $env(HOME)/.rtd-remote]} msg]} {
         set needed 1
      } else {
         lassign [read $fd] pid host port
         close $fd
      }

      #  See if the process is listening to this socket.
      if { ! $needed } {
         if {[catch {socket $host $port} msg]} {
            set needed 1
         } else {
            fconfigure $msg -buffering line
            return $msg
         }
      }

      #  If the process doesn't exist and we've not been around the
      #  loop already, then start a new GAIA.
      if { $needed && $tries == 0 } {
         puts stderr "Failed to connect to GAIA, starting new instance..."
         exec $env(GAIA_DIR)/gaia.sh &
         #exec $env(GAIA_DIR)/tgaia &
      }

      #  Now either wait and try again or give up if waited too long.
      if { $needed && $tries < 500 } {
         #  Wait for a while and then try again.
         incr tries
         after 1000
      } elseif { $needed } {
         puts stderr "Sorry timed out: failed to display image in GAIA"
         exit 1
      }
   }
}

#  Send the command to GAIA and return the results or generate an error.
proc send_to_gaia {args} {
    global gaia_fd
    puts $gaia_fd "$args"
    lassign [gets $gaia_fd] status length
    set result {}
    if {$length > 0} {
	set result [read $gaia_fd $length]
    }
    if {$status != 0} {
	error $result
    }
    return $result
}

#  Open up connection to GAIA.
set gaia_fd [connect_to_gaia]

#  Command needs to performed by Skycat or derived object. We just
#  talk to the first window on the list.
set cmd "skycat::SkyCat::get_skycat_images"
set images [send_to_gaia remotetcl $cmd]

#  Got list so select first and ask about the parent (should be 
#  top-level GAIA).
set ctrlwidget [lindex $images 0]
set cmd "winfo parent $ctrlwidget"
set gaia [send_to_gaia remotetcl $cmd]

#  Construct the command needed to display the image.
if { $clone != "" } { 
   set cmd "$gaia noblock_clone $clone [$namer fullname]"
} else {
   set cmd "$gaia open [$namer fullname]"
}
 
#  And send the command.
set ret [send_to_gaia remotetcl $cmd]
if { $ret == "" || $ret == ".gaia$clone" } { 
   puts stderr "Displayed image: $image."
} else {
   puts stderr "Failed to display image: ($ret)"
}
close $gaia_fd
exit
