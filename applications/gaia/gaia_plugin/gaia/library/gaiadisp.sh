#! /bin/sh
# The next line is executed by /bin/sh, but not Tcl \
TCL_LIBRARY=$GAIA_DIR; \
TCLX_LIBRARY=$GAIA_DIR; \
TK_LIBRARY=$GAIA_DIR; \
TKX_LIBRARY=$GAIA_DIR; \
ITCL_LIBRARY=$GAIA_DIR; \
ITK_LIBRARY=$GAIA_DIR; \
IWIDGETS_LIBRARY=$GAIA_DIR; \
RTD_LIBRARY=$GAIA_DIR; \
TCLADAM_DIR=$GAIA_DIR; \
export TCL_LIBRARY; \
export TCLX_LIBRARY; \
export TK_LIBRARY; \
export TKX_LIBRARY; \
export ITCL_LIBRARY; \
export ITK_LIBRARY; \
export IWIDGETS_LIBRARY; \
export RTD_LIBRARY; \
export TCLADAM_DIR; \
exec $GAIA_DIR/gaia_wish $0 ${1+"$@"}
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
#      This command uses the Tk "send" mechanism to communicate with
#      GAIA. It must therefore have access to the X display in which
#      GAIA is displayed. For security reasons this must also be a
#      display that is explicitly named by "xhosts", or controlled by
#      xauthority (the use of xauthority is strongly recommended).
#
#   Authors:
#      Peter W. Draper (PDRAPER):
#
#   History:
#      25-NOV-1996 (PDRAPER):
#         Original version.
#-
#.

#  Do not show the main window.
wm withdraw .

#  Procedure to get the names of any interpretors running, pick the
#  first one called GAIA*.
proc check_interp {} { 
   set names [winfo interps]
   set gaia ""
   foreach name "$names" { 
      if { [string match "GAIA*" $name] } { 
         set gaia $name
      }
   }
   return $gaia
}


#  Check the command-line arguments.
set clone ""
if { $argc >= 1 } { 
   set image [lindex $argv 0]
   if { $argc >= 2 } {
      set clone [lindex $argv 1]
   }
} else {
   puts {Usage: gaiadisp filename [clone_number]}
   exit
}

#  See if the file exists and it so transform into an absolute name
#  (GAIA may not be running in this directory).
if { ! [file readable $image] } { 
   puts "Cannot read image: $image"
   exit 1
} else { 
   if { ! [string match {/*} $image] } {
      #  Name isn't absolute so must be relative.
      set image [pwd]/$image
   }
}

#  If no interpretor found then start a new GAIA instance.
if { [set gaia [check_interp]] == "" } { 
   puts "Cannot locate a running GAIA. Starting a new instance."
   global env
   exec $env(GAIA_DIR)/gaia.sh &
   set n 0
   while { [set gaia [check_interp]] == "" } {
      after 1000
      if { $n == 30 } { 
         puts "Failed to make connection to GAIA."
         exit 1
      }
      incr n
   }
}

#  Locate the base clone for the current displayed setup.
set clones "[send $gaia info command .rtd?] \
            [send $gaia info command .rtd??] \
            [send $gaia info command .rtd???] \
            [send $gaia info command .rtd????]"
set baseclone [lindex $clones 0]

#  See if the requested clone is available.
set rtdwin ""
if { $clone != "" } { 
   foreach w "$clones" { 
      if { $w == ".rtd$clone" } { 
         set rtdwin "$w"
      }
   }
} else {
   
   #  No clone specified so use base clone.
   set rtdwin $baseclone
   regsub {\.rtd} $rtdwin {} clone
}

#  If no clone available then create it.
if { $rtdwin == "" } { 
   set rtdwin [send $gaia $baseclone clone $clone]


#  And wait for creation.
   set n 0
   while { ! [send $gaia winfo exists $rtdwin] } {
     after 1000
     if { $n == 120 } { 
       puts "Failed to make connection to GAIA."
       exit 1
     }
     incr n
   }
}

#  Now display the image (if not already done).
catch {send $gaia $rtdwin open $image}
exit
