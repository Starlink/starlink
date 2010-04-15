# This script acts as an "adapter" between the iraf cl protocol and the
# Adam message system so that a Starlink application can be run from the
# iraf cl with full access to the two systems parameter systems.
# History:
#    10-OCT-1996 (AJC):
#       Get output params even after error to reset program locator
#    11-Oct-1996 (AJC):
#       irafval2adam: Don't quote strings if already quoted
#    14-OCT-1996 (AJC):
#       Cancel 10-OCT mod as SUBPAR_DEACT now doesn't need it and bad
#       values could get put in IRAF parameter file.
#    17-OCT-1996 (AJC):
#       Corrected adamval2iraf for struct (adding quotes)
#    22-OCT-1996 (AJC):
#       Use DynParList on paramreq
#    22-JAN-1997 (AJC):
#       Remove OUTPUT from adampar2iraf
#    25-FEB-1997 (AJC):
#       Remove use of DynParList
#     7-MAR-1997 (AJC):
#       Inactivate ban on prompt for provided and hidden params by
#       not recording par_on_com_line in do_set_param
#       Force prompt if null default (including @) in param_req
#    12-MAR-1997 (AJC):
#       Output inform messages during do_output_params
#       And error message if not SAI__OK or PAR__NULL
#    11-APR-1997 (AJC):
#       Ignore mode and x.mode on command line (allows pset parameters).
#       Don't use ADAM suggested value
#       Restore DynParList for GLOBAL and Dynamic values which user can't set
#    21-APR-1997 (AJC):
#       Go through process of getting a value before deciding to force a prompt
#       if no value obtained.
#    23-APR-1997 (AJC):
#       Update IRAF value of params in DynParList so IRAF params reflect
#       value actually used (assuming learn mode is on for others).
#    29-MAY-1997 (AJC):
#       Only change isolated `NDF' in text to `image'.
#       In irafval2adam change [] in filenames to ()
#       Restore ! response if param on command line as there is no way to
#        force a prompt if the value is no good.
#       Force prompt if param already requested
#        (this prevents looping if value is no good).
#       Only update IRAF with non-blank suggested values of dynamic params
#        (so prompt will at least have the default IRAF has).
#     1-JUL-1997 (AJC):
#       Trap parameter flpar used by ncl.
#       Also requres handling in adampar2iraf and irafpar2adam
#     9-JUL-1997 (AJC):
#       Correct q to "q" in forceprompt
#    10-AUG-1997 (AJC):
#       Put directory in chdir through irafval2adam to handle env vars.
#    15-AUG-1997 (AJC):
#       Arrange to forward `set' messages to ADAM task - disable till
#       tasks can handle them (see loop reading messages at end of this script)
#    10-DEC-1997 (AJC):
#       Don't try to set NULL output parameters to INDEF - it causes
#       overwriting of parameter file by other tasks.
#-
# Procedure definitions...

proc iraf_text {text} {
#+
# Send some text to be written by the cl.
#-
    ipc_write "xmit(4,[expr [string length $text]+1])\n"
    ipc_write $text\n
}

proc do_action {action comline} {
#+
# Instructs the adam task to obey an action and responds to any messages
# from the adam task.
#-
    global adam_taskname
    global DynParList
    global par_type

# Initialise the list of parameters that appear on the command line.
    global par_on_com_line
    catch {set par_on_com_line {}}
    set par_got_already {}

#iraf_text "comline: $action $comline"
# Process the command line arguments.
    while {[regexp -indices {([^=]*="[^"]*")|([^=]*=[^ ]*( |$))} $comline \
	range] == 1} {
	set n [lindex $range 1]
	set arg [string range $comline 0 $n]
#iraf_text "arg: $arg"
	incr n
	set comline [string range $comline $n end]
	set n [string first = $arg]
	set par [string trim [string range $arg 0 [expr $n-1]]]
	set val [string range $arg [expr $n+1] end]

#   Set the parameter value in the ADAM task.
        do_set_param $action $par $val
    }

# Send an OBEY message.
    adam_send $adam_taskname $action obey "prompt"

# Loop collecting messages from the adam task.
    while {1} {
	set reply [adam_receive]
	switch [lindex $reply 0] {
	    paramreq {

#            Extract the parameter name.
#iraf_text "paramreq: [lindex $reply 6]"
		set param [adampar2iraf [lindex [lindex $reply 6] 0]]

#            Get its type.
	        set type $par_type($param)

#            If the parameter was on the command line then reply with a !
#		global par_on_com_line
		if {[lsearch -exact $par_on_com_line $param] != -1} {
                   iraf_text \
                  "Received request for parameter $param, given on command line"
                   iraf_text "reply with NULL value"
	           adam_reply [lindex $reply 3] [lindex $reply 4] paramrep "" !
                } {
#                Extract the suggested default
#iraf_text "reply is: $reply"
		    set default [string trim [lindex [lindex $reply 6] 2]]
#iraf_text "default is: $default"

#                Extract the prompt string and set the cl prompt.
#		    set prompt [adamtext2iraf [string trim [lindex [lindex \
#			$reply 6] 1]]]
#		    ipc_write ${param}.p_prompt=\"$prompt\"\n

#                If parameter is in DynParList, use default
#                otherwise prompt for it
                    set prompt 1
                    if { [info exists DynParList($action)] } {
#iraf_text "DynParList($action): $DynParList($action)"
                      if { [lsearch -exact $DynParList($action) $param] >= 0 } {
                          set prompt 0
                      }
                    }

#                  If required request the parameter from the cl.
                    if { $prompt } {
#iraf_text "Getting IRAF value"
#                   If the parameter has been obtained once already,
#                   force a prompt
   		      if {[lsearch -exact $par_got_already $param] != -1} {
#iraf_text "forcing prompt"
                         set value [forceprompt $param]
                      } {
#                    Get value from cl in the normal way
                         ipc_write =$param\n
                         set value \
                          [irafval2adam [string trimright [ipc_read] \n] $type]
#                    Register that a value has been got already
                         lappend par_got_already $param
                      }
                    } {
#iraf_text "Using suggested value"
#                   Using suggested value, also update IRAF value
#                   unless there is no suggested value
                       if { $default!="" } {
                          ipc_write \
                             "$param=[adamval2iraf $default $type]\n"
                       }
                       set value $default
                    }

#                Force a prompt if the value is blank (include a
#                blank filename, '@', in this)
#iraf_text "value is: $value"
		    if {($value=="") || ($value=="@")} {
                       iraf_text "A value must be given for parameter ${param}"
                       set value [forceprompt $param]
                    }

#                Reply to the prompt
#iraf_text "value is: $value"
		    adam_reply [lindex $reply 3] [lindex $reply 4] paramrep "" \
		        $value
                 }
	    }
	    inform {

#             Ask the cl to print the message.
		iraf_text [adamtext2iraf [lindex $reply 6]]
	    }
	    sync {

#             Send a syncreply.
		adam_reply [lindex $reply 3] [lindex $reply 4] syncrep "" ""
	    }
	    startmsg {
	    }
	    endmsg {
		set status [lindex $reply 5]
# workround bug in ADAM on Solaris
		if {[string first DTASK__ACTCOMP [lindex $status 0]] != -1} {
		    set status [lreplace $status 0 0 DTASK__ACTCOMPLETE]
		}
		if {[lindex $status 0]=="DTASK__ACTCOMPLETE"} {
#iraf_text "Doing output params"
		    do_output_params $action
		}
		adam_send $adam_taskname par_reset control ""
#iraf_text "sent par_reset"

		while {1} {
		    set reply [adam_receive]
		    switch [lindex $reply 0] {
			controlresponse {
			    break
			}
			inform {
			    iraf_text [adamtext2iraf [lindex $reply 6]]
			}
		    }
		}
		if {[lindex $status 0]=="DTASK__ACTCOMPLETE"} {
		    ipc_write bye\n
		} {
		    ipc_write "error 1 \"[lindex $status 1]\"\n"
		}
		break
	    }
	}
    }
}

proc adamval2iraf {val type} {
#+
#  Converts an ADAM parameter value to its IRAF equivalent.
#-
    if { $val == "!" } {return INDEF}

    switch $type {
	f {
	    return \"[string trimleft $val @]\"
	}
	struct {
	    if {[string match {\[*\]} $val]==1} {
		return \"[string range $val 1 [expr [string length $val]-2]]\"
	    } {
		return \"$val\"
	    }
	}
	s {
	    return \"$val\"
	}
	b {
	    switch [string toupper $val] {
                T -
                Y -
		TRUE -
                YES {return yes}
		F -
                N -
                YES -
                FALSE {return no}
		default {return no}
	    }
	}
	default {
	    if {[string match {\[*\]} $val]==1} {
		return [string range $val 1 [expr [string length $val]-2]]
	    } {
		return $val
	    }
	}
    }
}

proc irafval2adam {val type} {
#+
#  Converts an IRAF parameter value to its ADAM equivalent.
#-
    if {$val=="INDEF" || $val=="\"INDEF\""} {
	return !
    }
    set val [string trimleft $val {"}]
    set val [string trimright $val {"}]

    global env
    for {} {1} {} {
	set i [string first \$ $val]
	if { $i == -1 } break
	set logical [string range $val 0 [expr $i-1]]
	set rest [string range $val [expr $i+1] end]
	if {[catch {set env($logical)} trans] != 0} {
	    set trans ""
	}
	set val ${trans}${rest}
    }
    switch $type {
      s {
	if { [regexp {^".*"$|^'.*'$} $val] } {
            return $val
        } {
            return \"$val\"
        }
      }
      f {
        regsub {\[} $val {(} val
        regsub {\]} $val {)} val
        return $val
      }
      default {
	return $val
      }
    }
}

proc adampar2iraf {par} {
#+
#  Converts an adam parameter name to its iraf equivalent.
#  mode is special for IRAF
#  flpar is used by the SAO ncl alternative to cl.
#-
    switch $par {
	MODE {return mode_}
        FLPAR {return flpar_}
	default {return [string trim [string tolower $par]]}
    }
}

proc irafpar2adam {par} {
#+
#  Converts an iraf parameter name to its adam equivalent.
#-
    return [string toupper [string trimright $par _ ]]
}

proc adamtext2iraf text {
#+
#  Isolated occurrences of the word "NDF" are replaced by "image"
#  and leading ! is removed.
#-
    regsub -all -- {(^| )NDF($| )} $text " image " text
    return [string trimleft $text !]
}

proc do_output_params {action} {
#+
# Sends the values of any adam task output parameters to the cl.
#-
    global adam_taskname
    global OutputParList

    if {[info exists OutputParList($action)]} {
	foreach par $OutputParList($action) {
	    adam_send $adam_taskname $action:[irafpar2adam $par] get ""
	    while {1} {
		set reply [adam_receive]
		global par_type
#iraf_text "output: $par; type $par_type($par)"
#iraf_text "$reply"
		set type $par_type($par)
		switch [lindex $reply 0] {
		    getresponse {
                        set status [lindex [lindex $reply 5] 0]
#iraf_text "status is: $status"
			if {$status=="SAI__OK"} {
#iraf_text "$par=[adamval2iraf [lindex $reply 6] $type]"
			    ipc_write \
                              "$par=[adamval2iraf [lindex $reply 6] $type]\n"
			} {
                            if {$status!="PAR__NULL"} {
                               iraf_text " Failed to get output parameter value"
                            }
# It doesn't appear to be possible to set parameters to INDEF by this method
# So just leave them alone.
# There would need to be some type-dependent action anyway.
#			    ipc_write "$par=INDEF\n"
#iraf_text "$par=INDEF"
			}
			break
		    }
   	            inform {

#                     Ask the cl to print the message.
 		        iraf_text [adamtext2iraf [lindex $reply 6]]
		    }
                }
	    }
	}
    }
}

proc do_set_param {action par val} {
#+
#  Set a parameter value in an adam task.
#-
    if {$par=="\$nargs"} return
    if {$par=="mode"} return
    if { [regexp {\.mode} $par] } return
    if {$par=="flpar"} return

    global par_type
    global adam_taskname
    global par_on_com_line
    lappend par_on_com_line $par
#iraf_text "Set param $par: $val"
    adam_send $adam_taskname $action:[irafpar2adam $par] set \
	[irafval2adam "[string trim $val]" $par_type($par)]
    while {1} {
	set reply [adam_receive]
	switch [lindex $reply 0] {
	    setresponse {
		break
	    }
	    inform {
		iraf_text [lindex $reply 6]
	    }
	}
    }
}

proc do_chdir {iraf_dir} {
#+
# Changes the default directory of the adam task by sending it a control
# message.
#-
    global adam_taskname
    adam_send $adam_taskname default control [irafval2adam $iraf_dir none]
#iraf_text "New directory: [irafval2adam $iraf_dir none]"
    while {1} {
	set reply [adam_receive]
	switch [lindex $reply 0] {
	    controlresponse {
		break
	    }
	    inform {
		iraf_text [lindex $reply 6]
	    }
	}
    }
}

proc do_setenv {value} {
#+
# Sets and environment vaiable in the adam task by sending it a control
# message.
#-
    global adam_taskname
    adam_send $adam_taskname setenv control [irafval2adam $value none]
    while {1} {
	set reply [adam_receive]
	switch [lindex $reply 0] {
	    controlresponse {
		break
	    }
	    inform {
		iraf_text [lindex $reply 6]
	    }
	}
    }
}

proc adam_load {taskname exe} {
#-
# Loads an adam task and waits for it to attach to the message system.
# ADAM_TASK_TYPE is set to I so parameters are not cancelled automatically
# at the end of the task. One can then `get' values of output parameters
# in order to update the IRAF parameters before sending a PAR_RESET message.
#-
    global env
    set env(ICL_TASK_NAME) $taskname
    set env(ADAM_TASK_TYPE) I
    set pid [exec $exe &]
    set count 0
    while {[set ok [adam_path $taskname]] == 0} { }
    return $pid
}

proc read_par {parfile} {
#+
# Reads an iraf parameter file and loads a global array (par_type) with
# the type of each parameter.
#-
    global par_type
    catch {unset par_type}
    set f [open $parfile r]
    while {1} {
	set pardef [split [gets $f] ,]
	if {[eof $f]==1} break
	set par_type([lindex $pardef 0]) [lindex $pardef 1]
#iraf_text "read_par [lindex $pardef 0]"
	if {$par_type([lindex $pardef 0])=="struct"} {gets $f}
    }
    close $f
}

proc forceprompt { param } {
# Force IRAF to prompt for parameter param.
# return the value obtained - already converted to ADAM form
#                   get current mode
   global par_type
#iraf_text "get current mode $param.p_mode"
                        ipc_write "=${param}.p_mode\n"
                        set pmode [string trimright [ipc_read] \n]
#iraf_text "current mode is $pmode"
#                   set mode q
#iraf_text "set mode q"
		        ipc_write "${param}.p_mode=\"q\"\n"
#                   get value
                        ipc_write =$param\n
                        set value \
                         [irafval2adam [string trimright [ipc_read] \n] \
                         $par_type($param)]
#iraf_text "value: $value"
#                   reset mode to original
#iraf_text "reset mode"
		        ipc_write "${param}.p_mode=\"$pmode\"\n"
   return $value
}
# End procedure definitions.

# Loop reading messages from the iraf cl.
# These will be the initial setting up of the environment
    while {1} {
	set command [ipc_read]
	set i [string first " " $command]
	if {$i == -1} {
	    set action $command
	} {
	    set action [string trim [string range $command 0 $i]]
	}
	set rest [string trim [string range $command $i end]]
	switch $action {
	    set {
		set i [string first = $rest]
		set var [string range $rest 0 [expr $i-1]]
		set val [string range $rest [expr $i+1] end]
		set env($var) $val
	    }
	    _go_ {
		break
	    }
	    chdir {
		cd [string range $rest [expr [string first ! $rest]+1] end]
	    }
	    default {
	    }
	}
    }

# The environment for the task is now set so we can load the adam task and
# get hooked into the adam message system. argv0 contains the name used to
# run this script; the name of the adam task is obtained by removing the
# trailing ".e".
    set adam_root [file rootname $argv0]
    if {[string range $adam_root 0 0] != "/"} {
	set adam_root ./$adam_root
    }
    set adam_taskname [file tail $adam_root]_[pid]
    adam_start ${adam_taskname}_cl
    set adam_pid [adam_load  $adam_taskname $adam_root]
    ipc_pid $adam_pid

# Load initialisation script.
    source ${adam_root}.tcl

# Loop reading messages from the iraf cl.
    while {1} {
	set command [ipc_read]
	set i [string first " " $command]
	if {$i == -1} {
	    set action $command
	    set rest ""
	} {
	    set action [string trim [string range $command 0 $i]]
	    set rest [string trim [string range $command $i end]]
	}
	switch $action {
	    "" {}
	    bye {
		break
	    }
	    chdir {
		do_chdir $rest
	    }
	    set {
		set i [string first = $rest]
		set var [string range $rest 0 [expr $i-1]]
		set val [string range $rest [expr $i+1] end]
		set env($var) $val
#  Forward `set' message to ADAM task.
#  Comment out until tasks can handle it.
#      Should $rest be irafval2adam'd?
#      What about super tasks?
#iraf_text "$rest"
#                do_setenv $rest
	    }
	    default {
		read_par [file dirname $adam_root]/${action}.par
		do_action $action $rest
	    }
	}
    }

#  Kill the adam task
    exec kill -TERM $adam_pid

#  Exit
    exit
