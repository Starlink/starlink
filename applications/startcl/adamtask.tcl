#+
# adamtask.tcl
#
# This file implements an object oriented interface to adam tasks. A task
# is created with:
#
#		adamtask <name> [<file>]
#
# where <name> is the name of the task and <file> is the image file. If
# <file> is supplied, the adam task is spawned otherwise it is assumed to
# be already running. A tcl command called <name> is created that accepts
# the following arguments:
#
# <name> kill
# <name> forget
# <name> set <parameter> <value> <option>...
# <name> get <parameter> <option>...
# <name> obey <command> <args> <option>...
# <name> syncreply <command> <path> <messid>
# <name> cancel <command> <args> <option>...
# <name> control <command> <args> <option>...
# <name> paramreply <reply-token> <reply>
# <name> path
#
# where:
#	<parameter>:== adam parameter name
#	<command>:== adam command
#	<args>:== adam command arguments
#       <options>:== option/value pairs. The options recognised are:
#                      -inform
#                      -getresponse
#                      -actstart
#                      -setresponse
#                      -endmsg
#                      -paramreq
#                      -sync
#                      -trigger
#                    and the value is a tcl command to be executed when a
#                    message of the corresponding type arrives from the task.
#                    Before the command is executed, the following tokens are
#                    replaced by text taken from the ADAM message:
#
#                      %C    Context
#                      %T    Task name
#                      %N    Message name
#                      %P    Path
#                      %M    Mess id
#                      %S    Status
#                      %V    Value
#                      %R    Reply token (%P %M)
#                    and in paramreq messages
#                      %n    parameter name
#                      %p    prompt string
#                      %d    default value
#                      %h    help text
#                      %e    error text
#
# The global array adamtask_priv is used as follows:
#
# (PIPE)           : The file descriptor of the pipe to the message relay.
# (RELAY_PATH)     : The path to the relay task
# (RELAY_MESSID)   : The message id of the transaction with the relay task.
# (PID,<name>)     : The pid of an adam task.
# (<path>,<messid>,<type>) : The command to be executed when a message of
#                     type <type> arrives.
#
#  History:
#     5-MAY-1999 (DSB):
#        adamtask.exit modified to re-instate the original exit command
#        before doing anything else. This avoids recursion if anything
#        goes wrong within adamtask.exit.

proc adamtask.init {} {
#+
#  Starts the message relay process.
#-

# See if we already have a path to the message relay.
    global adamtask_priv
    if {[array names adamtask_priv RELAY_NAME] != ""} {
	if {[adam_path $adamtask_priv(RELAY_NAME)] == 1} return
    }

# Initialise the Adam message system using our tcl interpreter name as our
# message system name. This should be unique (except perhaps if we are using
# more than one X display.
    adam_start [winfo name .]

# Get the name of the message relay script. This is either the value of
# the environment variable ADAM_MESSAGE_RELAY or the file "adamMessageRelay"
# stored in the same directory as this file (adamtask.tcl).
    global startcl_library
    if {[catch {set env(ADAM_MESSAGE_RELAY)} relayName] != 0} {
	set relayName $startcl_library/adamMessageRelay
    }

# Create the message relay process.
    set adamtask_priv(PIPE) [open "|$relayName \"[winfo name .]\"" r]

# Wait for the initial OBEY from the relay.
    set message [adam_receive]

# Save the path and messid in the global array.
    set adamtask_priv(RELAY_NAME) [lindex $message 1]
    set adamtask_priv(RELAY_PATH) [lindex $message 3]
    set adamtask_priv(RELAY_MESSID) [lindex $message 4]

# Set up a routine to be called whenever the relay writes to the pipe.
    fileevent $adamtask_priv(PIPE) readable adamtask.read

# Reply to the obey.
    adam_reply $adamtask_priv(RELAY_PATH) $adamtask_priv(RELAY_MESSID) \
	ACTSTART [lindex $message 1] ""

# Replace the normal "exit" command with "adamtask.exit" to ensure that the
# message system gets closed down properly.
    rename exit adamtask.realExit
    proc exit {{n 0}} {adamtask.exit $n}

# Ensure that closing the application from the window manager exits cleanly.
    wm protocol . WM_DELETE_WINDOW exit

# Ensure that destroy . window exits cleanly.
    bind . <Destroy> exit
}

proc adamtask.read {} {
#+
# Called whenever the relay process writes to its standard output.
#-
    upvar #0 adamtask_priv priv

# Read a whatever is in the pipe.
    set message [gets $priv(PIPE)]

#    Call the message processing procedure with the complete message.
    if {[catch {adamtask.message $message} error] != 0 } {
	bgerror $error
    }
}

proc adamtask.message {message} {
#+
# Processes messages from the message relay.
#-
    upvar #0 adamtask_priv priv

# Extract the path, message id and value from the message.
    set path [lindex $message 3]
    set messid [lindex $message 4]
    set command [lindex $message 0]

# Extract all the fields in the message into the appropriate elements of
# the substitution array.
    set subst(%) %
    set subst(C) [lindex $message 0]
    set subst(T) [lindex $message 1]
    set subst(N) [lindex $message 2]
    set subst(P) [lindex $message 3]
    set subst(M) [lindex $message 4]
    set subst(S) [lindex $message 5]
    set subst(V) [lindex $message 6]
    set subst(R) [list $subst(P) $subst(M)]
    if { $command == "paramreq" } {
	set subst(n) [lindex $subst(V) 0]
	set subst(p) [lindex $subst(V) 1]
	set subst(d) [lindex $subst(V) 2]
	set subst(h) [lindex $subst(V) 3]
	set subst(x) [lindex $subst(V) 4]
    }

# Perform the subsitution. The first element of $priv($path,$messid,$command)
# contains the command to be executed (after substitution of % tokens).
    set proc [adamtask.subst [lindex $priv($path,$messid,$command) 0] subst]

# if the message ended a transaction then remove all the global array
# elements that correspond to the transaction.
    if { ($command == "endmsg") || ($command == "getresponse") \
	|| ($command == "setresponse") } {
	set priv($path,$messid,setresponse) \
	    [lreplace $priv($path,$messid,setresponse) 0 0]
	set priv($path,$messid,inform) \
	    [lreplace $priv($path,$messid,inform) 0 0]
	set priv($path,$messid,paramreq) \
	    [lreplace $priv($path,$messid,paramreq) 0 0]
	set priv($path,$messid,actstart) \
	    [lreplace $priv($path,$messid,actstart) 0 0]
	set priv($path,$messid,endmsg) \
	    [lreplace $priv($path,$messid,endmsg) 0 0]
	set priv($path,$messid,trigger) \
	    [lreplace $priv($path,$messid,trigger) 0 0]
	set priv($path,$messid,sync) \
	    [lreplace $priv($path,$messid,sync) 0 0]
    }

# Execute the command in global scope.
    uplevel #0 "eval \{$proc\}"
}


proc adamtask {taskname {execfile ""}} {
#+
# exec's an adam task (if specified) and creates a new tcl command with the
# name of the task. The pid of the new process is stored in
# adamtask_priv(PID,<taskname>) if it is created by this procedure.
#-
    upvar #0 adamtask_priv priv
    if { "$execfile" != ""} {

	global env

#     Set the type of any adam tasks started to "I".
	if {[info exists env(ADAM_TASK_TYPE)]} {
	    set adam_task_type $env(ADAM_TASK_TYPE)
	    set adam_task_type_set 1
	} {
	    set adam_task_type_set 0
	}
	set env(ADAM_TASK_TYPE) I

#     Set message system name of task.
	if {[info exists env(ICL_TASK_NAME)]} {
	    set icl_task_name $env(ICL_TASK_NAME)
	    set icl_task_name_set 1
	} {
	    set icl_task_name_set 0
	}
	set env(ICL_TASK_NAME) $taskname

#     Exec the task.
	set priv(PID,$taskname) [exec $execfile &]

#     Restore state of environment variables.
	if {$adam_task_type_set} {
	    set env(ADAM_TASK_TYPE) $adam_task_type
	} {
	    unset env(ADAM_TASK_TYPE)
	}
	if {$icl_task_name_set} {
	    set env(ICL_TASK_NAME) $icl_task_name
	} {
	    unset env(ICL_TASK_NAME)
	}
    } {
	set priv(PID,$taskname) ""
    }

# Create task procedure.
    proc $taskname {command args} "
	return \[adamtask.\$command \{$taskname\} \$args\]
    "
}

proc adamtask.send {command} {
#+
# Sends a command to be executed by the relay process and returns the
# result.
#-
    upvar #0 adamtask_priv priv
    adam_reply $priv(RELAY_PATH) $priv(RELAY_MESSID) SYNC "" $command
    set message [adam_getreply 10000 $priv(RELAY_PATH) $priv(RELAY_MESSID)]
    if {[lindex $message 2] == "TCL_OK"} {
	return [lindex $message 6]
    } {
	error "Error in message relay: [lindex $message 6]"
    }
}

proc adamtask.set {task arglist} {
#+
# Implements the set command.
#-
    set param [lindex $arglist 0]
    set val [lindex $arglist 1]
    set result [adamtask.send [list adam_send $task $param SET $val]]
    adamtask.options $result $arglist
}

proc adamtask.get {task arglist} {
#+
# implements the get command
#-
    set param [lindex $arglist 0]
    set result [adamtask.send [list adam_send $task $param GET {}]]
    adamtask.options $result $arglist
}

proc adamtask.obey {task arglist} {
#+
# Implements the obey command.
#-
    set command [lindex $arglist 0]
    set params [lindex $arglist 1]
    set result [adamtask.send [list adam_send $task $command OBEY $params]]
    adamtask.options $result $arglist
}

proc adamtask.paramreply {task arglist} {
#+
# Sends a reply to a paramreq message.
#-
    set reptok [lindex $arglist 0]
    set path [lindex $reptok 0]
    set messid [lindex $reptok 1]
    set reply [lindex $arglist 1]
    set result [adamtask.send \
	[list adam_reply $path $messid PARAMREP {} $reply]]
}

proc adamtask.syncreply {task arglist} {
#+
# Calls "update" and sends a reply to a sync message.
#-
    update
    set reptok [lindex $arglist 0]
    set path [lindex $reptok 0]
    set messid [lindex $reptok 1]
    set result [adamtask.send "adam_reply $path $messid SYNCREP {} {}"]
}

proc adamtask.control {task arglist} {
#+
# Implements the control command.
#-
    set command [lindex $arglist 0]
    set params [lindex $arglist 1]
    set result [adamtask.send [list adam_send $task $command CONTROL $params]]
    adamtask.options $result $arglist
}

proc adamtask.cancel {task arglist} {
#+
# Implements the cancel command.
#-
    set command [lindex $arglist 0]
    set params [lindex $arglist 1]
    set result [adamtask.send [list adam_send $task $command CANCEL $params]]
    adamtask.options $result $arglist
}

proc adamtask.kill {task arglist} {
#+
# Kills a task (if started by this tk application) and removes the task
# procedure.
#-
    upvar #0 adamtask_priv priv
    if { "$priv(PID,$task)" != ""} {
	set result [adamtask.send [list exec kill -TERM $priv(PID,$task)]]
    } {
	error "attached tasks cannot be killed"
    }
    unset priv(PID,$task)
    rename $task ""
}

proc adamtask.path {task arglist} {
#+
# Executes an "adam path" command in the relay and returns the result
#-
    set result [adamtask.send [list adam_path $task]]
    return $result
}

proc adamtask.exit {{n 0}} {
#+
# Kill all the tasks we know about and cause the relay process to exit
# then call adam_exit in order to shut down the message system.
#-

# First re-instate the original exit command to avoid calling this procedure
# recirsively if anything goes wrong within it.
    rename exit {}
    rename adamtask.realExit exit

    upvar #0 adamtask_priv priv
    set elements [array names priv "PID,*"]
    foreach task $elements {
	regsub "PID," $task  "" taskname
        $taskname kill
    }
    fileevent $priv(PIPE) readable ""
    adam_reply $priv(RELAY_PATH) $priv(RELAY_MESSID) SYNC "" exit
    exit $n
}

proc adamtask.forget {task arglist} {
#+
# Remove a task from the task list so that adamtask.exit doesn't kill it.
#-
    upvar #0 adamtask_priv priv
    unset priv(PID,$task)
    rename $task ""
}

proc adamtask.options {transaction arglist} {
#+
#  Processes an adamtask command options list and sets the appropriate
#  elements in the global array. This associates the commands to be executed
#  when an adam message arrives from the task with the path and message id
#  of the transaction.
#-
    upvar #0 adamtask_priv priv

    set path [lindex $transaction 0]
    set messid [lindex $transaction 1]
    lappend priv($path,$messid,inform) "adamtask.showMessage %T %V"
    lappend priv($path,$messid,actstart) ";"
    lappend priv($path,$messid,getresponse) ";"
    lappend priv($path,$messid,setresponse) ";"
    lappend priv($path,$messid,controlresponse) ";"
    lappend priv($path,$messid,paramreq) "adamtask.prompt %T %R %n %p %d"
    lappend priv($path,$messid,endmsg) ";"
    lappend priv($path,$messid,trigger) ";"
    lappend priv($path,$messid,sync) "%T syncreply %R"

    set end [llength $priv($path,$messid,inform)]
    incr end -1
    for {set i 0} { $i < [llength $arglist]} {incr i} {
	switch -- [lindex $arglist $i] {
	    -inform {
		incr i
		set priv($path,$messid,inform) \
		    [lreplace $priv($path,$messid,inform) $end $end \
		    [lindex $arglist $i]]
		}
	    -getresponse {
		incr i
		set priv($path,$messid,getresponse) \
		    [lreplace $priv($path,$messid,getresponse) $end $end \
		    [lindex $arglist $i]]
		}
	    -actstart {
		incr i
		set priv($path,$messid,actstart) \
		    [lreplace $priv($path,$messid,actstart) $end $end \
		    [lindex $arglist $i]]
		}
	    -setresponse {
		incr i
		set priv($path,$messid,setresponse) \
		    [lreplace $priv($path,$messid,setresponse) $end $end \
		    [lindex $arglist $i]]
		}
	    -controlresponse {
		incr i
		set priv($path,$messid,controlresponse) \
		    [lreplace $priv($path,$messid,controlresponse) $end $end \
		    [lindex $arglist $i]]
		}
	    -endmsg {
		incr i
		set priv($path,$messid,endmsg) \
		    [lreplace $priv($path,$messid,endmsg) $end $end \
		    [lindex $arglist $i]]
		}
	    -paramreq {
		incr i
		set priv($path,$messid,paramreq) \
		    [lreplace $priv($path,$messid,paramreq) $end $end \
		    [lindex $arglist $i]]
		}
	    -sync {
		incr i
		set priv($path,$messid,sync) \
		    [lreplace $priv($path,$messid,sync) $end $end \
		    [lindex $arglist $i]]
		}
	    -trigger {
		incr i
		set priv($path,$messid,trigger) \
		    [lreplace priv($path,$messid,trigger) $end $end \
		    [lindex $arglist $i]]
		}
	}
    }
}

proc adamtask.subst {string subst} {
#+
#  Replaces tokens in string with the translation stored in the array "subst".
#-
    upvar $subst sub
    set output ""
    set l [string length $string]
    for {set i 0} { $i < $l} {incr i} {
	set next [string index $string $i]
	if {[string compare "%" $next] == 0} {
	    incr i
	    catch {set output $output[list $sub([string index $string $i])]}
	} {
	    set output $output$next
	}
    }
    return $output
}

proc adamtask.showMessage {task message} {
#+
# Displays any information messages from a task in a scrolling text widget.
#-

# If the popup window doesn't exist, create it.
    if ![winfo exists .${task}_messages] {
	toplevel .${task}_messages
	wm title .${task}_messages "Messages from ADAM task: ${task}"
	text .${task}_messages.text -state disabled -wrap word \
	    -yscroll ".${task}_messages.scroll set" -bd 2 \
	    -relief sunken
	scrollbar .${task}_messages.scroll -orient vertical \
	    -command ".${task}_messages.text yview" -bd 2 \
	    -relief sunken
	pack .${task}_messages.scroll .${task}_messages.text \
	    -side right -fill y
	wm minsize .${task}_messages 0 0
	wm transient .${task}_messages .
    }

# Insert the messsage text at the end of the text widget.
    .${task}_messages.text configure -state normal
    .${task}_messages.text insert end "$message\n"
    .${task}_messages.text configure -state disabled
    .${task}_messages.text see end
}

proc adamtask.prompt {task reptok param string default} {
#+
# Handles a prompt request from a task.
#-
# Create dialog box to display the prompt and receive the user's reply.
    toplevel .${task}_prompt
    wm title .${task}_prompt "Parameter prompt"
    wm transient .${task}_prompt .

# Create the dialog box layout.
    label .${task}_prompt.label -bd 5 -text \
        "$task requires a value for $param"
    label .${task}_prompt.prompt -text $string -bd 5
    entry .${task}_prompt.entry -bd 2 -relief sunken
    .${task}_prompt.entry insert end $default
    button .${task}_prompt.ok -text OK -command "
        $task paramreply \{$reptok\} \[.${task}_prompt.entry get\]
        destroy .${task}_prompt
    "
    pack .${task}_prompt.label
    pack .${task}_prompt.ok -side bottom -pady 10
    pack .${task}_prompt.prompt .${task}_prompt.entry -side left
}

# Initialise the adamtask system as a side effect of sourcing this script.

adamtask.init
