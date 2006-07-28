# threadmgr.tcl
#	Wrappers around basic thread commands
#
#	"Thread" is the C-based Tcl extension
#	"threadmgr" is the TclHttpd thread manager
#
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::threadmgr 1.0

# The "Thread" package is implemented by a C extension.
# We let the main .rc script do the appropriate package
# require, and then fall back to the testthread command if necessary.

# Default is no threading until Thread_Init is called

if {![info exist Thread(enable)]} {
    set Thread(enable) 0
}

# Thread_Init
#
#	Initialize the thread dispatcher
#
# Arguments
#	max	Maximum number of threads to create.
#
# Side Effects
#	Initializes variables used by the thread dispatcher

proc Thread_Init {{max 4}} {
    global Thread
    package require Thread 2.0
    set Thread(maxthreads) $max	;# Number of threads we can create
    set Thread(threadlist) {}	;# List of threads we have created
    set Thread(freelist) {}	;# List of available threads
    set Thread(deletelist) {}	;# List of running threads to delete when done
    set Thread(queue) {}	;# List of queued requests, ?socket? cmd
    set Thread(enable) 1
}

# Thread_IsMaster
#
#	Find out if this thread is the master.
#
# Arguments
#	none
#
# Results
#	Returns true iff this thread is the master.

proc Thread_IsMaster {} {
    return [expr {![info exists ::Thread_MasterId]}]
}

# Thread_Enabled
#
#	Find out if the threading system is turned on
#
# Arguments
#	none
#
# Results
#	none

proc Thread_Enabled {} {
    global Thread
    return $Thread(enable)
}

# Thread_List
#
#	Return the list of threads
#
# Arguments
#	none
#
# Results
#	a list

proc Thread_List {} {
    global Thread
    if {$Thread(enable)} {
        return [thread::names]
    }
    return ""
}


# Thread_Start --
#	This starts a worker thread.  The big pain here is that a 
#	virgin thread has none of our Tcl scripts, so we have to 
#	bootstrap into a useful state.
#
# Arguments:
#	None
#
# Results:
#	The ID of the created thread

proc Thread_Start {} {
    global auto_path tcl_library
    set id [Thread_Create] 
    Thread_Send $id \
	{Stderr "Thread  starting."}

    # Set up auto_loading.  These steps may become redundent once the
    # TclpSetLibraryPath code works correctly in threads.

    Thread_Send $id \
	[list set tcl_library $tcl_library]
    Thread_Send $id \
	{source $tcl_library/init.tcl}
    Thread_Send $id \
	[list set auto_path $auto_path]

    # Suck up all the necessary packages
    # Most state comes from the initialization in the config file.
    # There is just a bit of info in the Httpd array that is set up
    # when the main server is started, which we need (e.g., the name)

    global Httpd Config
    Thread_Send $id \
	[list array set Httpd [array get Httpd]]
    Thread_Send $id \
	[list array set Config [array get Config]]

    # Let the new thread know its master's Id.  Only slave threads have
    # the "Thread_MasterId" global varaible set.
    Thread_Send $id [list set Thread_MasterId [Thread_Id]]

    Thread_Send $id [list source $Config(main)]

    return $id
}

# Thread_Dispatch --
#	This dispatches the HTTP request to a worker thread.
#	That thread should use Thread_Respond or raise an error
#	to complete the request.
#
# Arguments:
#	sock	Client connection
#	cmd	Command to invoke in a worker
#
# Side Effects:
#	Allocate a thread or queue the command/sock for later execution

proc Thread_Dispatch {sock cmd} {
    global Thread Url
    upvar #0 Httpd$sock data
    if {!$Thread(enable) || $Thread(maxthreads) == 0} {
	eval $cmd
    } else {
	if {[llength $Thread(freelist)] == 0} {
	    if {[llength $Thread(threadlist)] < $Thread(maxthreads)} {

		# Add a thread to the free pool

		set id [Thread_Start]
		lappend Thread(threadlist) $id
		lappend Thread(freelist) $id
	    } else {
		
		# Queue the request until a thread is available

		lappend Thread(queue) [list $sock $cmd]
		Count threadqueue
		return
	    }
	}
	set id [lindex $Thread(freelist) 0]
	set Thread(freelist) [lrange $Thread(freelist) 1 end]
	set data(master_thread) [Thread_Id]
	
	# Until we can pass sockets, read the post data here

	if {[info exist Url(postlength)]} {
	    while {$Url(postlength) > 0} {
		set Url(postlength) [Httpd_GetPostData $sock data(query)]
	    }
	    unset Url(postlength)
	}
	Thread_SendAsync $id [list Thread_Invoke $sock [array get data] $cmd]
    }
}

# Thread_Dispatch_General --
#	This dispatches the command to a worker thread.
#	That thread should use Thread_Respond_General or raise an error
#	to complete the request.
#
# Arguments:
#	cmd	Command to invoke in a worker
#
# Side Effects:
#	Allocate a thread or queue the command/sock for later execution

proc Thread_Dispatch_General {cmd} {
    global Thread Url
    if {!$Thread(enable) || $Thread(maxthreads) == 0} {
	eval $cmd
    } else {
	if {[llength $Thread(freelist)] == 0} {
	    if {[llength $Thread(threadlist)] < $Thread(maxthreads)} {

		# Add a thread to the free pool

		set id [Thread_Start]
		lappend Thread(threadlist) $id
		lappend Thread(freelist) $id
	    } else {
		
		# Queue the request until a thread is available

		lappend Thread(queue) [list $cmd]
		Count threadqueue
		return
	    }
	}
	set id [lindex $Thread(freelist) 0]
	set Thread(freelist) [lrange $Thread(freelist) 1 end]
	
	Thread_SendAsync $id [list Thread_Invoke_General $cmd]
    }
}

# Thread_Invoke --
#	This is invoked in a worker thread to handle an HTTP request.
#
# Arguments:
#	sock	The name of the socket connection.  Probably is not
#		an actual I/O socket.
#	datalist The contents of the connection state in array get format
#	cmd	Tcl command to eval in this thread
#
# Results:
#	None

proc Thread_Invoke {sock datalist cmd} {
    upvar #0 Httpd$sock data
    if {[info exist data]} {
	unset data
    }
    Count urlhits
    array set data $datalist

    # Make sure the thread knows which socket it's currently processing.
    # This is needed for redirects from within doc templates.
    set ::Httpd(currentSocket) $sock

    if {[catch $cmd result]} {
	global errorInfo errorCode
	Count errors
	Thread_Respond $sock [list Url_Unwind $sock $errorInfo $errorCode]
    } else {
	return $result
    }
}

# Thread_Invoke_General --
#	This is invoked in a worker thread to handle general eval request.
#
# Arguments:
#	cmd	Tcl command to eval in this thread
#
# Results:
#	None

proc Thread_Invoke_General {cmd} {
    if {[catch $cmd result]} {
	global errorInfo errorCode
	Count errors

        # An error occurred, so log it.
        Log {} bgerror "Error with code $errorCode in thread [Thread_Id]:\n$errorInfo"
    }
    
    Thread_MasterEvalAsync [list Thread_Free [Thread_Id]]
    return
}

# Thread_Respond --
#	This is invoked in a worker thread to respond to a request
#
# Arguments:
#	sock	Client connection
#	cmd	Command to invoke to complete the request
#
# Results:
#	1 if the request was passed to the master thread, else 0

proc Thread_Respond {sock cmd} {
    upvar #0 Httpd$sock data
    if {[info exist data(master_thread)] && 
	    $data(master_thread) != [Thread_Id]} {

	# Pass request back to the master thread
	# This includes a copy of the Httpd state (e.g., cookies)

	Thread_SendAsync $data(master_thread) [list Thread_Unwind \
		 [Thread_Id] $sock [array get data] $cmd]

        # The next iteration of the thread should not have access to
        # past connection data.
	unset data
	return 1
    } else {
	return 0
    }

}

# Thread_Unwind --
#	Invoke a response handler for a thread that handled an HTTP request.
#	This cleans up the connection in the main thread.
#
# Arguments:
#	id	The ID of the worker thread.
#	sock	The ID of the socket connection
#	datalist name, value list for the Httpd state array
#	cmd	The command to eval in the main thread
#
# Side Effects:
#	Invoke the response handler.

proc Thread_Unwind {id sock datalist cmd} {
    upvar #0 Httpd$sock data
    array set data $datalist
    if {[catch $cmd err]} {
	global errorCode errorInfo
	Url_Unwind $sock $errorInfo $errorCode
    }
    Thread_Free $id
}

# Thread_Free --
#	Mark a thread as available
#
# Arguments:
#	id	The ID of the worker thread.
#
# Side Effects:
#	Reap the thread, put it on the freelist, or perhaps handle
#	a queued request.

proc Thread_Free {id} {
    global Thread

    if {[ldelete Thread(deletelist) $id]} {
        # This thread was marked for deletion, so tell it to exit.
        Thread_SendAsync $t thread::exit
        ldelete Thread(threadlist) $id
    } elseif {[llength $Thread(queue)] > 0} {
	set state [lindex $Thread(queue) 0]
	set Thread(queue) [lrange $Thread(queue) 1 end]
        if {[llength $state] == 1} {
            # No socket was given, so just eval the command in the thread.
            set cmd [lindex $state 0]
            Thread_SendAsync $id [list Thread_Invoke_General $cmd]
        } else {
            set sock [lindex $state 0]
            set cmd [lindex $state 1]
            upvar #0 Httpd$sock data
            set data(master_thread) [Thread_Id]
            Thread_SendAsync $id [list Thread_Invoke $sock [array get data] $cmd]
        }
    } else {
	lappend Thread(freelist) $id
    }
}

# Thread_Create --
#	thread create
#
# Arguments:
#	Optional startup script
#
# Results:
#	The ID of the created thread

proc Thread_Create {{script {}}} {
    Count threads
    if {[string length $script]} {
	return [thread::create $script]
    } else {
	return [thread::create]
    }
}

# Thread_Send --
#	thread send
#
# Arguments:
#	id	Target thread
#	script	Script to send
#
# Results:
#	The results of the script

proc Thread_Send {id script} {
    if {[catch {thread::send $id $script} result]} {
	return -code error $result
    } else {
	return $result
    }
}

# Thread_MasterEval
#
#	Evaluate the given script synchronously in the master thread.
#
# Arguments
#	script	The script to evaluate.
#
# Results
#	Returns the result of evaluating the script in the master.

proc Thread_MasterEval {script} {
    if {[info exists ::Thread_MasterId]} {
        return [Thread_Send $::Thread_MasterId $script]
    } else {
        return [eval $script]
    }
}

# Thread_SendAsync --
#	thread send -async
#
# Arguments:
#	id	Target thread
#	script	Script to send
#
# Results:
#	The results of the script

proc Thread_SendAsync {id script} {
    if {[catch {thread::send -async $id $script} result]} {
	return -code error $result
    } else {
	return $result
    }
}

# Thread_MasterEvalAsync
#
#	Evaluate the given script asynchronously in the master thread.
#
# Arguments
#	script	The script to evaluate.
#
# Results
#	none.

proc Thread_MasterEvalAsync {script} {
    if {[info exists ::Thread_MasterId]} {
        return [Thread_SendAsync $::Thread_MasterId $script]
    } else {
        after 0 [list eval $script]
        return
    }
}

# Thread_Id --
#	thread::id
#
# Arguments:
#	none
#
# Results:
#	The thread ID

proc Thread_Id {} {
    thread::id
}

# Thread_IsFree --
#	Find out if a thread is on the free list.
#
# Arguments:
#	id	The thread ID
#
# Results:
#	1 or 0

proc Thread_IsFree {id} {
    global Thread
    return [expr {[lsearch $Thread(freelist) $id] <= 0}]
}

# Thread_ReapAll --
#	The master thread should reap all free threads and mark all busy
#	threads for deletion upon completion of their task.
#
# Arguments:
#	None.
#
# Results:
#	None.

proc Thread_ReapAll {} {
    if {![Thread_IsMaster]} {
        # This proc should only be called by the master thread.
        return
    }

    global Thread
    set Thread(deletelist) {}

    foreach id $Thread(threadlist) {
        if {[lsearch $Thread(freelist) $id] != -1} {
            # Free threads are told to exit and removed from the freelist.
            Thread_SendAsync $id thread::exit
        } else {
            # Busy threads are marked for deletion upon completion of their task.
            lappend Thread(deletelist) $id
        }
    }

    set Thread(threadlist) $Thread(deletelist)
    set Thread(freelist) {}
    return
}

# Thread_SendAll --
#	The master thread serially sends this script to all free threads,
#	including itself.
#
# Arguments:
#	cmd	The script to send to all threads.
#
# Results:
#	Returns the list of alternating thread id and result, or {} if
#	this thread is not the master.

proc Thread_SendAll {cmd} {
    if {![Thread_IsMaster]} {
        # This proc should only be called by the master thread.
        return {}
    }

    global Thread
    foreach id $Thread(threadlist) {
        if {[lsearch $Thread(freelist) $id] != -1} {
            # Send the same script to all free threads.
            lappend result $id [Thread_Send $id $cmd]
        }
    }
    # Also eval the script in this thread.
    catch {eval $cmd} res
    lappend result [thread::id] $res
    return $result
}

