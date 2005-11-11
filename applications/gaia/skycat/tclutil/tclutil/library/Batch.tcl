# E.S.O. - VLT project/ESO Archive
# @(#) $Id: Batch.tcl,v 1.2 2005/02/02 01:43:02 brighton Exp $
#
# Batch.tcl - Class to evaluate Tcl commands in a separate process.  
#
# usage:      Batch $w_.batch -command $command
#             $w_.batch bg_eval $cmd
#           
# where:      $command is evaluated with 2 args: $cmd's status and the result 
#             (or a filename containing the result, see the options...)
#
# See man page for a complete description.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 20 Dec 95   created

itk::usual Batch {}

# This class is used to evaluate a set of Tcl commands in a separate
# process, so that it can be interrupted. No exec is done, so the 
# process is just a copy of the current one and has a copy of the
# memory and open files. 

itcl::class util::Batch {
    # dummy inherit, just to catch Destroy event for clean up...
    inherit util::FrameWidget

    # constructor

    constructor {args} {
	eval itk_initialize $args

	if {"$itk_option(-tmpfile)" == ""} {
	    set itk_option(-tmpfile) /tmp/batch[pid].[incr count_]
	}
    }

    
    # destructor
    
    destructor {
	interrupt
    }


    # evaluate the given tcl commands in the background, so that they
    # can be interrupted.  The option $command is evaluated
    # when the results when done.

    public method bg_eval {cmd} {
	if {$itk_option(-debug)} {
	    fg_eval $cmd
	    return
	}
	flush stdout
	flush stderr
	pipe rfd wfd
	set pid [fork]
	if {$pid == 0} {
	    # child
	    set status [catch $cmd result]
	    set fd [open $itk_option(-tmpfile) w]
	    puts -nonewline $fd $result
	    close $fd
	    puts $wfd $status
	    close $wfd
	    kill [pid]
	} else {
	    set bg_pid_ $pid
	    fileevent $rfd readable [code $this read_pipe $rfd $wfd]
	}
    }


    # evaluate the given command in the foreground (useful for debugging)
    # The option $command is evaluated when the results when done.

    public method fg_eval {cmd} {
	set status [catch $cmd result]
	eval $itk_option(-command) $status [list $result]
    }


    # read the pipe from the child process
    
    protected method read_pipe {rfd wfd} {
	set status [read $rfd 1]
	close $rfd
	close $wfd
	
	if {[file exists $itk_option(-tmpfile)]} {
	    set fd [open $itk_option(-tmpfile)]
	    set info [read $fd]
	    close $fd
	    file delete $itk_option(-tmpfile)
	} else {
	    # may have been interrupted - ignore
	    return
	}
	eval $itk_option(-command) $status [list $info]
	catch {wait -nohang $bg_pid_}
    }


    # interrupt the current search 

    public method interrupt {} {
	catch {exec kill $bg_pid_}
	catch {file delete $itk_option(-tmpfile)}
	catch {wait -nohang $bg_pid_}
   }

    
    # -- options --

    # command to evaluate when process is done. Called with 2 args:
    # status and result (or error message)
    itk_option define -command command Command {}

    # temp file for results
    itk_option define -tmpfile tmpfile Tmpfile {}

    # flag: if true run commands in the foreground for better debugging
    itk_option define -debug debug Debug 0

    
    # -- protected members --

    # process id of background process
    protected variable bg_pid_
   
    
    # -- common variables (shared by all instances) --
    
    # count used for filename generation
    common count_ 0
}
	
