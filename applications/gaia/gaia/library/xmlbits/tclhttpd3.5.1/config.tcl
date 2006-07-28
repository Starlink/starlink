# config.tcl
#
# This provides a simple module to manage configuration variables
#
# Brent Welch (c) 2000 Scriptics Corporation
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::config 1.0

namespace eval config {
    
    # The main array holding the configuration information

    variable Config

    # The file containing the configuration information

    variable ConfigFile

    namespace export cget
}

# config::init --
#
#	Get the list of resources from disk.
#
# Arguments:
#	config		The name of the configuration file (optional)
#	aname		Name of array containing default configuration
#			values on input, and the new values after
#			processing the config file.
#
# Side Effects:
#	Defines the Config parameter array and records the name of
#	the configuration file in ConfigFile

proc config::init {config aname} {
    upvar 1 $aname TheirConfig
    variable Config
    variable ConfigFile

    if {[catch {open $config} in]} {
	return -code error "Cannot read configuration file \"$config\"\nError: $in"
    }
    set X [read $in]
    close $in

    # Load the file into a safe interpreter that just creates Config array

    set i [interp create -safe]
    interp expose $i file

    # Create the slave's Config array, then source the config script

    interp eval $i [list array set Config [array get TheirConfig]]

    interp eval $i {
	proc Config {name {value {}}} {
	    global Config
	    if {![info exist Config($name)] || [string length $value]} {
		set Config($name) $value
	    } else {
		set Config($name)
	    }
	}
    }
    if {[catch {interp eval $i $X} err]} {
	return -code error "Error in configuration file \"$config\"\n:Error: $err"
    }
    array set Config [interp eval $i {array get Config}]
    interp delete $i

    set ConfigFile $config
}

# config::cget --
#
#	Get the value for an index in the Config array.
#
# Arguments:
#	index	index in Config array to get.
#
# Results:
#	Returns the value of Config for the given index, or "" if
#	there is no such index.

proc config::cget {index} {
    variable Config

    if {![info exists Config($index)]} {
	return ""
    }
    return $Config($index)
}

# config::setValue --
#
#	Set one or more values for an index(es) in the Config array, and store
#	the array's new contents in the configuration file.
#
# Arguments:
#	args	A list of index, value pairs
#
# Results:
#	None.

proc config::setValue {args} {
    variable Config
    variable ConfigFile

    if {![info exist ConfigFile]} {
	return -code error "config is not initialized"
    }
    set in "(original lost)"
    if {![file exists $ConfigFile] || [catch {open $ConfigFile} in]} {
	set note "# Emergency copy: $in\n"
	unset in
    }
    append note "# Last modified: [clock format [clock seconds]]\n"

    # Re-write config file

    if {![info exist in]} {

	# We've lost the original, just dump out the array

	if {[catch {open $ConfigFile w} out]} {
	    return -code error "Cannot create configuration file: $out"
	}
	puts $out "# Server Configuration File\n$note\n"
	foreach name [lsort [array names Config]] {
	    puts $out [list Config $name $Config($name)]\n
	}
	close $out
    } else {
	
	# Re-write the original, inserting the new values.
	# This code only supports values that fit on one line

	set X [read $in]
	close $in
	if {[catch {open $ConfigFile w} out]} {
	    return -code error "Cannot re-write configuration file: $out"
	}

	regsub "^# Last modified:.*?\n" $X $note X

	foreach {name value} $args {
	    if {[regsub -all "\n\\s*?Config\\s*?[list $name](\\s*?).*?\n" $X \
			    "\nConfig [list $name]\\1[list $value]\n" X] == 0} {
		append X "\n[list Config $name $value]\n"
	    }
	}
	puts -nonewline $out $X
	close $out
    }
    
    # Update in-memory information

    array set Config $args

    return
}
