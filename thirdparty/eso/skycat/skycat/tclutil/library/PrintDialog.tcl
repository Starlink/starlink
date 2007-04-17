# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: PrintDialog.tcl,v 1.1.1.1 2006/01/12 16:40:41 abrighto Exp $"
#
# PrintDialog.tcl - Base class of Popup dialogs for specifying printer options
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created

itk::usual PrintDialog {}

# PrintDialog is the base class of Popup dialogs for specifying printer options.

itcl::class util::PrintDialog {
    inherit util::TopLevelWidget


    # constructor: create a PrintDialog window

    constructor {args} {
	eval itk_initialize $args
    }


    # this method is called after all options have been evaluated

    protected method init {} {
	wm title $w_ "Printer Options"

	# frame for printer name entry
	itk_component add top {
	    frame $w_.top -borderwidth 3 -relief groove
	}
	pack $itk_component(top) \
	    -side top -fill x -expand 1

	# LabelEntry for print command
	itk_component add printcmd {
	    util::LabelEntry $w_.printcmd -text "Print Command:" \
		  -command [code $this ok]
	}
	pack $itk_component(printcmd) \
	    -side left -expand 1 -fill x -in $itk_component(top) 

	# frame for misc other items added by sublasses (child-site...)
	itk_component add config {
	    frame $w_.config
	}
	pack $itk_component(config) \
	    -side top -fill both -expand 1

	# frame for print to file entry
	itk_component add ptf {
	    frame $w_.ptf -borderwidth 3 -relief groove
	}
	pack $itk_component(ptf) \
	    -side top -fill x -expand 1

	# print to file entry (CheckEntry(n))
	itk_component add print_to_file {
	    util::CheckEntry $w_.print_to_file -text "Print to File:" \
		 -command [code $this ok]
	}
	pack $itk_component(print_to_file) \
	    -side left -expand 1 -fill x -in $itk_component(ptf)

	# Row of buttons (ButtonFrame(n))
	itk_component add btns {
	    util::ButtonFrame $w_.btns \
		  -relief raised \
		  -borderwidth 2 \
		  -ok_label "Print" \
		  -ok_cmd [code $this ok] \
		  -cancel_cmd [code $this quit]
	}
	pack $itk_component(btns) \
	    -side bottom -fill x -expand 1 -ipady 1m

	#$w_.btns append {Save} [code $this save]

	if {"$itk_option(-userfile)" == ""} {
	    config -userfile [utilGetConfigFilename $this]
	}
	if {[file exists $itk_option(-userfile)]} {
	    load
	}

	$itk_component(printcmd) config -value $itk_option(-printcmd)

	if {$itk_option(-print_to_file)} {
	    $itk_component(print_to_file) config -value $itk_option(-filename)
	}
    }


    # this method is called when the save button is pressed 
    # to save the print configuration for this user in a file 
    # for later loading

    protected method save {} {
	set fd [open $itk_option(-userfile) w]
	puts $fd $magic_
	puts $fd [list $itk_option(-printcmd ) \
		      $itk_option(-print_to_file) $itk_option(-filename)]
	close $fd
    }

    
    # load the user config file (created by save) if one exists,
    # otherwise set the default values. The file is searched for
    # in $HOME and the current dir.

    protected method load {} {
	# read the file
	set fd [open $itk_option(-userfile)]

	if {"[gets $fd]" != "$magic_"} {
	    return
	} 
	lassign [gets $fd] printcmd print_to_file filename
	config \
	    -printcmd $printcmd \
	    -print_to_file $print_to_file \
	    -filename $filename
	
	close $fd
    }

    # This method checks if a file can be used for writing.
    # Returns the 'globbed' filename if so otherwise an 
    # empty string

    protected method file_writable {file} {
	set status [catch {file isdir $file} msg]
	if {$status} {
	    error_dialog $msg $w_
	    return ""
	}
	if {$msg} {
	    error_dialog "$file is a directory !" $w_
	    return ""
	}
	set file [get_pathname $file]
	if {[file isfile $file]} {
	    if {![file writable $file]} {
		error_dialog "File $file is not writable !"
		return ""
	    }
	    if {![confirm_dialog "File $file exists - Do you want to overwrite it ?" $w_]} {
		return ""
	    }
	}
	return $file
    }


    # This method is called when the OK button is pressed
    # Open the file or pipe and pass the fd to the subclass print
    # method.

    protected method ok {args} {
	set printcmd [$itk_component(printcmd) get]
	set file [$itk_component(print_to_file) get]

        if {"$file" != ""} {
	    if {"[set file [file_writable $file]]" == ""} {
		return
	    }
	    set fd [open $file w]
	} else {
	    set fd [open "|$printcmd" w]
	}
	busy {
	    print $fd
	}
	catch {close $fd}
	
	# make the window go away
	quit
    }

    
    # should be redefined in subclass

    protected method print {fd} {
	puts "nothing to print..."
    }


    # return 'globbed' pathname for a file. If the extension is
    # not set in $file then $itk_option(-suffix) is used.

    protected method get_pathname {file} {
	set dirname [lindex [file dirname $file] 0]
	if {[catch {glob $dirname} dirname]} {
	    return $file
	}
	set basename [file split $file]
	set basename [lindex $basename [expr {[llength $basename] -1}]]
	set ext [file extension $file]
	if {"$ext" == ""} {
	    set file [file join $dirname $basename$itk_option(-suffix)]
	} else {
	    set file [file join $dirname $basename]
	}
	if {[catch {glob $file} gfile]} {
	    return $file
	}
	return $gfile
    }

    # -- options --

    # user defaults file (without path, $HOME will be added)
    itk_option define -userfile userfile Userfile {}

    # default print command
    itk_option define -printcmd printcmd Printcmd {lpr}

    # bool: print to file ?
    itk_option define -print_to_file print_to_file Print_to_file {0}

    # file name for print to file
    itk_option define -filename filename Filename {}

    # default suffix for print to file
    itk_option define -suffix suffix Suffix {.ps}


    # -- protected variable vars --

    # header string for userfile for comparison
    protected variable magic_ {# print settings v1.0}

}

