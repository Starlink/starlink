#!/bin/sh
# \
exec wish "$0" "$@"

# tkxmllint --
#
#	Simple GUI for xmllint-style processing of XML documents
#
# Copyright (c) 2005 Explain
# http://www.explain.com.au/
# Copyright (c) 2003-2004 Zveno
# http://www.zveno.com/
#
# See the file "LICENSE" in this distribution for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id$

# Global initialisation

set VERSION 1.8

# Temporary hack for TclApp-wrapped executables
lappend auto_path [file dirname [info nameofexecutable]]

package require xml
package require xml::libxml2
package require dom

package require msgcat
namespace import ::msgcat::mc

package require uri 1.2

tk appname tkxmllint

# We need the code shared with tkxsltproc
source [file join [file dirname [info script]] common.tcl]

# Init --
#
#	Create the GUI
#
# Arguments:
#	win	toplevel window
#
# Results:
#	Tk widgets created

proc Init win {
    upvar \#0 State$win state

    set w [expr {$win == "." ? {} : $win}]

    array set state {
	url {}
	cwd {}
	noent 0
	nonet 0

	display:entrefs 0
    }

    wm title $win "Tk XML Lint"

    switch [tk windowingsystem] {
	aqua -
	classic {
	    set metakey Command
	    set metakeylabel Command-
	}
	default {
	    set metakey Control
	    set metakeylabel Ctrl+
	}
    }

    menu $w.menu -tearoff 0
    $win configure -menu $w.menu
    $w.menu add cascade -label [mc File] -menu $w.menu.file
    menu $w.menu.file -tearoff 1
    $w.menu.file add command -label [mc {New Window}] -command NewWindow -accel ${metakeylabel}N
    bind $win <${metakey}-n> NewWindow
    $w.menu.file add separator
    $w.menu.file add command -label [mc {Save As...}] -command [list SaveAs $win] -state disabled
    $w.menu.file add separator
    $w.menu.file add command -label [mc Quit] -command {destroy .} -accel ${metakeylabel}Q
    bind $win <${metakey}-q> {destroy .}

    $w.menu add cascade -label [mc Help] -menu $w.menu.help
    menu $w.menu.help -tearoff 0
    $w.menu.help add command -label [mc {About tkxmllint}] -command tkAboutDialog -accel ${metakeylabel}?
    # This fails on Linux
    catch {bind $win <${metakey}-?> tkAboutDialog}

    if {$::tcl_platform(platform) == "macintosh" ||
	($::tcl_platform(platform) == "unix" && $::tcl_platform(os) == "Darwin")} {
	$w.menu add cascade -label tkxmllint -menu $w.menu.apple
	menu $w.menu.apple -tearoff 0
	$w.menu.apple add command -label [mc {About tkxmllint}] -command tkAboutDialog
    }

    frame $w.controls
    grid $w.controls - -row 0 -column 0 -sticky ew
    button $w.controls.check -text [mc Check] -command [list Check $win]
    # TODO: add a nice icon
    grid $w.controls.check -row 0 -column 0 -sticky w
    grid columnconfigure $w.controls 0 -weight 1

    labelframe $w.doc -text [mc Document]
    grid $w.doc - -row 1 -column 0 -sticky ew
    label $w.doc.url -text [mc URL:]
    entry $w.doc.urlentry -width 60 -textvariable State${win}(url)
    button $w.doc.browse -text [mc Browse] -command [list Browse $win url -title {Select XML Document}]
    grid $w.doc.url -row 0 -column 0 -sticky w
    grid $w.doc.urlentry -row 0 -column 1 -sticky ew
    grid $w.doc.browse -row 0 -column 2 -sticky e
    grid columnconfigure $w.doc 1 -weight 1

    labelframe $w.options -text [mc Options]
    grid $w.options - -row 2 -column 0 -sticky news
    checkbutton $w.options.noout -text [mc {Display document}] -variable State${win}(display)
    checkbutton $w.options.timing -text [mc {Display timing}] -variable State${win}(timing)
    checkbutton $w.options.xinclude -text [mc XInclude] -variable State${win}(xinclude)
    checkbutton $w.options.noent -text [mc {Substitute entities}] -variable State${win}(noent)
    checkbutton $w.options.logentities -text [mc {Log entities}] -variable State${win}(display:entrefs)
    checkbutton $w.options.nonet -text [mc {No network}] -variable State${win}(nonet)
    menubutton $w.options.encode -text [mc Encoding...] -menu $w.options.encode.menu
    set m [menu $w.options.encode.menu -tearoff 0]
    $m add radiobutton -label [mc utf-8] -variable ::State${win}(encoding) -value utf-8
    $m add radiobutton -label [mc ascii] -variable ::State${win}(encoding) -value ascii
    entry $w.options.encoding -textvariable ::State${win}(encoding)
    set state(encoding) utf-8
    grid $w.options.noout -row 0 -column 0 -sticky w
    grid $w.options.timing -row 1 -column 0 -sticky w
    grid $w.options.logentities -row 2 -column 0 -sticky w
    grid $w.options.xinclude -row 0 -column 1 -sticky w
    grid $w.options.noent -row 1 -column 1 -sticky w
    grid $w.options.nonet -row 2 -column 1 -sticky w
    grid $w.options.encode -row 0 -column 2 -sticky w
    grid $w.options.encoding -row 0 -column 3 -sticky ew
    grid columnconfigure $w.options 3 -weight 1

    labelframe $w.validation -text [mc Validation]
    grid $w.validation - -row 3 -column 0 -sticky news
    radiobutton $w.validation.none -text [mc none] -variable State${win}(validate) -value no
    radiobutton $w.validation.dtd -text [mc DTD] -variable State${win}(validate) -value dtd
    radiobutton $w.validation.wxs -text [mc WXS] -variable State${win}(validate) -value wxs
    labelframe $w.validation.doc -text [mc {Schema Document}]
    label $w.validation.doc.url -text [mc URL:]
    entry $w.validation.doc.urlentry -width 40 -textvariable State${win}(schemaurl)
    button $w.validation.doc.browse -text [mc Browse] -command [list Browse $win schemaurl -title {Select Schema Document}]
    grid $w.validation.doc.url -row 0 -column 0 -sticky w
    grid $w.validation.doc.urlentry -row 0 -column 1 -sticky ew
    grid $w.validation.doc.browse -row 0 -column 2 -sticky e
    grid columnconfigure $w.validation.doc 1 -weight 1
    set state(validate) no
    grid $w.validation.none -row 0 -column 0 -sticky w
    grid $w.validation.dtd -row 0 -column 1 -sticky w
    grid $w.validation.wxs -row 0 -column 2 -sticky w
    grid $w.validation.doc - - - -row 1 -sticky ew
    grid columnconfigure $w.validation 2 -weight 1

    set state(messages) [labelframe $w.messages -text [mc Messages]]
    grid $w.messages - -row 4 -column 0 -sticky news
    text $w.messages.log -wrap none \
	-state disabled \
	-xscrollcommand [list $w.messages.xscroll set] \
	-yscrollcommand [list $w.messages.yscroll set]
    scrollbar $w.messages.xscroll -orient horizontal \
	-command [list $w.messages.log xview]
    scrollbar $w.messages.yscroll -orient vertical \
	-command [list $w.messages.log yview]
    grid $w.messages.log -row 0 -column 0 -sticky news
    grid $w.messages.yscroll -row 0 -column 1 -sticky ns
    grid $w.messages.xscroll -row 1 -column 0 -sticky ew
    grid rowconfigure $w.messages 0 -weight 1
    grid columnconfigure $w.messages 0 -weight 1

    SetProperties $win $w.messages.log

    frame $w.feedback
    grid $w.feedback - -row 5 -column 0 -sticky ew
    label $w.feedback.msg -textvariable State${win}(feedback)
    set state(progress) [canvas $w.feedback.progress \
			     -width 100 -height 25 -relief sunken]
    set state(progressbar) [$w.feedback.progress create rectangle 0 0 1 25 \
				-fill blue -disabledfill white -state disabled]
    grid $w.feedback.progress -row 0 -column 1
    grid $w.feedback.msg -row 0 -column 0 -sticky ew
    grid columnconfigure $w.feedback 0 -weight 1

    grid rowconfigure $win 3 -weight 1
    grid columnconfigure $win 1 -weight 1

    return {}
}

# tkAboutDialog --
#
#	Information about this application
#
# Arguments:
#	None
#
# Results:
#	Displays window

proc tkAboutDialog {} {
    catch {destroy .about}
    toplevel .about
    catch {::tk::unsupported::MacWindowStyle style .about floatProc}
    wm title .about [mc {About tkxmllint}]
    label .about.libxml2logo -image libxml2Logo
    label .about.tcllogo -image tclLogo
    text .about.msg -width 40 -height 10 -font Arial
    .about.msg insert end [mc [format "tkxmllint - A GUI for xmllint

Version %s

Powered by:
\tlibxml2\tv%s
\tTclXML\tv%s
\tTcl/Tk\tv%s

http://tclxml.sourceforge.net/tkxmllint.html
" $::VERSION $::xml::libxml2::libxml2version \
	[package require xml] [info patchlevel]]]

    .about.msg configure -state disabled

    grid .about.libxml2logo -row 0 -column 2 -sticky news
    grid .about.tcllogo -row 1 -column 2 -sticky news
    grid .about.msg -row 0 -column 1 -rowspan 2 -sticky news -padx 20 -pady 20
    grid rowconfigure .about 0 -weight 1
    grid rowconfigure .about 1 -weight 1
    grid columnconfigure .about 1 -weight 1

    return {}
}

# SaveAs --
#
#	Save document into a file
#
# Arguments:
#	win	toplevel
#
# Results:
#	File written

proc SaveAs {win} {
    upvar \#0 State$win state

    if {![info exists state(dom)]} {
	tk_messageBox -parent $win -title [mc {No Document}] \
	    -message [mc {No document to save}] -type ok
	return {}
    }

    set w [expr {$win == "." ? {} : $win}]

    set cwd [pwd]
    if {$state(cwd) != {}} {
	set cwd $state(cwd)
    }

    set fname [tk_getSaveFile -parent $win -title [mc {Save As...}] -initialdir $cwd]

    if {![string length $fname]} {
	return {}
    }

    set state(cwd) [file dirname $fname]

    if {[catch {open $fname w} ch]} {
	tk_messageBox -parent $win -icon error -type ok -message "Unable to open \"[file nativename $fname]\" for writing"
	return {}
    }

    fconfigure $ch -encoding $state(encoding)
    puts $ch [dom::serialize $state(dom) -encoding $state(encoding)]
    close $ch

    return {}
}

# NewWindow --
#
#	Create another toplevel window
#
# Arguments:
#	None
#
# Results:
#	Tk toplevel created and initialised

proc NewWindow {} {
    global counter

    Init [toplevel .top[Incr counter]]

    return {}
}

# Check --
#
#	Parse the given document and display report
#
# Arguments:
#	win	toplevel window
#
# Results:
#	Document read into memory, parsed and report displayed

proc Check win {
    upvar \#0 State$win state

    set w [expr {$win == "." ? {} : $win}]

    set fname [GetFilename $win $w.doc.urlentry url]
    if {![string length $fname]} {
	return
    }

    set dodisplay $state(display)

    Log clear $win
    $w.menu.file entryconfigure [mc {Save As...}] -state disabled

    catch {
	dom::destroy $state(dom)
	unset state(dom)
    }

    set time(start) [clock clicks -milliseconds]

    if {[catch {ReadAndParseXML $win [mc "source"] $fname $state(url) time \
		    -noent $state(noent) \
		-nonet $state(nonet)} doc]} {
	return
    }
    $w.menu.file entryconfigure [mc {Save As...}] -state normal

    switch -- $state(validate) {
	dtd {
	    Feedback $win [mc "Validating document"]
	    if {[catch {$doc dtd validate} msg]} {
		Log addXMLError $win [dom::serialize $doc] $msg
		Feedback $win [mc "Document is not valid"]
		set dodisplay 0
	    } else {
		Log add $win $msg
	    }
	    set time(validate) [clock clicks -milliseconds]
	    Log timing $win "Validation took [expr $time(validate) - $time(last)]ms\n"
	    set time(last) $time(validate)
	}
	wxs {
	    Feedback $win [mc "Schema-validating document"]
	    set schemafname [GetFilename $win $w.validation.doc.urlentry schemaurl]
	    if {[string length $schemafname]} {
		set schematime(start) $time(last)
		if {[catch {ReadAndParseXML $win [mc "schema"] $schemafname ? schematime} schemadoc]} {
		    # continue
		} else {
		    set time(last) $schematime(last)
		    Feedback $win [mc "Preparing schema"]
		    if {[catch {$schemadoc schema compile} msg]} {
			Log add $win $msg
			Feedback $win [mc "Preparing schema failed"]
			set time(schemacompile) [clock clicks -milliseconds]
			set time(last) $time(schemacompile)
			set dodisplay 0
		    } else {
			set time(schemacompile) [clock clicks -milliseconds]
			Log timing $win "Preparing schema took [expr $time(schemacompile) - $time(last)]ms\n"
			set time(last) $time(schemacompile)

			Feedback $win [mc "Schema-validating document"]
			if {[catch {$schemadoc schema validate $doc} msg]} {
			    Log addXMLError $win [dom::serialize $doc] $msg
			    Feedback $win [mc "Document is not schema-valid"]
			    set dodisplay 0
			} else {
			    Log add $win $msg
			    Feedback $win [mc "Document is schema-valid"]
			}
			set time(schemavalidate) [clock clicks -milliseconds]
			Log timing $win "Schema validation took [expr $time(schemavalidate) - $time(last)]ms\n"
			set time(last) $time(schemavalidate)
		    }

		    # TODO: cache the compiled schema
		    dom::destroy $schemadoc
		}
	    }
	}
    }

    if {$dodisplay} {
	Log add $win [dom::serialize $doc -encoding $state(encoding)]
	set time(serialize) [clock clicks -milliseconds]
	Log timing $win "Displaying document took [expr $time(serialize) - $time(last)]ms\n"
	set time(last) $time(serialize)
    }

    Feedback $win [mc "Processing completed"]
    after 2000 [list Feedback $win {}]

    # We no longer destroy the document, since the user may wish to save it
    set state(dom) $doc

    Log timing $win "Total time: [expr $time(last) - $time(start)]ms\n"

    Log view $win start

    return {}
}

### Image data - end of script

image create photo libxml2Logo -data {
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
dkAAADs=
}
image create photo tclLogo -data {
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
5V+g+JBhTvOaGyEIADs=
}

Init .
