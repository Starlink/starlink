# status.tcl --
#
#	Application-direct URLs to give out status of the server.
# 	Tcl procedures of the form Status/hello implement URLS
#	of the form /status/hello
#
# Brent Welch (c) Copyright 1997 Sun Microsystems, Inc.
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::status 1.0

proc Status_Url {dir {imgdir /images}} {
    global _status
    set _status(dir) $dir
    set _status(images) $imgdir
    Direct_Url $dir Status
}

# Status/hello --
#
#	Show the string "hello".
#
# Arguments:
#	args	arguments are ignored.
#
# Results:
#	Returns the string "hello".

proc Status/hello {args} {return hello}

# StatusHeader --
#
#       Standard HTML header for status pages.
#
# Arguments:
#       title - also used as <h1>
#
# Results:
#       Html for head and <body>

proc StatusHeader {title} {
    return "<html><head>
    <title>$title</title>
    </head>
    <body bgcolor=white text=black>
    <h1>$title</h1>
"
}

# Status/threads --
#
#	Show the list of threads running in the server.
#
# Arguments:
#	args	arguments are ignored.
#
# Results:
#	Returns the list of threads running in the server.

proc Status/threads {args} {
    append html [StatusHeader "Thread List"]
    append html [StatusMenu]\n
    if {[catch {Thread_List} x]} {
	append html "No thread support\n"
    } else {
	append html $x
    }
    return $html
}

proc StatusSortForm {action label {pattern *} {sort number}} {
    if {[string compare $sort "number"] == 0} {
	set numcheck checked
	set namecheck ""
    } else {
	set numcheck ""
	set namecheck checked
    }
    append result "<form action=$action>"
    append result "Pattern <input type=text name=pattern value=$pattern><br>"
    append result "Sort by $label <input type=radio name=sort value=number $numcheck> or Name <input type=radio name=sort value=name $namecheck><br>"
    append result "<input type=submit name=submit value=\"Again\">"
    append result "</form>"
}

proc StatusMenu {} {
    global _status
    set sep ""
    set html "<p>\n"
    foreach {url label} [list \
	/		"Home" \
	$_status(dir)/	"Graphical Status" \
	$_status(dir)/text	"Text Status" \
	$_status(dir)/domain	"Domains" \
	$_status(dir)/doc	"Documents" \
	$_status(dir)/notfound	"Not Found" \
	$_status(dir)/threads	"Threads" \
	$_status(dir)/size	"Memory Size" \
    ] {
	append html "$sep<a href=$url>$label</a>\n"
	set sep " | "
    }
    append html "</p>\n"
    return $html
}

# Status/doc --
#
#	Show the number of hits for documents matching the pattern.
#
# Arguments:
#	pattern	(optional) the glob pattern of the URL to report on.
#	sort	(optional) how to sort the output.  If the default "number" is
#		not given, output is sorted by url alphabetically.
#
# Results:
#	Returns HTML code that displays document hit info.

proc Status/doc {{pattern *} {sort number}} {
    global _status
    set result ""
    append result [StatusHeader "Document Hits"]
    append result [StatusMenu]
    append result [StatusSortForm $_status(dir)/doc "Hit Count" $pattern $sort]
    append result [StatusPrintArray [counter::get hit -histVar] * $sort Hits Url]
}

# Status/domain --
#
#	Show the number of hits for documents in different domains.
#
# Arguments:
#	pattern	(optional) the glob pattern of the domain to report on.
#	sort	(optional) how to sort the output.  If the default "number" is
#		not given, output is sorted by url alphabetically.
#
# Results:
#	Returns HTML code that displays document hit info.

proc Status/domain {{pattern *} {sort number}} {
    global _status
    set result ""
    append result [StatusHeader "Domain Hits"]
    append result [StatusMenu]
    append result [StatusSortForm $_status(dir)/domain "Hit Count" $pattern $sort]
    append result [StatusPrintArray [counter::get domainHit -histVar] * $sort Hits Domain]
}

proc StatusPrintArray {aname pattern sort col1 col2} {
    upvar #0 $aname a
    set result ""
    append result <pre>\n
    append result [format "%6s %s\n" $col1 $col2]
    set list {}
    set total 0
    foreach name [lsort [array names a $pattern]] {
	set value $a($name)
	lappend list [list $value $name]
	incr total $value
    }
    if {[string compare $sort "number"] == 0} {
	if [catch {lsort -index 0 -integer -decreasing $list} newlist] {
	    set newlist [lsort -command StatusSort $list]
	}
    } else {
	if [catch {lsort -index 1 -integer -decreasing $list} newlist] {
	    set newlist [lsort -command StatusSortName $list]
	}
    }
    append result [format "%6d %s\n" $total Total]
    foreach k $newlist {
	set url [lindex $k 1]
	append result [format "%6d %s\n" [lindex $k 0] $url]
    }
    append result </pre>\n
    return $result
}
proc StatusSort {a b} {
    set 1 [lindex $a 0]
    set 2 [lindex $b 0]
    if {$1 == $2} {
	return [string compare $a $b]
    } elseif {$1 < $2} {
	return 1
    } else {
	return -1
    }
}
proc StatusSortName {a b} {
    set 1 [lindex $a 1]
    set 2 [lindex $b 1]
    return [string compare $1 $2]
}

# Status/notfound --
#
#	Show the number of hits for nonexistent documents matching the pattern.
#
# Arguments:
#	pattern	(optional) the glob pattern of the URL to report on.
#	sort	(optional) how to sort the output.  If the default "number" is
#		not given, output is sorted by url alphabetically.
#
# Results:
#	Returns HTML code that displays document hit info for docs that were
#	not found.

proc Status/notfound {{pattern *} {sort number}} {
    global Doc Referer _status
    set result ""
    append result [StatusHeader "Documents Not Found"]
    append result [StatusMenu]
    append result [StatusSortForm $_status(dir)/notfound "Hit Count" $pattern $sort]
    append result [StatusPrintNotFound $pattern $sort]
}

proc StatusPrintNotFound {{pattern *} {sort number}} {
    global Doc Referer _status
    upvar #0 [counter::get notfound -histVar] histogram
    append result <pre>\n
    append result [format "%6s %s\n" Miss Url]
    set list {}
    foreach i [lsort [array names histogram $pattern]] {
	lappend list [list $histogram($i) $i]
    }
    if [catch {lsort -index 0 -integer -decreasing $list} newlist] {
	set newlist [lsort -command StatusSort $list]
    }
    foreach k $newlist {
	set url [lindex $k 1]
	append result [format "%6d <a href=%s>%s</a>\n" \
	    [lindex $k 0] [lindex $k 1] [lindex $k 1]]
	if {[info exists Referer($url)]} {
	    set i 0
	    append result <ul>
	    foreach r $Referer($url) {
		append result "<li> <a href=\"$r\">$r</a>\n"
	    }
	    append result </ul>\n
	}
    }
    append result </pre>\n
#    append result "<a href=$_status(dir)/notfound/reset>Reset counters</a>"
    return $result
}

# Status/notfound/reset --
#
#	Reset the number of hits for nonexistent documents to 0.
#
# Arguments:
#	args	arguments are ignored.
#
# Results:
#	Returns HTML code that confirms that the reset occurred.

proc Status/notfound/reset {args} {
    global Referer
    counter::reset notfound
    catch {unset Referer}
    return [StatusHeader "Reset Notfound Counters"]
}

# Status/size --
#
#	Show both the code size and data size.
#
# Arguments:
#	args	arguments are ignored.
#
# Results:
#	Returns HTML.

proc Status/size {args} {
    global StatusDataSize StatusCodeSize
    append top_html [StatusHeader "Memory Size"]
    append top_html [StatusMenu]\n
    append html [Status/datasize]\n
    append html [Status/codesize]\n
    append top_html "<h2>Grand Total</h3>"
    append top_html "Bytes [expr $StatusDataSize(bytes) + $StatusCodeSize(bytes)]"
    return $top_html$html
}

# Status/datasize --
#
#	Show the data size for the entire server or for a particular namespace.
#
# Arguments:
#	ns	(optional) if given, show the data size for this namespace
#
# Results:
#	Returns HTML.

proc Status/datasize {{ns ::}} {
    global StatusDataSize
    array set StatusDataSize {
	vars	0
	values	0
	bytes	0
    }
    append html [StatusHeader "Data Size"]
    append html [StatusDataSize $ns]
    append top_html "<h2>Total Data Size</h2>\n
	Num Variables $StatusDataSize(vars)<br>\n\
	Num Values $StatusDataSize(values)<br>\n\
	Data Bytes $StatusDataSize(bytes)"
    return $top_html$html
}
proc StatusDataSize {ns} {
    global StatusDataSize
    set ng 0
    set nv 0
    set size 0
    foreach g [info vars ${ns}::*] {
	incr ng
	if [array exists $g] {
	    foreach {name value} [array get $g] {
		set size [expr {$size + [string length $name] + [string length $value]}]
		incr nv
	    }
	} elseif {[info exist $g]} {
	    # info vars returns declared but undefined namespace vars
	    set size [expr {$size + [string length $g] + [string length [set $g]]}]
	    incr nv
	}
    }
    set html "<h3>$ns</h3>\n\
		Num Variables $ng<br>\n\
		Num Values $nv<br>\n\
		Data Bytes $size"
    incr StatusDataSize(vars) $ng
    incr StatusDataSize(values) $nv
    incr StatusDataSize(bytes) $size

    append html "<ul>"
    foreach child [namespace children $ns] {
	append html [StatusDataSize $child]
    }
    append html "</ul>"
    return $html
}

# Status/codesize --
#
#	Show the code size for the entire server or for a particular namespace.
#
# Arguments:
#	ns	(optional) if given, show the code size for this namespace
#
# Results:
#	Returns HTML.

proc Status/codesize {{ns ::}} {
    global StatusCodeSize
    array set StatusCodeSize {
	procs	0
	bytes	0
    }
    set html [StatusHeader "Code Size"]
    append html [StatusCodeSize $ns]
    append top_html "<h2>Total Code Size</h2>\n
	Num Procs $StatusCodeSize(procs)<br>\n\
	Code Bytes $StatusCodeSize(bytes)"
    return $top_html$html
}
proc StatusCodeSize {{ns ::}} {
    global StatusCodeSize
    set np 0
    set size 0
    foreach g [info procs ${ns}::*] {
	incr np
	set size [expr {$size + [string length $g] +
			    [string length [info args $g]] +
			    [string length [info body $g]]}]
    }
    set html "<h3>$ns</h3>\n\
		Num Procs $np<br>\n\
		Code Bytes $size"
    incr StatusCodeSize(procs) $np
    incr StatusCodeSize(bytes) $size
    append html "<ul>"
    foreach child [namespace children $ns] {
	append html [StatusCodeSize $child]
    }
    append html "</ul>"
}

# StatusMainTable --
#
#	Display the main status counters. This gathers information
#	from worker threads, if possible.
#
# Arguments
#	None
#
# Results
#	Html

proc StatusMainTable {} {
    global Httpd Doc status tcl_patchLevel tcl_platform Thread
    global StatusThreadTable

    set html "<H1>$Httpd(name):$Httpd(port)</h1>\n"
    append html "<H2>Server Info</h2>"
    append html "<table border=0>"
    append html "<tr><td>Start Time</td><td>[clock format [Counter_StartTime]]</td></tr>\n"
    append html "<tr><td>Current Time</td><td>[clock format [clock seconds]]</td></tr>\n"
    append html "<tr><td>Server</td><td>$Httpd(server)</td></tr>\n"
    append html "<tr><td>Tcl Version</td><td>$tcl_patchLevel</td></tr>"
    switch $tcl_platform(platform) {
	unix {
	    append html "<tr><td colspan=2>[exec uname -a]</td></tr>"
	}
	macintosh -
	windows {
	    append html "<tr><td colspan=2>$tcl_platform(os) $tcl_platform(osVersion)</td></tr>"
	}
    }

    append html </table>

    append html "<br><br><br>\n"

    append html "<p>[StatusTable]<p>\n"

    # Per thread stats
    if {[Thread_Enabled]} {
        set self [Thread_Id]
	foreach id [lsort -integer [Thread_List]] {
	    if {$id == $self} {
		continue
	    }
	    append html "<h4>Thread $id</h4>\n"
	    global counter_thread_$id
	    if {[Thread_IsFree $id]} {
		set StatusThreadTable($id) \
		    [Thread_Send $id StatusTable]
	    } else {
		# Use cached version of the other threads counters,
		# but update our stats for next time.
		append html "<i>busy, using cached values</i>\n"
		Thread_SendAsync $id [list StatusThreadUpdate $id $self]
	    }
	    if {[info exist StatusThreadTable($id)]} {
		append html $StatusThreadTable($id)
	    }
	}
    }
    return $html
}

proc StatusThreadUpdate {self master} {
    Thread_SendAsync $master [list array set ::counter_thread_$self \
	[array get ::counter]]
}

proc StatusTable {} {
    append html "<table bgcolor=#eeeeee>\n"

    set hit 0
    foreach {c label} {
	    / "Home Page Hits"
	    } {
	set N [counter::get hit -hist $c]
	if {$N > 0} {
	    append html "<tr><td>$label</td><td>$N</td>\n"
	    set hit 1
	    append html </tr>\n
	}
    }
    foreach {c label} {
	    urlhits "URL Requests"
	    Url_Dispatch "URL Dispatch"
	    UrlToThread "Thread Dispatch"
	    UrlEval "Direct Dispatch"
	    UrlCacheHit "UrlCache eval"
	    urlreply "URL Replies"
	    accepts "Total Connections"
	    accept_https "HTTPS Connections"
	    keepalive "KeepAlive Requests"
	    http1.0 "OneShot Connections"
	    http1.1 "Http1.1 Connections"
	    threads "Worker Threads"
	    sockets "Open Sockets"
	    cgihits "CGI Hits"
	    tclhits "Tcl Safe-CGIO Hits"
	    maphits "Image Map Hits"
	    cancel	"Timeouts"
	    notfound	"Not Found"
	    errors	"Errors"
	    Status	"Status"
	    } {
	if {[counter::exists $c]} {
	    set t [counter::get $c -total]
	    if {$t > 0} {
		append html "<tr><td>$label</td><td>$t</td>\n"
		set hit 1
		set resetDate [counter::get $c -resetDate]
		if {[string length $resetDate]} {
		    append html "<td>[clock format $resetDate -format "%B %d, %Y"]</td>"
		}
		append html </tr>\n
	    }
	}
    }
    if {!$hit} {
	foreach c [counter::get "" -allTagNames] {
	    append html "<tr><td>$name</td><td>[counter::get $c -total]</td>\n"
	    set resetDate [counter::get $c -resetDate]
	    if {[string length $resetDate]} {
		append html "<td>[clock format $resetDate -format "%B %d, %Y"]</td>"
	    }
	    append html </tr>\n
	}
    }
    append html </table>\n

    return $html
}

proc StatusTclPower {{align left}} {
    global _status
    set html "<img src=$_status(images)/pwrdLogo150.gif align=$align width=97 height=150>\n"
}

# Status/all --
#
#	Show the page hist per minute, hour, and day using images.
#
# Arguments:
#	args	arguments are ignored
#
# Results:
#	Returns HTML.

proc Status/all {args} {
    global _status page
    set html [StatusHeader "Tcl HTTPD Status"]
    append html [StatusMenu]
    append html [StatusTclPower left]
    append html [StatusMainTable]
    append html "<br><a href=/status/text>Text only view.</a>\n"

    append html "<p>\n<table border=0 cellpadding=0 cellspacing=0>\n"
    append html [counter::histHtmlDisplayRow serviceTime \
	    -title "Service Time" -unit seconds \
	    -width 1 -skip 10 -min 0 -max 400 \
	    -images $_status(images)]

    append html [counter::histHtmlDisplayRow urlhits \
	    -title "Url Hits" -unit minutes \
	    -min 0 -max 60 \
	    -images $_status(images)]
    append html [counter::histHtmlDisplayRow urlhits \
	    -title "Url Hits" -unit hours \
	    -min 0 -max 24 \
	    -images $_status(images)]
    append html [counter::histHtmlDisplayRow urlhits \
	    -title "Url Hits" -unit days \
	    -images $_status(images)]
    
    append html </table>
    return $html
}

# Status/text --
#
#	Show the page hist per minute, hour, and day in text format.
#
# Arguments:
#	args	arguments are ignored
#
# Results:
#	Returns HTML.

proc Status/text {args} {
    global _status
    set html [StatusHeader "Tcl HTTPD Status"]
    append html [StatusMenu]
    append html [StatusTclPower left]
    append html [StatusMainTable]
    append html "<p><a href=$_status(dir)/all>Bar Chart View.</a>"
    catch {
	append html [counter::histHtmlDisplay serviceTime \
		-title "Service Time" -unit seconds -max 100 -text 1]
    }
    catch {
	append html [counter::histHtmlDisplay urlhits \
		-title "Per Minute Url Hits" -unit minutes -text 1]
	append html [counter::histHtmlDisplay urlhits \
		-title "Hourly Url Hits" -unit hours -text 1]
	append html [counter::histHtmlDisplay urlhits \
		-title "Daily Url Hits" -unit days -text 1]
    }
    return $html
}

# Status/ --
#
#	Same as Status/all.
#
# Arguments:
#	args	arguments are ignored
#
# Results:
#	Returns HTML.

proc Status/ {args} {
    eval Status/all $args
}

# Status --
#
#	Same as Status/all.
#
# Arguments:
#	args	arguments are ignored
#
# Results:
#	Returns HTML.

proc Status {args} {
    eval Status/all $args
}

proc Version {} {
    global tcl_patchLevel Httpd
    append html "$Httpd(server)"
    append html "<br>Tcl version $tcl_patchLevel"
    return $html
}

# NOTUSED

proc StatusTimeText {array title unit what time} {
    global counter
    upvar #0 $array data
    regsub ^Cnt $array Age agebitName
    upvar #0 $agebitName agebit
    set total 0
    set max 0
    set base 100
    foreach {name value} [array get data] {
	setmax max $value
    }
    switch $unit {
	Min	{set delta 60 ; set fmt %R}
	Hour	{set delta 3600 ; set fmt "%h %e %R"}
	Day	{set delta 86400 ; set fmt %D}
    }

    append result "<h3>$title ($max max)</h3>"
    append result <ul>
    append result "<h4>Starting at [clock format $time]</h4>"
    append result "<table cellpadding=2 cellspacing=2 border><tr>\n"
    append result "<tr><th>$unit</th><th>$what</th></tr>"
    foreach t [lsort -integer [array names data]] {
	set value $data($t)

	# Minutes time we infer from the starting time and the agebits,
	# which indicate minute buckets for the previous hour.

	if [info exists agebit($t)] {
	    set tt [expr $time - 3600]
	} else {
	    set tt $time
	}

	# Hours have their own base time in counter(hour,$hour)

	if {$unit == "Hour"} {
	    set tt $counter(hour,$t)
	}

	# Wrap separator

	if {[info exists lasttime] && ($lasttime > $tt)} {
	    append result "<tr><td><hr></td><td><hr></td></tr>"
	}
	set lasttime $tt
	append result "<tr><td>[clock format $tt -format $fmt]</td><td>$value</td></tr>"
	incr time $delta
    }
    append result "</table>"
    append result </ul>
    return $result
}

# Handle .stat templates. (NOTUSED)
# First process the incoming form data in an Status-specific way,
# then do a normal Subst into a safe interpreter
#   path:	The path name of the document
#   suffix:     The file suffix
#   sock:	The name of the socket, and a handle for the socket state

# It turns out this is not used, but you could use it as a template
# for your own application's template processor.

proc Doc_application/x-tcl-status {path suffix sock} {
    global status
    upvar #0 Httpd$sock data

    append data(query) ""
    set queryList [Url_DecodeQuery $data(query)]

    # Put the query data into an array.
    # If a key appears multiple times, the resultant array value will
    # be a list of all the values.

    foreach {name value} $queryList {
    	lappend query($name) $value
    }

    if ![info exists status(session)] {
	set status(session) [session_create Status]
    }

    # Process the query data from the previous page.

    if [catch {StatusProcess $session $queryList} result] {
	Httpd_ReturnData $sock text/html $result
	return
    } 

    # Expand the page in the correct session interpreter, or treat
    # the page as ordinary html if the session has ended.

    if {$result} {
	Subst_ReturnFile $sock $path interp$session
    } else {
	Httpd_ReturnFile $sock text/html $path
    }
}

if {0} {
    proc StatusPrintHits {aname {pattern *} {sort number}} {
	append result [StatusPrintArray [counter::get hit -histVar] *\
		$sort Hits Url]
    }
}

