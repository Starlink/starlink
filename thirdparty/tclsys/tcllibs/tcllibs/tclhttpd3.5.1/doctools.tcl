# doctools.tcl
#
# tclllib doctools support

package provide httpd::doctools 1.0

if {[catch {package require doctools}]} {
    # tcllib's doctools package must be available
    return
}

# register the man suffix as a doctool application
set Doctools(suffix) .man
Mtype_Add $Doctools(suffix) application/x-doctool

# Doc_application/x-doctool --
#
# use doctools to format up and return an html document
#
# Arguments:
#	path	The file pathname.
#	suffix	The URL suffix.
#	sock	The socket connection.
#
# Results:
#	None
#
# Side Effects:
#	Sets up the interpreter context and runs doctools over the page
#	if necessary, to generate a cached version which is returned to the client.

proc Doc_application/x-doctool {path suffix sock} {

    # allow selection of doctool output types
    DoctoolsQuery $sock
    set format [ncgi::value format]
    if {$format == ""} {
	set format html
    }

    # check if a cached version exists and is newer
    if {[file exists ${path}.$format]} {
	set mtime [file mtime ${path}.$format]
	if {$mtime > [file mtime $path]} {
	    return [Httpd_ReturnFile $sock text/$format ${path}.$format]
	}
    }

    # generate the requested format
    set fd [open $path]
    ::doctools::new doctool -format $format
    if {[catch {doctool format [read $fd]} result]} {
    }
    doctool destroy
    close $fd

    # write the cached version
    if {![catch {set fd [open ${path}.$format w]}]} {
	puts $fd $result
	close $fd
	# return the file
	return [Httpd_ReturnFile $sock text/$format ${path}.$format]
    } else {
	# can't cache - return as data
	Httpd_ReturnData $sock text/$format $result
    }
}

# this should be a global facility somewhere
proc DoctoolsQuery {sock} {
    upvar #0 Httpd$sock data

    if {[Httpd_PostDataSize $sock] > 0 && ![info exists data(query)]} {
	set data(query) {}
    }
    if {[info exist data(query)]} {
	if {![info exist data(mime,content-type)] || $data(proto) == "GET"} {
	    
	    # The check against GET is because IE 5 has the following bug.
	    # If it does a POST with content-type multipart/form-data and
	    # keep-alive reuses the connection for a subsequent GET request,
	    # then the GET request erroneously has a content-type header
	    # that is a copy of the one from the previous POST!

	    set type application/x-www-urlencoded
	} else {
	    set type $data(mime,content-type)
	}

	# Read and append the pending post data to data(query).

	Url_ReadPost $sock data(query)

	# Initialize the Standard Tcl Library ncgi package so its
	# ncgi::value can be used to get the data.
	ncgi::reset $data(query) $type
	ncgi::parse
	ncgi::urlStub $data(url)
    }
}
