# upload.tcl
#
# File upload domain.  This implements a domain handler that
# specializes in file uploading of multipart/form-data.
# It uploads files into a particular directory and enforces
# limits on the number of files, their size, etc.
# It invokes handler procedures similar to application-direct
# handlers after doing the file upload.
#
# Brent Welch (c) 2001
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::upload 1.0
package require ncgi

# Upload_Url
#	Define a subtree of the URL hierarchy that handles
#	file uploads.
#
# Arguments
#	virtual The URL prefix of the upload domain.
#	dir	The upload directory - files get placed here.	
#	command	The Tcl command to invoke after an upload.
#	args	option-value flags described below.
#		-thread <boolean>
#			If true, dispatch domain in a thread
#		-maxfiles <integer>
#			Maximum files in upload directory
#		-maxbytes <integer>
#			Maximum number of bytes in a file
#		-totalbytes <integer>
#			Limit on total bytes in all files
#			in the upload directory
#		-unique <boolean>
#			Give uploads unique names
#
# Side Effects
#	Register a prefix

proc Upload_Url {virtual dir command args} {
    global Upload
    array set opt {
	    -inThread 0
	    -maxfiles -1
	    -maxbytes -1
	    -totalbytes -1
	    -unique 0
    }
    array set opt $args
    Url_PrefixInstall $virtual [list UploadDomain $dir $command \
	$opt(-maxfiles) $opt(-maxbytes) $opt(-totalbytes) $opt(-unique)] \
	-thread $opt(-inThread) \
        -callback [list UploadTidyUp] \
	-readpost 0
}

# UploadDomain
# Main handler for Upload domains 
# This uploads the file, if it does not exceed the byte
# and file limits, then invokes the callback.
#
# Arguments
#	dir	Upload directory
#	cmd	Tcl command to call after upload
#	maxfiles	Max files per upload diretory
#	maxbytes	File byte limit
#	totalbytes	Total bytes per upload directory
#	sock	The socket back to the client
#	suffix	The part of the url after the domain prefix.
#

proc UploadDomain {dir cmd maxfiles maxbytes totalbytes unique sock suffix} {
    upvar #0 Httpd$sock data
    upvar #0 Upload$sock upload

    # Testing
    if {[string match *test* $suffix]} {
	set fd [open $dir/test w]
	fconfigure $fd -trans binary
	puts -nonewline $fd [read $sock $data(mime,content-length)]
	close $fd
	Httpd_ReturnData $sock text/html $dir/test
    }
    # Extract multi-part boundary from the headers

    if {![info exist data(mime,content-type)] || $data(proto) == "GET"} {
	set type application/x-www-urlencoded
    } else {
	set type $data(mime,content-type)
    }
    set parsedType [ncgi::parseMimeValue $type]
    if {![string match multipart/* [lindex $parsedType 0]]} {
	return -code error "Not a multipart Content-Type: [lindex $parsedType 0]"
    }
    array set options [lindex $parsedType 1]
    if {![info exists options(boundary)]} {
	return -code error "No boundary given for multipart document"
    }

    # Record upload instance data and set up a read event handler
    # so we can read large files without blocking.

    if {[info exist upload]} {
	unset upload
    }
    set upload(boundary) $options(boundary)
    set upload(dir) $dir
    set upload(cmd) $cmd
    set upload(maxfiles) $maxfiles
    set upload(totalbytes) $totalbytes
    set upload(maxbytes) $maxbytes
    set upload(suffix) $suffix
    set upload(count) $data(count)
    set upload(unique) $unique

    # These are temporary storage used when parsing the headers of each part

    set upload(headers) {}
    set upload(formName) {}
    set upload(formNames) {}

    if {$upload(maxfiles) != -1} {
	set files [glob -nocomplain -- [file join $upload(dir) *]]
	if {[llength $files] >= $upload(maxfiles)} {
	    Httpd_Error $sock 503 "Max files ($upload(maxfiles)) exceeded.<br>Unable to upload"
	    return
	}
    }

    if {$upload(maxbytes) != -1} {
	if {$upload(count) >= $upload(maxbytes)} {
	    Httpd_Error $sock 503 "File size limit ($upload(maxbytes) bytes) exceeded.<br>Unable to upload."
	    return
	}
    }

    if {$upload(totalbytes) != -1} {
	set tot $upload(count)
	foreach f [glob -nocomplain -- [file join $upload(dir) 0]] {
	    incr tot [file size $f]
	}
	if {$tot >= $upload(totalbytes)} {
	    Httpd_Error $sock 503 "Total size limit ($upload(totalbytes) bytes) exceeded.<br>Unable to upload."
	    return
	}
    }

    # Now that we are going to read the post data, clear out the
    # hook and the data count so noone else tries to read it

    Url_PostHook $sock 0
    set data(count) 0

    # Accept cr-lf endings in the headers
    fconfigure $sock -trans auto
    fileevent $sock readable [list UploadFindBoundary $sock]
}

# Look for the first boundary - should be the first line read,
# but done in a fileevent to avoid blocking.

proc UploadFindBoundary {sock} {
    upvar #0 Upload$sock upload
    if {[eof $sock]} {
	UploadDone $sock
	return
    }
    if {[gets $sock line] > 0} {
	if {[regexp ^--$upload(boundary) $line]} {
	    fileevent $sock readable [list UploadReadHeader $sock]
	} else {
	    Stderr "UploadFindBoundary Unexpected line $line"
	}
    }
}

# UploadReadHeader --
#	This is a fileeventhandler used to read the header
#	of each part in a multipart POST payload.

proc UploadReadHeader {sock} {
    upvar #0 Upload$sock upload

    if {[eof $sock]} {
	UploadDone $sock
	return
    }

    # Read through the POST data line-by-line looking for the
    # boundary and diverting file content into files in the
    # upload directory.  We read in binary mode to preserve whatever
    # line-ending mode is in the uploaded file.

    while {[gets $sock line] >= 0} {
        # Check for an empty or white-space only line.
	if {[string length [string trim $line]] == 0} {

	    # End of headers.  We should have seen a name header
	    # that is now in formName.  Keep a list of those, and 
	    # store the other headers under that key.

	    lappend upload(formNames) $upload(formName)
	    set upload(hdrs,$upload(formName)) $upload(headers)

	    # Now switch to reading the content of that part.

	    # Use binary mode so we don't corrupt the file
	    fconfigure $sock -trans binary -encoding binary

	    if {[info exist upload(fd)]} {
		fileevent $sock readable [list UploadReadFile $sock]
	    } else {
		fileevent $sock readable [list UploadReadPart $sock]
	    }
	    return
	}
	if {[regexp {([^:	 ]+):(.*)$} $line x hdrname value]} {
	    set hdrname [string tolower $hdrname]
	    set valueList [ncgi::parseMimeValue $value]
	    if {[string equal $hdrname "content-disposition"]} {

		# Promote Conent-Disposition parameters up to headers,
		# and look for the "name" that identifies the form element

		lappend upload(headers) $hdrname [lindex $valueList 0]
		foreach {n v} [lindex $valueList 1] {
		    lappend upload(headers) $n $v
		    switch -- $n {
			"name" {
			    set upload(formName) $v
			}
			"filename" {
			    # Open the upload file

			    # If the uploade file is empty string,
			    # then use a temporary name--this request
			    # will fail later.  Netscape allows you
			    # to upload dirnames like /a/b/, which
			    # end up as empty string here.
			    if {$v == ""} {
				set v "directory"
			    }

			    # I'm a bit suprised that "file tail"
			    # when run on a Unix box will not deal
			    # with c:\a\b\c.txt correctly...
			    regsub -all {\\} $v / v
			    set tail [file tail $v]
			    if {$upload(unique)} {
				# make the file a unique name
				set path [file join $upload(dir) ${tail}.[clock clicks]]
			    } else {
				set path [file join $upload(dir) $tail]
			    }

			    set upload(fd) [open $path w]
			    set upload(lastLineExists) 0

			    # always do binary transfers
			    fconfigure $upload(fd) -trans binary
			    set upload(file,$upload(formName)) $path
			}
		    }
		}
	    } else {
		lappend upload(headers) $hdrname $valueList
	    }
	}
    }

    if {[fblocked $sock]} {
	return
    }
}

# Look for the boundary at the end of a content part.

proc UploadReadPart {sock} {
    upvar #0 Upload$sock upload
    if {[eof $sock]} {
	UploadDone $sock
	return
    }
    if {[gets $sock line] > 0} {
	if {[regexp ^--$upload(boundary)(--)? $line x end]} {
	    if {$end == "--"} {
		UploadDone $sock
	    } else {
		set upload(formName) ""
		set upload(headers) ""
		fileevent $sock readable [list UploadReadHeader $sock]
	    }
	} else {
            # Trim the string to remove carriage returns.
	    append upload(data,$upload(formName)) [string trim $line]
	}
    }
}

# Read a content part and copy it to a file.
# Because the server is non-blocking, this may be called multiple times to
# process a file.

proc UploadReadFile {sock} {
    upvar #0 Upload$sock upload

    # Maximum size of the buffer, in characters, before we schedule
    # another read and update other waiting tasks.  Increasing this
    # number will probably make uploads a little faster at the expense
    # of some responsiveness on the part of other clients attempting
    # to connect to the server.
    set maxbuffersize 1000
    set buffersize 0

    if {[eof $sock]} {
	UploadDone $sock
	return
    }

    while {[gets $sock line] >= 0} {
	if {[regexp ^--$upload(boundary)(--)? $line x end]} {
	    if {$upload(lastLineExists)} {
		# At least 1 line was read.  Write the last line to the
		# file without the trailing newline character.
		puts -nonewline $upload(fd) [string range $upload(lastLine) 0 end-1]
	    }

	    set buffersize 0
	    close $upload(fd)
	    unset upload(fd)
	    if {$end == "--"} {
		UploadDone $sock
	    } else {
		set upload(formName) ""
		set upload(headers) ""
		fileevent $sock readable [list UploadReadHeader $sock]
	    }
	    return
	} else {
	    # Delay the writing of each line to make sure we don't add an
	    # extra trailing newline to the last line.
	    if {$upload(lastLineExists)} {
		puts $upload(fd) $upload(lastLine)
	    } else {
		set upload(lastLineExists) 1
	    }
	    set upload(lastLine) $line
	}
	incr buffersize [string bytelength $line]
	if { $buffersize > $maxbuffersize } {
	    set buffersize 0
	    fileevent $sock readable [list UploadReadFile $sock]
	    update idletasks
	    break
	}
    }
}

proc UploadDone {sock} {
    upvar #0 Upload$sock upload

    if {[catch {
	# The first argument is a list of file names that were uploaded
	# The second argument is a name-value list of the other data
	set flist {}
	set vlist {}
	foreach x $upload(formNames) {
	    if {[info exist upload(file,$x)]} {
		lappend flist $x $upload(file,$x)
	    } else {
		lappend vlist $x $upload(data,$x)
	    }
	}
	eval $upload(cmd) [list $flist $vlist]
    } result] == 1} {
	# handle special error codes
	set ec $::errorCode
	switch -- [lindex $ec 0] {
	    HTTPD_REDIRECT {
		Httpd_Redirect [lindex $ec 1] $sock
		return
	    }
	    HTTPD_SUSPEND {
		Httpd_Suspend $sock
		return
	    }
	}
    }

    Httpd_ReturnData $sock text/html $result
}

# This is called as the "final" completion callback to clean up

proc UploadTidyUp {sock errmsg} {
    upvar #0 Upload$sock upload

    unset upload
}

# UploadTest --
#	Sample callback procedure for an upload domain

proc UploadTest {flist vlist} {
    set html "<title>Upload Test</title>\n"
    append html "<h1>Upload Test</h1>\n"
    append html "<h2>File List</h2>\n"
    append html [html::tableFromList $flist]\n
    append html "<h2>Data List</h2>\n"
    append html [html::tableFromList $vlist]\n
    return $html
}
