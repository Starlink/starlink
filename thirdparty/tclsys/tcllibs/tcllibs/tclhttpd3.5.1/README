This HTTPD is written in Tcl and Tk.
Written by Stephen Uhler and Brent Welch of Sun Microsystems Laboratory.
Brent is now at Panasas, Inc.
See the file "license.terms" for information on usage and redistribution
of this file, and for a DISCLAIMER OF ALL WARRANTIES.

Version: 3.5.1
Tue May 18 21:15:39 PDT 2004

STARKIT QUICK START

tclkit tclhttpd3.5.1.kit -port 80 -docRoot htmldir -library tcldir

The server loads all the tcl sources in the library.  Try also
tclkit tclhttpd3.5.1.kit -help

STANDARD QUICK START

It requires Tcl 8.3 (or higher) and the Standard Tcl Library.
This uses the cmdline, base64, ncgi, html, and counter
modules of the Standard Tcl Library. If you must, you
can probably get by with Tcl 8.0 or later if you install
the Standard Tcl Library.

The Web server runs under either tclsh or wish.
With wish you get a simple GUI that shows some counters.

For a quick test that runs a server on port 8015, do
UNIX:

1) Test preconditions
	Tcl interpreter: tclsh8.3, tclsh8.4, tclsh8.5, or tclkit (or wish)
        tcllib 1.3 or higher
Try
        tclsh (whatever version)
	package require counter

2) If you can do the above, then you should be able to run the
server directly from this distribution directory without bothering
to configure, make, and install it. e.g., 

	tclsh8.4 bin/httpd.tcl -debug 1
Windows and Macintosh:
	Run wish or tclsh and source bin/httpd.tcl

3) For more detailed installation instructions, see the INSTALL file.

For a complete set of command line options, do
httpd.tcl -help
This lets you set basic parameters like port number and server name.

VERSION SUMMARY

Version 3.5 has various bug fixes, improvements to the docs, and
some sample applications.  3.5.1 adds a mail client module.

Version 3.4.3  fixes loading code from the custom code directory,
which broke in 3.4.2

Version 3.4.2 has bug fixes in the directory listing code (dirlist.tcl).
Prevent listing of directory trees outside the URL tree, and get them to work
at all.  Added a fix in upload.tcl so that extra post data is cleaned up
correctly - fixes bugs that occur when GET and POST uploads are mixed.
Minor cleanup to the sample htdocs tree.

Version 3.4.1 fixes CGI POST forms on Windows, a typo in upload.tcl
and the lib/pkgIndex.tcl.  It also disallows the ability to retrieve
.tml, .tclaccess, and .htaccess files.  These simple new APIs crept in:
  Url_AccessInstallPrepend, Url_AccessUnInstall
  Mtype_Add, Mtype_Reset
  Direct_UrlRemove

Version 3.4 has several updates to the Doc, Upload, and Thread modules
done by Melissa Chawla of Panasas. Please see the ChangeLog for details.
There is also a small modification to support If-Modified-Since optimization
in Httpd_ReturnFile contributed by Brian Meagher.  Don Porter contributed
virtual host support, so you can support several sites with one server.
Please see README.virthost for details

Version 3.3.1 was just a cleanup of the installation files and
configure scripts. (acruz)

Version 3.3 fixes more bugs in CGI handling and
adds a simple file upload domain handler.

Version 3.2.1 fixes bugs in CGI handling related to the changes
in 3.2 in the way POST data is handled.  

Version 3.2 changes the way you customize a TclHttpd installation with
the goal of not having to modify the bin/httpd.tcl and bin/httpdthread.tcl
scripts, although in some cases this will still be necessary.  A new
"custom code" directory is supported and the server will load all the
code from that directory during startup.  Specify this with the -library
command line option or the "library" Config specification in your .rc file.

Version 3.1.0 renames all the packages to be httpd::foo instead of just
foo, so that the package namespace is a bit more organized when TclHttpd
is installed along with the rest of Tcl and Tk.  Other that that,
it is essentially identical with 3.0.4.

Version 3.0.4 fixes yet-another bug with Doc_AddRoot.

Version 3.0.3 fixes another bug with Doc_AddRoot, and fixes the
bundled distribution so you can use the Thread extension again.

Version 3.0.2 fixes a serious SECURITY BUG that let 3.0.0 and 3.0.1 
serve up any old file on your system.  That bug was not present in earlier
versions of the server.

Version 3.0.1 has minor bug fixes and (hopefully) some improvements
in the bundled distribution.

Version 3.0.0 has three major changes:
* Use of Standard Tcl Library modules.  The server looks around for the
tcllib distribution and automatically uses it.  One goal of this conversion
is to eliminate the use of the page global array and replace that with
calls to the ncgi module.
* Support for threads.  If you configure and build Tclsh with --enable-threads
and you have the Thread extension available, the the server can 
use threads to service requests.
* Support for SSL.  If you have the "tls" extension installed, then you can
start an SSL server by giving the -https_port command line argument.

Version 2.3.6 is the last in the 2.3 series.  It has a few minor
bug fixes.  The next release, 3.0, will support threading using
the Thread extension and SSL using the TLS extension.

Version 2.3 changes the way post data is  handled.
The various point releases (e.g., 2.3.4) fix important
bugs in cookie handling and the new post data handler.

Version 2.1.6 introduces a configuration file, tclhttpd.rc
You can edit this to tune the set of modules used by the server,
or to hardwire the port, etc.

If you have Scotty installed, then you can try out the SNMP demo.
You'll need to arrange to have the Tnm*.so library on the
auto_path for your shell.  I have found that 2.1.5 works
with 8.0a2, but not with 8.0b1 because Tcl_GetFile has gone.

MAILING LISTS

There is a mailing list for users of the Tcl web server at
SourceForge as part of the tclhttpd project.  Please sign
up via the web site at www.sourceforge.net/projects/tclhttpd.
If that is dead, send email to
welch@acm.org
to find out the current location of the mailing list

Send messages for the list to
tclhttpd-users@lists.sourceforge.net

Bugs and comments to the mailing list, or to <welch@acm.org>
Put "httpd" in the subject of the mail so I can sort it automatically.

WWW

The current URL for the distribution can be found in
ftp://www.tcl.tk/pub/tcl/httpd/

There is documentation at
http://www.beedub.com/tclhttpd/
http://www.tcl.tk/software/tclhttpd/

SourceForge

http://www.sourceforge.net/projects/tclhttpd
http://tclhttpd.sourceforge.net/
