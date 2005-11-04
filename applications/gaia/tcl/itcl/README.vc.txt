------------------------------------------------------------------------
     How to build [Incr Tcl/Tk] with Visual C++ and makefile.vc
------------------------------------------------------------------------

[The condition of this "new thing" is experimental.  All non-functional
aspects are a bug, not a feature.]

1) Open config.vc into a text editor and fill in the required information
   following the notes in the comments.  The makefile.vc files should not
   ever need editing.  If they do need editing to build Itcl, it's a bug.

2) There are a number of macros you can specify on the commandline to
   effect the build.  These are all optional.  They're contained in
   rules.vc, but here are the meanings:

	DEBUG=(0|1)
		Set to one for a symbols build.  Defaults to non-symbols
		when left out.
	STATIC_BUILD=(0|1)
		Will make a static library instead of a dll.
	NOMSVCRT=(0|1)
		Will reference libcmt(d).lib for the runtime when set to one.
		This is zero by default unless STATIC_BUILD is set to one.
	OUT_DIR=<someDir>
		You may specify where the output binaries are placed.  Defaults
		to the build directory when not specified.

Example commandlines:

C:\itcl3.2.1> nmake -f makefile.vc

	Builds Itcl and Itk full optimization and no symbols for a dll

C:\itcl3.2.1> nmake -f makefile.vc DEBUG=1 OUT_DIR=c:\progra~1\tcl\lib\itcl3.2

	Builds Itcl and Itk with debugging info for edit/continue (if vc6) and
	places both DLLs in c:\progra~1\tcl\lib\itcl3.2 .  I can see the obvious
	bug with that now <shrug>.  Darn.  You could go into each directory
	seperately instead or just replace 'makefile.vc' above with
	'itcl\win\makefile.vc' and specify OUT_DIR as you would like it.

C:\itcl3.2.1> nmake -f makefile.vc STATIC_BUILD=1

	Builds a static library of both Itcl and Itk instead of a DLL and the
	static libcmt run-time.

C:\itcl3.2.1> nmake -f makefile.vc STATIC_BUILD=1 NOMSVCRT=0

	Same as above, but uses msvcrt.  In TclPro, these were known with the 'x'
	suffix.

C:\itcl3.2.1> nmake -f makefile.vc STATIC_BUILD=0 NOMSVCRT=1

	Invalid.  Makes bloaty DLLs.

C:\itcl3.2.1> nmake -f makefile.vc install

	Installs to wherever INSTALLDIR was set in config.vc .  At the moment,
	this isn't an over-ride as the commandline macros are.  Should I make
	it an over-ride?

Enjoy...
--
David Gravereaux <davygrvy@pobox.com>
7:07 PM 5/23/2001
