#+
#  Name:
#     convert.tcl
#
#  Purpose:
#     Set up environment for convert for a Tcl application.
#
#  Type of Module:
#     Tcl script
#
#  Invocation:
#     setup_convert
#
#  Description:
#     This procedure sets up CONVERT for use from a Tcl script by defining the
#     environment variables needed to execute each application or command.
#     It also initialises ADAM, if this has not already been done.
#
#  Authors:
#     MJC: Malcolm J. Currie (STARLINK)
#     AJC: A.J.Chipperfield (STARLINK)
#     ALLAN: Allan Brighton (ESO)
#     {enter_new_authors_here}
#
#  History:
#     1993 August 20 (MJC):
#        Original version.
#     1993 September 5 (MJC):
#        Added IRCAM2NDF.
#     1993 November 5 (MJC):
#        Removed DIPSO2NDF and NDF2DIPSO.
#     1995 October 30 (AJC):
#        Updated to version 0.6.  Organized to be modified at install
#        time.
#     1995 November 30 (AJC):
#        Added TIFF, GIF and PGM.
#     1997 August 1 (MJC):
#        Added DA2NDF, FITS2NDF, and NDF2DA tasks.  Added automatic
#        conversions for STREAM and GZIP.  Added FITS and STREAM synonym
#        file extensions.  Some reordering of NDF_FORMATS_IN/OUT; also
#        used multi-line creation for clarity.  Corrected con_tiff2ndf
#        and con_ndf2tiff aliases.
#     1998 May 1 (ALLAN):
#        Made Tcl version from convert.csh
#     {enter_further_changes_here}
#
#  Bugs:
#     {note_any_bugs_here}
#
#-
#
#  Prepare to run ADAM applications if this has not been done already.
#  ===================================================================
#
#  Here look to see if there is ADAM_USER directory.  If there is not
#  check whether or not there is an adam file that is not a directory.
#  If there is, issue a warning and exit.  Otherwise create the required
#  directory.
#

proc setup_convert {} {
    global ::env

    if {! [file isdirectory $env(HOME)/adam]} {
	if {[file exists $env(HOME)/adam]} {
	    error "You have a file called adam in your home directory.\n\
	           Please rename it since adam must be a directory for ADAM files."
	} else {
	    exec mkdir $env(HOME)/adam
	}
    }
    
    #
    #  Define environment variables.
    #  =============================
    #
    if {![info exists env(CONVERT_DIR)]} {
	if {[info exists env(STARLINK)] && [file exists $env(STARLINK)/bin/convert/ndf2fits]} {
	    set env(CONVERT_DIR) $env(STARLINK)/bin/convert
	} else {
	    puts "warning: The environment variable CONVERT_DIR should be set to \
              \$STARLINK/bin/convert (or the install directory for the convert package)"
	    return
	}
    }
    puts "Setting up environment variables for convert..."


    #
    #  Now set the environment variables for NDF automatic conversion.
    #  ===============================================================

    #  Define input and output formats recognised.
    #  ==========================================

    #  Formats recognised when accessing pre-existing datasets.  There are
    #  numerous products in FITS each uses a different file extension.
    set env(NDF_FORMATS_IN) \
	{FITS(.fit),FIGARO(.dst),IRAF(.imh),STREAM(.das),UNFORMATTED(.unf)}

    append env(NDF_FORMATS_IN) \
	{,UNF0(.dat),ASCII(.asc),TEXT(.txt),GIF(.gif),TIFF(.tif)}

    append env(NDF_FORMATS_IN) \
	{,GASP(.hdr),COMPRESSED(.sdf.Z),GZIP(.sdf.gz),FITS(.fits)}

    append env(NDF_FORMATS_IN) \
	{,FITS(.fts),FITS(.FTS),FITS(.FITS),FITS(.FIT)}

    append env(NDF_FORMATS_IN) \
	{,FITS(.lilo),FITS(.lihi),FITS(.silo),FITS(.sihi)}

    append env(NDF_FORMATS_IN) \
         {,FITS(.mxlo),FITS(.rilo),FITS(.rihi),FITS(.vdlo)}

    append env(NDF_FORMATS_IN) \
	{,FITS(.vdhi),STREAM(.str)}

    #  Formats recognised when creating new datasets.
    set env(NDF_FORMATS_OUT) \
	{.,FITS(.fit),FIGARO(.dst),IRAF(.imh),STREAM(.das),UNFORMATTED(.unf)}

    append env(NDF_FORMATS_OUT) \
	{,UNF0(.dat),ASCII(.asc),TEXT(.txt),GIF(.gif),TIFF(.tif)}

    append env(NDF_FORMATS_OUT) \
	{,GASP(.hdr),COMPRESSED(.sdf.Z),GZIP(.sdf.gz)}

    #  Define format conversion commands.
    #  =================================

    #  FITS conversions.
    #  ----------------
    #  Because NDF2FITS uses HDS_WILD which will only accept container files 
    #  as input, NDF_TEMP_FITS must specify an alternative to the default
    #  HDS scratch file.
    if {[info exists env(HDS_SCRATCH)]} {
	set env(NDF_TEMP_FITS) "$env(HDS_SCRATCH)/temp_fits_^name"
    } else {
	set env(NDF_TEMP_FITS) "temp_fits_^name"
    }

    set env(NDF_FROM_FITS) \
	{$CONVERT_DIR/convertndf from '^fmt' '^dir' '^name' '^type' '^ndf'}

    set env(NDF_TO_FITS) \
	{$CONVERT_DIR/convertndf to '^fmt' '^dir' '^name' '^type' '^ndf'}

    #  FIGARO conversions.
    #  ------------------
    set env(NDF_FROM_FIGARO) \
	{$CONVERT_DIR/convertndf from '^fmt' '^dir' '^name' '^type' '^ndf'}

    set env(NDF_TO_FIGARO) \
	{$CONVERT_DIR/convertndf to '^fmt' '^dir' '^name' '^type' '^ndf'}

    #  IRAF conversions:
    #  ----------------
    set env(NDF_FROM_IRAF) \
	{$CONVERT_DIR/convertndf from '^fmt' '^dir' '^name' '^type' '^ndf'}

    set env(NDF_TO_IRAF) \
	{$CONVERT_DIR/convertndf to '^fmt' '^dir' '^name' '^type' '^ndf'}

    #  N.B. deletion of IRAF datasets requires a separate command, since two
    #  files are involved.
    set env(NDF_DEL_IRAF) \
	{f='^dir^name';touch "$f.imh" "$f.pix";rm "$f.imh" "$f.pix"}

    #  GASP conversions:
    #  ----------------
    set env(NDF_FROM_GASP) \
	{$CONVERT_DIR/convertndf from '^fmt' '^dir' '^name' '^type' '^ndf'}

    set env(NDF_TO_GASP) \
	{$CONVERT_DIR/convertndf to '^fmt' '^dir' '^name' '^type' '^ndf'}

    #  N.B. deletion of GASP datasets requires a separate command, since two
    #  files are involved.
    set env(NDF_DEL_GASP) \
	{f='^dir^name';touch "$f.hdr" "$f.dat";rm "$f.hdr" "$f.dat"}

    #  UNFORMATTED conversions (with FITS headers).
    #  -------------------------------------------
    set env(NDF_FROM_UNFORMATTED) \
	{$CONVERT_DIR/convertndf from '^fmt' '^dir' '^name' '^type' '^ndf'}

    set env(NDF_TO_UNFORMATTED) \
	{$CONVERT_DIR/convertndf to '^fmt' '^dir' '^name' '^type' '^ndf'}

    #  UNF0 conversions (no FITS headers).
    #  ----------------------------------
    set env(NDF_FROM_UNF0) \
	{$CONVERT_DIR/convertndf from '^fmt' '^dir' '^name' '^type' '^ndf'}

    set env(NDF_TO_UNF0) \
	{$CONVERT_DIR/convertndf to '^fmt' '^dir' '^name' '^type' '^ndf'}

    #  STREAM (direct-access) conversions
    #  ----------------------------------
    set env(NDF_FROM_STREAM) \
	{$CONVERT_DIR/convertndf from '^fmt' '^dir' '^name' '^type' '^ndf'}

    set env(NDF_TO_STREAM) \
	{$CONVERT_DIR/convertndf to '^fmt' '^dir' '^name' '^type' '^ndf'}

    #  ASCII conversions (with FITS headers).
    #  -------------------------------------
    set env(NDF_FROM_ASCII) \
	{$CONVERT_DIR/convertndf from '^fmt' '^dir' '^name' '^type' '^ndf'}

    set env(NDF_TO_ASCII) \
	{$CONVERT_DIR/convertndf to '^fmt' '^dir' '^name' '^type' '^ndf'}

    #  TEXT conversions (like ASCII but without FITS headers).
    #  ------------------------------------------------------
    set env(NDF_FROM_TEXT) \
	{$CONVERT_DIR/convertndf from '^fmt' '^dir' '^name' '^type' '^ndf'}

    set env(NDF_TO_TEXT) \
	{$CONVERT_DIR/convertndf to '^fmt' '^dir' '^name' '^type' '^ndf'}

    #  GIF conversions.
    #  ----------------
    set env(NDF_FROM_GIF) \
	{$CONVERT_DIR/convertndf from '^fmt' '^dir' '^name' '^type' '^ndf'}

    set env(NDF_TO_GIF) \
	{$CONVERT_DIR/convertndf to '^fmt' '^dir' '^name' '^type' '^ndf'}

    #  TIFF conversions.
    #  -----------------
    set env(NDF_FROM_TIFF) \
	{$CONVERT_DIR/convertndf from '^fmt' '^dir' '^name' '^type' '^ndf'}

    set env(NDF_TO_TIFF) \
	{$CONVERT_DIR/convertndf to '^fmt' '^dir' '^name' '^type' '^ndf'}

    #  COMPRESSED conversions.
    #  ----------------------
    #  N.B. These commands sometimes return an error status to the calling
    #  process, even when they appear to have worked OK. It's not clear why.
    set env(NDF_FROM_COMPRESSED) \
	{$CONVERT_DIR/convertndf from '^fmt' '^dir' '^name' '^type' '^ndf'}

    set env(NDF_TO_COMPRESSED) \
	{$CONVERT_DIR/convertndf to '^fmt' '^dir' '^name' '^type' '^ndf'}

    if {[info exists env(HDS_SCRATCH)]} {
	set env(NDF_TEMP_COMPRESSED) "$env(HDS_SCRATCH)/temp_Z_^name"
    } else {
	set env(NDF_TEMP_COMPRESSED) "temp_Z_^name"
    }
 
    #  GZIP compression conversions.
    #  -----------------------------
    set env(NDF_FROM_GZIP) \
	{$CONVERT_DIR/convertndf from '^fmt' '^dir' '^name' '^type' '^ndf'}

    set env(NDF_TO_GZIP) \
	{$CONVERT_DIR/convertndf to '^fmt' '^dir' '^name' '^type' '^ndf'}

    if {[info exists env(HDS_SCRATCH)]} {
	set env(NDF_TEMP_GZIP) "$env(HDS_SCRATCH)/temp_Z_^name"
    } else {
	set env(NDF_TEMP_GZIP) "temp_Z_^name"
    }
 
    #  DEBUG
    #  -----
    #  Switch off display all conversion operations for debugging.
    set env(NDF_SHCVT) 0
}
