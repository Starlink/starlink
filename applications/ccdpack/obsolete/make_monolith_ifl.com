$!+
$!  Name:
$!     MAKE_MONOLITH_IFL.COM
$!
$!  Purpose:
$!     To make the CCDPACK monolith IFL file.
$!
$!  Language:
$!     DCL
$!
$!  Description:
$!     The routine extracts all the modules from the CCDPACK_IFL.TLB
$!     library, writing the headers (including the monolith keyword)
$!     and the trailer endmonolith.
$!
$!  Authors:
$!     PDRAPER: Peter Draper (STARLINK)
$!     {enter_new_authors_here}
$!
$!  History:
$!     28-MAY-1991 (PDRAPER):
$!        Original version.
$!     {enter_changes_here}
$!
$!  Notes:
$!     This routine uses a read on the library extracted file
$!     as there is a file attribute clash, between files which have
$!     been OPENed and those created by lib/extract.
$!-
$!
$! extract all the interface modules from the library.
$!
$library/extract=*/output=ccdpack.txt ccdpack_libdir:ccdpack_ifl/text
$!
$! open the file and write the header.
$!
$open/write ifl_file ccdpack.ifl
$write ifl_file "#+"
$write ifl_file "#  Name:"
$write ifl_file "#     CCDPACK.IFL"
$write ifl_file " "
$write ifl_file "#  Type of Module:"
$write ifl_file "#     ADAM monolith parameter interface."
$write ifl_file " "
$write ifl_file "#  Authors:"
$write ifl_file "#     PDRAPER: Peter Draper (STARLINK)"
$write ifl_file "#     {enter_new_authors_here}"
$write ifl_file " "
$write ifl_file "#  History:"
$write ifl_file "#     ''f$time()' (PDRAPER):"
$write ifl_file "#     	 Original version."
$write ifl_file "#     {enter_changes_here}"
$write ifl_file " "
$write ifl_file "#-"
$write ifl_file ""
$write ifl_file "monolith CCDPACK"
$!
$! open the library listing and append this to the file.
$!
$open/read lib_file ccdpack.txt
$!
$! loop reading the file and writing to IFL file.
$!
$loop:
$read/end_of_file=next lib_file current_line
$write ifl_file "  ''current_line'"
$goto loop
$next:
$!
$! append endmonolith keyword
$!
$open/append ifl_file ccdpack.ifl
$write ifl_file "endmonolith"
$close ifl_file
$close lib_file
$!
$! tidy up
$!
$delete/noconf/nolog ccdpack.txt;
$!
$! compile it
$!
$compifl ccdpack
$!
$! end
$!
$exit      
$! $Id$
