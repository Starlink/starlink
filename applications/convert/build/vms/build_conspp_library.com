$!+
$!  Name:
$!     BUILD_CONSPP_LIBRARY.COM
$!
$!  Purpose:
$!     Makes an object library CONSPP.OLB corresponding to all the
$!     source-code modules held in the text library CONSPP.TLB.
$!
$!  Type of Module:
$!     DCL command procedure.
$!
$!  Invocation:
$!     $@BUILD_CONSPP_LIBRARY
$!
$!  Authors:
$!     MJC: Malcolm J. Currie (STARLINK)
$!     {enter_new_authors_here}
$!
$!  History:
$!     1993 July 28 (MJC):
$!        Original version.
$!     {enter_changes_here}
$!
$!  Bugs:
$!     {note_any_bugs_here}
$!
$!-
$!
$!  Compile each module and place it in the object library.
$!
$ @[]BUILD_LIBRARY_MODULE CONSPP ADLINE.FOR
$ @[]BUILD_LIBRARY_MODULE CONSPP GETHIS.FOR
$ @[]BUILD_LIBRARY_MODULE CONSPP GETLIN.FOR
$ @[]BUILD_LIBRARY_MODULE CONSPP NHIST.FOR
$ @[]BUILD_LIBRARY_MODULE CONSPP NLINES.FOR
