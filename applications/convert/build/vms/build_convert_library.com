$!+
$!  Name:
$!     BUILD_CONVERT_LIBRARY.COM
$!
$!  Purpose:
$!     Makes an object library CONVERT.OLB corresponding to all the
$!     source-code modules held in the text library CONVERT.TLB.
$!
$!  Type of Module:
$!     DCL command procedure.
$!
$!  Invocation:
$!     $@BUILD_CONVERT_LIBRARY
$!
$!  Authors:
$!     MJC: Malcolm J. Currie (STARLINK)
$!     {enter_new_authors_here}
$!
$!  History:
$!     1993 July 28 (MJC):
$!        Original version.
$!     1993 September 2 (MJC):
$!        Added IRCAM2NDF.
$!     {enter_further_changes_here}
$!
$!  Bugs:
$!     {note_any_bugs_here}
$!
$!-
$!
$!  Compile each module and place it in the object library.
$!
$ @[]BUILD_LIBRARY_MODULE CONVERT ASCII2NDF.FOR
$ @[]BUILD_LIBRARY_MODULE CONVERT BDF2NDF.FOR
$ @[]BUILD_LIBRARY_MODULE CONVERT DIPSO2NDF.FOR
$ @[]BUILD_LIBRARY_MODULE CONVERT CONVERT.FOR
$ @[]BUILD_LIBRARY_MODULE CONVERT DST2NDF.FOR
$ @[]BUILD_LIBRARY_MODULE CONVERT GASP2NDF.FOR
$ @[]BUILD_LIBRARY_MODULE CONVERT IRAF2NDF.FOR
$ @[]BUILD_LIBRARY_MODULE CONVERT IRCAM2NDF.FOR
$ @[]BUILD_LIBRARY_MODULE CONVERT NDF2ASCII.FOR
$ @[]BUILD_LIBRARY_MODULE CONVERT NDF2BDF.FOR
$ @[]BUILD_LIBRARY_MODULE CONVERT NDF2DIPSO.FOR
$ @[]BUILD_LIBRARY_MODULE CONVERT NDF2DST.FOR
$ @[]BUILD_LIBRARY_MODULE CONVERT NDF2GASP.FOR
$ @[]BUILD_LIBRARY_MODULE CONVERT NDF2IRAF.FOR
$ @[]BUILD_LIBRARY_MODULE CONVERT NDF2UNF.FOR
$ @[]BUILD_LIBRARY_MODULE CONVERT UNF2NDF.FOR
