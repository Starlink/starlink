$! VFLAG = F$VERIFY(0)
$!+
$!  Name:
$!     CCDBEND.COM
$!
$!  Type of Module:
$!     DCL command procedure.
$!
$!  Purpose:
$!     To tidy up after a CCDPACK batch procedure.
$!
$!  Description:
$!
$!  Authors:
$!     PDRAPER: Peter Draper (STARLINK)
$!     {enter_new_authors_here}
$!
$!  History:
$!     20-MAY-1991 (PDRAPER):
$!        Original Version.
$!     {enter_further_changes_here}
$!
$!  Bugs:
$!     {note_any_bugs_here}
$!
$!-
$! Clear up the mess. Delete the saved environment."
$!
$ DELETE/NOLOG/NOCONFIRM 'P1'*.*;*
$!
$ TOPDIR =  P1 - ( "." + P2 + "]" ) + "]" + P2 + ".DIR"
$ SET PROTECTION=(O:RWED) 'TOPDIR'
$!
$ DELETE/NOLOG/NOCONFIRM 'TOPDIR';
$!
$! IF( VFLAG ) THEN SET VERIFY
$! $Id$
