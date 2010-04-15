      SUBROUTINE FV4_FILCLS( IFAIL )
      ENTRY          FSYCLF( IFAIL )
*+
*  Name:
*     FV4_FILCLS

*  Purpose:
*     Close a file with spectra for Specx.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_FILCLS( IFAIL )

*  Description:
*     This routine serves the CLOSE-FILE command of Specx. it closes a
*     file with spectra for Specx.

*  Arguments:
*     IFAIL = INTEGER (Given and Returned)
*        The global status. This routine will execute even if the status
*        is bad on entry. The status is reset on entry.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     06 Dec 1993 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'FILES'            ! Open files information

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Variables:
      INTEGER IFILE              ! Internal number for this file
      INTEGER STATUS             ! Starlink status

*.

*  Reset global status.
      IFAIL = 0

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Find out which file to close.
      CALL GETFIL( 'X', IFILE, IFAIL )
      IF ( IFAIL .NE. 0 ) THEN
         WRITE( *, * ) ' -- filsys -- '
         WRITE( *, * ) '    Error in GETFIL'
         GO TO 500
      END IF

*  Annull the locator to the SPECTRUM array and close the file.
      CALL DAT_ANNUL( SPXLOC(IFILE), STATUS )
      CALL HDS_CLOSE( TOPLOC(IFILE), STATUS )

*  Release the slot in common blocks.
      FILELUNS(IFILE) = 0
      FILNAMS(IFILE) = ' '
      ACCESS(IFILE)  = ' '

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE

*  Return.
      END
