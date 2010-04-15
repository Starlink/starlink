      SUBROUTINE FV4_FILHDE( IFAIL )
      ENTRY          EDITFH( IFAIL )
*+
*  Name:
*     FV4_FILHDE

*  Purpose:
*     Change title and owner of file with spectra for Specx.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_FILHDE( IFAIL )

*  Description:
*     This routine serves the EDIT-FILE-HEADER command of Specx. It
*     reads the current title and owner from the file header and prompts
*     for new values.

*  Arguments:
*     IFAIL = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     08 Dec 1993 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Global Variables:
      INCLUDE 'FILES'            ! Open file information

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Variables:
      INTEGER IFILE              ! Internal number for this file
      INTEGER STATUS             ! Starlink status
      INTEGER JDEF               ! Returend by gen_getstr
      CHARACTER * ( 12 ) NAME    ! File owner
      CHARACTER * ( 40 ) ID      ! File title

*.

*  Check inherited global status.
      IF ( IFAIL .NE. 0 ) RETURN

*  Reset global status.
      IFAIL = 0

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Find out which file.
      CALL GETFIL( 'W', IFILE, IFAIL )
      IF ( IFAIL .NE. 0 ) GO TO 500

*  Read owner and title from file.
      CALL CMP_GET0C( TOPLOC(IFILE), 'NAME', NAME, STATUS )
      CALL CMP_GET0C( TOPLOC(IFILE), 'ID',     ID, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IFAIL = 38
         GO TO 500
      END IF

*  Get file title and owner.
      CALL GEN_GETSTR( 'File title?', ID,   'A',   ID, JDEF )
      CALL GEN_GETSTR( 'File owner?', NAME, 'A', NAME, JDEF )

*  Write owner and title to file.
      CALL CMP_PUT0C( TOPLOC(IFILE), 'NAME', NAME, STATUS )
      CALL CMP_PUT0C( TOPLOC(IFILE), 'ID',     ID, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IFAIL = 44
         GO TO 500
      END IF

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE

*  Return.
      END
