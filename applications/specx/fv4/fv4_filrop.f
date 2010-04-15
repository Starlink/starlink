      SUBROUTINE FV4_FILROP( IFAIL )
      ENTRY          FSYRES( IFAIL )
*+
*  Name:
*     FV4_FILROP

*  Purpose:
*     Re-open all files for Specx.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_FILROP( IFAIL )

* Description:
*     This routine serves the RESTART command of Specx. It tries to open
*     all files registered in the common blocks, and clears the slots in
*     the common blocks for which the file could not be opened. It may
*     also update the internal access mode to read only, if a file could
*     only be opened for read access.

*  Arguments:
*     IFAIL = INTEGER (Given and Returned)
*        The global status. This routine executes even if the status is
*        set on entry. It will reset the status on entry.

*  Prior Requirements:
*     The HDS system must have been started and the NDF system must have
*     been begun.

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
      INCLUDE 'FILES'            ! Open files information

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Variables:
      INTEGER IFILE              ! Loop index
      INTEGER STATUS             ! Starlink status
      CHARACTER * ( 8 ) VERSION  ! File format version

*.

*  Reset global status.
      IFAIL = 0

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Loop through all used files.
      DO 1001 IFILE = 1, MAX_DFILES
         IF ( FILELUNS(IFILE) .NE. 0 ) THEN

*        Try to open an existing file for update access.
            CALL HDS_OPEN( FILNAMS(IFILE), 'UPDATE',
     :         TOPLOC(IFILE), STATUS )

*        If file exists and can be updated.
            IF ( STATUS .EQ. SAI__OK ) THEN

*           Check that version 4.
               CALL CMP_GET0C( TOPLOC(IFILE), 'VERSION',
     :            VERSION, STATUS )
               IF ( STATUS .NE. SAI__OK .OR.
     :               VERSION(:3) .NE. 'V4.' ) THEN
                  CALL HDS_CLOSE( TOPLOC(IFILE), STATUS )
                  FILELUNS(IFILE) = 0
                  FILNAMS(IFILE)  = ' '
                  ACCESS(IFILE)   = ' '
               END IF

*           Locate the SPECTRUM array for the common blocks.
               CALL DAT_FIND( TOPLOC(IFILE), 'SPECTRUM',
     :            SPXLOC(IFILE), STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL HDS_CLOSE( TOPLOC(IFILE), STATUS )
                  FILELUNS(IFILE) = 0
                  FILNAMS(IFILE)  = ' '
                  ACCESS(IFILE)   = ' '
               END IF

*        Else (file cannot be updated).
            ELSE
               CALL ERR_ANNUL( STATUS )

*           Try to open the file for read access.
               CALL HDS_OPEN( FILNAMS(IFILE), 'READ',
     :            TOPLOC(IFILE), STATUS )

*           If file exists and can be read.
               IF ( STATUS .EQ. SAI__OK ) THEN

*              Restrict access.
                  ACCESS(IFILE) = 'R'

*              Check that version 4.
                  CALL CMP_GET0C( TOPLOC(IFILE), 'VERSION',
     :               VERSION, STATUS )
                  IF ( STATUS .NE. SAI__OK .OR.
     :                  VERSION(:3) .NE. 'V4.' ) THEN
                     CALL HDS_CLOSE( TOPLOC(IFILE), STATUS )
                     FILELUNS(IFILE) = 0
                     FILNAMS(IFILE)  = ' '
                     ACCESS(IFILE)   = ' '
                  END IF

*              Locate the SPECTRUM array for the common blocks.
                  CALL DAT_FIND( TOPLOC(IFILE), 'SPECTRUM',
     :               SPXLOC(IFILE), STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL HDS_CLOSE( TOPLOC(IFILE), STATUS )
                     FILELUNS(IFILE) = 0
                     FILNAMS(IFILE)  = ' '
                     ACCESS(IFILE)   = ' '
                  END IF

*           Else (file does not exist).
               ELSE
                  CALL ERR_ANNUL( STATUS )
                  FILELUNS(IFILE) = 0
                  FILNAMS(IFILE)  = ' '
                  ACCESS(IFILE)   = ' '
               END IF
            END IF
         END IF
 1001 CONTINUE

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE

*  Return.
      END
