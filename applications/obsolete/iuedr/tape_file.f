      SUBROUTINE TAPE_FILE( FILE, STATUS )
*+
*  Name:
*     SUBROUTINE TAPE_FILE

*  Purpose:
*     Get DRIVE and FILE parameters and use them to position tape.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TAPE_FILE( FILE, STATUS )

*  Arguments:
*     FILE = INTEGER (Returned)
*        Number of file on tape.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     04-NOV-81 (JRG):
*         AT4 version.
*     26-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*     01-JUN-89 (PCTR):
*       IUEDR Vn. 2.1
*       Conversion to SGP/16 style.
*     08-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'

*  Arguments Returned:
      INTEGER FILE       ! Tape file number.

*  Status:
      INTEGER STATUS     ! Global status.

*  Local Variables:
      LOGICAL CURFIL     ! Whether current file is to be used.

      INTEGER ACTVAL     ! Parameter value count.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Try to open tape.
      CALL MT_OPEN( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERRPAR( 'DRIVE\\')
         CALL ERROUT( ': tape not available\\', STATUS )
         GO TO 999
      END IF

*  Force MT library to know about tape positions.
      CALL MT_FIX( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error fixing tape position\\', STATUS )
         GO TO 999
      END IF

*  FILE parameter - if not specified, get next tape file.
      CALL RDPARI( 'FILE\\', .FALSE., 1, FILE, ACTVAL, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_FLUSH( STATUS )
         CURFIL = .TRUE.

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         CURFIL = .FALSE.

      ELSE
         CALL PARFER( 'FILE\\', STATUS )
         GO TO 999
      END IF

*  Cancel FILE parameter
      CALL CNPAR( 'FILE\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PCANER( 'FILE\\', STATUS )
         GO TO 999
      END IF

*  Find next or requested tape file.
      CALL TAPE_FIND( CURFIL, FILE, STATUS )

 999  CONTINUE

      END
