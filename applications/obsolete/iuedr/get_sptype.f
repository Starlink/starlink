      SUBROUTINE GET_SPTYPE( SPTYPE, STATUS )
*+
*  Name:
*     SUBROUTINE GET_SPTYPE

*  Purpose:
*     Get SPECTYPE parameter value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GET_SPTYPE( SPTYPE, STATUS )

*  Arguments:
*     SPTYPE = INTEGER (Returned)
*        The SPECTRUM file type.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     03-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     09-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Returned:
      INTEGER SPTYPE

*  Status:
      INTEGER STATUS     ! Global status.

*  Local Variables:
      INTEGER ACTVAL     ! Parameter value count.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get SPECTYPE parameter, range check.
      DO WHILE ( .TRUE. )
         CALL RDPARI( 'SPECTYPE\\', .FALSE., 1, SPTYPE, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'SPECTYPE\\', STATUS )
            GO TO 999

         ELSE IF ( SPTYPE.LT.0 .OR. SPTYPE.GT.2 ) THEN
            CALL ERRPAR( 'SPECTYPE\\' )
            CALL ERROUT( ': must be either 0, 1 or 2\\', STATUS )

            CALL CNPAR( 'SPECTYPE\\', STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PCANER( 'SPECTYPE\\', STATUS )
               GO TO 999
            END IF

         ELSE
            GO TO 999
         END IF
      END DO

  999 CONTINUE

      END
